;; -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Martin Edström <meedstrom91@gmail.com>
;;
;; Description: Make Anki Cards in Org-mode
;; Author: Martin Edström
;; Version:
;; Package-Requires: ((emacs "25") (request "0.3.0") (dash "2.12.0"))
;; URL:

;;; Commentary:

;; TODO: send breadcrumbs as an extra field

;;; Code:

;; (require 'inline-anki-fork)
(load "/home/me/.doom.d/wip/inline-anki-anki-editor-fork")

(defun inline-anki-push-notes ()
  (interactive)
  ;; Check that necessary variables are on.
  (when (and (eq major-mode 'org-mode)
             org-fontify-emphasized-text)
    (let ((failed 0)
          (times
           (inline-anki-map-note-things
            (lambda ()
              (message "Processing notes in buffer \"%s\", wait a moment..." (buffer-name))
              (condition-case-unless-debug err
                  (inline-anki--push-note (inline-anki-note-at-point))
                (error
                 ;; (cl-incf failed)
                 (message "Note at point %d failed: %s" (point) (error-message-string err)))))))))))

(defvar inline-anki-default-tags '("from_emacs"))

(defun inline-anki-note-at-point ()
  "Construct an alist representing a note from current entry."
  (let* ((org-trust-scanner-tags t)
         (deck (or (org-entry-get-with-inheritance inline-anki-prop-deck)
                   inline-anki-deck))
         (note-id (string-to-number (inline-anki-thing-id)))
         (note-type (car inline-anki-note-type))
         (tags (append inline-anki-default-tags (org-get-tags) (list (format-time-string "%F"))))
         (begin (org-element-property :contents-begin (org-element-at-point)))
         (end (save-excursion
                (goto-char (org-element-property :contents-end (org-element-at-point)))
                (re-search-backward "[_^]{" begin)
                (point)))
         (fields (list (cons (cadr inline-anki-note-type)
                             (inline-anki-convert-implicit-clozes
                              (buffer-substring begin end)))
                       )))

    (unless deck (error "No deck specified"))
    (unless note-type (error "Missing note type"))
    (unless fields (error "Missing fields"))

    `((deck . ,deck)
      (note-id . ,(if (= 0 note-id)
                      -1
                    note-id))
      (note-type . ,note-type)
      (tags . ,tags)
      (fields . ,fields))))

(defun inline-anki--set-note-id (id)
  (unless id
    (error "Note creation failed for unknown reason"))
  (goto-char (line-beginning-position))
  (re-search-forward (rx (literal inline-anki-flag) (or "^" "_") "{"))
  (when (looking-at-p "[[:digit:]]+?}[[:space:]]*?$")
    (error "Attempted to create note for note already with ID"))
  (when (looking-at-p "anki}[[:space:]]*?$")
    (insert (number-to-string id))
    (delete-char 4)))

(defvar inline-anki-note-type '("Cloze" "Text"))

(defun inline-anki-thing-id ()
  (goto-char (line-beginning-position))
  (let ((end (if (org-at-item-p)
                 ;; List item
                 (save-excursion
                   (org-end-of-item)
                   (1- (point)))
               ;; Paragraph
               (save-excursion
                 (org-forward-paragraph)
                 (1- (point))))))
    ;; (rx "@" (or "_" "^" "anki"))
    (re-search-forward (rx "@" (or "_" "^") "{" (group (*? nonl)) "}") end)
    (let ((id-maybe (match-string 1)))
      (if (equal id-maybe "")
          (error "id empty")
        id-maybe))))

;; the anki note ID is always a 13-digit number.
(defun inline-anki-map-note-things (func)
  (let* ((ctr 0)
         (list-bullet (rx (or (any "-+*") (seq (*? digit) (any ".)")))))
         (card@item-start&has-id  (rx bol (*? space) (regexp list-bullet) (+? space) "@" (any "_^") "{" (= 13 digit) "}"))
         (card@item-start&new (rx bol (*? space) (regexp list-bullet) (+? space) "@anki"))
         (card@eol&has-id (rx (? "@") (any "_^") "{" (= 13 digit) "}" (*? space) eol))
         (card@eol&new (rx (or "@anki" "_{anki}" "^{anki}") (*? space) eol)))
    (dolist (regexp '(card@item-start&has-id
                      card@item-start&new
                      card@eol&has-id
                      card@eol&new))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        ;; (forward-char -1)
        (cl-incf ctr)
        (funcall func)))
    ctr))

(defcustom inline-anki-emphasis-type '(bold)
  "Which emphasis type to parse as implicit clozes.
Set this to '(bold), '(italic), or '(underline)."
  :type 'sexp)

(defun inline-anki-convert-implicit-clozes (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((n 0))
      (while (setq prop (text-property-search-forward 'face inline-anki-emphasis-type t))
        (let ((num (number-to-string (cl-incf n))))
          (goto-char (prop-match-beginning prop))
          (delete-char -1)
          (insert "{{c" num "::")
          (goto-char (+ (prop-match-end prop) 4 (length num)))
          (delete-char 1)
          (insert "}}"))))
    ;; (buffer-string)
    (buffer-substring-no-properties (point-min) (point-max))))

(defvar inline-anki-deck "Default")

(provide 'inline-anki)
