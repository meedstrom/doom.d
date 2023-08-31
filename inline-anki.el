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
                  (anki-editor--push-note (anki-editor-note-at-point))
                (error
                 ;; (cl-incf failed)
                 (message "Note at point %d failed: %s" (point) (error-message-string err)))))))))))

(defvar inline-anki-default-tags '("from-emacs"))

(defun inline-anki-note-at-point ()
  "Construct an alist representing a note from current entry."
  (let* ((org-trust-scanner-tags t)
         (deck (or (org-entry-get-with-inheritance anki-editor-prop-deck)
                   inline-anki-deck))
         (note-id (inline-anki-thing-id))
         (note-type (car inline-anki-note-type))
         (tags (seq-union inline-anki-default-tags (org-get-tags-at)))
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
      (note-id . ,(if note-id
                      (base64-to-int note-id)
                    -1))
      (note-type . ,note-type)
      (tags . ,tags)
      (fields . ,fields))))

(defun anki-editor--set-note-id (id)
  (unless id
    (error "Note creation failed for unknown reason"))
  (goto-char (line-beginning-position))
  (re-search-forward (rx (literal inline-anki-flag) (or "^" "_") "{"))
  (when (looking-at-p "[[:digit:]]+?}[[:space:]]*?$")
    (error "Attempted to create note for note already with ID"))
  (when (looking-at-p "anki}[[:space:]]*?$")
    (insert id)))

(defvar inline-anki-note-type '("Cloze" "Text"))

;; Unused
(defcustom inline-anki-flag ""
  "A string for flagging a paragraph or list item as a flashcard.
I recommend this string to be quite unique, so if you change your
mind you can confidently search-replace across all your files.
If you were to set this to a common string like \"#\", you will
need more effort to migrate from it.  To also have it compact,
the trick is to pick some Unicode symbol you never use otherwise,
like this card emoji: 🃏.")

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
    (re-search-forward (rx (literal inline-anki-flag) (or "_" "^") "{" (group (*? nonl)) "}") end)
    (let ((id-maybe (match-string 1)))
      (if (equal id-maybe "")
          ;; No ID yet
          nil
        id-maybe))))

(defun inline-anki-map-note-things (func)
  (let ((ctr 0)
        (start-of-item-rx (rx bol (*? space) (any "+-") (+? space) (literal inline-anki-flag) (?? (or "^" "_")) "{" (group (*? nonl)) "}"))
        (eol-rx "[[:graph:]][_^]{\\(.*?\\)}$"))
    (goto-char (point-min))
    (while (re-search-forward eol-rx nil t)
      (forward-char -1)
      (cl-incf ctr)
      (funcall func))
    (goto-char (point-min))
    (while (re-search-forward start-of-item-rx nil t)
      (forward-char -1)
      (cl-incf ctr)
      (funcall func))
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
