;; -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Martin Edström <meedstrom91@gmail.com>
;;
;; Description: Make Anki Cards in Org-mode
;; Author: Martin Edström
;; Version:
;; Package-Requires: ((emacs "25") (request "0.3.0") (dash "2.12.0"))
;; URL:

;;; Commentary:

;; TODO: command to auto-process an entire directory

;; TODO: send breadcrumbs as an extra field

;;; Code:

;; (require 'inline-anki-anki-editor-fork)
(load "/home/me/.doom.d/wip/inline-anki-anki-editor-fork")

(defcustom inline-anki-deck "Default"
  "Name of deck to upload to."
  :type string)

;; (setq inline-anki-deck "Mixed")

(defcustom inline-anki-note-type '("Cloze" "Text")
  "Name of note type and its fields, with note-type as first item."
  :type '(repeat string))

(defcustom inline-anki-default-tags nil
  "Tags to always include."
  :type '(repeat string))

(defcustom inline-anki-emphasis-type '(bold)
  "Which emphasis type to parse as implicit clozes.
Set this to '(bold), '(italic), or '(underline)."
  :type 'sexp)

(defcustom inline-anki-subscript-new-notes nil
  "Whether to subscript instead of superscript."
  :type 'boolean)

(defun inline-anki-push-notes ()
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not org-mode"))
  (unless org-fontify-emphasized-text
    (error "Inline-anki relies on `org-fontify-emphasized-text'"))
  (inline-anki-map-note-things
   (lambda ()
     (message "Processing notes in buffer \"%s\", wait a moment..." (buffer-name))
     (condition-case-unless-debug err
         (inline-anki--push-note (inline-anki-note-at-point))
       (error (message "Note at point %d failed: %s"
                       (point)
                       (error-message-string err)))))))



(defun inline-anki-note-at-point ()
  "Construct an alist representing a note at point."
  (let* (;;(org-trust-scanner-tags t)
         (deck inline-anki-deck)
         (note-id (inline-anki-thing-id))
         (note-type (car inline-anki-note-type))
         (tags (append inline-anki-default-tags
                       (mapcar #'substring-no-properties (org-get-tags))
                       (list (format-time-string "from-emacs-%F"))))
         ;; Flag at start of list item? Exclude it from note text.
         (begin (save-excursion
                  ;; Org's magic puts us at at start of the list item after the
                  ;; bullet point
                  (goto-char (org-element-property
                              :contents-begin (org-element-at-point)))
                  (unless (search-forward "@anki" (+ 6 (point)) t)
                    (re-search-forward (rx "@" (any "_^") "{" (= 13 digit) "}")
                                       (+ 18 (point))
                                       t))
                  (point)))
         ;; Flag at end of line?  Exclude it from note text.
         (end (save-excursion
                ;; Alas, (org-element-property :contents-end) jumps past all
                ;; sub-items of a list-item.  So we just line-orient and hope
                ;; that the user doesn't hard-wrap.
                (goto-char (line-end-position))
                (unless (re-search-backward (rx "@anki" (*? space) eol) begin t)
                  (re-search-backward (rx (?? "@") (any "_^") "{" (*? nonl) "}" (*? space) eol)
                                      begin
                                      t))
                (point)))
         ;; Drop text into the first field, however that's labeled
         (fields (list (cons (cadr inline-anki-note-type)
                             (inline-anki-convert-implicit-clozes
                              (buffer-substring begin end))))))

    (unless deck (error "No deck specified"))
    (unless note-type (error "Missing note type"))
    (unless fields (error "Missing fields"))

    `((deck . ,deck)
      (note-id . ,note-id)
      (note-type . ,note-type)
      (tags . ,tags)
      (fields . ,fields))))

(defun inline-anki--set-note-id (id)
  (unless id
    (error "Note creation failed for unknown reason"))
  (goto-char (line-beginning-position))
  (let ((eol-flag (rx (any "_^") "{" (group "anki") "}" (*? space) eol)))
    (cond
     ((search-forward "@anki" (line-end-position) t)
      (delete-char -4)
      (insert "^{" (number-to-string id) "}"))

     ((re-search-forward eol-flag (line-end-position) t)
       (replace-match (number-to-string id) t t nil 1))

     (t
      (error "Inline-anki magic string not found on line")))))

(defun inline-anki-thing-id ()
  (save-excursion
    (let ((beg (line-beginning-position))
          (bound (progn
                   (if (org-at-item-p)
                       (org-end-of-item)
                     (org-forward-paragraph))
                   (point))))
      (goto-char beg)
      (if (re-search-forward (rx (any "_^") "{" (group (= 13 digit)) "}") bound t)
          (string-to-number (match-string 1))
        -1))))

;; TOOD: detect if a line is a comment, and ignore
;; TODO: detect if there is more than one flag on the same line (run all
;; searches upfront, record the line numbers, then check for duplicates among
;; the recorded line numbers)
(defun inline-anki-map-note-things (func)
  "Run FUNC with point on each flashcard in the buffer.
Note: FUNC should not move point backwards, or you can get an
infinite loop."
  (let* ((ctr 0)
         (list-bullet (rx (or (any "-+*") (seq (*? digit) (any ".)")))))
         (card@item-start&has-id (rx bol (*? space) (regexp list-bullet) (+? space) "@" (any "_^") "{" (= 13 digit) "}"))
         (card@item-start&new (rx bol (*? space) (regexp list-bullet) (+? space) "@anki"))
         (card@eol&has-id (rx (? "@") (any "_^") "{" (= 13 digit) "}" (*? space) eol))
         (card@eol&new (rx (or "@anki" "_{anki}" "^{anki}") (*? space) eol)))
    (dolist (regexp (list card@item-start&has-id
                          card@item-start&new
                          card@eol&has-id
                          card@eol&new))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (forward-char -1)
        (cl-incf ctr)
        (funcall func)))
    ctr))

(defun inline-anki-convert-implicit-clozes (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((n 0))
      (while (setq prop (text-property-search-forward
                         'face inline-anki-emphasis-type t))
        (let ((num (number-to-string (cl-incf n))))
          (goto-char (prop-match-beginning prop))
          (delete-char -1)
          (insert "{{c" num "::")
          (goto-char (+ (prop-match-end prop) 4 (length num)))
          (delete-char 1)
          (insert "}}")))
      (when (= n 0)
        (error "No clozes in note: %s" text)))
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'inline-anki)
