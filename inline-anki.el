;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'anki-editor)

(defun anki-editor-push-notes ()
  (interactive)
  ;; org-fontify-emphasized-text should be t in all relevant buffers (don't
  ;; worry, this is a very uncommon default to disable), AND none of those buffers
  ;; are visited literally (with find-file-literally).  That is to say, when we
  ;; scan for flashcards, we need that those text bits wrapped in asterisks are in
  ;; fact given the 'bold text property in your Org buffers.  Org is already good
  ;; at rooting out false matches via org-do-emphasis-faces, so
  ;; inline-anki-convert-implicit-clozes follows in its tracks and look only at
  ;; the substrings Org already bolded.
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

(defun anki-editor-note-at-point ()
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
  (when (looking-at-p "[^}]+?}")
    (error "Attempted to create note for note already with ID"))
  (when (looking-at-p "[^}]")
    (error "no closing brace"))
  (if (looking-at-p "}[[:space:]]*?$")
      (insert (int-to-base64 id))
    (error "Note creation failed for unknown reason")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; As per RFC 4648 https://en.wikipedia.org/wiki/Base_64
(defconst base64-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defconst base62-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
(defconst base36-alphabet "0123456789abcdefghijklmnopqrstuvwxyz")

;; Unicode's entire Basic Multilingual Plane: from 0033 to FFFF.
;; (quiet! (setq base65503-alphabet (apply #'string (cl-loop for x from 33 to 65535 collect x))))

(defun int-to-base64 (num)
  "Re-express a number in base-64, and return that as a string.
See also the inverse, `base64-to-int'.  Do not confuse with
`base64-encode-string'."
  (let ((sign "")
        (b64-string ""))
    (when (> 0 num)
      (setq sign "-")
      (setq num (abs num)))
    (if (< num 64)
        ;; Simply return alphabet[num].
        (concat sign (char-to-string (aref base64-alphabet num)))
      ;; 64 or more, now it's complicated.
      (while (/= 0 num)
        (let* ((remainder (% num 64))
               ;; not sure it's necessary to do this interim result, but
               (integer-dividable-dividend (- num remainder))
               (quotient (/ integer-dividable-dividend 64)))
          (setq num quotient)
          (setq b64-string (concat (char-to-string (aref base64-alphabet remainder))
                                   b64-string))))
      (concat sign b64-string))))

(defun base64-to-int (b64-string)
  "Turn a number expressed as a base-64 string, into a base-10 integer.
Please note that you most likely want `base64-decode-string'\;
most uses of base-64 encoding are not meant to be decoded
mathematically as base-10 numbers, but as arrays of bytes, such
as strings of UTF-8 characters or binary \"blobs\" like the
on-disk content of an image file.  A number is A COMPLETELY DIFFERENT
THING from an array of bytes.  You have been warned."
  (let ((negative nil))
    (when (equal "-" (substring b64-string 0 1))
      (setq b64-string (substring b64-string 1))
      (setq negative t))
    (let* ((highest-place (length b64-string))
           (total (cl-loop
                   for i from 1 to highest-place
                   sum (let* ((glyph (char-to-string (aref b64-string (1- i))))
                              (glyph-value (string-search glyph base64-alphabet))
                              (place-value (^ 64 (- highest-place i))))
                         (* glyph-value place-value)))))
      (if negative
          (- total)
        total))))

;; (base64-to-int (int-to-base64 65342334))
;; (int-to-base64 (base64-to-int "D5Qt+"))
