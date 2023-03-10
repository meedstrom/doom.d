
;; Modified version of `org-roam-node-slug'
(defun my-slugify (title)
    (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s) (string-glyph-compose
                                              (apply #'string
                                                     (seq-remove #'nonspacing-mark-p
                                                                 (string-glyph-decompose s)))))
                 (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* (

               ;; (pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
               ;;        ("--*" . "-")                   ;; remove sequential underscores
               ;;        ("^-" . "")                     ;; remove starting underscore
               ;;        ("-$" . "")))

               (pairs `(
                        ("[[:space:]]+" . "-")
                        ("[^[:alnum:][:digit:]\\/+=-]" . "")
                        ("\\/" . "-")
                        ("--*" . "-")
                        ("^-" . "")
                        ("-$" . "")
                        ;; ("-a-" . "-")
                        ;; ("-the-" . "-")
                        ;; ("-i-" . "-")
                        ;; ("-in-" . "-")
                        ;; ("-of-" . "-")
                        ;; ("-is-" . "-")
                        ;; ("-to-" . "-")
                        ;; ("-as-" . "-")
                        ;; ("-that-" . "-")
                        ;; ("-are-" . "-")
                        ;; ("-you-" . "-")
                        ("-\\+-" . "+")
                        ("-=-" . "=")
                        ))


               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (downcase slug)))))

;; (my-slugify "A/B testing")
;; (my-slugify "Someday/Maybe whale carcass")
;; (my-slugify "No one can feel a probability that small")
;; (my-slugify "\"But there's still a chance, right?\"")
;; (my-slugify "Löb's Theorem")
;; (my-slugify "How to convince me that 2 + 2 = 3")
;; (my-slugify "C. S. Peirce")
;; (my-slugify "Do one thing at a time")
;; (my-slugify "Are you losing items in recentf, bookmarks, org-id-locations? Solution: Run kill-emacs-hook periodically.")
;; (my-slugify "Slimline/\"pizza box\" computer chassis")

(defun my-rename-roam-file-by-title (&optional path title)
  (interactive)
  (unless path
    (setq path (buffer-file-name)))
  (unless (equal "org" (file-name-extension path))
    (error "Unexpected that file doesn't end in .org, halting on: %s" path))
  (unless title
    (with-temp-buffer
      (insert-file-contents path)
      (let ((case-fold-search t))
        (setq title (save-excursion
                      (goto-char (point-min))
                      (when (search-forward "#+title: " nil t)
                        (buffer-substring (point) (line-end-position))))))))
  (let* ((filename-preamble
          (when (string-match-p (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit))
                                (file-name-nondirectory path))
            (substring (file-name-nondirectory path) 0 10)))
         (slugified-path (concat (file-name-directory path)
                                 filename-preamble
                                 "-"
                                 (my-slugify title)
                                 ".org"))
         (visiting (find-buffer-visiting path)))
    (unless (equal slugified-path path)
      (if (and visiting (buffer-modified-p visiting))
          (message "Unsaved file, letting it be: %s" path)
        (when visiting
          (kill-buffer visiting))
        (and (file-writable-p path)
             (file-writable-p slugified-path)
             (rename-file path slugified-path))
        (when visiting
          (find-file slugified-path))))))

(add-hook 'org-export-before-parsing-functions #'my-add-backlinks-if-roam)

(defun my-add-backlinks-if-roam (&rest _)
  (when (ignore-errors (org-roam-node-at-point))
    (let ((backlinks nil))
      (dolist (obj (org-roam-backlinks-get (org-roam-node-at-point)
                                           :unique t))
        (let ((node (org-roam-backlink-source-node obj)))
          (cl-pushnew (cons (org-roam-node-id node) (org-roam-node-title node)) backlinks)))
      (when backlinks
        (if (bobp)
            (progn
              (goto-char (point-max))
              (insert "* What links here"))
          (org-insert-subheading nil)
          (insert "What links here"))
        (dolist (backlink backlinks)
          (newline)
          (insert "- [[id:" (car backlink) "][" (cdr backlink) "]]"))))))


(defun my-publish-to-blog2 (plist filename pub-dir)
  ;; (my-rename-roam-file-by-title filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (let* ((case-fold-search t)
           (title (save-excursion
                    (goto-char (point-min))
                    (when (search-forward "#+title: " nil t)
                      (buffer-substring (point) (line-end-position)))))
           (planted-date (save-excursion
                   (goto-char (point-min))
                   (when (search-forward "#+date: " nil t)
                     (buffer-substring (1+ (point)) (1- (line-end-position))))))
           (org-html-extension ""))

      ;; Black box
      (org-publish-org-to 'html filename org-html-extension plist pub-dir)

      ;; Some logic borrowed from `org-publish-org-to' 2023-02-02
      (let* ((org-inhibit-startup t)
             (visiting (find-buffer-visiting filename))
             (work-buffer (or visiting (find-file-noselect filename))))
        (unwind-protect
            (with-current-buffer work-buffer
              (let* ((output (org-export-output-file-name org-html-extension nil pub-dir))
                     (output-buf (find-buffer-visiting output))
                     (was-opened nil)
                     (relative-slug (replace-regexp-in-string "^.*/posts/" "" output))
                     (data `((slug . ,relative-slug)
                             (title . ,title)
                             (date . ,planted-date)
                             (content . nil))))
                (unless (and title planted-date)
                  (delete-file output)
                  (message "FILE LACKING TITLE OR DATE: %s" output))
                ;; This file has no body because it met :exclude-tags, idk why it gets created
                (if (= 0 (doom-file-size output))
                    (progn
                      (delete-file output)
                      (message "File deleted because empty: %s" output))
                  (when (and title planted-date)
                    (when output-buf
                      (setq was-opened t)
                      (unless (buffer-modified-p output-buf)
                        (kill-buffer output-buf)))
                    (with-temp-file output
                      (insert
                       (with-temp-buffer
                         (insert "\n<h1>" title "</h1>")
                         (insert "\n<p>Planted " planted-date "</p>")
                         (insert-file-contents output)

                         ;; Adjust the result from `my-add-backlinks-if-roam'
                         (goto-char (point-max))
                         (when (search-backward "What links here<" nil t)
                           (goto-char (line-beginning-position))
                           (while (search-forward "h2" (line-end-position) t)
                             (replace-match "h1" nil t)))

                         (setf (alist-get 'content data) (buffer-string))
                         (json-encode data))))
                    (when was-opened
                      (find-file-noselect output))))))
          (unless visiting (kill-buffer work-buffer))))
      )))


;; Struggled so long looking for a hook that would work like the old
;; before-export-hook.  Let this be a lesson.  We never actually need there to
;; exist before-hooks or after-hooks, since it is always possible to use
;; add-function or write a wrapper like this.  The hook system exists to let you
;; subtly modify a function in the middle of its body.
(defun my-publish-to-blog (plist filename pub-dir)
  ;; (my-rename-roam-file-by-title filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (let* ((case-fold-search t)
           (title (save-excursion
                    (when (search-forward "#+title: " nil t)
                      (buffer-substring (point) (line-end-position)))))
           (date (save-excursion
                   (when (search-forward "#+date: " nil t)
                     (buffer-substring (1+ (point)) (+ 11 (point))))))
           (wordcount (save-excursion
                        (re-search-forward "^[^#:\n]" nil t)
                        (count-words (point) (point-max))))
           (tags (org-get-tags))
           (updated (format-time-string "%F" (f-modification-time filename)))
           (org-html-extension ""))

      (org-publish-org-to 'html filename org-html-extension plist pub-dir)

      ;; Some modified logic from `org-publish-org-to' that was pasted on 2023-02-02
      ;;
      ;; 2023-02-19: Dafuq? There is an `org-publish-after-publishing-hook' that
      ;; I prolly could've used instead of this defun -- not that it makes much
      ;; difference, I'm just ticked I didn't discover it just because there's
      ;; no "export" in the variable name.
      (let* ((org-inhibit-startup t)
             (visiting (find-buffer-visiting filename))
             (work-buffer (or visiting (find-file-noselect filename))))
        (unwind-protect
            (with-current-buffer work-buffer
              (let* ((output (org-export-output-file-name org-html-extension nil pub-dir))
                     (output-buf (find-buffer-visiting output))
                     (was-opened nil)
                     (relative-slug (replace-regexp-in-string "^.*/posts/" "" output)))
                (cond ((not (and title date))
                       (delete-file output)
                       (message "FILE LACKING TITLE OR DATE: %s" output))
                      ;; This file has no body because it met :exclude-tags, idk why it gets created
                      ((= 0 (doom-file-size output))
                       (delete-file output)
                       (message "FILE DELETED BECAUSE EMPTY: %s" output))
                      ((and title date)
                       (when output-buf
                         (setq was-opened t)
                         (unless (buffer-modified-p output-buf)
                           (kill-buffer output-buf)))
                       ;; Hand-craft a JSON object.  Apparently, Elisp's
                       ;; `json-encode' doesn't escape quotes enough for
                       ;; javascript's JSON.parse() to understand (the latter
                       ;; seems shitty overall with shitty error messages).
                       ;;
                       ;; TODO: Try using json-encode again, think the problem
                       ;; is fixed somehow
                       (with-temp-file output
                         ;; JSON
                         (insert "  {")
                         (insert "\n    \"slug\": \"" relative-slug "\",")
                         (insert "\n    \"title\": \"" (string-replace "\"" "\\\"" title) "\",")
                         (insert "\n    \"date\": \"" date "\",")
                         (insert "\n    \"updated\": \"" updated "\",")
                         (insert "\n    \"tags\": [\"" (string-join tags "\",\"") "\"],")
                         (insert "\n    \"wordcount\": " (number-to-string wordcount) ",")
                         (insert "\n    \"content\": \"")
                         (setq content-start-pos (point))

                         ;; Content
                         (insert "<h1>" title "</h1>")
                         (insert "<p>Planted " date "<br />Updated " updated "</p>")
                         (insert-file-contents output)

                         ;; Adjust the result from `my-add-backlinks-if-roam'
                         (goto-char (point-max))
                         (when (search-backward "What links here<" nil t)
                           (goto-char (line-beginning-position))
                           (while (search-forward "h2" (line-end-position) t)
                             (replace-match "h1" nil t))
                           (when (search-forward "outline-text-2" nil t)
                             (replace-match "backlinks-text" t t))
                           (when (search-backward "outline-2" nil t)
                             (replace-match "backlinks-div" t t)))

                         ;; Logic from `json--print-string'. Thanks!
                         (goto-char content-start-pos)
                         (while (re-search-forward (rx (in "\"" "\\" cntrl)) nil t)
                           (let ((char (preceding-char)))
                             (delete-char -1)
                             (insert "\\" (or
                                          ;; Special JSON character (\n, \r, etc.).
                                          (car (rassq char json-special-chars))
                                          ;; Fallback: UCS code point in \uNNNN form.
                                          (format "u%04x" char)))))

                         ;;                          ;; JSON again
                         ;;                          (goto-char content-start-pos)
                         ;;                          (while (search-forward "\\" nil t)
                         ;;                            (replace-match "\\\\" nil t))
                         ;;                          (goto-char content-start-pos)
                         ;;                          (while (search-forward "
                         ;; " nil t)
                         ;;                            (replace-match "\\n" nil t))

                         ;; ;; For some reason, I need triple-escaped quotes for
                         ;; ;; where I call JSON.parse() in my Javascript.
                         ;; (goto-char content-start-pos)
                         ;; (while (search-forward "\\\"" nil t)
                         ;;   (replace-match "\\\\\\\"" nil t))
                         ;; (goto-char (point-max))

                         (insert "\"\n  }")
                         )
                       (when was-opened
                         (find-file-noselect output))))))
          (unless visiting (kill-buffer work-buffer)))))))

(setopt org-html-checkbox-type 'html)
(setopt org-publish-project-alist
        ;; TODO: Rename the exported file as a Jekyll-compatible slug, so I don't need
        ;; the original filename to be any particular way.
        '(("blag"
           :base-directory "/home/kept/roam/blog/"
           :publishing-directory "/home/kept/blog/meedstrom.github.io/_posts/"
           :publishing-function org-md-publish-to-md
           )
          ("react-blog"
           :base-directory "/home/kept/roam/"
           :publishing-directory "/home/kept/blog/baz/baz-backend/posts/"
           :publishing-function my-publish-to-blog
           :recursive t
           :preparation-function (lambda (_) (setopt org-export-use-babel nil)
                                   (setopt org-export-with-broken-links t))
           :completion-function (lambda (_) (setopt org-export-use-babel t)
                                  (setopt org-export-with-broken-links nil))
           :with-toc nil
           :section-numbers nil
           :body-only t
           :exclude "daily/"
           ;; this does not work! because filetag?
           :exclude-tags ("noexport" "private" "personal" "censor" "drill" "fc")
           )))
