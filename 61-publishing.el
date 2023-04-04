
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
      (let* ((pairs `(("[[:space:]]+" . "-")
                      ("[^[:alnum:][:digit:]\\/+=-]" . "")
                      ("\\/" . "-")
                      ("--*" . "-")
                      ("^-" . "")
                      ("-$" . "")
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
(add-hook 'org-export-before-parsing-functions #'my-replace-web-links-with-note-links-if-ref-exists)

(defun my-add-backlinks-if-roam (&rest _)
  (let (this-node
        backlinks
        reflinks)
    (when (ignore-errors (setq this-node (org-roam-node-at-point)))
      (dolist (obj (org-roam-backlinks-get this-node :unique t))
        (let ((node (org-roam-backlink-source-node obj)))
          (cl-pushnew (cons (org-roam-node-id node)
                            (org-roam-node-title node))
                      backlinks)))
      (dolist (obj (org-roam-reflinks-get this-node))
        (let* ((ref-node (org-roam-reflink-source-node obj)))
          (unless (equal ref-node this-node)
            (cl-pushnew (cons (org-roam-node-id ref-node)
                              (org-roam-node-title ref-node))
                        reflinks))))
      (when (or backlinks reflinks)
        (save-excursion 
          (if (bobp)
              (progn
                (goto-char (point-max))
                (insert "* What links here"))
            (org-insert-subheading nil)
            (insert "What links here"))
          (dolist (backlink backlinks)
            (newline)
            (insert "- [[id:" (car backlink) "][" (cdr backlink) "]]"))
          (dolist (reflink reflinks)
            (newline)
            (insert "- [[id:" (car reflink) "][" (cdr reflink) "]]")))))))

(defun my-replace-web-links-with-note-links-if-ref-exists (&rest _)
  (when (ignore-errors (org-roam-node-at-point))
    (let ((all-refs (org-roam-db-query
                     [:select [ref id title]
                      :from refs
                      :left-join nodes
                      :on (= refs:node-id nodes:id)])))
      (save-excursion
        (while (not (equal "No further link found" (org-next-link)))
          (let* ((elem (org-element-context))
                 (link (org-element-property :path elem))
                 (ref (assoc link all-refs)))
            (when (and ref
                       ;; ignore if same page
                       (not (equal (caddr ref) (org-get-title))))
              (delete-region (point) (org-element-property :end elem))
              (insert "[[id:" (cadr ref) "][" (caddr ref) "]]"))))))))

;; I'd like the final URLs on my website to exclude date.  So when publishing,
;; I'll have to work from a /tmp/roam/ copy of the roam dir, with the dates
;; stripped from filenames, and the org-id-locations table modified likewise, so
;; id links will still resolve correctly.  This sets that up.
(defun my-prep-fn (_)
  (setq my-real-orgids (copy-hash-table org-id-locations))
  (setq new (org-id-hash-to-alist org-id-locations))

  ;; Strip dates
  (setq new (cl-loop
             for pair in new
             if (string-match-p (rx bol (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "-")
                                (file-name-nondirectory (car pair)))
             collect (cons (concat (file-name-directory (car pair))
                                   (substring (file-name-nondirectory (car pair)) 11))
                           (cdr pair))
             else collect pair))

  (setq new (cl-loop
             for pair in new
             collect (cons (replace-regexp-in-string
                            ;; Flatten directory structure (put all posts into top dir)
                            (rx (group bol "/tmp/roam/") (* nonl) "/" )
                            "\\1"
                            (replace-regexp-in-string
                             ;; Pretend roam directory is /tmp/roam since we'll work from there
                             "^/home/kept/roam/"
                             "/tmp/roam/"
                             (replace-regexp-in-string
                              ;; Bonus: Include my Beorg files (they're outside roam dir to prevent
                              ;; potential issues from nesting Syncthing folders)
                              "^/home/sync-phone/beorg/"
                              "/home/kept/roam/beorg/"
                              (car pair))))
                           (cdr pair))))

  ;; ;; Bonus: Include my Beorg files (they're outside roam dir to prevent
  ;; ;; potential issues from nesting Syncthing folders)
  ;; (setq new (cl-loop
  ;;            for pair in new
  ;;            collect  (cons (replace-regexp-in-string
  ;;                            "^/home/sync-phone/beorg/" "/home/kept/roam/beorg/" (car pair))
  ;;                           (cdr pair))))
  ;; ;; Pretend roam directory is /tmp/roam since we'll work from there
  ;; (setq new (cl-loop
  ;;            for pair in new
  ;;            collect (cons (replace-regexp-in-string
  ;;                           "^/home/kept/roam/" "/tmp/roam/" (car pair))
  ;;                          (cdr pair))))
  ;; ;; Flatten directory structure (put all posts into top dir)
  ;; (setq new (cl-loop
  ;;            for pair in new
  ;;            collect (cons (replace-regexp-in-string
  ;;                           (rx (group bol "/tmp/roam/") (* nonl) "/" ) "\\1" (car pair))
  ;;                          (cdr pair))))
  (setq my-fake-orgids (org-id-alist-to-hash new))

  (shell-command "rm -rf /tmp/roam")
  (copy-directory "/home/kept/roam/" "/tmp/" t)
  ;; Bonus: include my Beorg files.
  (shell-command "rm /tmp/roam/beorg") ;; rm the symlink
  (copy-directory "/home/sync-phone/beorg/" "/tmp/roam/" t)

  ;; Strip dates from filenames
  (cl-loop
   for file in (directory-files-recursively
                "/tmp/roam"
                (rx bol (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "-" (+ nonl) ".org" eol)
                nil
                (lambda (dir)
                  (unless (string-search "daily" dir)
                    t)))
   do (rename-file file
                   (concat (file-name-directory file)
                           (substring (file-name-nondirectory file) 11))))
  ;; Move from subdirs into top level
  (cl-loop
   for file in (directory-files-recursively
                "/tmp/roam" "\\.org$"
                nil
                (lambda (dir)
                  (if (or (string-search "daily" dir)
                          (string-search "version-files" dir)
                          (string-search "bak" dir))
                      nil
                    t)))
   unless (equal (file-name-directory file) "/tmp/roam/")
   do (rename-file file "/tmp/roam/"))

  (fset 'org-id-update-id-locations #'ignore))

;; Struggled so long looking for a hook that would work like the old
;; before-export-hook.  Let this be a lesson.  The hook system exists to let you
;; subtly modify a function IN THE MIDDLE of its body. We never actually need
;; there to exist a before-hook, since in such a simple case it is always
;; possible to use add-function or write a wrapper like this.
(defun my-publish-to-blog (plist filename pub-dir)
  (let* ((org-inhibit-startup t)
         (visiting (find-buffer-visiting filename))
         (work-buffer (or visiting (find-file-noselect filename)))
         (org-html-extension ""))
    (unwind-protect
        (progn
          (setq org-id-locations my-fake-orgids)
          (org-publish-org-to 'html filename org-html-extension plist pub-dir))
      ;; I'd have put this cleanup in the project's :completion-function, but
      ;; it's not guaranteed to run when we trip an error.
      ;; Still, we could just spawn a subordinate Emacs for publishing, and do no cleanup at all.
      (setq org-id-locations my-real-orgids))

    (unwind-protect
        (with-current-buffer work-buffer
          (let* ((output-path (org-export-output-file-name org-html-extension nil pub-dir))
                 (output-buf (find-buffer-visiting output-path))
                 (was-opened nil)
                 (case-fold-search t)
                 (slug (string-replace pub-dir "" output-path))
                 (title (save-excursion
                          (when (search-forward "#+title: " nil t)
                            (buffer-substring (point) (line-end-position)))))
                 (created (save-excursion
                            (when (search-forward "#+date: " nil t)
                              (buffer-substring (1+ (point)) (+ 11 (point))))))
                 (updated (format-time-string "%F" (f-modification-time filename)))
                 (tags (or (sort (org-get-tags) #'string-lessp) '("")))
                 (refs (save-excursion
                         (when (search-forward ":roam_refs: " nil t)
                           (unless (search-backward "\n*" nil t)                             
                             (buffer-substring (point) (line-end-position))))))
                 (wordcount (save-excursion
                              (re-search-forward "^[^#:\n]" nil t)
                              (count-words (point) (point-max))))
                 (backlinks (save-excursion
                             (goto-char (point-max))
                             (when (search-backward "* What links here" nil t)
                               (cl-loop while (re-search-forward "^- " nil t)
                                        count t))))
                 (data `((slug . ,slug)
                         (title . ,title)
                         (created . ,created)
                         (updated . ,updated)
                         (wordcount . ,wordcount)
                         (backlinks . ,backlinks)
                         (refs . ,refs)
                         (tags . ,tags)
                         (content . nil))))
            (cond ((not (and title created))
                   (delete-file output-path)
                   (message "FILE DELETED: LACKING TITLE OR LACKING DATE: %s" output-path))
                  ;; This file has no body because it met :exclude-tags, idk why it gets created
                  ((= 0 (doom-file-size output-path))
                   (delete-file output-path)
                   (message "FILE DELETED BECAUSE EMPTY: %s" output-path))
                  ((not (org-id-get))
                   (delete-file output-path)
                   (message "FILE DELETED BECAUSE NO ID: %s" output-path))
                  ((seq-intersection tags '("noexport" "private" "censor" "drill" "fc" "anki"))
                   (delete-file output-path)
                   (message "FILE DELETED BECAUSE FOUND EXCLUDED-TAG: %s" output-path))
                  (t
                   (when output-buf
                     (setq was-opened t)
                     (unless (buffer-modified-p output-buf)
                       (kill-buffer output-buf)))
                   (with-temp-buffer
                     (goto-char (point-min))
                     (insert "<h1 id='title'>" title "</h1>")
                     (when refs
                       (insert "Reference: "  )
                       (dolist (ref (split-string refs))
                         (setq ref (string-replace "\"" "" ref))
                         (insert "<a href=\"" ref "\">" (replace-regexp-in-string "http.?://" "" ref) "</a> "))
                       (insert "<br>"))
                     (insert-file-contents output-path)

                     ;; Remove divs since they mess up the look of Bulma CSS.
                     ;; As an alternative, we could probably keep the divs
                     ;; classed .outline-[123456] and manually fix the slight
                     ;; misalignment of headlines (or give the divs the Bulma
                     ;; class .section).  These divs would be a handy CSS target
                     ;; if we want to emulate the look of org-indent-mode.
                     ;; Maybe it'd work better if they were spans, not divs?
                     (goto-char (point-min))
                     (while (re-search-forward "</?div.*?>" nil t)
                       (replace-match ""))

                     (setf (alist-get 'content data) (buffer-string)))
                   (with-temp-file output-path
                     (insert (json-encode data))
                     (when was-opened
                       (find-file-noselect output-path)))))))
      (unless visiting (kill-buffer work-buffer)))))

(setopt org-html-checkbox-type 'html)

;; Give headings their org-ids, so that hash links such as
;; #ID-e10bbdfe-1ffb-4c54-9228-2818afdfc5ba will make the browser jump to that heading.
;; (after! org
;;   (require 'org-roam-export))

(defvar my-real-orgids nil)
(defvar my-fake-orgids nil)
;; (fset 'my-org-id-update-id-locations-original #'org-id-update-id-locations)
;; (fset 'org-id-update-id-locations #'ignore)
(setopt org-export-use-babel nil)
(setopt org-export-with-broken-links t)
(setopt org-publish-project-alist
        '(("blag" ;; old one
           :base-directory "/home/kept/roam/blog/"
           :publishing-directory "/home/kept/blog/meedstrom.github.io/_posts/"
           :publishing-function org-md-publish-to-md)

          ("react-blog"
           :base-directory "/tmp/roam/"
           :publishing-directory "/home/kept/blog/posts/"
           :publishing-function my-publish-to-blog
           :recursive t
           :preparation-function my-prep-fn
           :with-toc nil
           :section-numbers nil
           :body-only t
           :exclude "daily/\\|logseq/"
           ;; this does not work! because of filetag?
           :exclude-tags ("noexport" "private" "censor" "drill" "fc" "anki"))))


;; WIP
;; see syntax on https://validator.w3.org/feed/docs/atom.html
(defun my-make-atom-feed ()
  (insert"<?xml version=\"1.0\" encoding=\"utf-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <title>Martin Edström's notes</title>
  <link href=\"http://example.org/\"/>
  <updated>2003-12-13T18:30:02Z</updated>
  <author>
    <name>Martin Edström</name>
  </author>
  <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>")

  ;; now do this for every month generated by my as yet nonexistent news generator
  (let ((title "Changelog March 2023")
        (updated "2023-04-01")
        (content "this string should be the entire content of the changes post"))
    (insert "
  <entry>
    <title>Atom-Powered Robots Run Amok</title>
    <link href=\"http://example.org/2003/12/13/atom03\"/>
    <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
    <updated>2003-12-13T18:30:02Z</updated>
    <summary>Some text.</summary>
    <content type=\"html\">a billion words go here</content>
  </entry>")
    )
  (insert "
</feed>"))

;; WIP
;; coalesce git logs for the month
;;
;; how should i flag dailies worth sharing?  the :star: tag i guess
;;
;; how should i add arbitrary text for the month?  i guess it would be easier if
;; I have a preexisting roam node named Changelog, and I write into it headings
;; for each month.  That way, this function would just plug in during
;; org-publish for this one file to expand each (preexisting) heading with lots
;; of info for that momnth.  and the heading must be formatted as Year Month.
(defun my-make-changelog ()
  )
