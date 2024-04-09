;; Experiment zone -*- lexical-binding: t; -*-

(use-package! quickroam
  :disabled
  :config
  (setopt quickroam-extra-rg-args
          '("--glob" "**/*.org"
            "--glob" "!logseq/**"
            "--glob" "!noagenda/**"
            "--glob" "!*archive*")))

(hookgen doom-after-init-hook
  (setq my-stim-collection (my-stim-collection-generate)))

;; try to make delete-file fast again
(advice-remove 'delete-file #'delete-file-projectile-remove-from-cache)

(defun my-write-roam-graph-tsv ()
  (let ((tsv-string
         (concat "src\tdest\n"
                 (mapconcat (lambda (pair)
                              (concat (car pair) "\t" (cadr pair)))
                            (org-roam-db-query [:select [source dest]
                                                :from links
                                                :where (= type "id")])
                            "\n"))))
    (f-write tsv-string 'utf-8 "/tmp/org-roam-digraph.tsv")))

(when nil
  (my-write-roam-graph-tsv)
  (setq feedbacks (my-read-lisp (f-read "/tmp/feedback_arcs.el")))
  ;; Yup, already sorted by src
  ;; (equal feedbacks (cl-sort feedbacks #'string-lessp :key #'car))
  (prog1 nil
    (setq foo
          (cl-loop
           for (src . dest) in feedbacks
           concat (concat "\n" (org-roam-node-title (org-roam-node-from-id src))
                          " --> " (org-roam-node-title (org-roam-node-from-id dest)))
           ))))



;;; roam db

(after! org-roam-db
  (fset 'org-roam-db-update-file #'my-org-roam-db-update-file)
  (fset 'org-roam-db-sync #'my-org-roam-db-sync))

(defun my-org-roam-db-update-file (&optional file-path _)
  "Version of `org-roam-db-update-file' calling `my-org-roam-pre-scan-hook'."
  (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
  (let ((content-hash (org-roam-db--file-hash file-path))
        (db-hash (caar (org-roam-db-query [:select hash :from files
                                           :where (= file $s1)] file-path)))
        info)
    (unless (string= content-hash db-hash)
      (require 'org-ref nil 'noerror)
      (require 'oc nil 'noerror)
      (org-roam-with-file file-path nil
        (org-with-wide-buffer (run-hooks 'my-org-roam-pre-scan-hook))
        (emacsql-with-transaction (org-roam-db)
          (org-with-wide-buffer
           ;; please comment why
           (org-set-regexps-and-options 'tags-only)
           ;; Maybe not necessary anymore
           ;; 2021 https://github.com/org-roam/org-roam/issues/1844
           ;; 2023 https://code.tecosaur.net/tec/org-mode/commit/5ed3e1dfc3e3bc6f88a4300a0bcb46d23cdb57fa
           (org-refresh-category-properties)
           (org-roam-db-clear-file)
           (org-roam-db-insert-file content-hash)
           (org-roam-db-insert-file-node)
           ;; please comment why
           (setq org-outline-path-cache nil)
           (goto-char (point-min))
           (let ((end (point-max)))
             (when (re-search-forward org-outline-regexp-bol nil t)
               (while (progn
                        (when (org-roam-db-node-p)
                          (org-roam-db-insert-node-data)
                          (org-roam-db-insert-aliases)
                          (org-roam-db-insert-tags)
                          (org-roam-db-insert-refs))
                        (outline-next-heading)
                        (< (point) end)))))
           (setq org-outline-path-cache nil)
           (setq info (org-element-parse-buffer))
           (org-roam-db-map-links
            (list #'org-roam-db-insert-link))
           (when (featurep 'oc)
             (org-roam-db-map-citations
              info
              (list #'org-roam-db-insert-citation)))
           ))
        ))))

;; Make the processing message more informative
(defun my-org-roam-db-sync (&optional force)
  "Synchronize the cache state with the current Org files on-disk.
If FORCE, force a rebuild of the cache from scratch."
  (interactive "P")
  (org-roam-db--close) ;; Force a reconnect
  (when force (delete-file org-roam-db-location))
  (org-roam-db) ;; To initialize the database, no-op if already initialized
  (org-roam-require '(org-ref oc))
  (let* ((gc-cons-threshold org-roam-db-gc-threshold)
         (org-agenda-files nil)
         (org-roam-files (org-roam-list-files))
         (current-files (org-roam-db--get-current-files))
         (modified-files nil))
    (dolist (file org-roam-files)
      (let ((contents-hash (org-roam-db--file-hash file)))
        (unless (string= (gethash file current-files)
                         contents-hash)
          (push file modified-files)))
      (remhash file current-files))
    (emacsql-with-transaction (org-roam-db)
      ;; Bruh. Just load compat.
      (org-roam-dolist-with-progress (file (hash-table-keys current-files))
          "Clearing removed files..."
        (org-roam-db-clear-file file))
      ;; Unfortunately it's good for debugging to show which file you got
      ;; stuck on. So we can't use the message log combination feature with
      ;; the dolist with ...
      ;; I've always felt that the "Processing modified files...38%" was too
      ;; little information.
      (let ((ctr 0))
        (dolist (file modified-files)
          (message "Processing modified files... (%d/%d) %s"
                   (cl-incf ctr)
                   (length modified-files)
                   (file-name-nondirectory file))
          (condition-case err
              (org-roam-db-update-file file)
            (error
             (org-roam-db-clear-file file)
             (lwarn 'org-roam :error "Failed to process %s with error %s, skipping..."
                    file (error-message-string err)))))))))

;; Try to implement a timeout

;; (after! org-roam
;;   (defun org-roam-file-p (&optional file)
;;     "Return t if FILE is an Org-roam file, nil otherwise.
;; If FILE is not specified, use the current buffer's file-path.

;; FILE is an Org-roam file if:
;; - It's located somewhere under `org-roam-directory'
;; - It has a matching file extension (`org-roam-file-extensions')
;; - It doesn't match excluded regexp (`org-roam-file-exclude-regexp')"
;;     (setq file (or file (buffer-file-name (buffer-base-buffer))))
;;     (when file
;;       (save-match-data
;;         (let* ((relative-path (file-relative-name file org-roam-directory))
;;                (ext (org-roam--file-name-extension file))
;;                (ext (if (or (string= ext "gpg")
;;                             (string= ext "age"))
;;                         (org-roam--file-name-extension (file-name-sans-extension file))
;;                       ext))
;;                (org-roam-dir-p (org-roam-descendant-of-p file org-roam-directory))
;;                (valid-file-ext-p (member ext org-roam-file-extensions))
;;                (match-exclude-regexp-p
;;                 (cond
;;                  ((not org-roam-file-exclude-regexp) nil)
;;                  ((stringp org-roam-file-exclude-regexp)
;;                   (string-match-p org-roam-file-exclude-regexp relative-path))
;;                  ((listp org-roam-file-exclude-regexp)
;;                   (let (is-match)
;;                     (dolist (exclude-re org-roam-file-exclude-regexp)
;;                       (setq is-match (or is-match (string-match-p exclude-re relative-path))))
;;                     is-match))))
;;                (top-level-exclude-p
;;                 (with-temp-buffer
;;                   (insert-file-contents file)
;;                   ;; Bound the search to before the first heading
;;                   (let ((end (re-search-forward "\n *\\*" nil t)))
;;                     (goto-char (point-min))
;;                     (re-search-forward "^ *:roam_exclude: +t$" end t))))
;;                (has-title-p
;;                 (with-temp-buffer
;;                   (insert-file-contents file)
;;                   (search-forward "\n#+title" nil t))))
;;           (and
;;            file
;;            org-roam-dir-p
;;            valid-file-ext-p
;;            (not match-exclude-regexp-p)
;;            has-title-p
;;            (not top-level-exclude-p)))))))



;; (let ((then (current-time))
;;       (retry t))
;;   (while retry
;;     (if (eq 'done (while-no-input
;;                     (my-org-roam-db-try-update content-hash)))
;;         (setq retry nil)
;;       (when (> (float-time (time-since then)) 10)
;;         (warn "Timed out processing file %s" file-path)))))

;; (defun my-org-roam-db-try-update (content-hash)
;;   (emacsql-with-transaction (org-roam-db)
;;     (org-with-wide-buffer
;;      ;; please comment why
;;      (org-set-regexps-and-options 'tags-only)
;;      ;; Maybe not necessary anymore
;;      ;; 2021 https://github.com/org-roam/org-roam/issues/1844
;;      ;; 2023 https://code.tecosaur.net/tec/org-mode/commit/5ed3e1dfc3e3bc6f88a4300a0bcb46d23cdb57fa
;;      ;; (org-refresh-category-properties)
;;      (org-roam-db-clear-file)
;;      (org-roam-db-insert-file content-hash)
;;      (org-roam-db-insert-file-node)
;;      ;; please comment why
;;      (setq org-outline-path-cache nil)
;;      (org-roam-db-map-nodes
;;       (list #'org-roam-db-insert-node-data
;;             #'org-roam-db-insert-aliases
;;             #'org-roam-db-insert-tags
;;             #'org-roam-db-insert-refs))
;;      (setq org-outline-path-cache nil)
;;      (setq info (org-element-parse-buffer))
;;      (org-roam-db-map-links
;;       (list #'org-roam-db-insert-link))
;;      (when (featurep 'oc)
;;        (org-roam-db-map-citations
;;         info
;;         (list #'org-roam-db-insert-citation)))))
;;   'done)

;; (setq counsel-ffdata-database-path "/home/me/.mozilla/firefox/wrki7yvc.dev-edition-default/places.sqlite")
;; (setq helm-firefox-bookmark-user-directory "/home/me/.mozilla/firefox/wrki7yvc.dev-edition-default/")

;; ;; Fix
;; (defun helm-get-firefox-user-init-dir (directory)
;;   "Guess the default Firefox user directory name."
;;   (with-temp-buffer
;;     (insert-file-contents
;;      (expand-file-name "profiles.ini" directory))
;;     (goto-char (point-min))
;;     (search-forward "Default=1")
;;     (search-backward "Path=")
;;     (file-name-as-directory (expand-file-name
;;                              (buffer-substring-no-properties
;;                               (match-end 0) (point-at-eol))
;;                              directory))))


;; Fix: don't publish files that have a #+FILETAGS matching :exclude-tags
(after! ox-publish
  (defun org-publish-get-base-files (project)
    "Return a list of all files in PROJECT."
    (let* ((base-dir (file-name-as-directory
                      (org-publish-property :base-directory project)))
           (extension (or (org-publish-property :base-extension project) "org"))
           (match (if (eq extension 'any) ""
                    (format "^[^\\.].*\\.\\(%s\\)$" extension)))
           (base-files
            (cond ((not (file-exists-p base-dir)) nil)
                  ((not (org-publish-property :recursive project))
                   (cl-remove-if #'file-directory-p
                                 (directory-files base-dir t match t)))
                  (t
                   ;; Find all files recursively.  Unlike to
                   ;; `directory-files-recursively', we follow symlinks
                   ;; to other directories.
                   (letrec ((files nil)
                            (walk-tree
                             (lambda (dir depth)
                               (when (> depth 100)
                                 (error "Apparent cycle of symbolic links for %S"
                                        base-dir))
                               (dolist (f (file-name-all-completions "" dir))
                                 (pcase f
                                   ((or "./" "../") nil)
                                   ((pred directory-name-p)
                                    (funcall walk-tree
                                             (expand-file-name f dir)
                                             (1+ depth)))
                                   ((pred (string-match match))
                                    (push (expand-file-name f dir) files))
                                   (_ nil)))
                               files)))
                     (funcall walk-tree base-dir 0))))))

      (org-uniquify
       (append
        ;; Files from BASE-DIR.  Apply exclusion filter before adding
        ;; included files.
        (let* ((exclude-regexp (org-publish-property :exclude project))
               (exclude-tags (org-publish-property :exclude-tags project))
               (filtered-by-regexp
                (if exclude-regexp
                    (cl-remove-if
                     (lambda (f)
                       ;; Match against relative names, yet BASE-DIR file
                       ;; names are absolute.
                       (string-match exclude-regexp
                                     (file-relative-name f base-dir)))
                     base-files)
                  base-files)))
          (if exclude-tags
              (cl-remove-if
               (lambda (f)
                 (cl-intersection exclude-tags (my-org-file-tags f)
                                  :test #'string-equal-ignore-case))
               filtered-by-regexp)
            filtered-by-regexp))
        ;; Sitemap file.
        (and (org-publish-property :auto-sitemap project)
             (list (expand-file-name
                    (or (org-publish-property :sitemap-filename project)
                        "sitemap.org")
                    base-dir)))
        ;; Included files.
        (mapcar (lambda (f) (expand-file-name f base-dir))
                (org-publish-property :include project))))))

  (defun org-publish-get-project-from-filename (filename &optional up)
    "Return a project that FILENAME belongs to.
When UP is non-nil, return a meta-project (i.e., with a :components part)
publishing FILENAME."
    (let* ((filename (expand-file-name filename))
           (project
            (cl-some
             (lambda (p)
               ;; Ignore meta-projects.
               (unless (org-publish-property :components p)
                 (let ((base (expand-file-name
                              (org-publish-property :base-directory p))))
                   (cond
                    ;; Check if FILENAME is explicitly included in one
                    ;; project.
                    ((cl-some (lambda (f) (file-equal-p f filename))
                              (mapcar (lambda (f) (expand-file-name f base))
                                      (org-publish-property :include p)))
                     p)
                    ;; Exclude file names matching :exclude property.
                    ((let ((exclude-re (org-publish-property :exclude p)))
                       (and exclude-re
                            (string-match-p exclude-re
                                            (file-relative-name filename base))))
                     nil)
                    ;; Check :extension.  Handle special `any'
                    ;; extension.
                    ((let ((extension (org-publish-property :base-extension p)))
                       (not (or (eq extension 'any)
                                (string= (or extension "org")
                                         (file-name-extension filename)))))
                     nil)
                    ;; Check if FILENAME belong to project's base
                    ;; directory, or some of its sub-directories
                    ;; if :recursive in non-nil.
                    ((member filename (org-publish-get-base-files p)) p)
                    (t nil)))))
             org-publish-project-alist)))
      (cond
       ((not project) nil)
       ((not up) project)
       ;; When optional argument UP is non-nil, return the top-most
       ;; meta-project effectively publishing FILENAME.
       (t
        (letrec ((find-parent-project
                  (lambda (project)
                    (or (cl-some
                         (lambda (p)
                           (and (member (car project)
                                        (org-publish-property :components p))
                                (funcall find-parent-project p)))
                         org-publish-project-alist)
                        project))))
          (funcall find-parent-project project)))))))
