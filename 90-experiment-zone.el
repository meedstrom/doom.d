;; Experiment zone -*- lexical-binding: t; -*-

;; Yep it's slow.  I see why org-roam-db-sync is slow - it's actually kind of
;; fast for what it does.
(defun quickroam-resolve-backlinks ()
  "Using ripgrep, find all backlinks to current file."
  (interactive)
  (unless (org-roam-file-p)
    (error "Not an org-roam file %s" (buffer-name)))
  (let ((heading-ids-in-file nil)
        (backlinks nil)
        (default-directory org-roam-directory))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (org-roam-db-node-p)
          (push (org-id-get) heading-ids-in-file))
        (outline-next-heading)))
    (dolist (id heading-ids-in-file)
      (let* ((rg-result (apply #'quickroam--program-output "rg"
                               `("--line-number"
                                 "--no-heading"
                                 "--with-filename"
                                 "--only-matching"
                                 "--replace" ""
                                 ,@quickroam-extra-rg-args
                                 ,(concat "\\[\\[id:" id "\\]"))))
             (file-lnum-alist (--map (string-split it ":" t)
                                     (string-split rg-result "\n" t)))
             (by-file (-group-by #'car file-lnum-alist))
             (n-files (length by-file))
             (ctr 0))
        (dolist (data by-file)
          (org-roam-with-file (expand-file-name (car data) org-roam-directory) nil
            ;; (message "Visiting... (%d/%d) %s" (cl-incf ctr) n-files (car data))
            (cl-loop
             for lnum in (cdadr data)
             do (progn
                  (goto-line (string-to-number lnum))
                  (org-back-to-heading-or-point-min)
                  (cl-loop until (or (bobp) (org-roam-db-node-p))
                           do (org-roam-up-heading-or-point-min))
                  (push (list :src-title (if (org-before-first-heading-p)
                                             (org-get-title)
                                           (nth 4 (org-heading-components)))
                              :src-id (org-id-get))
                        backlinks)))))))
    (message "%s" backlinks)))


;;; Crap

(defvar qic (make-hash-table :test #'equal))
(defun qic-make ()
  (interactive)
  (clrhash qic)
  (cl-loop for node being the hash-values of quickroam-cache
           do (progn ;; (plist-put node :backlinks nil)
                (puthash (plist-get node :id) node qic))))

(defun qic-peek ()
  (interactive)
  (let ((rows (hash-table-values qic)))
    (dotimes (_ 4)
      (print (nth (random (length rows)) rows)))))

;; Should probably use org-ql, not ripgrep directly.
;;
;; Anyway, this proof of concept shows that a naive ripgrep approach would
;; limit me to just storing the line-number where a link was found.  Can't
;; detect the local subtree.
(defun quickroam-seek-backlinks ()
  (interactive)
  (let* ((default-directory org-roam-directory)
         (results (apply #'quickroam--program-output "rg"
                         `("--line-number"
                           "--only-matching"
                           "--replace" "$1"
                           ,@quickroam-extra-rg-args
                           "\\[\\[id:(.+?)\\]"))))
    (dolist (line (string-split results "\n" t))
      (let* ((splits (string-split line ":"))
             (file (pop splits))
             (lnum (pop splits))
             (id (string-join splits)))
        (if (gethash id qic)
            (push (list :src-file file
                        :src-lnum (string-to-number lnum)
                        :src-title nil
                        :src-id nil)
                  (plist-get (gethash id qic) :backlinks))
          (message "not found %s" id))))))

(defun quickroam-resolve-backlinks* ()
  (interactive)
  (let ((all-backlinks (--mapcat (plist-get it :backlinks)
                                 (hash-table-values qic))))
    (dolist (node (hash-table-values qic))
      (cl-loop
       for (file . backlinks) in (--group-by (plist-get it :src-file)
                                             all-backlinks)
       do (org-roam-with-file (expand-file-name file org-roam-directory) nil
            (message "Visiting... %s" file)
            (cl-loop
             for backlink in backlinks
             do (progn
                  (cl-letf ((push-mark #'ignore)
                            (set-mark #'ignore))
                    (goto-line (plist-get backlink :src-lnum))
                    (org-back-to-heading-or-point-min)
                    (cl-loop until (or (org-roam-db-node-p) (bobp))
                             do (org-roam-up-heading-or-point-min)))
                  (plist-put backlink :src-title
                             (if (org-before-first-heading-p)
                                 (org-get-title)
                               (nth 4 (org-heading-components))))
                  (plist-put backlink :src-id (org-id-get)))))))))


;;

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
