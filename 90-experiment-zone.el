;; Experiment zone -*- lexical-binding: t; -*-

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



;;; Saner db stuff

;; At first, I didn't like that there's an `emacsql-with-transaction' at such a
;; high level in `org-roam-db-sync' since the emacsql docstring says there
;; should be no side effects and that's harder to ensure with such deeply
;; nested code.  Then I read the source and I see why.  It's a beautiful
;; algorithm.  Anyway there's no problem running a lot of sanity checks in BODY
;; that gets re-tried when the database is busy, it's just slow.
;;
;; Come to think.  Since org-roam-db-sync can take minutes, it's still a bad
;; idea to use `emacsql-with-transaction'.  We don't want to re-run for
;; minutes!  Tho it's never happened to me so not really an issue, but it
;; would be ideal to find a way to do all the heavy calculations only once.
;;
;; Say org-roam builds a big list of stuff to push to sql, then attempts one
;; tiny emacsql-with-transaction sexp at the end.  Might not be hard to hack.
;; Just override org-roam-db-query so it pushes its arguments onto a list.

;; Aaanyway, probs not the origin of bugs so leave it.

(after! org-roam-db
  (defun org-roam-db-update-file (&optional file-path _)
    "Update Org-roam cache for FILE-PATH.

If the file does not exist anymore, remove it from the cache.

If the file exists, update the cache with information.

If NO-REQUIRE, don't require optional libraries. Set NO-REQUIRE
when the libraries are already required at some toplevel, e.g.
in `org-roam-db-sync'."
    (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
    (let ((content-hash (org-roam-db--file-hash file-path))
          (db-hash (caar (org-roam-db-query [:select hash :from files
                                             :where (= file $s1)] file-path)))
          info)
      (unless (string= content-hash db-hash)
        ;; org-roam-require should be either removed or named
        ;; org-roam-soft-require for clarity.
        ;; anyway, it does not make the code easier to read.  DRY is taken too
        ;; far if it's just an alias: thin wrappers over commonly known
        ;; functions to call those functions with the same args every time.
        ;; the fewer sexps in the codebase that start with "org-roam-", the
        ;; easier it is to get into.
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
             ;; (org-refresh-category-properties)
             (org-roam-db-clear-file)
             (org-roam-db-insert-file content-hash)
             (org-roam-db-insert-file-node)
             ;; please comment why
             (setq org-outline-path-cache nil)
             (org-roam-db-map-nodes
              (list #'org-roam-db-insert-node-data
                    #'org-roam-db-insert-aliases
                    #'org-roam-db-insert-tags
                    #'org-roam-db-insert-refs))
             (setq org-outline-path-cache nil)
             (setq info (org-element-parse-buffer))
             (org-roam-db-map-links
              (list #'org-roam-db-insert-link))
             (when (featurep 'oc)
               (org-roam-db-map-citations
                info
                (list #'org-roam-db-insert-citation)))))
          ))))

  (defun org-roam-db-sync (&optional force)
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
  )

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
