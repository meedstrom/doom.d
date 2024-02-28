;; -*- lexical-binding: t; -*-

(require 'named-timer)

(setq my-all-git-repos
      (seq-filter (lambda (x)
                    (and (file-directory-p x)
                         (member ".git" (directory-files x))))
                  ;; List of my git repos
                  (apply #'append
                         ;; Specific dirs
                         (list "~/doomemacs/.local/straight/repos/eva/"
                               "~/doomemacs/.local/straight/repos/deianira/"
                               "~/doomemacs/.local/straight/repos/chain/"
                               )
                         ;; Recursive (many subdirs of these are git repos)
                         (-keep (lambda (path)
                                  (if (file-directory-p path)
                                      (directory-files path t)
                                    (warn "Not found: %s" path)
                                    nil))
                                '("/home/kept/"
                                  "/home/kept/emacs/"
                                  "/home/kept/code/"
                                  "/home/kept/archive/uni/"
                                  )))))

;; duplicate repos (i make them sometimes)
(setq my-all-git-repos
      (--remove (s-matches-p (rx (or "deianira2" "eva2" "eva3" "secretary")) it)
                my-all-git-repos))

;; NOTE: you must have org-archive-location set to %s.org_archive:: or the stub
;; files will not register their archive co-files. You can work around it by
;; setting it locally with #+ARCHIVE: %s_archive::
;;
;; Or you can probably just set #+ARCHIVE: :: to designate everything in the
;; file as archive items, then you have a standalone agenda-file and attached
;; archive.
(defun my-memacs-scan-git ()
  (let ((my-archive-dir (shell-quote-argument "/home/kept/archive/memacs/git/")))
    (require 'f)
    (require 'cl-lib)
    (make-directory "/tmp/rev-lists" t)
    (and
     (executable-find "git")
     (executable-find "memacs_git")
     (bound-and-true-p my-all-git-repos)
     (dolist (repo my-all-git-repos t)
       (let ((default-directory repo))
         (start-process-shell-command
          "Memacs_Job_Git_1"
          nil
          (concat "git rev-list --all --pretty=raw > /tmp/rev-lists/"
                  (shell-quote-argument (file-name-nondirectory repo))))))
     (file-exists-p my-archive-dir)
     (run-with-timer
      5 nil `(lambda ()
               (dolist (revlist (directory-files "/tmp/rev-lists" t
                                                 (rx bol (not (any "." "..")))))
                 (unless (= 0 (doom-file-size revlist))
                   (let ((basename (shell-quote-argument (file-name-nondirectory revlist))))
                     (start-process
                      "Memacs_Job_Git_2" nil
                      "memacs_git" "-f" revlist "-o"
                      (concat ,my-archive-dir basename ".org_archive"))
                     ;; (f-touch (concat ,my-archive-dir basename ".org"))
                     (f-write "#+ARCHIVE: %s_archive::" 'utf-8
                              (concat ,my-archive-dir basename ".org"))
                     (customize-set-variable
                      'org-agenda-files
                      (cl-pushnew (concat ,my-archive-dir basename ".org")
                                  org-agenda-files :test #'equal)))))))))
  ;; Re-run myself in an hour.
  (run-with-timer (* 60 60) nil #'my-memacs-scan-git))

;; (defun my-memacs-scan-git ()
;;   (let ((my-archive-dir (shell-quote-argument "/home/kept/archive/memacs/git/")))
;;     (require 'f)
;;     (require 'cl-lib)
;;     (make-directory "/tmp/rev-lists" t)
;;     (and
;;      (executable-find "git")
;;      (executable-find "memacs_git")
;;      (bound-and-true-p my-all-git-repos)
;;      (dolist (repo my-all-git-repos t)
;;        (start-process-shell-command
;;           "Memacs_Job_Git_1"
;;           nil
;;           (concat "git rev-list --all --pretty=raw > /tmp/rev-lists/"
;;                   (shell-quote-argument (file-name-nondirectory repo)))))
;;      (file-exists-p my-archive-dir)
;;      (run-with-timer
;;       5 nil `(lambda ()
;;                (dolist (revlist (directory-files "/tmp/rev-lists" t
;;                                                  (rx bol (not (any "." "..")))))
;;                  (unless (= 0 (doom-file-size revlist))
;;                    (let ((basename (shell-quote-argument (file-name-nondirectory revlist))))
;;                      (start-process
;;                       "Memacs_Job_Git_2" nil
;;                       "memacs_git" "-f" revlist "-o"
;;                       (concat ,my-archive-dir basename ".org"))
;;                      (with-temp-file (concat ,my-archive-dir basename ".org")
;;                        (insert "#+ARCHIVE: ::\n")
;;                        (insert-file-contents-literally (concat ,my-archive-dir basename ".org")))
;;                      (customize-set-variable
;;                       'org-agenda-files
;;                       (cl-pushnew (concat ,my-archive-dir basename ".org")
;;                                   org-agenda-files :test #'equal))))))))))

(my-memacs-scan-git)

;; (named-timer-run :memacs-git 20 (* 60 60) #'my-memacs-scan-git)

(defun my-memacs-scan-git* ()
  (let ((my-archive-dir (shell-quote-argument "/home/kept/archive/memacs/git/"))
        (tmpdir "/tmp/rev-lists/"))
    (require 'f)
    (require 'cl-lib)
    (require 'pfuture)
    (make-directory tmpdir t)
    (and
     (file-exists-p my-archive-dir)
     (executable-find "git")
     (executable-find "memacs_git")
     (bound-and-true-p my-all-git-repos)
     (dolist (repo my-all-git-repos t)
       (let ((default-directory repo)
             (basename (shell-quote-argument (file-name-nondirectory repo))))
         (make-process
          :name "my-memacs-scan-git"
          :command (concat "git rev-list --all --pretty=raw > " tmpdir basename)
          :sentinel
          `(lambda ()
             (unless (= 0 (doom-file-size ,(concat tmpdir basename)))
               (pfuture-new "memacs_git" "-f" ,(concat tmpdir basename) "-o"
                            ,(concat my-archive-dir basename ".org_archive"))
               (f-touch ,(concat my-archive-dir basename ".org"))
               (cl-pushnew ,(concat my-archive-dir basename ".org")
                           org-agenda-files))))))))
  ;; Re-run myself in an hour.
  (run-with-timer (* 60 60) nil #'my-memacs-scan-git))
