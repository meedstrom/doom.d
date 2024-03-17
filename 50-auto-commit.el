;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Auto-commit on save in specific directories.
;;
;; Multiple auto-commits in a row, i.e. commits with the message "Auto-commit",
;; are amended if it's still the same day.  In typical conditions, that means
;; your git log ends up showing one commit per day.

;;; Code:

(defvar my-auto-commit-dirs
  '("/home/kept/roam/"
    ;; "/home/kept/roam/organice/"
    "/home/me/.doom.d/")
  "Which directories do you want to auto-commit?")

(defun my-auto-commit-maybe ()
  "Create a new commit if the last was on a different day.
Otherwise just amend today's commit.

Only operate if the project root directory is a member of
`my-auto-commit-dirs'.

If there are untracked files, do nothing and print a message,
because in this situation it's more possible that the user will
want to do the commits manually.

Suitable on `after-save-hook'."
  (require 'magit)
  (require 'project)
  (when (and (project-current)
             (member (project-root (project-current)) my-auto-commit-dirs))
    (let ((last-commit-date (shell-command-to-string
                             "git log -n 1 --pretty=format:%cs"))
          (last-commit-msg (shell-command-to-string
                            "git log -n 1 --pretty=format:%s")))
      (if (string-search "Fatal" last-commit-date)
          (message "Git failed, probably not a Git repo: %s" default-directory)
        ;; Special case for Org-Roam: auto-stage new notes, bc it happens often
        (and (equal "org" (file-name-extension (buffer-file-name)))
             (string-search org-roam-directory default-directory)
             (magit-run-git "add" (buffer-file-name)))

        (if (magit-untracked-files)
            (message "Won't auto-commit.  Stage untracked files or edit .gitignore")
          ;; TODO: check if you pushed this auto-commit to origin. then we need
          ;;       a new commit
          (if (and (equal last-commit-date (format-time-string "%F"))
                   (equal last-commit-msg "Auto-commit"))
              ;; Same day, so amend today's autocommit
              (magit-commit-amend '("--all" "--reuse-message=HEAD"))
            ;; New day, new commit
            (magit-commit-create '("--all" "--message=Auto-commit"))))))))

(define-minor-mode my-auto-commit-mode ""
  :global t
  (if my-auto-commit-mode
      (add-hook 'after-save-hook #'my-auto-commit-maybe)
    (remove-hook 'after-save-hook #'my-auto-commit-maybe)))

(my-auto-commit-mode)
