;; -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Martin Edström
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


;; Which directories do you want to auto-commit?
(defvar my-auto-commit-dirs
  '("/home/kept/roam/"
    "/home/me/.doom.d/"))

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
