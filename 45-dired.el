;; -*- lexical-binding: t; -*-
;; Copyright (C) 2020-2023 Martin Edström
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

(require 'dired-git-info)
(require 'dired-hist)

(remove-hook 'dired-mode-hook #'dired-omit-mode) ;; undoom
(add-hook 'dired-mode-hook #'dired-hide-details-mode) ;; press ( to toggle

(setopt wdired-allow-to-change-permissions 'advanced)
(setopt global-auto-revert-non-file-buffers t)
;; (setopt dired-du-size-format t) ;; human-readable
(setopt dired-recursive-copies 'always)

(after! dired
  (dired-hist-mode))

(after! dired-hacks
  (add-hook 'dired-mode-hook #'dired-collapse-mode))

(defun my-dired-git-info-prevent-maybe ()
  "Prevent Git Info mode in large directories."
  (let ((dirname
         (expand-file-name
          (if (consp dired-directory)
              (car dired-directory)
            dired-directory))))
    (when (> (length (directory-files dirname)) 30)
      (remove-hook 'dired-after-readin-hook #'dired-git-info-auto-enable 'local)
      ;; TODO: The above did not work, so there's still a long initial load time
      (dired-git-info-mode 0)
      )))

(after! dired-git-info
  ;; (setopt dgi-commit-message-format "%s") ;; undoom
  ;; Disabling for now -- slow as hell on large dirs
  ;; (add-hook 'dired-after-readin-hook #'dired-git-info-auto-enable)
  ;; (add-hook 'dired-before-readin-hook #'my-dired-git-info-prevent-maybe) ;; doesnt prevent
  )

(after! dired-x
  (add-to-list 'dired-omit-extensions ".eshell-command-history")
  (add-to-list 'dired-omit-extensions ".eshell-scrollback"))

(after! async
  (dired-async-mode))

;; Show true folder sizes, but only if we have duc, which is fast.  Orthodox
;; file managers solve this with laziness and async, also valid but this
;; approach seems it'll invite less bugs.
(after! dired-du
  (when (and (executable-find "duc")
             (not (string-match-p "Error" (my-process-output-to-string "duc" "info"))))
    (setopt dired-du-used-space-program '("duc" "ls -bD"))
    (add-hook 'dired-mode-hook #'dired-du-mode)))
