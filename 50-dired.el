;; Dired config -*- lexical-binding: t; -*-

(require 'dired-git-info)
(require 'dired-hist)

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
  ;; (setopt dgi-commit-message-format "%s")
  ;; Disabling for now -- slow as hell on large dirs
  ;; (add-hook 'dired-after-readin-hook #'dired-git-info-auto-enable)
  ;; (add-hook 'dired-before-readin-hook #'my-dired-git-info-prevent-maybe) ;; doesnt prevent
  )

(after! dired-x
  (add-to-list 'dired-omit-extensions ".eshell-command-history")
  (add-to-list 'dired-omit-extensions ".eshell-scrollback"))

(after! async
  (dired-async-mode))

;; Show true folder sizes, but only if computer has duc, which is fast.
(after! dired-du
  (when (and (executable-find "duc")
             (not (string-match-p "Error" (my-process-output-to-string "duc" "info"))))
    (setopt dired-du-used-space-program '("duc" "ls -bD"))
    (run-with-timer 10 3600 #'my-index-duc) ;; index every hour
    (add-hook 'dired-mode-hook #'dired-du-mode)))
