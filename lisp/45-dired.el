;; -*- lexical-binding: t; -*-

(require 'dired-git-info)

(after! dired
  (bind-key "b"      #'dired-up-directory dired-mode-map)
  (bind-key ")"      #'dired-git-info-mode dired-mode-map)
  (bind-key "M-<up>" #'dired-up-directory dired-mode-map)
  (bind-key "s-RET"  #'my-dired-open-file-with-default-tool dired-mode-map))

(remove-hook 'dired-mode-hook #'dired-omit-mode) ;; undoom
(add-hook 'dired-mode-hook #'dired-hide-details-mode) ;; press ( to toggle

(setq wdired-allow-to-change-permissions 'advanced)
(setc global-auto-revert-non-file-buffers t)
;; (setc dired-du-size-format t) ;; human-readable
(setc dired-recursive-copies 'always)

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
  (setq dgi-commit-message-format "%s") ;; undoom
  (add-hook 'dired-before-readin-hook #'my-dired-git-info-prevent-maybe)
  ;; Disabling for now -- long load time on large dirs
  ;; (add-hook 'dired-after-readin-hook #'dired-git-info-auto-enable)
  )

(after! dired-x
  (add-to-list 'dired-omit-extensions ".eshell-command-history")
  (add-to-list 'dired-omit-extensions ".eshell-scrollback"))

(after! async
  (dired-async-mode))

;; Show true folder sizes, but only if we have duc, which is fast. Orthodox
;; file managers get away with laziness and async, which I could fall back on
;; (and should be a dired default), but instant.
;(after 'dired-du-autoloads
;  (when (and (executable-find "duc")
;             (not (string-match-p "Error" (my-process-output-to-string "duc" "info"))))
;    (setc dired-du-used-space-program '("duc" "ls -bD"))
;    (add-hook 'dired-mode-hook #'dired-du-mode)))

;; undoom
(after! dired
  (bind-key "q" #'kill-current-buffer dired-mode-map))
