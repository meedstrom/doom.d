;; -*- lexical-binding: t; -*-

(require 'dired-git-info) ;; must load for the hook to work

(remove-hook 'dired-mode-hook #'dired-omit-mode) ;; undoom

(setc wdired-allow-to-change-permissions t)
(setc global-auto-revert-non-file-buffers t)
;; (setc dired-du-size-format t) ;; human-readable
(setc dired-recursive-copies 'always)

(after! dired-hacks
  (add-hook 'dired-mode-hook #'dired-collapse-mode))

(after! dired-git-info
  (setq dgi-commit-message-format "%s") ;; undoom
  (add-hook 'dired-after-readin-hook #'dired-git-info-auto-enable))

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

;; (add-hook 'dired-mode-hook #'my-trunc-lines)
(add-hook 'dired-mode-hook #'dired-hide-details-mode) ;; press ( to toggle
;; (add-hook 'dired-mode-hook #'dired-omit-mode)
