;; -*- lexical-binding: t; -*-

(after! dired-git-info
  (setq dgi-commit-message-format "%s") ;; undoom
  (add-hook 'dired-after-readin-hook #'dired-git-info-auto-enable))

(after! dired-x
  (add-to-list 'dired-omit-extensions ".eshell-command-history")
  (add-to-list 'dired-omit-extensions ".eshell-scrollback"))
