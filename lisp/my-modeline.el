;; -*- lexical-binding: t; -*-

(setq mode-line-percent-position nil)

(use-package! mini-modeline
  :config (mini-modeline-mode))

(after! mini-modeline
  (set-face-background 'mini-modeline-mode-line "#001100")
  (column-number-mode)
  (setq mini-modeline-face-attr '(:background "#001100" :foreground "pale green"))
  (setq mini-modeline-r-format
        (seq-difference mini-modeline-r-format
                        '((:eval (string-trim (format-mode-line mode-line-modes)))
                          mode-line-mule-info))))

;; (after! feebleline
;;   (setq feebleline-msg-functions
;;         '((feebleline-line-number :post "" :fmt "%5s")
;;           (feebleline-column-number :pre ":" :fmt "%-2s")
;;           (feebleline-file-directory :face feebleline-dir-face :post "")
;;           (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
;;           (feebleline-file-modified-star :face font-lock-warning-face :post "")
;;           (feebleline-git-branch :face feebleline-git-face :pre " - "))))

(after! doom-modeline
  (setq doom-modeline-buffer-encoding nil)
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
  (size-indication-mode 0))
