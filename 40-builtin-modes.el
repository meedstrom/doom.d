;; -*- lexical-binding: t; -*-

(save-place-mode)
(auto-save-visited-mode) ;; NOTE see undoom.el
(display-battery-mode)

(when (version<= "29" emacs-version)
  (pixel-scroll-precision-mode)
  ;; (add-to-list 'default-frame-alist '(alpha-background . 90))
  ;; (add-hook 'org-cycle-hook #'org-cycle-display-inline-images)
  )

(when (version<= "28" emacs-version)
  (context-menu-mode)
  (repeat-mode))
