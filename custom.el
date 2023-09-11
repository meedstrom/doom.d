(put 'customize-themes 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'customize-variable 'disabled nil)
(put 'projectile-grep 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'customize-changed 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'customize-set-variable 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'View-quit 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-values
   '((elisp-lint-indent-specs (describe . 1) (it . 1) (thread-first . 0)
      (cl-flet . 1) (cl-flet* . 1) (org-element-map . defun)
      (org-roam-dolist-with-progress . 2) (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1) (magit-insert-section . defun)
      (magit-section-case . 0) (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")
     (org-refile-targets quote
      (("/home/kept/roam/noagenda/2021-08-27-somedaymaybe.org" :maxlevel . 3)))
     (org-refile-targets quote
      (("/home/kept/roam/20210827184025-someday_maybe.org" :maxlevel . 3)))
     (nameless-current-name . "my") (org-confirm-babel-evaluate)
     (org-drill-scope . directory) (require-final-newline . t)
     (require-final-newline))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
