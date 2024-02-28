;; Completion-at-point (company/corfu) -*- lexical-binding: t; -*-

;; Because CAPFs don't do what I want in Roam
;;(add-hook 'org-mode-hook #'my-corfu-turn-off 99)
(setq global-corfu-modes '((not org-mode) t))

;; https://github.com/minad/corfu/wiki#same-key-used-for-both-the-separator-and-the-insertion
(defun my-complete-on-double-space ()
  "Makes sense only with `corfu-separator' 32 (space)."
  (interactive)
  (if current-prefix-arg
      ;;we suppose that we want leave the word like that, so do a space
      (progn
        (corfu-quit)
        (insert " "))
    (if (and (= (char-before) corfu-separator)
             (or
              ;; check if space, return or nothing after
              (not (char-after))
              (= (char-after) ?\s)
              (= (char-after) ?\n)))
        (progn
          (corfu-insert)
          (insert " "))
      (corfu-insert-separator))))

;; completion-at-point, TAB -> corfu-complete
;; RET -> corfu-insert
;; M-g -> corfu-show-location
;; M-h -> corfu-show-documentation
(use-package! corfu
  :init
  (setopt corfu-auto t)
  (setopt corfu-auto-delay 0.35)
  (setopt tab-always-indent 'complete) ;; or use M-TAB to complete
  :config
  (setopt completion-cycle-threshold 3)
  ;; invoke corfu for dabbrev instead of its own dabbrev-expand
  ;; (keymap-set [remap hippie-expand])
  ;; (global-corfu-mode)

  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell.
The idea is to avoid pressing RET twice; see README at
https://github.com/minad/corfu."
    (cond
     ((and (derived-mode-p 'eshell-mode)
           (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)
           (fboundp 'comint-send-input))
      (comint-send-input))))

  (advice-add #'corfu-insert :after #'corfu-send-shell)

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (bound-and-true-p vertico--input)
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (define-key corfu-map (kbd "SPC") #'my-complete-on-double-space)

  ;; Complete on punctuation
  ;; https://github.com/minad/corfu/wiki#tab-and-go-completion
  (dolist (c (list (cons "." ".")
                   (cons "," ",")
                   (cons ":" ":")
                   (cons ")" ")")
                   (cons "}" "}")
                   (cons "]" "]")))
    (define-key corfu-map (kbd (car c)) `(lambda ()
                                           (interactive)
                                           (corfu-insert)
                                           (insert ,(cdr c)))))
  )

(use-package! cape
  :config
  ;; https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-super-capf
                       #'eglot-completion-at-point
                       ;; #'tempel-expand
                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf))
