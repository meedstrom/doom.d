;; -*- lexical-binding: t; -*-
;; Prevent all-the-icons from fattening text line heights on small terminus font

(defun my-adjust-scale-1 ()
  (text-scale-decrease 1))

(defun my-adjust-scale-2 ()
  (text-scale-set -2)
  ;; (text-scale-decrease 2)
  )

;; my first action after startup is often to reach for virtual buffers, so preload please
(recentf-mode)

(add-hook 'minibuffer-setup-hook #'my-adjust-scale-2)
(add-hook 'dired-mode-hook #'my-adjust-scale-2)

(setc orderless-matching-styles '(orderless-literal
                                  orderless-regexp
                                  orderless-initialism))

;; (setc consult-line-start-from-top t)

(after! vertico
  ;; (setc vertico-count 12)

  )

(bind-key "M-g o" #'consult-outline)
(bind-key "M-g l" #'consult-line)
(bind-key "M-g m" #'consult-mark)
(bind-key "M-g k" #'consult-global-mark)
(bind-key "M-g i" #'consult-imenu)
(bind-key "M-g e" #'consult-error)
(bind-key "M-s m" #'consult-multi-occur)
;; (bind-key "" #'consult-kmacro)
;; (bind-key "" #'consult-focus-lines)
;; (bind-key "" #'consult-imenu-multi)
;; (bind-key "" #'consult-file-externally)
(bind-key "<f1> m" #'consult-minor-mode-menu)
(bind-key "<f1> M" #'describe-mode)
(bind-key "<f2> h" #'consult-find)

(bind-key "C-\;" #'embark-act)

;; per doom: embark-act on C-;

;; completion-at-point, TAB -> corfu-complete
;; RET -> corfu-insert
;; M-g -> corfu-show-location
;; M-h -> corfu-show-documentation
(use-package! corfu
  :init
  (setq corfu-auto t)
  (setq corfu-excluded-modes '(org-mode))
  (setq tab-always-indent 'complete) ;; or use M-TAB
  :config
  (setq completion-cycle-threshold 3)
  ;; invoke corfu for dabbrev instead of the opportunistic expand
  (bind-key "M-/" #'dabbrev-completion)
  (bind-key "C-M-/" #'dabbrev-expand)
  (global-corfu-mode)

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
    (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  )

(use-package! cape
  :after corfu
  :config
  ;; Fix eshell: https://github.com/minad/corfu/issues/61
  ;; (the pcomplete capf does not behave as a proper capf by default)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

