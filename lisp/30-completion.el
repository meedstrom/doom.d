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

(setopt orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    orderless-initialism))

;; embark
(setopt y-or-n-p-use-read-key t)

;; (setopt consult-line-start-from-top t)

;; completion-at-point, TAB -> corfu-complete
;; RET -> corfu-insert
;; M-g -> corfu-show-location
;; M-h -> corfu-show-documentation
(use-package! corfu
  :init
  (setopt corfu-auto t)
  (setopt corfu-auto-delay 0.35)
  (setopt corfu-excluded-modes '(org-mode))
  (setopt tab-always-indent 'complete) ;; or use M-TAB to complete
  :config
  (setopt completion-cycle-threshold 3)
  ;; invoke corfu for dabbrev instead of the opportunistic expand
  (keymap-set "M-/" #'dabbrev-completion)
  (keymap-set "C-M-/" #'dabbrev-expand)
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

;; NOTE: probably fixed in Emacs 29
(use-package! cape
  :after corfu
  :config
  ;; Fix eshell: https://github.com/minad/corfu/issues/61
  ;; (the pcomplete capf does not behave as a proper capf by default)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
