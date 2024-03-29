;; -*- lexical-binding: t; -*-

(run-with-idle-timer 10 nil #'require 'org)
(run-with-idle-timer 12 nil #'require 'org-roam)
(run-with-idle-timer 14 nil #'require 'dired)
(run-with-idle-timer 16 nil #'require 'eshell)

(require 'beginend)
(beginend-global-mode)

(require 'form-feed)
(global-form-feed-mode)
(add-hook 'emacs-lisp-compilation-mode-hook #'form-feed-mode)

(require 'apheleia)
(apheleia-global-mode)
(setopt apheleia-log-debug-info t)

(use-package circadian
  :disabled
  :config
  (el-patch-defun circadian-a-earlier-b-p (time-a time-b)
    "Compare to time strings TIME-A and TIME-B by hour and minutes."
    (or (and (= (cl-first time-a) (cl-first time-b))
             ((el-patch-swap <= <) (cl-second time-a) (cl-second time-b)))
        (< (cl-first time-a) (cl-first time-b))))
  ;; (add-hook 'circadian-after-load-theme-hook #'prism-set-colors)
  (setopt circadian-themes '(("5:00" . doom-gruvbox-light)
                             ("11:00" . doom-flatwhite)
                             ("15:00" . doom-sourcerer)
                             ("18:00" . ef-rosa)))
  (circadian-setup))

(use-package prism
  :defer
  :init
  (setopt prism-comments nil)
  ;; The default (40 50 60) is a nice fix for fruit-salad themes but if the
  ;; theme already uses muted colors, the effect is... not good
  (setopt prism-desaturations '(0 20 60))
  ;; (setopt prism-desaturations '(0))
  ;; Btw, another odd default for Lisp is that the parens enclosing a sexp
  ;; differ in color from the symbols inside -- people arent used to this
  ;; (unless they are already used to rainbow-delimiters)
  :config
  ;; Replace rainbow-delimiters (it's on a dozen hooks in Doom, so this method
  ;; is easiest).
  (fset 'rainbow-delimiters-mode #'prism-mode)
  (add-hook 'doom-load-theme-hook #'prism-set-colors)
  ;; (add-hook 'web-mode-hook #'prism-mode) ;; infinite loop in .svelte files
  (add-hook 'typescript-mode-hook #'prism-mode)
  (add-hook 'typescript-tsx-mode-hook #'prism-mode)
  (add-hook 'js-base-mode-hook #'prism-mode))
