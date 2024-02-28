;; -*- lexical-binding: t; -*-

(require 'beginend)
(beginend-global-mode)

(require 'form-feed)
(global-form-feed-mode)
(add-hook 'emacs-lisp-compilation-mode-hook #'form-feed-mode)

(require 'apheleia)
(apheleia-global-mode)
(setopt apheleia-log-debug-info t)

;; note that "javascript" actually just applies to js2-mode. this
;; package needs more love
(require 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml)

;; (use-package circadian
;;   :config
;;   (add-hook 'circadian-after-load-theme-hook #'prism-set-colors)
;;   (setq circadian-themes '(("8:00"   . doom-storage-tube-green)
;;                            ("18:00"  . doom-storage-tube-amber-2)))
;;   (circadian-setup))

(use-package prism
  :init
  (setopt prism-comments nil)
  ;; The default (40 50 60) is a nice fix for fruit-salad themes but if the
  ;; theme already uses muted colors, the effect is...well...
  (setopt prism-desaturations '(0 20 60))
  ;; (setopt prism-desaturations '(0))
  ;; Btw, another odd default for Lisp is that the parens enclosing a sexp
  ;; differ in color from the symbols inside -- people arent used to this
  ;; (unless they already used rainbow-delimiters)
  :config
  ;; Replace rainbow-delimiters (it's on a dozen hooks in Doom, so this method
  ;; is easiest).
  (fset 'rainbow-delimiters-mode #'prism-mode)
  (add-hook 'doom-load-theme-hook #'prism-set-colors)
  ;; (add-hook 'web-mode-hook #'prism-mode) ;; infinite loop in .svelte files
  (add-hook 'typescript-mode-hook #'prism-mode)
  (add-hook 'typescript-tsx-mode-hook #'prism-mode)
  (add-hook 'js-base-mode-hook #'prism-mode))
