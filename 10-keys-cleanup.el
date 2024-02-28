;; Unbind keys to clean house -*- lexical-binding: t; -*-

;; Commands on 'too good' locations (risk that I get used to them).
(keymap-unset global-map "<f2>" t) ;; 2C-command
(keymap-unset global-map "<f3>" t) ;; kmacro-start-macro-or-insert-counter
(keymap-unset global-map "<f4>" t) ;; kmacro-end-or-call-macro
(keymap-unset global-map "<f5>" t) ;; NOTE: which-key-paging-key is here by default
(keymap-unset global-map "<f6>" t)
(keymap-unset global-map "<f7>" t)
(keymap-unset global-map "<f8>" t)
(keymap-unset global-map "<f9>" t)
(keymap-unset global-map "<f10>" t) ;; menu-bar-open
(keymap-unset global-map "<insert>" t) ;; overwrite-mode
(keymap-unset global-map "C-SPC" t)
(keymap-unset global-map "C-\\" t) ;; toggle-input-method
(keymap-unset global-map "C-q" t) ;; quoted-insert
(keymap-unset global-map "C-x (" t)
(keymap-unset global-map "C-x )" t)
(keymap-unset global-map "C-x *" t)
(keymap-unset global-map "C-x C-SPC" t)
(keymap-unset global-map "C-x C-z" t)
(keymap-unset global-map "C-x DEL" t) ;; bro just use M-- M-k
(keymap-unset global-map "C-x SPC" t)
(keymap-unset global-map "C-x k" t) ;; Discourage unproductive behavior
(keymap-unset global-map "C-x z" t)
(keymap-unset global-map "C-z" t) ;; suspend-frame
(keymap-unset global-map "M-." t) ;; xref-find-definitions
(keymap-unset global-map "M-`" t) ;; tmm-menubar
(keymap-unset global-map "M-i" t) ;; tab-to-tab-stop
(keymap-unset global-map "M-j" t) ;; default-indent-new-line
(keymap-unset global-map "M-m" t) ;; back-to-indentation
(keymap-unset global-map "M-o" t) ;; facemenu-keymap
(keymap-unset global-map "M-q" t) ;; fill-paragraph
(keymap-unset global-map "M-r" t) ;; move-to-window-line-top-bottom
(keymap-unset global-map "M-z" t) ;; zap-to-char
(keymap-unset global-map "M-~" t) ;; not-modified
(keymap-unset global-map "<XF86Back>" t) ;; previous-buffer
(keymap-unset global-map "<XF86Forward>" t) ;; next-buffer

;; (Nice commands I discovered)
;; "C-]" ;; abort-recursive-edit
;; C-x i ;; insert-file

;; Unbinding these has all kinds of consequences, why I'll migrate to Super one
;; day and deprecate Control, using Control only outside Emacs and use Super
;; only inside Emacs.  Same idea employed by Mac OS, but I include modern GUI
;; apps -- all apps other than Emacs -- in the "legacy" category of things to
;; be operated with Control.  That will make EXWM run like a dream.
;;
;; (general-unbind "C-g") ;; keyboard-quit
;; (general-unbind "C-j") ;; newline
;; (general-unbind "C-i")
;; (general-unbind "C-]")
;; (general-unbind "C-m")

;; these unbindings hurt too much until I have more modal editing (deianira)
;; (general-unbind "<f1>")
;; (general-unbind "<down>")
;; (general-unbind "<left>")
;; (general-unbind "<next>")
;; (general-unbind "<prior>")
;; (general-unbind "<right>")
;; (general-unbind "<up>")
;; (general-unbind "<return>")

(when (boundp 'doom-version)
  (keymap-unset global-map "C-'" t) ;; imenu
  (keymap-unset global-map "M--" t)
  (keymap-unset global-map "M-=" t))

(when (boundp 'spacemacs-version)
  (keymap-unset elisp-slime-nav-mode-map "M-," t)
  (keymap-unset elisp-slime-nav-mode-map "M-." t)
  (keymap-unset evil-emacs-state-map "C-z" t))

;; default is C-;
(setopt iedit-toggle-key-default nil)

(after! geiser-mode
  (keymap-unset geiser-mode-map "M-," t)
  (keymap-unset geiser-mode-map "M-." t)
  (keymap-unset geiser-mode-map "M-`" t))

(after! geiser-repl
  (keymap-unset geiser-repl-mode-map "M-," t)
  (keymap-unset geiser-repl-mode-map "M-." t)
  (keymap-unset geiser-repl-mode-map "M-`" t))

(after! em-hist
  ;; be docile like M-x shell (don't "hijack" point)
  (keymap-unset eshell-hist-mode-map "<up>" t)
  (keymap-unset eshell-hist-mode-map "<down>" t))
