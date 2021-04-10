;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Backups still save my skin, as of 2020.
(setq
 backup-directory-alist `((,tramp-file-name-regexp . nil) ;; exclude TRAMP, bad experience
                          ("." . "/home/backups"))
 delete-old-versions t ;; nil led to Emacs appearing broken for newbie me
 vc-make-backup-files t ;; I don't commit regularly in every project
 make-backup-files t ;; WHY did Doom disable it
 version-control t)

(setq user-full-name "Martin Edström")

(setq doom-font (font-spec :family "Terminus" :size 10))
;; (setq doom-font (font-spec :family "monospace" :size 15))
;; (setq doom-font (font-spec :family "Hasklig" :size 14))
;; (setq doom-font (font-spec :family "Iosevka" :size 14))

;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-tomorrow-night)
;; (setq doom-theme 'doom-dark+)
;;(setq doom-theme 'doom-manegarm)
(setq doom-theme 'doom-Iosvkem)

;; (add-hook 'doom-load-theme-hook
          ;; (lambda () (set-face-attribute 'fixed-pitch-serif () :inherit 'default)))
;; (setq doom-theme 'doom-outrun-electric)

;; DEPRECATED: use init-file-debug
;; (setq my-debug-p doom-debug-p)

;;;; Loads

(autoload #'exwm-edit--compose "exwm-edit")
(autoload #'helm-fd "helm-fd")

;; ensure that guix org comes first
;; Tends to break unless i install everything that depends on org thru guix. Wait and see what hlissner does with Nix...
;(use-package org
;  :load-path "/home/me/.guix-profile/share/emacs/site-lisp/")

(setq-default load-prefer-newer t)
(add-to-list 'load-path "~/.doom.d/subed/")
(add-load-path! (expand-file-name "Emacs/common" (getenv "MY_FILES")))
(add-load-path! (expand-file-name "Emacs/conf-vanilla/lisp" (getenv "MY_FILES")))
(add-load-path! (expand-file-name "Emacs/conf-doom/lisp" (getenv "MY_FILES")))
(add-load-path! (expand-file-name "Emacs/deianira" (getenv "MY_FILES")))
(add-load-path! (expand-file-name "Emacs/twee-mode" (getenv "MY_FILES")))
(add-load-path! (expand-file-name "Emacs/secretary" (getenv "MY_FILES")))

(dolist (module (directory-files "/home/kept/Emacs/common/" t ".el$"))
  (load! module))

(dolist (module (directory-files "/home/kept/Emacs/conf-doom/lisp/" t ".el$"))
  (load! module))
