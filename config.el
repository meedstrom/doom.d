;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Backups still save my skin, as of 2020.
(setq
 backup-directory-alist `((,tramp-file-name-regexp . nil) ;; exclude TRAMP, bad experience
                          ("." . "/home/backups"))
 delete-old-versions t ;; nil led to Emacs appearing broken for newbie me
 vc-make-backup-files t ;; I don't commit regularly in every project
 make-backup-files t ;; WHY did you disable it, Doom?!
 version-control t)

(setq display-line-numbers-type nil)

;; (setq doom-font (font-spec :family "monospace" :size 14))
;; (setq doom-font (font-spec :family "Hasklig" :size 14))
(setq doom-font (font-spec :family "Iosevka" :size 14))

;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-tomorrow-night)
;; (setq doom-theme 'doom-dark+)
;; (setq doom-theme 'doom-outrun-electric)

;;;; Loads

(add-load-path! (expand-file-name "Emacs/common" (getenv "MY_FILES")))
(add-load-path! (expand-file-name "Emacs/conf-vanilla/lisp" (getenv "MY_FILES")))
(add-load-path! (expand-file-name "Emacs/conf-doom/lisp" (getenv "MY_FILES")))
(add-load-path! (expand-file-name "Emacs/escape-modality" (getenv "MY_FILES")))
(add-load-path! (expand-file-name "Emacs/twee-mode" (getenv "MY_FILES")))

(use-package! my-lib)

(dolist (module (directory-files "/home/kept/Emacs/conf-doom/lisp/" nil (rx (* nonl) ".el" eol)))
  (load! (concat "/home/kept/Emacs/conf-doom/lisp/" module)))

;; (general-after-init
;;   (load! "lisp/doom-keys.el"))

(autoload #'exwm-edit--compose "exwm-edit")
(autoload #'helm-fd "helm-fd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


