;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Ensure that Guix' Org comes first.
;; NOTE: Tends to break unless I install everything that depends on org thru
;; Guix too.  Wait and see what hlissner does with Nix...
;;(use-package org :load-path "/home/me/.guix-profile/share/emacs/site-lisp/")

(setq-default load-prefer-newer t)
(add-load-path!
 (file-name-concat (getenv "MY_FILES") "emacs/conf-doom/lisp/")
 (file-name-concat (getenv "MY_FILES") "emacs/deianira/")
 (file-name-concat (getenv "MY_FILES") "emacs/twee-mode/"))

;; Deprecated fix?
;; (defadvice! fix-solaire-mode-fix-minibuffer (&optional unset)
;;   :override #'solaire-mode-fix-minibuffer
;;   (dolist (buf '(" *Minibuf-0*" " *Minibuf-1*"
;;                  " *Echo Area 0*" " *Echo Area 1*"))
;;     (with-current-buffer (get-buffer-create buf)
;;       (if (or unset (not solaire-global-mode))
;;           (solaire-mode -1)
;;         (setq-local evil-buffer-regexps '((".")))
;;         (when (= (buffer-size) 0)
;;           (insert " "))
;;         (add-hook 'kill-buffer-query-functions #'ignore nil 'local)
;;         (solaire-mode +1)))))

;; Load.
;; NOTE: I use `load', not `load!', and the `directory-files' FULL argument, so
;;       I can `ffap' my way to the broken file when init breaks (in error
;;       messages, relative file paths aren't helpful).
(dolist (module (directory-files "/home/kept/emacs/conf-doom/lisp/" t "^[^.#].*.el$"))
  (load module))
