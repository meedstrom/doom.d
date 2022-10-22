;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq-default load-prefer-newer t)
(add-load-path! "/home/kept/emacs/conf-doom/lisp/"
                "/home/kept/emacs/deianira/"
                "/home/kept/emacs/twee-mode/")

;; NOTE: I use `load', not `load!', and the `directory-files' FULL argument, so
;;       when init breaks,, the error messages print full file paths and I can
;;       `ffap' my way to the broken file.
(dolist (module (directory-files "/home/kept/emacs/conf-doom/lisp/" t "^[^.#].*.el$"))
  (load module))
