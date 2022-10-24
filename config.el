;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq-default load-prefer-newer t)
(add-load-path! "/home/kept/emacs/deianira/"
                "/home/kept/emacs/twee-mode/")

;; Start the real init. I use `load' (not `load!' nor `require'), and the
;; `directory-files' FULL argument, so when init breaks, the error messages
;; print the full path to the broken file and I can `ffap' my way to it.
(dolist (module (directory-files "lisp/" t "[0-9].+el$"))
  (unless (string-search ".sync-conflict" module)
    (load module)))
