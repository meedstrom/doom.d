;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Start the real init. I use `load' (not `load!' nor `require'), and the
;; `directory-files' FULL argument, so when init breaks, the error messages
;; print the full path to the broken file and I can `ffap' my way to it.
(let ((load-prefer-newer t))
  (dolist (module (directory-files (concat (getenv "DOOMDIR") "/lisp/") t "^[0-9].+el$"))
    (unless (string-search ".sync-conflict" module)
      (load module))))
