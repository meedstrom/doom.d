;; -*- lexical-binding: t; -*-

;; It's starting to annoy me the delay in the first eshell, dired, and org-roam
;; buffers. Either design a :defer-incrementally list for each, or do some
;; general "load everything" during idle so that we have nothing left waiting to
;; be autoloaded...

;; (use-package! eshell
;;   :defer-incrementally
;;   ())

;; (use-package! dired
;;   :defer-incrementally
;;   ())

;; (use-package! org-roam
;;   :defer-incrementally
;;   ())

(use-package! ess-r-mode :disabled
              :defer-incrementally
              (ess-custom
               ess-utils
               ess
               ess-r-syntax
               ess-tracebug
               ess-inf
               ess-r-package
               ess-mode
               ess-trns
               ess-help
               ess-rd
               ess-roxy
               ess-s-lang
               ess-r-xref
               ess-r-completion
               ess-r-flymake))

;; ;; FIXME: doesn't work
;; (named-timer-idle-run
;;     'my-first-load 5 nil
;;     (defun my-load-everything ()
;;       ;; (cl-loop for x in '(eshell dired org-roam)
;;       ;; until (not (featurep x))
;;       ;; finally do (require x))
;;       (require 'eshell)
;;       (require 'dired)
;;       (require 'org-roam)
;;       ))
