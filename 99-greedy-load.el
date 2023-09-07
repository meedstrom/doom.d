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


(setq timer1 (run-with-idle-timer
              10 nil (lambda ()
                       (require 'org-roam)
                       (cancel-timer timer1))))

(setq timer2 (run-with-idle-timer
              20 nil (lambda ()
                       (require 'dired)
                       (cancel-timer timer2))))

(setq timer3 (run-with-idle-timer
              30 nil (lambda ()
                       (require 'eshell)
                       (cancel-timer timer3))))


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
