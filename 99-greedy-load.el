;; -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Edström
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; It's starting to annoy me the delay in the first eshell, dired, and org-roam
;; buffers. Either design a :defer-incrementally list for each, or do some
;; general "load everything" during idle so that we have nothing left waiting to
;; be autoloaded...

;;; Code:

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
