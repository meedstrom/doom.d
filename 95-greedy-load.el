;; -*- lexical-binding: t; -*-

;;; Commentary:

;; It's starting to annoy me the delay in the first eshell, dired, and org-roam
;; buffers.  Load 'em during idle.

;;; Code:

(run-with-idle-timer 10 nil #'require 'org-mode)
(run-with-idle-timer 12 nil #'require 'org-roam)
(run-with-idle-timer 14 nil #'require 'dired)
(run-with-idle-timer 16 nil #'require 'eshell)
(run-with-idle-timer 20 nil #'require 'ess-r-mode)

;; I don't know a systematic way to find the sequence of files to load,
;; so it's a PITA to figure out and it could change in future updates...
(use-package! ess-r-mode
  :disabled
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
