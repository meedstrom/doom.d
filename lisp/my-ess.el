;; -*- lexical-binding: t; -*-

(defun my-append-to-rhistory (input)
  (with-temp-buffer
    (insert (concat (format-time-string "《%FT%T%z》") input))
    (kill-matching-buffers "^.Rhistory" nil t)
    (quiet! (append-to-file (point-min) (point-max)
                            (expand-file-name ".Rhistory" default-directory))))
  input)

(setc inferior-R-args "--no-save --no-restore")
(setc ess-use-ido nil)
(setc ess-use-flymake nil)
(setc ess-use-tracebug nil) ;; sidestep a bug that destroys performance
(setc ess-use-auto-complete nil)
(setc ess-indent-with-fancy-comments nil)
(setc ess-ask-for-ess-directory nil)
(setc ess-eval-visibly 'nowait)

;; (add-hook 'ess-presend-filter-functions #'my-append-to-rhistory)

(use-package! ess-r-mode
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

;; Because early setq does not work in Doom
(add-hook 'ess-r-mode-hook (defun my-ess-setup ()
                             (ess-set-style 'RStudio)))

;; (defun my-set-rhistory (&rest r)
  ;; (setq! ess-history-directory default-directory)
  ;; (setq! ess-history-file ".Rhistory"))

;; (advice-add #'ess-set-working-directory :after #'my-set-rhistory)
