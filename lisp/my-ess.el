;; -*- lexical-binding: t; -*-

(defun my-append-to-rhistory (input)
  (with-temp-buffer
    (insert (concat (format-time-string "《%FT%T%z》") input))
    (kill-matching-buffers "^.Rhistory" nil t)
    (quietly (append-to-file (point-min) (point-max)
                            (expand-file-name ".Rhistory" default-directory))))
  input)

(setq-default
  inferior-R-args "--no-save --no-restore"
 ess-use-ido nil
 ess-use-flymake nil
 ess-use-tracebug nil ;; undo horrible performance hit
 ess-use-auto-complete nil
 ess-indent-with-fancy-comments nil
 ess-ask-for-ess-directory nil
 ess-eval-visibly 'nowait)

(add-hook 'ess-presend-filter-functions #'my-append-to-rhistory)

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
