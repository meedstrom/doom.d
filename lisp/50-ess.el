;; -*- lexical-binding: t; -*-

;; Make previous-buffer not skip the R console
(el-patch-defun doom-buffer-frame-predicate (buf)
  "To be used as the default frame buffer-predicate parameter. Returns nil if
BUF should be skipped over by functions like `next-buffer' and `other-buffer'."
  (or (s-starts-with-p "*R:" (buffer-name buf))
      (doom-real-buffer-p buf)
      (eq buf (doom-fallback-buffer))))

(defun my-append-to-rhistory (input)
  (with-temp-buffer
    (insert (concat (format-time-string "《%FT%T%z》") input))
    (kill-matching-buffers "^.Rhistory" nil t)
    (quiet! (append-to-file (point-min) (point-max)
                            (expand-file-name ".Rhistory" default-directory))))
  input)

(setopt inferior-R-args "--no-save --no-restore")
(setopt ess-use-ido nil)
(setopt ess-use-flymake nil)
(setopt ess-use-tracebug nil) ;; sidestep a bug that destroys performance
(setopt ess-use-auto-complete nil)
(setopt ess-indent-with-fancy-comments nil)
(setopt ess-history-file nil)
(setopt ess-ask-for-ess-directory nil)
;; http://chainsawriot.com/mannheim/2020/07/19/elisp.html
(setopt ess-eval-visibly 'nowait)

;; (setopt ess-directory-function
;;       (lambda ()
;;         (or (ignore-errors (car (project-roots (project-current))))
;;             nil)))

;; (add-hook 'ess-presend-filter-functions #'my-append-to-rhistory)

;; Because early setq does not work in Doom
(add-hook 'ess-r-mode-hook (defun my-ess-setup ()
                             (ess-set-style 'RStudio)))

;; (defun my-set-rhistory (&rest r)
;; (setq! ess-history-directory default-directory)
;; (setq! ess-history-file ".Rhistory"))

;; (advice-add #'ess-set-working-directory :after #'my-set-rhistory)
