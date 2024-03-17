;; ESS (Emacs Speaks Statistics) config -*- lexical-binding: t; -*-

;; TODO: submit PR https://github.com/radian-software/apheleia?tab=readme-ov-file#adding-a-formatter
(after! apheleia-formatters
  (add-to-list 'apheleia-formatters '(ess-r "R" "-s" "--no-save" "--no-restore" "-e" "styler::style_text(readLines(file('stdin')))"))
  (add-to-list 'apheleia-mode-alist '(ess-r-mode . ess-r)))

;; Make command `previous-buffer' not skip the R console
(el-patch-defun doom-buffer-frame-predicate (buf)
  "To be used as the default frame buffer-predicate parameter. Returns nil if
BUF should be skipped over by functions like `next-buffer' and `other-buffer'."
  (or (string-prefix-p "*R:" (buffer-name buf))
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
(setopt ess-ask-for-ess-directory nil) ;; Muffle annoying ESS startup prompt
(setopt ess-use-ido nil)
(setopt ess-use-flymake nil)
(setopt ess-use-tracebug nil) ;; Sidestep a bug that destroys performance
(setopt ess-use-auto-complete nil)
(setopt ess-indent-with-fancy-comments nil)
(setopt ess-history-file nil)
(setopt ess-ask-for-ess-directory nil)
(setopt ess-eval-visibly 'nowait)
(anon-hook ess-r-mode-hook (ess-set-style 'RStudio))

;; http://chainsawriot.com/mannheim/2020/07/19/elisp.html

;; (setopt ess-directory-function
;;       (lambda ()
;;         (or (ignore-errors (car (project-roots (project-current))))
;;             nil)))

;; (add-hook 'ess-presend-filter-functions #'my-append-to-rhistory)

;; (defun my-set-rhistory (&rest r)
;; (setq! ess-history-directory default-directory)
;; (setq! ess-history-file ".Rhistory"))

;; (advice-add #'ess-set-working-directory :after #'my-set-rhistory)
