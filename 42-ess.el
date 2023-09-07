;; -*- lexical-binding: t; -*-
;; Copyright (C) 2020-2023 Martin Edström
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
(setopt ess-eval-visibly 'nowait)

;; http://chainsawriot.com/mannheim/2020/07/19/elisp.html

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
