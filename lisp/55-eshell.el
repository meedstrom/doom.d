;; -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'my-lib)
(require 'my-lib-shells)
(require 'general)
(require 'subr-x)

(defvar my-motd "
   /home/kept/Code/R/playpen/lab2b.R
   /home/kept/Knowledge_base/stats.org
   /home/kept/Journal/Finances/clean_start.ledger
")

(defun my-eshell-timestamp-update ()
    "When added to `eshell-pre-command-hook', the first string --:-- in the
  prompt becomes a timestamp like 13:59 after you run a command."
    (save-excursion
      (forward-line -1)
      (when-let* ((unfilled-timestamp "--:--")
                  (end (search-forward unfilled-timestamp nil t))
                  (beg (- end (length unfilled-timestamp)))
                  (inhibit-read-only t))
        (delete-region beg end)
        (insert (format-time-string "%H:%M"))
        (add-text-properties beg (point) '(font-lock-face eshell-prompt)))))

;; Custom prompt
(after! eshell
  (add-hook 'eshell-pre-command-hook #'my-eshell-timestamp-update)
  (setq eshell-prompt-regexp (rx bol (repeat 7 nonl) " Sir? ")
        eshell-prompt-function (lambda () (concat "[--:--] Sir? "))))

;; Undoom (I like to put the working directory in the modeline)
(after! (eshell doom-modeline)
  (remove-hook 'eshell-mode-hook #'hide-mode-line-mode))

(after! em-ls
  (fset #'eshell/ls #'dired-jump))

(after! eshell
  ;; The natural choice for shell/eshell. Bear in mind it will also apply to
  ;; programs spawned from Emacs, such as terminals, RStudio, VSCode.
  (setenv "PAGER" "cat")

  (setq
   eshell-error-if-no-glob t
   eshell-scroll-to-bottom-on-input nil
   eshell-scroll-to-bottom-on-output nil
   eshell-scroll-show-maximum-output nil
   eshell-show-lisp-completions t
   ;; eshell-review-quick-commands nil
   ;; eshell-smart-space-goes-to-end t
   ;; eshell-where-to-jump 'begin
   )

  (setq eshell-banner-message
        '(concat "Welcome to the Emacs shell ⚘" "\n\n"
                 (when (executable-find "fortune")
                   (my-process-output-to-string
                    "fortune"
                    (expand-file-name "fortunedb-showerthoughts"
                                      (or (getenv "XDG_DATA_HOME")
                                          "~/.local/share"))))
                 my-motd "\n"))

  ;; See <f1> P esh-groups
  (setq eshell-modules-list '(eshell-alias eshell-banner eshell-basic eshell-cmpl
                              eshell-glob eshell-hist eshell-pred eshell-prompt eshell-script
                              eshell-term eshell-unix eshell-tramp eshell-xtra))

  ;; Encourage idiomatic ways to work with Emacs
  (defun eshell/cd () nil "cd: command not allowed")
  (defun eshell/b (&optional _args)
    (let ((default-directory (expand-file-name "..")))
      (my-eshell-here)))
  (fset #'eshell/ls #'dired-jump)
  (fset #'eshell/r #'dired-jump)
  (fset #'eshell/q #'eshell-quit-and-close) ;; in doom already: +eshell-aliases
  ;; (general-def dired-mode-map "M-r" #'my-eshell-here)
  ;; (general-def eshell-mode-map "M-r" #'dired-jump)
  ;; (define-key my-on-shell-output-map (kbd "z") #'dired-jump)

  ;; (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m) ;; This shouldn't affect eshell, but does
  (add-hook 'eshell-directory-change-hook #'my-eshell-rename)
  (add-hook 'eshell-mode-hook #'my-eshell-rename)
  ;; (add-hook 'eshell-before-prompt-hook #'save-eshell-scrollback)

  (defun eshell/less (&rest args)
    "Invoke `view-file' on a file. \"less +42 foo\" will go to line 42 in
    the buffer for foo."
    (while args
      (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
          (let* ((line (string-to-number (match-string 1 (pop args))))
                 (file (pop args)))
            (tyler-eshell-view-file file)
            (goto-char (point-min))
            (forward-line (- line 1)))
        (tyler-eshell-view-file (pop args))))))
