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

(defun my-eshell-here (&optional dir)
  "Open a shell in the current directory. If one was already
open, visit that."
  (interactive)
  (let* ((dir (or dir default-directory))
         (name (my-generate-eshell-name dir)))
    (if (get-buffer name)
        (switch-to-buffer name)
      (let ((default-directory dir))
        (eshell "new"))
      (setq-local eshell-history-file-name (my-eshell-history-file dir))
      (setq-local my-eshell-scrollback-file (my-eshell-scrollback-file dir))
      (eshell-hist-initialize)
      ;; (my-restore-scrollback)
      )))

(defun my-eshell-history-file (&optional dir)
  (expand-file-name ".eshell-command-history" (or dir default-directory)))

(defun my-eshell-scrollback-file (&optional dir)
  (expand-file-name ".eshell-scrollback" (or dir default-directory)))

(defun my-truncate-buffer-and-move-excess (&optional _string)
  (save-excursion
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (forward-line (- comint-buffer-maximum-size))
    (beginning-of-line)
    (let ((inhibit-read-only t)
          (beg (point-min))
          (end (point)))
      (append-to-buffer "*comint-excess*" beg end)
      (delete-region beg end))))

(defun my-restore-scrollback* ()
  (when (eq major-mode 'eshell-mode)
    (insert
     (with-temp-buffer
       (insert-file-contents-literally (or (bound-and-true-p my-eshell-scrollback-file)
                                           (my-eshell-scrollback-file)))
       (princ (buffer-string))))))

(defun my-restore-scrollback ()
  (when (eq major-mode 'eshell-mode)
    (insert-file-contents (or (bound-and-true-p my-eshell-scrollback-file)
                              (my-eshell-scrollback-file)))))

(defun save-eshell-scrollback* ()
  (let* ((file (or (bound-and-true-p my-eshell-scrollback-file)
                   (my-eshell-scrollback-file)))
         (maybe (get-file-buffer file)))
    (when maybe (kill-buffer maybe))
    (save-mark-and-excursion
      (let ((end (point)))
        (eshell-previous-prompt 1)
        (forward-line 0)
        (let* ((beg (point))
               (savestring (prin1-to-string (buffer-substring beg end))))
          (with-temp-buffer
            (insert savestring)
            (write-region (point-min) (point-max) file
                          'append 'silently)
            ))))))

(defun save-eshell-scrollback ()
  (let* ((file (or (bound-and-true-p my-eshell-scrollback-file)
                   (my-eshell-scrollback-file)))
         (maybe (get-file-buffer file)))
    (when maybe (kill-buffer maybe))
    (save-mark-and-excursion
      (let ((end (point)))
        (eshell-previous-prompt 1)
        (forward-line 0)
        (let ((beg (point)))
          (write-region beg end file
                        'append 'silently))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-motd "
   /home/kept/Code/R/playpen/lab2b.R
   /home/kept/Knowledge_base/stats.org
   /home/kept/Journal/Finances/clean_start.ledger
")

(setq-default
 eshell-error-if-no-glob t
 eshell-scroll-to-bottom-on-input nil
 eshell-show-lisp-completions t
 ;; eshell-review-quick-commands nil
 ;; eshell-smart-space-goes-to-end t
 ;; eshell-where-to-jump 'begin
 eshell-banner-message
 '(concat "Welcome to the Emacs shell ⚘" "\n\n"
          (when (executable-find "fortune")
            (my-process-output-to-string
             "fortune"
             (expand-file-name "fortunedb-showerthoughts"
                               (or (getenv "XDG_DATA_HOME") "~/.local/share"))))
          my-motd "\n"))

;; Limit scrollback because my computer sucks. This is mainly for R.
(setq comint-buffer-maximum-size (^ 2 12))
(add-hook 'comint-output-filter-functions #'my-truncate-buffer-and-move-excess)

;; Tell pop-to-buffer to reuse current window, so M-x shell does too.
(add-to-list 'display-buffer-alist
             (cons "\\*shell\\*" display-buffer--same-window-action))

;; Guessing this wasn't default because it'd affect term-mode, but I'm fine with
;; it. Natural choice for shell/eshell.
(setenv "PAGER" "cat")

;; See <f1> P esh-groups
(setq eshell-modules-list
      '(eshell-alias eshell-banner eshell-basic eshell-cmpl
        eshell-glob eshell-hist eshell-pred eshell-prompt
        eshell-script eshell-term eshell-unix eshell-tramp
        eshell-xtra))

(after! em-ls
  (fset #'eshell/ls #'dired-jump))

(after! eshell

  ;; I like shell-mode's interaction style
  (general-unbind eshell-mode-map "<up>")
  (general-unbind eshell-mode-map "<left>")
  (general-unbind eshell-mode-map "<down>")
  (general-unbind eshell-mode-map "<right>")
  (general-def dired-mode-map "b" #'dired-up-directory)
  (general-def dired-mode-map "r" #'my-eshell-here)

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

  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
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
        (tyler-eshell-view-file (pop args)))))

  ;; Easy swapping between dired and eshell
  ;; Dired default unbound keys: `, b, E, J, K, r, z, <backspace>
  ;; Dired useless keys: h, 1234567890
  )

(after! (eshell doom-modeline)
  (remove-hook 'eshell-mode-hook #'hide-mode-line-mode))
