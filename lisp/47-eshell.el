;; -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'my-lib)
(require 'my-lib-shells)
;; (require 'named-timer)
;; (require 'general)
(require 'subr-x)
;; (require 'loopy)

;; TODO: Automatically do scroll-right after coming off a long line
;; (add-hook 'window-scroll-functions
;;           (defun my-watch-next-line-motion ()
;;             (advice-add 'forward-line)
;;             ) 0 t)
;; (defun my-reset-hscroll (&rest _)
;;   (run-with-timer
;;    .1 nil
;;    (lambda ()
;;      (when (< (current-column) 80)
;;        (advice-remove 'line-move-finish #'my-reset-hscroll)
;;        (advice-remove 'mouse-set-point #'my-reset-hscroll)
;;        (advice-remove 'goto-char #'my-reset-hscroll)
;;        (advice-remove 'forward-char #'my-reset-hscroll)
;;        (advice-remove 'backward-char #'my-reset-hscroll)
;;        (scroll-right most-positive-fixnum))))
;;   _)
;; (advice-add 'set-window-hscroll :after
;;             (defun my-watch-point-motion (&rest _)
;;               (advice-add 'line-move-finish :after #'my-reset-hscroll)
;;               (advice-add 'mouse-set-point :after #'my-reset-hscroll)
;;               (advice-add 'goto-char :after #'my-reset-hscroll)
;;               (advice-add 'forward-char :after #'my-reset-hscroll)
;;               (advice-add 'backward-char :after #'my-reset-hscroll)
;;               _))
;; HACK b/c i can't seem to hook anything onto when auto-hscroll-mode does its
;;      thing, it doesnt call set-window-hscroll
;; (named-timer-run :my-hscroll-reset .2 .2
;;                  (defun my-hscroll-reset ()
;;                    (and (/= 0 (window-hscroll))
;;                         (< (current-column) (window-width))
;;                         (scroll-right most-positive-fixnum))))

;; (use-package eshell
;;   :defer
;;   :custom
;;   ((eshell-prompt-function
;;    (lambda ()
;;      (concat "[--:--] " (if (>= my-esh-backref-counter 35)
;;                             "---"
;;                           "--") " λ ")))
;;    (eshell-prompt-regexp
;;    (rx (?? bol "Command finished, " (*? anychar))
;;        "[" (= 5 nonl) "]" (* nonl) " λ "))
;;    (eshell-scroll-show-maximum-output nil)
;;    (eshell-show-lisp-completions t)
;;    (eshell-banner-message
;;    '(concat "Favored/recent files:\n"
;;             (loopy (list item (my-recentf-for-motd))
;;                    (concat (concat "\n   \"" item "\"")))
;;             "\n\n"))))

(defun my-commands-starting-with (prefix)
  (let (commands)
    (mapatoms (lambda (sym)
                (and (commandp sym)
                     (string-prefix-p prefix (symbol-name sym))
                     (push sym commands))))
    commands))

;; (my-syms-starting-with "my-esh-")

(after! eshell
  (setopt eshell-prompt-function
          (lambda ()
            (concat "〈 [--:--] " (if (>= my-esh-backref-counter 35)
                                      "---"
                                    "--") " 〉 ")))
  ;; something is broken with the regexp, C-c C-n wont work
  (setopt eshell-prompt-regexp (rx "〈 " (*? anychar) " 〉 "))
  ;; (eshell-prompt-regexp (rx "⟨‾🐂" (*? anychar) " λ_⟩ "))
  ;; (eshell-prompt-regexp
  ;; (rx (?? bol "Time elapsed: " (*? anychar))
  ;; "[" (= 5 nonl) "]" (* nonl) " λ "))
  (setopt eshell-scroll-show-maximum-output nil)
  (setopt eshell-show-lisp-completions t)

  (setopt eshell-banner-message
          '(cl-loop
            for cmd in
            (append
             (my-commands-starting-with "my-esh-")
             '(app-launcher-run-app
               dired-jump
               helm-selector-shell
               my-copy-region-or-rest-of-line-to-other-window
               my-copy-region-to-variable
               my-cycle-path-at-point-repeat
               my-eval-and-replace-print
               my-insert-other-buffer-file-name-and-cycle
               my-new-eshell
               my-next-buffer-of-same-mode-repeat
               my-pipe
               my-previous-buffer-of-same-mode-repeat
               my-replace-var-at-point-with-value
               shelldon))
            with x
            collect
            (concat
             (string-pad (string-join (seq-take (my-locate-keys cmd)
                                                2)
                                      ", ")
                         14)
             "  "
             (symbol-name cmd))
            into x
            finally return
            (concat
             "Welcome to the Emacs shell ⚘  \nCheatsheet \n\n"
             (string-join x "\n")
             "\n")))

  ;; (eshell-banner-message
  ;;  '(concat "Favored/recent files:\n"
  ;;           (loopy (list item (my-recentf-for-motd))
  ;;                  (concat (concat "\n   \"" item "\"")))
  ;;           "\n\n"))
  )

;; Set up the hook my-real-eshell-post-command-hook as a substitute for
;; eshell-post-command-hook.
(add-hook 'eshell-pre-command-hook #'my-eshell-time-cmd-1)
(add-hook 'eshell-post-command-hook #'my-eshell-time-cmd-2)

;; Always time commands that take longer than one second.
(add-hook 'my-real-eshell-post-command-hook #'my-esh-print-elapsed-maybe)

;; Fully automatic backrefs!  No more my-copy-region-into-variable.
(add-hook 'eshell-mode-hook #'my-esh-assign-id)
(add-hook 'my-real-eshell-post-command-hook #'my-esh-save-output-into-backref)

;; Sync history on every command, not just on exit, in case I powercycle the computer
(add-hook 'eshell-post-command-hook #'eshell-write-history)

;; Misc
;; (add-hook 'my-real-eshell-post-command-hook #'my-esh-narrow-to-output 95)
(add-hook 'eshell-pre-command-hook #'my-eshell-timestamp-update)
(add-hook 'eshell-directory-change-hook #'my-eshell-rename)
(add-hook 'eshell-mode-hook #'my-eshell-rename)
(add-hook 'eshell-before-prompt-hook #'my-eshell-save-scrollback)

;; This shouldn't affect eshell, but does...  Maybe it's been fixed since.
;; (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)

;; Workaround bug in typing strings (Emacs tab-completion bug, see corfu #61)
(add-hook 'eshell-mode-hook #'my-corfu-turn-off)

(after! eshell
  ;; The natural pager for shell.el/eshell, since they lack all terminal features.
  ;; Bear in mind the setting will also apply to programs spawned from Emacs,
  ;; such as (possibly) Alacritty, RStudio & VSCodium, which may not be a problem,
  ;; but it would be hygienic to revert this setting when calling make-process.
  (setenv "PAGER" "cat"))

;; (use-package em-smart
;;   :custom ((eshell-review-quick-commands nil)
;;            (eshell-smart-space-goes-to-end t)
;;            (eshell-where-to-jump 'begin)))

;; See <f1> P esh-groups
(after! esh-module
  ;; (add-to-list 'eshell-modules-list 'eshell-smart)
  (add-to-list 'eshell-modules-list 'eshell-xtra))


;;; Keys


(after! eshell
  ;; civilize
  (keymap-set eshell-mode-map "C-S-n" #'my-new-eshell)

  (after! em-hist
    ;; be docile like M-x shell (don't hijack point)
    (keymap-unset eshell-hist-mode-map "<up>")
    (keymap-unset eshell-hist-mode-map "<down>")

    (keymap-set eshell-hist-mode-map [remap consult-history] #'my-esh-consult-history))

  ;; Narrow-widen helpers
  (keymap-set eshell-mode-map [remap next-line] #'my-esh-next-line)
  (keymap-set eshell-mode-map [remap previous-line] #'my-esh-prev-line)
  (keymap-set eshell-mode-map [remap eshell-next-prompt] #'my-esh-next-prompt)
  (keymap-set eshell-mode-map [remap eshell-previous-prompt] #'my-esh-previous-prompt)
  (keymap-set eshell-mode-map "<f4> n" (defrepeater #'my-esh-narrow-dwim))
  
  )

(defmacro my-hook-once (hook &rest body)
  "Add temporary actions to HOOK to run only once.
BODY is wrapped in a function run the next time HOOK is
triggered, and the function removes itself from HOOK before
executing BODY."
  (declare (indent defun))
  (let ((funcname (cl-gensym)))
    `(add-hook
      ,hook
      (defun ,funcname (&rest _)
        (remove-hook ,hook #',funcname)
        ;; (fmakunbound ',funcname)
        ;; (unintern (symbol-name ',funcname))
        ,@body))))

(set-eshell-alias! "less" "view-file $1")

;; Encourage idiomatic ways to work with Emacs
(after! eshell
  (after! em-ls
    (defun eshell/ls (&rest args)
      (if (null args)
          (dired-jump)
        (kill-new (apply #'concat args))
        "ls: ls is blocked, but added your input to kill ring.  Try find-file and yank?")))
  (after! em-dirs
    (defun eshell/cd (&rest args)
      (if (null args)
          (let ((default-directory "~"))
            (my-eshell-here))
        (kill-new (apply #'concat args))
        ;; (my-hook-once 'my-real-eshell-post-command-hook
        ;;   (eshell-previous-prompt 1))
        "cd: cd is blocked, but added your input to kill ring.  Try find-file and yank?"))))

;; Easy swapping between dired and eshell
;; Dired default unbound keys: `, b, E, J, K, r, z, <backspace>
;; Dired useless keys: h, 1234567890

(after! dired
  (bind-key "r"  #'my-eshell-here dired-mode-map))
(set-eshell-alias! "r" "dired-jump")
;; (set-eshell-alias! "q" "eshell/quit-and-close") ;; in doom already
;; (general-def dired-mode-map "M-r" #'my-eshell-here)
;; (general-def eshell-mode-map "M-r" #'dired-jump)
;; (general-def my-on-shell-output-map "z" #'dired-jump)
;; (defun eshell/b (&optional _args)
;;   (let ((default-directory (expand-file-name "..")))
;;     (my-eshell-here)))


;; undoom
(after! eshell
  (fmakunbound #'eshell/emacs) ;; give me access to emacs --help
  (setq! +eshell-enable-new-shell-on-split nil) ;; I prefer it pick a recent buffer
  (setq! eshell-input-filter #'eshell-input-filter-default)
  (setq! eshell-scroll-to-bottom-on-input nil)
  (setq! eshell-scroll-to-bottom-on-output nil)
  (map! :map eshell-mode-map "C-l" #'recenter-top-bottom))
