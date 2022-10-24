;; -*- lexical-binding: t; -*-

(require 'subr-x)

(set-eshell-alias! "less" "view-file $1")
(set-eshell-alias! "r" "dired-jump")

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
;;      (concat "[--:--] " (if (>= my-eshell-backref-counter 35)
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

;; (my-syms-starting-with "my-eshell-")

(after! eshell
  (setopt eshell-prompt-function
          (lambda ()
            (concat "〈 [--:--] " (if (>= my-eshell-backref-counter 35)
                                      "---"
                                    "--") " 〉 ")))
  (setopt eshell-prompt-regexp (rx "〈 " (*? anychar) " 〉 "))
  (setopt eshell-scroll-show-maximum-output nil)
  (setopt eshell-show-lisp-completions t)

  ;; TODO: instead of building a string to return all at once, insert text
  ;; iteratively in the buffer so we can give it text properties.
  (setopt eshell-banner-message
          '(cl-loop
            for cmd in
            (append
             (my-commands-starting-with "my-eshell-")
             '(my-copy-region-or-rest-of-line-to-other-window
               my-cycle-path-at-point-repeat
               my-insert-other-buffer-file-name-and-cycle-repeat
               my-eval-and-replace-print
               my-replace-var-at-point-with-value
               my-pipe
               my-new-eshell
               my-next-buffer-of-same-mode-repeat
               my-previous-buffer-of-same-mode-repeat
               app-launcher-run-app
               dired-jump
               helm-selector-shell
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
             "\n"))))

;; Set up the hook `my-real-eshell-post-command-hook' as a reliable substitute
;; for eshell-post-command-hook.
(add-hook 'eshell-pre-command-hook #'my-eshell-time-cmd-1)
(add-hook 'eshell-post-command-hook #'my-eshell-time-cmd-2)

;; Always time slow commands. No more rerunning just to prepend "time ..."
(add-hook 'my-real-eshell-post-command-hook #'my-eshell-print-elapsed-maybe)

;; Save all command outputs as variables! No more my-copy-region-into-variable.
(add-hook 'eshell-mode-hook #'my-eshell-assign-id) ;; used in naming variables
(add-hook 'my-real-eshell-post-command-hook #'my-eshell-save-output-into-backref)

;; Sync history on every command, in case I powercycle the computer
(add-hook 'my-real-eshell-post-command-hook #'eshell-write-history)
(add-hook 'eshell-before-prompt-hook #'my-eshell-save-scrollback)

;; Timestamp the exact time they command was executed
(add-hook 'eshell-pre-command-hook #'my-eshell-timestamp-update)

;; Name the buffer so I can see the direetory in the minibuffer.
(add-hook 'eshell-directory-change-hook #'my-eshell-rename)
(add-hook 'eshell-mode-hook #'my-eshell-rename)

;; Misc
;; (add-hook 'my-real-eshell-post-command-hook #'my-eshell-narrow-to-output 95)

;; The natural pager for shell.el/eshell, since they lack all terminal features.
;; Bear in mind the setting will also apply to programs spawned from Emacs,
;; such as (let's say) Alacritty, RStudio & VSCodium, which may not be a problem,
;; but it would be hygienic to revert this setting when calling make-process.
(setenv "PAGER" "cat")

;; TODO: try the "smart" thing for a while
;; (use-package em-smart
;;   :custom ((eshell-review-quick-commands nil)
;;            (eshell-smart-space-goes-to-end t)
;;            (eshell-where-to-jump 'begin)))

;; Try some extra modules, see C-h P esh-groups
(after! esh-module
  ;; (add-to-list 'eshell-modules-list 'eshell-smart)
  (add-to-list 'eshell-modules-list 'eshell-xtra))

(after! em-hist
  (keymap-set eshell-hist-mode-map [remap consult-history] #'my-eshell-consult-history))

(after! eshell
  ;; Automatically narrow/widen to output on point motion.  Damn, it's weird
  ;; and often not what I want, but that's me abusing point motion.
  (keymap-set eshell-mode-map [remap next-line] #'my-eshell-next-line)
  (keymap-set eshell-mode-map [remap previous-line] #'my-eshell-prev-line)
  (keymap-set eshell-mode-map [remap eshell-next-prompt] #'my-eshell-next-prompt)
  (keymap-set eshell-mode-map [remap eshell-previous-prompt] #'my-eshell-previous-prompt))

;; Encourage idiomatic ways to work with Emacs
(after! eshell
  (after! em-ls
    (defun eshell/ls (&rest args)
      (if (null args)
          (dired-jump)
        (kill-new (apply #'concat args))
        "ls: ls is blocked, but added your input to kill ring.  Try C-x C-f C-y RET?")))
  (after! em-dirs
    (defun eshell/cd (&rest args)
      (if (null args)
          (let ((default-directory "~"))
            (my-eshell-here))
        (kill-new (apply #'concat args))
        ;; (my-hook-once 'my-real-eshell-post-command-hook
        ;;   (eshell-previous-prompt 1))
        "cd: cd is blocked, but added your input to kill ring.  Try C-x C-f C-y RET?"))))

;; TODO: Make a command that cycles between a trio of buffers: the dired, the
;; eshell, and the buffer it was first called from.
(keymap-set global-map "M-r" #'my-dired-jump)
(keymap-set eshell-mode-map "M-r" #'dired-jump)
(keymap-set dired-mode-map "M-r" #'my-eshell-here)

;; Emulate my Dired "b" key for going up one directory.
(defun eshell/b (&optional _args)
  (let ((default-directory (expand-file-name "..")))
    (my-eshell-here)))

;; undoom
(after! eshell
  (fmakunbound #'eshell/emacs) ;; give me access to emacs --help
  (setq! +eshell-enable-new-shell-on-split nil) ;; I prefer it pick a recent buffer
  (setq! eshell-input-filter #'eshell-input-filter-default)
  (setq! eshell-scroll-to-bottom-on-input nil)
  (setq! eshell-scroll-to-bottom-on-output nil)
  (map! :map eshell-mode-map "C-l" #'recenter-top-bottom))
