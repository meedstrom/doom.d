;; Eshell config -*- lexical-binding: t; -*-

;; Wishlist:

;; - Unbreak the prompts occasionally going read-only
;;   (Fixed by just not applying read-only)

;; - Stop writing .eshell-scrolback and .eshell-command-history, keep track in
;;   .emacs.d/cache/ instead

;;   - Stretch goal: For robustness, attempt to sync to at least two places:
;;     locally in the dir AND in .emacs.d.  To merge mismatched syncs, just add
;;     together the logs, which are of course timestamped to the unix
;;     nanosecond, sort by time, and dedup. This way, the local dir file can
;;     regenerate from .emacs.d, and .emacs.d can regenerate from the local dir
;;     file!  And local file need not exist at all (helps when dir unwritable,
;;     or when the user disabled writing local files).

(require 'subr-x)

(set-eshell-alias! "less" "view-file $1")

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

;; OK, so using a regexp is pretty unreliable.  The way shell-mode does it, it
;; has a shell-prompt-pattern, but won't use it by default for anything other
;; than C-c C-p motion.  Instead, it relies on comint use of the "field" text
;; property to mark a prompt.  Docstring of `shell-prompt-pattern' says: The
;; pattern should probably not match more than one line.  If it does, Shell mode
;; may become confused trying to distinguish prompt from input on lines which
;; don't start with a prompt.
(after! eshell
  (setopt eshell-prompt-function (lambda () "〈 ／／ 〉 "))
  (setopt eshell-prompt-regexp "^〈 .*? 〉 ")
  (setopt eshell-show-lisp-completions t)
  (setopt eshell-scroll-show-maximum-output nil) ;; ??
  (setopt eshell-scroll-to-bottom-on-output 'this)

  ;; TODO: give the different parts of the string different text properties
  (setopt eshell-banner-message
          '(cl-loop
            for cmd in
            (append
             ;; (my-commands-starting-with "my-esh-")
             '(my-esh-consult-history
               my-esh-switch
               my-esh-narrow-to-output
               my-esh-narrow-to-prompt
               my-esh-narrow-dwim
               my-copy-region-or-rest-of-line-to-other-window
               my-cycle-path-at-point-repeat
               my-dired-shell-cycle
               my-insert-other-buffer-file-name-and-cycle-repeat
               my-eval-and-replace-print
               my-replace-var-at-point-with-value
               my-pipe
               my-new-eshell
               my-next-buffer-of-same-mode-repeat
               my-previous-buffer-of-same-mode-repeat
               dired-jump
               shelldon))
            with hints
            collect (concat (string-pad (car (my-locate-keys cmd)) 12)
                            "  "
                            (symbol-name cmd))
            into hints
            finally return
            (concat
             "Welcome to the Emacs shell ⚘  \nCommands you may find nifty: \n\n"
             (string-join (cl-sort hints #'string-lessp) "\n")
             "\n"))))

;; Set up the hook `my-real-eshell-post-command-hook' as a reliable substitute
;; for eshell-post-command-hook.
(add-hook 'eshell-pre-command-hook #'my-esh-time-cmd-1)
(add-hook 'eshell-post-command-hook #'my-esh-time-cmd-2)
;; (add-hook 'eshell-post-command-hook #'end-of-buffer)

(hookgen eshell-mode-hook
  (add-hook 'post-command-hook #'scroll-right nil t))

;; Always time slow commands. No more rerunning just to prepend "time ..."
(add-hook 'my-real-eshell-post-command-hook #'my-esh-print-elapsed-maybe)

;; Save all command outputs as variables! No more my-copy-region-into-variable.
(add-hook 'my-real-eshell-post-command-hook #'my-esh-save-output-into-backref)

;; Sync history on every command, in case I powercycle the computer
(add-hook 'my-real-eshell-post-command-hook #'eshell-write-history)
(add-hook 'eshell-before-prompt-hook #'my-esh-save-scrollback)

;; Timestamp the exact time they command was executed
(add-hook 'eshell-pre-command-hook #'my-esh-timestamp-update)

;; Name the buffer so I can see the direetory in the minibuffer.
(add-hook 'eshell-directory-change-hook #'my-esh-rename)
(add-hook 'eshell-mode-hook #'my-esh-rename)

;; Misc
;; (add-hook 'my-real-eshell-post-command-hook #'my-esh-narrow-to-output 95)

;; The natural pager for shell.el/eshell, since they lack all terminal features.
;; Bear in mind the setting will also apply to programs spawned from Emacs,
;; such as (let's say) Alacritty, RStudio & VSCodium, which may not be a problem,
;; but it would be hygienic to revert this setting when calling make-process.
;; (setenv "PAGER" "cat")

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
  (setopt eshell-hist-ignoredups t)
  (define-key eshell-hist-mode-map [remap consult-history] #'my-esh-consult-history))

(after! esh-mode
  ;; Automatically narrow/widen to output on point motion.  Damn, it's weird
  ;; and often not what I want, but that's me abusing point motion.
  ;; (define-key eshell-mode-map [remap next-line] #'my-esh-next-line)
  ;; (define-key eshell-mode-map [remap previous-line] #'my-esh-prev-line)
  ;; (define-key eshell-mode-map [remap eshell-next-prompt] #'my-esh-next-prompt)
  ;; (define-key eshell-mode-map [remap eshell-previous-prompt] #'my-esh-previous-prompt)
  )

;; Encourage idiomatic ways to work with Emacs
;; (after! eshell
;;   (after! em-ls
;;     (defun eshell/ls (&rest args)
;;       (if (null args)
;;           (dired-jump)
;;         (kill-new (apply #'concat args))
;;         "ls: ls is blocked, but added your input to kill ring.  Try C-x C-f C-y RET?")))
;;   (after! em-dirs
;;     (defun eshell/cd (&rest args)
;;       (if (null args)
;;           (let ((default-directory "~"))
;;             (my-esh-here))
;;         (kill-new (apply #'concat args))
;;         ;; (my-hook-once 'my-real-eshell-post-command-hook
;;         ;;   (eshell-previous-prompt 1))
;;         "cd: cd is blocked, but added your input to kill ring.  Try C-x C-f C-y RET?"))))

;; Emulate my Dired "b" key for going up one directory.
(defun eshell/b (&optional _args)
  (let ((default-directory (file-name-parent-directory default-directory)))
    (my-esh-here)))
