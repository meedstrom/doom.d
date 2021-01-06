;; -*- lexical-binding: t; -*-

(use-package! rainbow-blocks :defer :hook (ess-r-mode . rainbow-blocks-mode))
(use-package! twee-mode)
(use-package! crux)
(use-package! esup)
(use-package! beancount
  :defer t
  :mode ((rx ".bean" (? "count") eot) . beancount-mode))
(use-package! form-feed
  :config (global-form-feed-mode))
(use-package! secretary
  :init
  (setc org-clock-x11idle-program-name "xprintidle")
  (setc secretary-user-name "Martin")
  (setc secretary-user-short-title "sir")
  (setc secretary-user-birthday "1991-12-07")
  (setc secretary-ai-name "Maya")
  :config
  ;; (secretary-mode)
  (add-hook 'secretary-plot-hook #'secretary-plot-mood 50)
  (add-hook 'secretary-plot-hook #'secretary-plot-weight)
  ;; (remove-hook 'window-selection-change-functions #'secretary-log-buffer)
  ;; (remove-hook 'window-buffer-change-functions #'secretary-log-buffer)
  )

(use-package! escape-modality
  :config
  ;; (esm-xmodmap-reload)
  ;; (esm-xcape-reload)
  ;; (massmap-tidy-mode)
  ;; (escape-modality-global-mode)
  ;; (deianira-global-mode)
  (esm--get-relevant-bindings)
  (general-def "C-x ;" #'comment-or-uncomment-region)
  (general-def "C-x C-;" #'comment-or-uncomment-region))

;; Lisp-friendly hippie expand
;; Thanks https://github.com/flyingmachine/emacs-for-clojure
(setc hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(set-frame-parameter nil 'fullscreen 'fullheight)

;; undoom
(put 'customize-themes 'disabled nil)
(put 'customize-group 'disabled nil)
(put 'customize-face 'disabled nil)
(put 'customize-variable 'disabled nil)


(setc abbrev-file-name (expand-file-name "abbrevs" doom-private-dir))
;;(setc mouse-yank-at-point t)
(setc save-interprogram-paste-before-kill t)
(setc select-enable-primary t)
(setc custom-safe-themes t)
(setc recentf-max-saved-items 600)
(setc shr-max-image-proportion 0.5)
(setc suggest-key-bindings nil)
(setc kill-read-only-ok t)
(setc kill-ring-max 600)
;; (setc byte-compile-warnings '(not free-vars))
(setc which-key-idle-delay 0.25)
(setc view-read-only t)
(setc load-prefer-newer t) ;; don't spend another minute confused by this
(setc tab-always-indent t)
(setc vc-msg-newbie-friendly-msg nil)
(setc vc-msg-copy-id-to-kill-ring nil)
(setc display-line-numbers-type nil) ; undoom
(setc ws-butler-keep-whitespace-before-point t) ; undoom
(setc garbage-collection-messages nil)
(setc auto-save-no-message t)
(setc fill-column 79)
(setc nameless-prefix "✳")
;; (setc nameless-prefix "")
(setc nameless-private-prefix nil)
;; (setc nameless-private-prefix "🔒-")
;; (setc nameless-prefix "⚘")

(setc +doom-dashboard-functions
      '(
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        ))

;; (set-face-attribute 'nameless-face () :inherit nil)

(setc mediawiki-site-alist
      '(("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" nil "Main Page") ; put your user name and password if not using .authinfo
        ("WikEmacs" "http://wikemacs.org/" "username" "password" nil "Main Page")))

(setc mediawiki-site-default "WikEmacs")

(defun my-file-size (file)
  "Returns the size of FILE (in DIR) in bytes."
  (unless (file-readable-p file)
    (error "File %S is unreadable; can't acquire its filesize"
           file))
  (nth 7 (file-attributes file)))

(setq my-all-git-repos
      (seq-filter (lambda (x)
                    (and (file-directory-p x)
                         (member ".git" (directory-files x))))
                  (append '("/home/kept/Knowledge_base"
                            "/home/kept/Journal/Finances"
                            "/home/kept/Journal"
                            "/home/kept/Guix"
                            "/home/kept/Guix channel"
                            "/home/kept/Fiction"
                            "/home/kept/Dotfiles")
                          (directory-files "/home/kept/Emacs" t)
                          (directory-files "/home/kept/Code" t)
                          (directory-files "/home/kept/Coursework" t))))

(defun my-memacs-scan-git ()
  (make-directory "/tmp/rev-lists" t)
  (and (file-exists-p "/home/kept/Archive/memacs/git/")
       (executable-find "git")
       (executable-find "memacs_git")
       (bound-and-true-p my-all-git-repos)
       (dolist (x my-all-git-repos t)
         (start-process-shell-command
          "memacs_git_stage_1"
          nil
          (concat "cd \"" x "\" && git rev-list --all --pretty=raw > \"/tmp/rev-lists/"
                  (file-name-nondirectory x) "\"")))
       (run-with-timer
        5 nil (lambda ()
                (dolist (x (directory-files "/tmp/rev-lists" t
                                            (rx bol (not (any "." "..")))))
                  (unless (= 0 (my-file-size x))
                    (start-process-shell-command
                     "memacs_git_stage_2" nil
                     (concat "memacs_git -f "
                             x
                             " -o /home/kept/Archive/memacs/git/"
                             (file-name-nondirectory x)
                             ".org_archive")))))))
  (run-with-timer (* 60 60) nil #'my-memacs-scan-git))
;; (my-memacs-scan-git)


;; mu4e manually
;;
;; mail-user-agent 'mu4e-user-agent
;; mu4e-sent-folder "/Sent"
;; mu4e-drafts-folder "/Drafts"
;; mu4e-trash-folder "/Trash"
;; mu4e-refile-folder  "/Archive"
;; message-send-mail-function 'smtpmail-send-it
;; smtpmail-default-smtp-server "mail.teknik.io"
;; smtpmail-smtp-server "mail.teknik.io"
;; smtpmail-local-domain "teknik.io"
;; smtpmail-smtp-user "meedstrom@teknik.io"
;; mu4e-get-mail-command "offlineimap"

(setq +mu4e-backend 'offlineimap
      +mu4e-mu4e-mail-path "~/Maildir/")

(set-email-account! "Teknik.io"
  '((mu4e-sent-folder       . "/Sent")
    (mu4e-drafts-folder     . "/Drafts")
    (mu4e-trash-folder      . "/Trash")
    (mu4e-refile-folder     . "/Archive")
    (smtpmail-smtp-user     . "meedstrom@teknik.io")
    (smtpmail-default-smtp-server . "mail.teknik.io")
    (smtpmail-smtp-server . "mail.teknik.io")
    (mu4e-compose-signature . "\nMartin Edström"))
  t)

;; (defun my-log-process-name (&optional process _group)
;;   "See `interrupt-process-functions'."
;;   (when process
;;     (message (process-name process)))
;;   nil)

;; (add-to-list 'interrupt-process-functions #'my-log-process-name)

;; ;; (setq-default debug-on-signal 'quit debug-on-quit t)

(add-hook 'doom-load-theme-hook #'my-fix-pdf-midnight-colors)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'java-mode-hook (defun my-java-setup ()
                            (setq c-basic-offset 4
                                  tab-width 4)))

(auto-save-visited-mode)
(save-place-mode)
(display-battery-mode)

;; No unclosed/unfinished quotes messing up the highlighting all over the
;; buffer. Do other people not get this problem? I don't hear about it...
;; TODO: take a screenshot of the problem, for a blog post, then uncomment this
;; (after! ess-r-mode
  ;; (modify-syntax-entry (string-to-char "\"") "w" inferior-ess-r-mode-syntax-table))
;; (after! ledger-report
  ;; (modify-syntax-entry (string-to-char "\"") "w" ledger-report-mode-syntax-table))

;;;; Ledger

(setq ledger-post-amount-alignment-at :decimal)
(setq ledger-post-auto-align t)
(setq ledger-default-date-format "%Y%m%d")

(after! ledger-report
  (setq ledger-reports
        '(("balance"    "%(binary) -f %(ledger-file) -cHX SEK balance")
          ("balance 2020"       "%(binary) -f %(ledger-file) -cHX SEK balance -p 2020")
          ("balance monthly"    "%(binary) -f %(ledger-file) -cHX SEK balance -p %(month) --empty")
          ("balance Aug-Dec (with budget)"     "%(binary) -f %(ledger-file) -cHX SEK balance -b 2020-8 -e 2021 --empty --budget")
          ("expenses monthly"   "%(binary) -f %(ledger-file) -cHX SEK register ^Expenses -p %(month)")
          ("register"           "%(binary) -f %(ledger-file) -cHX SEK register")
          ("account"            "%(binary) -f %(ledger-file) -cHX SEK register %(account)")
          ("accounts"           "%(binary) -f %(ledger-file) accounts"))))

;;;; Templates

;; For existing templates see `+file-templates-alist' and associated snippets in
;; /home/kept/Dotfiles/.emacs.d/modules/editor/file-templates/templates

(set-file-template! #'org-mode :ignore t)

(set-file-template! "\\.el$"
  :when #'+file-templates-in-emacs-dirs-p
  :trigger "__el"
  :mode #'emacs-lisp-mode)

;;;; Locate

(when (executable-find "updatedb")
  (run-with-timer 10 3600 #'my-index-locatedb))

(when (executable-find "duc")
  (run-with-timer 15 3600 #'my-index-duc))

(setc helm-locate-command "locate %s --database=${HOME}/locate.db -e -A --regex %s")

;; Hopefully make shell commands like `helm-locate-command' a bit faster.
(when-let (dash (executable-find "dash"))
  (setc shell-file-name dash))
