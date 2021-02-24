;; -*- lexical-binding: t; -*-

(scroll-bar-mode)

(use-package! rainbow-blocks :defer :hook (ess-r-mode . rainbow-blocks-mode))
(use-package! twee-mode)
(use-package! crux)
;; (use-package! esup)
(use-package! beancount
  :defer t
  :mode ((rx ".bean" (? "count") eot) . beancount-mode))
(use-package! form-feed
  :config (global-form-feed-mode))
(use-package! secretary-config
  :init
  (setc secretary-x11idle-program-name "xprintidle")
  (setc secretary-user-name "Martin")
  (setc secretary-user-short-title "sir")
  (setc secretary-user-birthday "1991-12-07")
  (setc secretary-ai-name "Maya")
  :config
  ;; (secretary-mode)
  (add-hook 'secretary-plot-hook #'secretary-plot-mood 50)
  (add-hook 'secretary-plot-hook #'secretary-plot-weight)
  ;; (add-hook 'window-selection-change-functions #'secretary-log-buffer)
  ;; (add-hook 'window-buffer-change-functions #'secretary-log-buffer)
  )

(el-patch-defun doom-buffer-frame-predicate (buf)
  "To be used as the default frame buffer-predicate parameter. Returns nil if
BUF should be skipped over by functions like `next-buffer' and `other-buffer'."
  (or (s-starts-with-p "*R:" (buffer-name buf))
      (doom-real-buffer-p buf)
      (eq buf (doom-fallback-buffer))))

(defhydra goto (:color blue :hint nil)
  "
Goto:
^Char^              ^Word^                ^org^                    ^search^
^^^^^^^^---------------------------------------------------------------------------
_c_: 2 chars        _w_: word by char     _h_: headline in buffer  _o_: helm-occur
_C_: char           _W_: some word        _a_: heading in agenda   _p_: helm-swiper
_L_: char in line   _s_: subword by char  _q_: swoop org buffers   _f_: search forward
^  ^                _S_: some subword     ^ ^                      _b_: search backward
-----------------------------------------------------------------------------------
_B_: helm-buffers       _l_: avy-goto-line
_m_: helm-mini          _i_: ace-window
_R_: helm-recentf

_n_: Navigate           _._: mark position _/_: jump to mark
"
  ("c" avy-goto-char-2)
  ("C" avy-goto-char)
  ("L" avy-goto-char-in-line)
  ("w" avy-goto-word-1)
  ;; jump to beginning of some word
  ("W" avy-goto-word-0)
  ;; jump to subword starting with a char
  ("s" avy-goto-subword-1)
  ;; jump to some subword
  ("S" avy-goto-subword-0)

  ("l" avy-goto-line)
  ("i" ace-window)

  ("h" helm-org-headlines)
  ("a" helm-org-agenda-files-headings)
  ("q" helm-multi-swoop-org)

  ("o" helm-occur)
  ("p" swiper-helm)

  ("f" isearch-forward)
  ("b" isearch-backward)

  ("." org-mark-ring-push :color red)
  ("/" org-mark-ring-goto :color blue)
  ("B" helm-buffers-list)
  ("m" helm-mini)
  ("R" helm-recentf)
  ("n" hydra-navigate/body))

(general-def (kbd "M-r") #'goto/body)

(defun my-negative-argument (arg)
  "Begin a negative numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (interactive "P")
  ;; (message (key-description (vector last-command-event)))
  ;; (prefix-command-preserve-state)
  (let ((stem (esm--get-stem (key-description (vector last-command-event)))))
    (cond ((equal stem "C-")
           (event-apply-control-modifier t))
          ((equal stem "M-")
           (event-apply-meta-modifier t))
          ((equal stem "s-")
           (event-apply-super-modifier t))
          ((equal stem "H-")
           (event-apply-hyper-modifier t))
          ((equal stem "A-")
           (event-apply-alt-modifier t))))
  (setq prefix-arg (cond ((integerp arg) (- arg))
                         ((eq arg '-) nil)
                         (t '-)))
  (universal-argument--mode))

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

;; Undoom. Was this a Vimism?
(after! ws-butler
  (setc ws-butler-keep-whitespace-before-point t))
(general-after-init
  (setc ws-butler-keep-whitespace-before-point t))

;; (defun org-version () nil "9.5") ;; org-drill breaks when this returns empty

(setc company-idle-delay 0.25)
(setc auth-sources '("~/.authinfo")) ;; https://magit.vc/manual/ghub/Storing-a-Token.html
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
(setc indent-tabs-mode nil)
(setc vc-msg-newbie-friendly-msg nil)
(setc vc-msg-copy-id-to-kill-ring nil)
(setc display-line-numbers-type nil) ; undoom
(setc garbage-collection-messages nil)
(setc auto-save-no-message t)
(setc fill-column 79)
(setc nameless-prefix ".")
;; (setc nameless-prefix "")
;; (setc nameless-prefix "✳")
(setc nameless-private-prefix t)
;; (setc nameless-private-prefix nil)
;; (setc nameless-private-prefix "🔒-")
;; (setc nameless-prefix "⚘")
(setc calendar-chinese-all-holidays-flag t)
(setc holiday-bahai-holidays nil)
(setc holiday-hebrew-holidays nil)
(setc holiday-other-holidays '((holiday-fixed 6 27 "Diana's birthday")
                               (holiday-fixed 1 25 "Joel's birthday")))

;; remove holidays I'm not familiar with
(after! holidays   
  (setc calendar-holidays
        (seq-difference calendar-holidays '((holiday-fixed 7 4 "Independence Day")
                                            (holiday-float 10 1 2 "Columbus Day")
                                            (holiday-fixed 11 11 "Veteran's Day")
                                            (holiday-fixed 6 14 "Flag Day")
                                            (holiday-float 9 1 1 "Labor Day")
                                            (holiday-fixed 3 17 "St. Patrick's Day")
                                            (holiday-fixed 2 2 "Groundhog Day")
                                            (holiday-float 2 1 3 "President's Day")))))

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

(add-hook 'prog-mode-hook #'company-mode)
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

(sachac/convert-shell-scripts-to-interactive-commands "~/bin")
