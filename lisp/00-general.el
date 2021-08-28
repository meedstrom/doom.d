;; -*- lexical-binding: t; -*-

;; (debug-watch 'org-mode)

;; broken in native-comp
(use-package! snitch
  :config
  ;; (setq snitch-log-policy '(allowed blocked whitelisted blacklisted))
  ;; (setq snitch-log-verbose nil)
  (setq snitch-enable-notifications t)
  ;; (setq snitch-log-buffer-max-lines)
  (setq snitch-trace-timers nil)
  (snitch-mode))

(if (display-graphic-p)
    (progn
      ;; use caps instead of M-x
      (my-exec "setxkbmap" "-option" "caps:menu")
      (global-unset-key (kbd "M-x"))
      (global-unset-key (kbd "A-x"))
      (after! general ;; dafuq is this set
        (define-key general-override-mode-map (kbd "M-x") nil)
        (define-key general-override-mode-map (kbd "A-x") nil))
      ;; civilize emacs
      (define-key input-decode-map (kbd "<escape>") (kbd "C-g"))
      (define-key input-decode-map (kbd "C-g") (kbd "s-g")) ;; to unlearn
      )
  ;; (kill-emacs "Terminal unsupported. Run emacs -Q.")
  )

;; fix guix.el
(after! ws-butler
  (add-to-list 'ws-butler-global-exempt-modes #'minibuffer-inactive-mode))

(scroll-bar-mode)

(general-def magit-section-mode-map "M-1" nil)
(general-def magit-section-mode-map "M-2" nil)

(setc browse-url-handlers
      '(
        ;;("http.*\/\/github.com" . browse-url-generic)
        ("melpa.org" . browse-url-generic)
        ("." . eww-browse-url)))

(use-package! subed
  :config
  (define-key subed-mode-map (kbd "M-3") #'subed-decrease-start-time)
  (define-key subed-mode-map (kbd "M-4") #'subed-increase-start-time)
  (define-key subed-mode-map (kbd "M-5") #'subed-decrease-stop-time)
  (define-key subed-mode-map (kbd "M-6") #'subed-increase-stop-time)
  (define-key subed-mode-map (kbd "<f4>") #'subed-mpv-toggle-pause)
  )

(use-package! twee-mode)
(use-package! crux)
;; (use-package! esup)

(use-package! beancount
  :defer t
  :mode ((rx ".bean" (? "count") eot) . beancount-mode))

(use-package! form-feed
  :config (global-form-feed-mode))

;; Make previous-buffer not skip the R buffer
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

;; Good on floating WMs.
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))

(add-to-list 'default-frame-alist '(alpha . 85))
(set-face-background 'default "#000000")
(after! solaire-mode
 (set-face-background 'solaire-default-face "#000000")
 (set-face-foreground 'solaire-default-face "pale green"))

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

;; undoom
(after! company
  (run-with-timer 3 nil #'global-company-mode 0))

;; undoom, horrid performance hit in org
(global-hl-line-mode 0)

;; improve org performance
(global-auto-composition-mode 0)
(setc bidi-display-reordering nil)
(general-after-init ;; doesnt take
  (setc gc-cons-threshold 1600000))

(setc company-idle-delay 0.35)
(setc auth-sources '("~/.authinfo")) ;; https://magit.vc/manual/ghub/Storing-a-Token.html
(setc abbrev-file-name (expand-file-name "abbrevs" doom-private-dir))
;;(setc mouse-yank-at-point t)
(setc save-interprogram-paste-before-kill t)
(setc select-enable-primary t)
(setc custom-safe-themes t)
(setc recentf-max-saved-items 1200)
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
(setc ranger-map-style 'emacs)
(setc holiday-bahai-holidays nil)
(setc holiday-hebrew-holidays nil)
(setc holiday-other-holidays ;; personal holidays
      '((holiday-fixed 1 25 "Joel's birthday")
        (holiday-fixed 3 8 "Clarence's birthday")
        (holiday-fixed 4 1 "Karin's birthday")
        (holiday-fixed 4 11 "Griselda's birthday")
        (holiday-fixed 6 18 "Rickard's birthday")
        ;; (holiday-fixed 7 18 "Nath's birthday") ;; date?
        (holiday-fixed 6 27 "Yang Yu Ting's birthday")
        (holiday-fixed 9 24 "Lena's birthday")
        (holiday-fixed 12 10 "Simon's birthday")))
(setc holiday-general-holidays ;; Sweden
      '((holiday-fixed 1 1 "New Year's Day")
        (holiday-fixed 2 14 "Valentine's Day")
        (holiday-fixed 4 1 "April Fools' Day")
        (holiday-float 5 0 -1 "Mother's Day")
        (holiday-float 11 0 2 "Father's Day")
        (holiday-fixed 10 31 "Halloween")))

(setc +doom-dashboard-functions
      '(
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        ))

(add-to-list 'safe-local-variable-values '(require-final-newline . nil))
(add-to-list 'safe-local-variable-values '(require-final-newline . t))
(add-to-list 'safe-local-variable-values '(nameless-current-name . "secretary"))

;; (set-face-attribute 'nameless-face () :inherit nil)

(setc mediawiki-site-alist
      '(("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" nil "Main Page") ; put your user name and password if not using .authinfo
        ("WikEmacs" "http://wikemacs.org/" "username" "password" nil "Main Page")))

(setc mediawiki-site-default "WikEmacs")

(use-package! prism
  :init
  (add-hook 'prog-mode-hook #'prism-mode))

(general-after-init
  (global-hl-line-mode 0) ;; doesn't play well with paren-mode
  )

;; (setc prism-desaturations '(0))
;; (setc prism-lightens '(0))
;; (setc prism-comments nil)
(setc prism-parens t)

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

(set-email-account! "Teknik"
  '((mu4e-sent-folder       . "/Sent")
    (mu4e-drafts-folder     . "/Drafts")
    (mu4e-trash-folder      . "/Trash")
    (mu4e-refile-folder     . "/Archive")
    (smtpmail-smtp-user     . "meedstrom@teknik.io")
    (smtpmail-default-smtp-server . "mail.teknik.io")
    (smtpmail-smtp-server . "mail.teknik.io")
    (mu4e-compose-signature . "Martin Edström"))
  t)

;; (defun my-log-process-name (&optional process _group)
;;   "See `interrupt-process-functions'."
;;   (when process
;;     (message (process-name process)))
;;   nil)

;; (add-to-list 'interrupt-process-functions #'my-log-process-name)

;; ;; (setq-default debug-on-signal 'quit debug-on-quit t)

(add-hook 'emacs-lisp-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'company-mode)
(add-hook 'doom-load-theme-hook #'my-fix-pdf-midnight-colors)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'java-mode-hook (defun my-java-setup ()
                            (setq c-basic-offset 4
                                  tab-width 4)))

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

;; (set-file-template! #'org-mode :ignore t)

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

(sachac/convert-shell-scripts-to-interactive-commands "~/bin")

(setc rmh-elfeed-org-files
      (list (expand-file-name "elfeed.org" doom-private-dir)))

(use-package! elfeed
  :defer
  :config
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :entry-title (rx (or "MCMXXX" "A&R"))
                                :add 'junk))
  (setc elfeed-curl-max-connections 1)
  (setc elfeed-search-filter "@2-months-ago -junk +unread +fav")
  ;; (ignore-errors (elfeed-org)) ;; does not work
  )
