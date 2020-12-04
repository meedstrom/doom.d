;;; ../../kept/Emacs/conf-doom/lisp/junk.el -*- lexical-binding: t; -*-

(use-package! rainbow-blocks :defer :hook (ess-r-mode . rainbow-blocks-mode))

(use-package! twee-mode)

(use-package! crux)

(use-package! beancount
  :defer t
  :mode ((rx ".bean" (? "count") eot) . beancount-mode))

(set-frame-parameter nil 'fullscreen 'fullheight)

(put 'customize-themes 'disabled nil)
(put 'customize-group 'disabled nil)
(put 'customize-face 'disabled nil)

(setq pdf-view-midnight-colors (cons (face-foreground 'default)
                                     (face-background 'default)))

(setq-default
 abbrev-file-name (expand-file-name "abbrevs" doom-private-dir)
 ;; mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 select-enable-primary t
 shr-max-image-proportion 0.5
 ;; transient-mark-mode nil
 kill-read-only-ok t
 kill-ring-max 600
 byte-compile-warnings '(not free-vars)
 which-key-idle-delay 0.25
 view-read-only t
 +mu4e-backend 'offlineimap
 +mu4e-mu4e-mail-path "~/Maildir/"
 tab-always-indent t
 vc-msg-newbie-friendly-msg nil
 vc-msg-copy-id-to-kill-ring nil
 )

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


;; (defun my-set-rhistory (&rest r)
  ;; (setq! ess-history-directory default-directory)
  ;; (setq! ess-history-file ".Rhistory"))

;; (advice-add #'ess-set-working-directory :after #'my-set-rhistory)

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

(set-file-template! "\\.el$"
  :when #'+file-templates-in-emacs-dirs-p
  :trigger "__el"
  :mode #'emacs-lisp-mode)

;;;; Locate

(when (executable-find "updatedb")
  (run-with-timer 5 3600 #'my-index-locatedb))
(when (executable-find "duc")
  (run-with-timer 10 3600 #'my-index-duc))
(setq helm-locate-command "locate %s --database=${HOME}/locate.db -e -A --regex %s")
(setq counsel-locate-cmd #'my-counsel-locate-cmd)
(defun my-counsel-locate-cmd (input)
  "Return a shell command based on INPUT."
  (counsel-require-program "locate")
  (format "locate -i --database=\"$HOME/locate.db\" --regex '%s'"
          (counsel--elisp-to-pcre (ivy--regex input))))

;; Make counsel-locate & helm-locate faster? idk.
(when-let (dash (executable-find "dash"))
  (setq shell-file-name dash))

