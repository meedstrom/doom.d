;; -*- lexical-binding: t; -*-

;; Backups still save my skin, as of 2020.
(setq
 backup-directory-alist `((,tramp-file-name-regexp . nil) ;; exclude TRAMP, bad experience
                          ("." . "/home/backups"))
 delete-old-versions t ;; nil led to Emacs appearing broken for newbie me
 vc-make-backup-files t ;; I don't commit regularly in every project
 make-backup-files t ;; WHY did Doom disable it
 version-control t)

;; for magit, maybe email
(setq user-full-name "Martin Edström")
(setq user-mail-address "meedstrom@teknik.io")

;; (setq doom-font (font-spec :family "Cozette" :size 10))
;; (setq doom-font (font-spec :family "Tamzen" :size 10))
(setq doom-font (font-spec :family "Terminus" :size 15))
;; (setq doom-font (font-spec :family "monospace" :size 30))
;; (setq doom-font (font-spec :family "Hasklig" :size 14))
;; (setq doom-font (font-spec :family "Iosevka" :size 14))

;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-tomorrow-night)
;; (setq doom-theme 'doom-dark+)
;; (setq doom-theme 'doom-manegarm)
;; (setq doom-theme 'doom-storage-tube-amber-2)
;; (setq doom-theme 'doom-Iosvkem)
;; (setq doom-theme 'doom-zenburn)
;; (setq doom-theme 'doom-outrun-electric)
;; (setq doom-theme 'doom-badger)
(setq doom-theme 'doom-rouge)

;; forgot why
;; (add-hook 'doom-load-theme-hook
;;           (lambda ()
;;             (set-face-attribute 'fixed-pitch-serif () :inherit 'default)))

;; (defun my-log-process-name (&optional process _group)
;;   "See `interrupt-process-functions'."
;;   (when process
;;     (message (process-name process)))
;;   nil)

;; (add-to-list 'interrupt-process-functions #'my-log-process-name)

;; (setq-default debug-on-signal 'quit debug-on-quit t)


;; DEPRECATED: use the variable init-file-debug
;; (setq my-debug-p doom-debug-p)

;; (debug-watch 'org-mode)

;; ;; NOTE: broken in native-comp
;; (use-package! snitch
;;   :config
;;   ;; (setq snitch-log-policy '(allowed blocked whitelisted blacklisted))
;;   ;; (setq snitch-log-verbose nil)
;;   (setq snitch-enable-notifications t)
;;   ;; (setq snitch-log-buffer-max-lines)
;;   (setq snitch-trace-timers nil)
;;   (snitch-mode))

;;; Some keyboard config

(if (display-graphic-p)
    (progn
      ;; use caps instead of M-x
      (when (eq 'window-system 'x)
        (my-exec "setxkbmap" "-option" "caps:menu"))
      (global-unset-key (kbd "M-x"))
      (global-unset-key (kbd "A-x"))
      (global-set-key (kbd "<menu>") #'execute-extended-command)
      (after! general
        ;; dafuq is this set for?
        (general-unbind general-override-mode-map "M-x")
        (general-unbind general-override-mode-map "A-x")
        ;; guess I should take a page from their book
        (general-def general-override-mode-map "<menu>" #'execute-extended-command))
      ;; civilize emacs
      (define-key input-decode-map (kbd "<escape>") (kbd "C-g"))
      (define-key input-decode-map (kbd "C-g") (kbd "s-g")) ;; to unlearn
      )
  ;; (kill-emacs "Terminal unsupported. Run emacs -Q.")
  )

;;; Visuals shit

;; Good on floating WMs.
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))

;; good for ... most themes, including Iosvkem
;; (set-face-background 'default "#000000")
(after! solaire-mode
  ;; (set-face-background 'solaire-default-face "#000700")
  ;; (set-face-foreground 'solaire-default-face "pale green")
  )
(my-fix-pdf-midnight-colors)

;;; General settings

;; undoom
(put 'customize-themes 'disabled nil)
(put 'customize-group 'disabled nil)
(put 'customize-changed 'disabled nil)
(put 'customize-face 'disabled nil)
(put 'customize-variable 'disabled nil)

;; Undoom. Was this a Vimism?
(after! ws-butler
  (setopt ws-butler-keep-whitespace-before-point t))
;; (general-after-init
;;   (setopt ws-butler-keep-whitespace-before-point t))

(setopt auth-sources '("~/.authinfo")) ;; https://magit.vc/manual/ghub/Storing-a-Token.html
(setopt abbrev-file-name (expand-file-name "abbrevs" doom-private-dir))
;;(setopt mouse-yank-at-point t)
(setopt save-interprogram-paste-before-kill t)
(setopt select-enable-primary t)
(setopt rainbow-x-colors nil) ;; only colorize hex strings
;; (setopt browse-url-handlers
;; '(
;; ("http.*\/\/github.com" . browse-url-generic)
;; ("melpa.org" . browse-url-generic-program)
;; ("." . eww-browse-url)))
(setopt custom-safe-themes t)
(setopt recentf-max-saved-items 1200)
(setopt shr-max-image-proportion 0.5)
(setopt suggest-key-bindings nil)
(setopt calibredb-root-dir "~/Calibre Library/")
(setopt calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
(setopt calibredb-format-width 8)
(setopt kill-read-only-ok t)
(setopt kill-ring-max 600)
;; (setopt byte-compile-warnings '(not free-vars))
(setopt which-key-idle-delay 0.25)
(setopt corfu-auto-delay 0.35)
(setopt view-read-only t)
(setopt load-prefer-newer t) ;; don't spend another minute confused by this
(setopt tab-always-indent 'complete)
(setopt indent-tabs-mode nil)
(setopt vc-msg-newbie-friendly-msg nil)
(setopt vc-msg-copy-id-to-kill-ring nil)
(setopt display-line-numbers-type nil) ; undoom
(setopt garbage-collection-messages nil)
(setopt auto-save-no-message t)
(setopt fill-column 79)
(setopt calendar-chinese-all-holidays-flag t)
(setopt ranger-map-style 'emacs)
(setopt holiday-bahai-holidays nil)
(setopt holiday-hebrew-holidays nil)
(setopt holiday-other-holidays ;; personal holidays
        '((holiday-fixed 1 25 "Joel's birthday")
          (holiday-fixed 3 8 "Clarence's birthday")
          (holiday-fixed 4 1 "Karin's birthday")
          (holiday-fixed 4 11 "Griselda's birthday")
          (holiday-fixed 6 18 "Rickard's birthday")
          ;; (holiday-fixed 7 18 "Nath's birthday") ;; date?
          (holiday-fixed 6 27 "Yang Yu Ting's birthday")
          (holiday-fixed 9 24 "Lena's birthday")
          (holiday-fixed 12 10 "Simon's birthday")))
(setopt holiday-general-holidays ;; Sweden
        '((holiday-fixed 1 1 "New Year's Day")
          (holiday-fixed 2 14 "Valentine's Day")
          (holiday-fixed 4 1 "April Fools' Day")
          (holiday-float 5 0 -1 "Mother's Day")
          (holiday-float 11 0 2 "Father's Day")
          (holiday-fixed 10 31 "Halloween")))

(setopt +doom-dashboard-functions
        '(
          doom-dashboard-widget-shortmenu
          doom-dashboard-widget-loaded
          ))

;; NOTE: put your user name and password if not using .authinfo
(setopt mediawiki-site-alist
        '(("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" nil "Main Page")
          ("WikEmacs" "http://wikemacs.org/" "username" "password" nil "Main Page")))

(setopt mediawiki-site-default "WikEmacs")

(add-to-list 'safe-local-variable-values '(require-final-newline . nil))
(add-to-list 'safe-local-variable-values '(require-final-newline . t))


;;; Hooks

;; Config hippie-expand
(add-hook! (special-mode prog-mode text-mode) #'my-hippie-mod)

(add-hook 'after-save-hook #'my-compile-and-drop)
;; (add-hook 'emacs-lisp-mode-hook #'display-fill-column-indicator-mode)
;; (add-hook 'prog-mode-hook #'company-mode)
(add-hook 'doom-load-theme-hook #'my-fix-pdf-midnight-colors)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'java-mode-hook (defun my-java-setup ()
                            (require 'cc-vars)
                            (setq c-basic-offset 4
                                  tab-width 4)))

;; i finally got auto-save-visited-mode to work again, so dont need this
;; (require 'l)
;; (add-hook 'doom-switch-buffer-hook (l'save-some-buffers t))
;; (add-hook 'doom-switch-window-hook (l'save-some-buffers t))

;; doesn't play well with paren-mode + certain themes
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; seems something quietly disables auto save mode
;; (add-hook 'after-save-hook #'my-fix-invalid-backup-settings)
