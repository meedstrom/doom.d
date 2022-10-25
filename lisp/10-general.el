;; -*- lexical-binding: t; -*-

(add-load-path! "/home/kept/emacs/key-seqs-finder/"
                "/home/kept/emacs/twee-mode/")

;; Backups still save my skin, as of 2020.
;; I put them in the unusual place /home/backups to prevent cluttering rg.
(setq
 backup-directory-alist `((,tramp-file-name-regexp . nil) ;; exclude TRAMP, bad experience
                          ("." . "/home/backups"))
 delete-old-versions t ;; nil led to Emacs appearing broken for newbie-me
 vc-make-backup-files t ;; I don't commit regularly in every project
 make-backup-files t ;; WHY did Doom disable it
 version-control t)

;; check
(unless (file-writable-p "/home/backups/")
  (error "Disabling backups because can't write to: /home/backups/")
  (setq backup-directory-alist nil)
  (setq make-backup-files nil))

;; For Magit, also for email when I get around to that.
(setq user-full-name "Martin Edström")
(setq user-mail-address "meedstrom@teknik.io")


;;; Font and theme

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


;;; Debugging

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


;;; Stuff

;; Set up booleans I use here and there throughout init.
(defvar debian (executable-find "apt-get"))
(defvar arch (string-match "arch" operating-system-release))
(defvar gentoo (string-match "gentoo" operating-system-release))
(defvar guix (string-match "shepherd" (shell-command-to-string "ps -q 1 -o comm=")))

(let* ((oses (list debian arch gentoo guix)))
  (unless (<= 1 (length (seq-filter (-not #'null) oses)))
    (warn "My init: Two or more OS checks succeeded")))

;; In case I'm not on a tiling WM.
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))

;; Blacken background, to make transparent windows more back-transparent
;; (set-face-background 'default "#000000")
(after! solaire-mode
  ;; (set-face-background 'solaire-default-face "#000700")
  ;; (set-face-foreground 'solaire-default-face "pale green")
  )
(my-fix-pdf-midnight-colors)

;; undoom
(put 'customize-themes 'disabled nil)
(put 'customize-group 'disabled nil)
(put 'customize-changed 'disabled nil)
(put 'customize-face 'disabled nil)
(put 'customize-variable 'disabled nil)

(setopt auth-sources '("~/.authinfo")) ;; https://magit.vc/manual/ghub/Storing-a-Token.html
(setopt abbrev-file-name (expand-file-name "abbrevs" doom-user-dir))
(setopt mouse-yank-at-point t)
(setopt save-interprogram-paste-before-kill t)
(setopt select-enable-primary t)
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
(setopt view-read-only t)
(setopt load-prefer-newer t) ;; don't spend another minute confused by this
(setopt indent-tabs-mode nil)
(setopt vc-msg-newbie-friendly-msg nil)
(setopt vc-msg-copy-id-to-kill-ring nil)
(setopt display-line-numbers-type nil) ; undoom
(setopt garbage-collection-messages nil)
(setopt auto-save-no-message t)
(setopt fill-column 79)

(setopt browse-url-generic-program "firefox")
(setopt browse-url-handlers
        '(
          ("github.com" . browse-url-generic)
          ("melpa.org" . browse-url-generic)
          ("fanfiction.net" . browse-url-generic)

          ;; Default
          ("." . eww-browse-url)))

;; List common values so they don't bother me.
;; I've not decided yet what's the best approach here.
(add-to-list 'safe-local-variable-values '(require-final-newline . nil))
(add-to-list 'safe-local-variable-values '(require-final-newline . t))
(add-to-list 'safe-local-variable-values '(org-drill-scope . directory))


;;; Calendar...
;; Phones track holidays too, but it's less aggravating to add many events at
;; once here.  Interestingly, Org can probably take these into the agenda, and
;; Beorg in turn can sync agenda stuff onto the actual phone calendar.  I've not
;; tried that yet.

(setopt calendar-chinese-all-holidays-flag t)
(setopt holiday-bahai-holidays nil)
(setopt holiday-hebrew-holidays nil)

;; Add some Swedish holidays
(setopt holiday-general-holidays
        '((holiday-fixed 1 1 "New Year's Day")
          (holiday-fixed 2 14 "Valentine's Day")
          (holiday-fixed 4 1 "April Fools' Day")
          (holiday-float 5 0 -1 "Mother's Day")
          (holiday-float 11 0 2 "Father's Day")
          (holiday-fixed 10 31 "Halloween")))

;; Add personal holidays
(setopt holiday-other-holidays
        '((holiday-fixed 1 25 "Joel's birthday")
          (holiday-fixed 3 8 "Clarence's birthday")
          (holiday-fixed 4 1 "Karin's birthday")
          (holiday-fixed 4 11 "Griselda's birthday")
          (holiday-fixed 6 18 "Rickard's birthday")
          ;; (holiday-fixed 7 18 "Nath's birthday") ;; when was it exactly
          (holiday-fixed 6 27 "Yang Yu Ting's birthday")
          (holiday-fixed 9 24 "Lena's birthday")
          (holiday-fixed 9 26 "Petrov Day")
          (holiday-fixed 10 27 "Arkhipov Day")
          (holiday-fixed 12 10 "Simon's birthday")))


;;; Hooks

(add-hook! (special-mode prog-mode text-mode) #'my-hippie-config)
(add-hook 'after-save-hook #'my-compile-and-drop)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'doom-load-theme-hook #'my-fix-pdf-midnight-colors)
(add-hook 'java-mode-hook (defun my-java-setup ()
                            (require 'cc-vars)
                            (setq c-basic-offset 4
                                  tab-width 4)))

;; doesn't play well with paren-mode + certain themes
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; (add-hook 'after-save-hook #'my-fix-invalid-backup-settings)
