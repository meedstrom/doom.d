;; -*- lexical-binding: t; -*-

(add-load-path! "/home/kept/emacs/key-seqs-finder/"
                "/home/kept/emacs/twee-mode/")

;; Backups have saved my skin in 2015, 2016, 2018, and 2020.
;; So I should not stop using them until 2030 or so, given no more incidents.
;; I put them in the unusual path /home/backups/ to prevent cluttered rg output.
(setq
 backup-directory-alist `(("." . "/home/backups"))
 delete-old-versions t ;; nil led to Emacs appearing broken for newbie-me
 vc-make-backup-files t ;; I don't commit regularly in every project
 make-backup-files t ;; WHY did Doom disable it
 version-control t)

;; undoom; I want readable backup names since I rename files and directories all
;; the time.
(advice-remove #'make-backup-file-name-1 #'doom-make-hashed-backup-file-name-a)

;; check
(unless (file-writable-p "/home/backups/")
  (error "Disabling backups because can't write to: /home/backups/")
  (setq backup-directory-alist nil)
  (setq make-backup-files nil))

;; Doom puts eww-bookmarks in doomemacs/.local/cache, which I find dangerous
;; since I may unthinkingly wipe it.  Put it where I won't delete it.
(setopt eww-bookmarks-directory doom-user-dir)

(setopt abbrev-file-name (expand-file-name "abbrevs" doom-user-dir))


;;; Font and theme

;; The default is Symbola, but I can't find it on pacman repos.
;; Note that this is ignored when the module :ui unicode is enabled.
(setq doom-unicode-font (font-spec :family "DejaVu Sans Mono"))

;; (setq doom-font (font-spec :family "Cozette" :size 10))
;; (setq doom-font (font-spec :family "Tamzen" :size 10))
(setq doom-font (font-spec :family "Terminus" :size 15))
;; (setq doom-font (font-spec :family "Dina" :size 20))
;; (setq doom-font (font-spec :family "Modd" :size 20))
;; (setq doom-font (font-spec :family "orp" :size 20))
;; (setq doom-font (font-spec :family "peep" :size 20))
;; (setq doom-font (font-spec :family "ProFont" :size 20))
;; (setq doom-font (font-spec :family "monospace" :size 30))
;; (setq doom-font (font-spec :family "Hasklig" :size 14))
;; (setq doom-font (font-spec :family "Iosevka" :size 14))

;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-tomorrow-night)
(setq doom-theme 'doom-dark+)
;; (setq doom-theme 'doom-manegarm)
;; (setq doom-theme 'doom-storage-tube-amber-2)
;; (setq doom-theme 'doom-Iosvkem)
;; (setq doom-theme 'doom-zenburn)
;; (setq doom-theme 'doom-outrun-electric)
;; (setq doom-theme 'doom-badger)
;; (setq doom-theme 'doom-rouge)


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

;; For Magit, also for email when I get around to that.
(setq user-full-name "Martin Edström")
(setq user-mail-address "meedstrom91@gmail.com")

;; Set up booleans I can use here and there throughout init.
(defvar debian (executable-find "apt-get"))
(defvar arch (or (string-search "arch" operating-system-release)
                 (string-search "arch" (shell-command-to-string "cat /etc/os-release"))))
(defvar gentoo (string-search "gentoo" operating-system-release))
(defvar guix (string-search "shepherd" (shell-command-to-string "ps -q 1 -o comm=")))

(let* ((oses (list debian arch gentoo guix)))
  (unless (>= 1 (- (length oses) (-count #'null oses)))
    (warn "My init: Two or more OS checks succeeded")))

;; TODO: periodically re-test internet connectivity and set this, would be
;; useful for `my-stim'.
(defvar internet nil)

;; TODO: how to find out parent process
(defvar child-emacs nil)
  ;; (process-attributes (emacs-pid))

;; In case I'm not on a tiling WM.
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))

;; Blacken background, to make transparent windows more back-transparent
;; (set-face-background 'default "#000000")
(after! solaire-mode
  ;; (set-face-background 'solaire-default-face "#000700")
  ;; (set-face-foreground 'solaire-default-face "pale green")
  )
(my-fix-pdf-midnight-colors)

;; undoom; I find customize a handy exploration tool
(put 'customize-themes 'disabled nil)
(put 'customize-group 'disabled nil)
(put 'customize-changed 'disabled nil)
(put 'customize-face 'disabled nil)
(put 'customize-variable 'disabled nil)

(after! recentf
  (setopt recentf-max-saved-items 1200))

;; FIXME: doom doesn't respect this
(setopt load-prefer-newer t) ;; don't spend another minute confused by this

(setopt auth-sources '("~/.authinfo")) ;; https://magit.vc/manual/ghub/Storing-a-Token.html
(setopt shr-max-image-proportion 0.5)
(setopt mouse-yank-at-point t)
(setopt save-interprogram-paste-before-kill t)
(setopt select-enable-primary t)
(setopt custom-safe-themes t)
(setopt suggest-key-bindings nil)
(setopt kill-read-only-ok t)
(setopt kill-ring-max 600)
;; (setopt byte-compile-warnings '(not free-vars))
(setopt view-read-only t)
(setopt indent-tabs-mode nil)
(setopt vc-msg-newbie-friendly-msg nil)
(setopt vc-msg-copy-id-to-kill-ring nil)
(setopt display-line-numbers-type nil) ; undoom
(setopt garbage-collection-messages nil)
(setopt auto-save-no-message t)
;; (setopt shift-select-mode nil)

(setopt browse-url-generic-program "firefox")
(setopt browse-url-handlers
        '(
          ;; ("github.com" . browse-url-generic)
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
;; Modern phones do calendar, but it's less aggravating to add and remove many
;; events at once here (to my knowledge, neither iPhone nor Android expose
;; anything remotely as convenient as xfconf or even a yaml/toml file).  Emacs
;; provides a neat solution: since Org takes these into the agenda, and the app
;; Beorg can sync all agenda stuff onto the actual phone calendar, adding it
;; here adds it there.  Magic.

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


;;; Some hooks

(add-hook! (special-mode prog-mode text-mode) #'my-hippie-config)
(add-hook 'after-save-hook #'my-compile-and-drop)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'doom-load-theme-hook #'my-fix-pdf-midnight-colors)
(add-hook 'java-mode-hook (defun my-java-setup ()
                            (require 'cc-vars)
                            (setq c-basic-offset 4
                                  tab-width 4)))

;; Hl-line-mode doesn't play along well when paren-mode is combined with certain
;; themes (maybe fixed when prism-mode is used in place of paren-mode).  Also
;; interferes with the trick of calling M-x describe-face to discover which face
;; is under point.  But this is more appropriately fixed with a command
;; "describe-face-at-point" or maybe a vertico/consult hack. Later...
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; undoom
(remove-hook 'term-mode-hook #'hide-mode-line-mode)

;; (add-hook 'after-save-hook #'my-fix-invalid-backup-settings)
