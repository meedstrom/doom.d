;; -*- lexical-binding: t; -*-
;; Copyright (C) 2020-2023 Martin Edström
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(add-load-path! "/home/kept/emacs/key-seqs-finder/"
                ;; "/home/kept/emacs/inline-anki/"
                "/home/kept/emacs/twee-mode/")

;; Backups have saved my skin in 2015, 2016, 2018, and 2020.
;; So I should not stop using them until 2030 or so, given no more incidents.
;; I put them in the unusual path /home/backups/ to prevent cluttered rg output.
(setopt
 backup-directory-alist `(("." . "/home/backups"))
 delete-old-versions t ;; nil led to Emacs appearing broken for newbie-me
 vc-make-backup-files t ;; I don't commit regularly in every project
 make-backup-files t ;; Why did Doom disable it
 version-control t)

;; undoom; I want readable backup names since I rename files and directories all
;; the time.
(advice-remove #'make-backup-file-name-1 #'doom-make-hashed-backup-file-name-a)

;; Have grace
(unless (file-writable-p "/home/backups/")
  (error "Disabling backups because can't write to: /home/backups/")
  (setq backup-directory-alist nil)
  (setq make-backup-files nil))

;; Learned a lesson
(add-hook 'after-save-hook #'my-fix-invalid-backup-settings)

;; Doom puts eww-bookmarks in doomemacs/.local/cache, which I find dangerous
;; since I may unthinkingly wipe it.  Put it where I won't delete it.
(setopt eww-bookmarks-directory doom-user-dir)

(setopt abbrev-file-name (expand-file-name "abbrevs" doom-user-dir))


;;; Font and theme

;; (setq doom-font (font-spec :family "Cozette" :size 10))
;; (setq doom-font (font-spec :family "Tamzen" :size 10))
;; (setq doom-font (font-spec :family "Terminus" :size 15))
;; (setq doom-font (font-spec :family "Dina" :size 20))
;; (setq doom-font (font-spec :family "Modd" :size 20))
;; (setq doom-font (font-spec :family "orp" :size 20))
;; (setq doom-font (font-spec :family "peep" :size 20))
;; (setq doom-font (font-spec :family "ProFont" :size 20))
;; (setq doom-font (font-spec :family "monospace" :size 30))
;; (setq doom-font (font-spec :family "Hasklig" :size 14))
;; (setq doom-font (font-spec :family "Iosevka" :size 14))

;; For my Surface Pro screen (2736x1824).  Here are the respective fonts'
;; maximum size that still let you split the screen into two 80-column panes.
;; ASSUMING NO DPI SCALING.
;;
;; FONTS THAT SUPPORT LIGATURES https://wiki.archlinux.org/title/Font#Monospaced
;; (setq doom-font (font-spec :family "Iosevka Nerd Font" :size 32))
;; (setq doom-font (font-spec :family "Hurmit Nerd Font" :size 26))
;; (setq doom-font (font-spec :family "Hasklug Nerd Font" :size 27))
;; (setq doom-font (font-spec :family "JetBrains Mono Nerd Font" :size 27))
;; (setq doom-font (font-spec :family "Lilex Nerd Font" :size 27))
;; (setq doom-font (font-spec :family "CaskaydiaCove Nerd Font" :size 28))
;; (setq doom-font (font-spec :family "FiraCode Nerd Font" :size 26))
;; OTHER FONTS
;; (setq doom-font (font-spec :family "DejaVu SansM Nerd Font" :size 27))
;; (setq doom-font (font-spec :family "Noto Sans Mono" :size 27))

;; (setq doom-font (font-spec :family "Iosevka Nerd Font" :size 15))
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 33))

;; The default is Symbola, but I can't find it on pacman repos.
;; (setq doom-unicode-font (font-spec :family "DejaVu Sans Mono"))
;; (setq doom-unicode-font (font-spec :family "Noto Sans Mono"))
(setq doom-unicode-font doom-font) ;; if it has good unicode coverage alrdy

;; -----------------------------------------------------------------------------


;; (setq doom-theme 'doom-pine)
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-tomorrow-night)
;; (setq doom-theme 'doom-dark+)
;; (setq doom-theme 'doom-manegarm)
;; (setq doom-theme 'doom-storage-tube-amber-2)
(setq doom-theme 'doom-Iosvkem)
;; (setq doom-theme 'doom-zenburn)
;; (setq doom-theme 'doom-outrun-electric)
;; (setq doom-theme 'doom-badger)
;; (setq doom-theme 'doom-rouge)
;; (setq doom-theme 'doom-dracula)

;; Some quick rules of thumb to select a theme
;; - if you have a high-DPI monitor, no need to be allergic to italics
;;   - but I prefer italics on comments ONLY
;;     - counterintuitively, if I see a theme with italic comments, I should
;;       avoid it bc it's likely this theme designer also saw fit to italicize
;;       other things. So in the end, move along---nothing to see here.
;; - with prism-mode, none of the faces should be grey !!!
;; - with prism-mode, none of the faces should be much darker than the rest

;; Good WITH prism-desaturations
;; (load-theme 'monokai-pro)
;; (load-theme 'doom-storage-tube)

;; Good WITHOUT prism-desaturations
;; (load-theme 'doom-Iosvkem)
;; (load-theme 'doom-solarized-dark-high-contrast)
;; (load-theme 'doom-rouge)

;; (setq doom-theme 'modus-vivendi)
;; (general-after-init
;;   ;; Recommended for a modus theme
;;   (prism-set-colors
;;     :desaturations (cl-loop for i from 0 below 16 collect (* i 2.5))
;;     :lightens (cl-loop for i from 0 below 16 collect (* i 2.5))
;;     :colors (modus-themes-with-colors
;;               (list fg-main
;;                     magenta
;;                     cyan-cooler
;;                     magenta-cooler
;;                     blue
;;                     magenta-warmer
;;                     cyan-warmer
;;                     red-cooler
;;                     green
;;                     fg-main
;;                     cyan
;;                     yellow
;;                     blue-warmer
;;                     red-warmer
;;                     green-cooler
;;                     yellow-faint))))


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
  (setopt recentf-max-saved-items nil))

;; FIXME: doom doesn't respect this during init
(setopt load-prefer-newer t) ;; don't spend another minute confused by this
(general-after-init
  (setopt load-prefer-newer t))

(setopt auth-sources '("~/.authinfo")) ;; https://magit.vc/manual/ghub/Storing-a-Token.html
(setopt shr-max-image-proportion 0.5)
(setopt mouse-yank-at-point t)
(setopt save-interprogram-paste-before-kill t)
(setopt select-enable-primary t)
(setopt custom-safe-themes t)
;; (setopt suggest-key-bindings nil)
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
(setopt shift-select-mode nil)

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


;;; Calendar...
;; Modern phones do calendar, but it's less aggravating to add and remove many
;; events at once here (if only iOS or Android exposed a yaml/toml file for
;; system settings, that'd be another story).  Emacs provides a neat solution:
;; since org-agenda integrates holiday.el info, and the app Beorg can sync all
;; agenda stuff into the actual iOS calendar, adding it here adds it there.
;; Magic.

(setopt holiday-bahai-holidays nil)
(setopt holiday-hebrew-holidays nil)
(setopt holiday-islamic-holidays nil)
(setopt holiday-oriental-holidays nil)
(setopt calendar-view-holidays-initially-flag t)

;; Swedish holidays
(setopt holiday-general-holidays
        '((holiday-fixed 1 1 "New Year's Day")
          (holiday-fixed 2 14 "Valentine's Day")
          (holiday-fixed 3 8 "International Women's Day")
          (holiday-easter-etc) ;; surprisingly hard to calculate
          (holiday-fixed 4 1 "April Fools' Day")
          (holiday-fixed 4 30 "Walpurgis Night")
          (holiday-float 5 0 -1 "Mother's Day")
          (holiday-fixed 6 24 "Midsummer")
          (holiday-fixed 10 31 "Halloween")
          (holiday-float 11 0 2 "Father's Day")
          (holiday-fixed 12 13 "Lucia")
          (holiday-fixed 12 24 "Christmas Eve")
          (holiday-fixed 12 31 "New Year's Eve")))

;; Personal holidays
(setopt holiday-other-holidays
        ;; Birthdays
        '((holiday-fixed 1 25 "Joel's birthday")
          (holiday-fixed 2 25 "Ann-Julie's birthday")
          (holiday-fixed 3 8 "Clarence's birthday")
          (holiday-fixed 4 1 "Karin's birthday")
          (holiday-fixed 4 11 "Griselda's birthday")
          (holiday-fixed 4 11 "Lena Duske's birthday")
          (holiday-fixed 6 18 "Rickard's birthday")
          (holiday-fixed 6 27 "Yang Yu Ting's birthday")
          (holiday-fixed 7 5 "Nath's birthday")
          (holiday-fixed 9 13 "Tuyana's birthday")
          (holiday-fixed 9 24 "Lena A's birthday")
          (holiday-fixed 11 6 "Lore's birthday")
          (holiday-fixed 12 10 "Simon's birthday")

          ;; Other things
          (holiday-fixed 7 1 "Ignaz Semmelweis Day")
          (holiday-fixed 9 26 "Petrov Day")
          (holiday-fixed 10 27 "Arkhipov Day")))

;; This shouldn't be necessary but somehow `calendar-holidays' is being pre-set
;; in my emacs (and it has no :set-after as of emacs 30)
(setopt calendar-holidays (append holiday-general-holidays
                                  holiday-other-holidays
                                  ))

;; add these birthdays
;; - Cristina
;; - Jesus
;; - Laia
;; - Marc-Antoine
;; - Mirela
;; - Sammer
;; - Seda
;; - Tim
;; - Thor & Emil


;;; Some hooks

(add-hook 'special-mode-hook #'my-hippie-config)
(add-hook 'prog-mode-hook #'my-hippie-config)
(add-hook 'text-mode-hook #'my-hippie-config)
(add-hook 'after-save-hook #'my-compile-and-drop)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'doom-load-theme-hook #'my-fix-pdf-midnight-colors)
(add-hook 'java-mode-hook (defun my-java-setup ()
                            (require 'cc-vars)
                            (setq c-basic-offset 4
                                  tab-width 4)))

;; ;; FIXME
;; ;; Try to fix the interference from hl-line-mode that deprives us of the trick
;; ;; of calling M-x describe-face to discover which face is under point.
;; (defun my-turn-off-hl-line-mode (&rest args)
;;   (hl-line-mode 0))
;; (defun my-turn-on-hl-line-mode (&rest args)
;;   (hl-line-mode 1))
;; (advice-add 'describe-face :before #'my-turn-off-hl-line-mode)
;; (advice-add 'describe-face :after #'my-turn-on-hl-line-mode)
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; undoom
(remove-hook 'term-mode-hook #'hide-mode-line-mode)

;; (remove-hook 'doom-first-input-hook #'which-key-mode)



;; Prevent accidental edits
;; NOTE: Emacs 29 no longer recommends `so-long-mode' (new system is automatic,
;;       see `long-line-optimizations-p').  Still doesn't hurt to keep this.
(add-hook 'so-long-mode-hook #'read-only-mode)
