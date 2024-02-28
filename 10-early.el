;; -*- lexical-binding: t; -*-

(add-load-path! "/home/kept/emacs/key-seqs-finder/"
                ;; "/home/kept/emacs/inline-anki/"
                "/home/kept/emacs/twee-mode/")


;;; Backups

;; Backups have saved my skin in 2015, 2016, 2018, and 2020.
;; So I should not stop using them until 2030 or so, given no more incidents.
;; And I'll probably keep them forever.  The fundamental issue with "git can
;; replace backups!" is I don't commit regularly in every project nor do I even
;; have a git project everywhere.
(setopt
 ;; Put them in the unusual path /home/backups/ to avoid cluttering rg output.
 backup-directory-alist `(("." . "/home/backups"))
 delete-old-versions t ;; nil led to Emacs looking broken for newbie-me
 vc-make-backup-files t ;; I don't commit regularly in every project
 make-backup-files t ;; undoom
 version-control t)

;; Graceful degradation
(unless (file-writable-p "/home/backups/")
  (error "Disabling backups because can't write to: /home/backups/")
  (setq backup-directory-alist nil)
  (setq make-backup-files nil))

;; Lesson learned
(add-hook 'after-save-hook #'my-fix-invalid-backup-settings)


;;; Some hooks

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'doom-load-theme-hook #'my-fix-pdf-midnight-colors)

;; Configure `hippie-expand'
(add-hook 'special-mode-hook #'my-hippie-config)
(add-hook 'prog-mode-hook #'my-hippie-config)
(add-hook 'text-mode-hook #'my-hippie-config)

;; Let the compiler tell me if an elisp file is broken, even if I don't
;; normally compile it (helpful when working on initfiles)
(add-hook 'after-save-hook #'my-compile-and-drop)

;; Prevent accidental edits (easy to miss on files like this)
(add-hook 'so-long-mode-hook #'read-only-mode)


;;; Stuff

;; Set up booleans I can use here and there throughout init.
(defvar os-debian (executable-find "apt-get"))
(defvar os-arch (or (string-search "arch" operating-system-release)
                    (string-search "arch" (shell-command-to-string "cat /etc/os-release"))))
(defvar os-gentoo (string-search "gentoo" operating-system-release))
(defvar os-guix (string-search "shepherd" (shell-command-to-string "ps -q 1 -o comm=")))

(let ((oses (list os-debian os-arch os-gentoo os-guix)))
  (when (< 1 (length (remove nil oses)))
    (warn "In my init: Two or more OS checks succeeded")))

;; TODO: Periodically re-test internet connectivity and set this, would be
;; useful for `my-stim' among other commands.
(defvar internet-connected nil)

;; TODO: how to find parent process?
;; (defvar child-emacs nil)
;; (process-attributes (emacs-pid))

;; When I'm not on a tiling WM, maximized Emacs acts as my wallpaper.
;; (add-to-list 'initial-frame-alist '(fullscreen . fullboth))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


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



;; (setq doom-theme 'doom-pine)
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-tomorrow-night)
;; (setq doom-theme 'doom-dark+)
;; (setq doom-theme 'doom-manegarm)
;; (setq doom-theme 'doom-storage-tube-amber-2)
;; (setq doom-theme 'doom-Iosvkem)
(setq doom-theme 'ef-bio)
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


;; (unless (modulep! :ui doom)
;;   ;; copy-pasted
;;   (defadvice! doom--load-theme-a (fn theme &optional no-confirm no-enable)
;;     "Disable old themes."
;;     :around #'load-theme
;;     ;; Run `load-theme' from an estranged buffer, where we can ensure that
;;     ;; buffer-local face remaps (by `mixed-pitch-mode', for instance) won't
;;     ;; interfere with recalculating faces in new themes.
;;     (with-temp-buffer
;;       (let ((last-themes (copy-sequence custom-enabled-themes)))
;;         (mapc #'disable-theme custom-enabled-themes)
;;         (funcall fn theme no-confirm no-enable))))
;;   (setq custom-safe-themes t)
;;   (use-package! doom-themes
;;     :config
;;     (load-theme 'doom-Iosvkem))
;;   ;; (custom-set-faces '(default (:family "Iosevka Nerd Font" :size 33)))
;;   )


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

