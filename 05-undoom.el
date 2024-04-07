;; Reverse some Doom Emacs defaults -*- lexical-binding: t; -*-

;; Yep... long slow init please!
(add-hook 'emacs-startup-hook
          (defun my-eager-startup ()
            (run-hooks 'doom-first-input-hook)
            (run-hooks 'doom-first-buffer-hook)
            (run-hooks 'doom-first-file-hook)))

;; Gimme readable backup names, because I rename files and dirs all the time.
(advice-remove #'make-backup-file-name-1 #'doom-make-hashed-backup-file-name-a)

;; I find customize a handy exploration tool, so gimme access
(put 'customize-themes 'disabled nil)
(put 'customize-group 'disabled nil)
(put 'customize-changed 'disabled nil)
(put 'customize-face 'disabled nil)
(put 'customize-variable 'disabled nil)

;; Doom puts eww-bookmarks in doomemacs/.local/cache, which I find dangerous
;; since I may unthinkingly wipe that entire folder.  Put it where I won't
;; delete it: my own .doom.d.  Do same for abbrev.  This stuff is NOT mere
;; "cache".
(setopt eww-bookmarks-directory doom-user-dir)
(setopt abbrev-file-name (expand-file-name "abbrevs" doom-user-dir))

;; I'll do M-x dlnm RET when I want it (couple of occasions per year)
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  #'display-line-numbers-mode)

(after! vertico
  (keymap-unset vertico-map "<backspace>" t))

(after! org
  ;; Having exactly two states makes for comfy toggling.
  (setopt org-todo-keywords '((sequence "TODO" "DONE"))))

;; This "doom-docs-mode" was a nice idea, but I find it mainly gets in my way.
(fset 'doom-docs-org-mode #'ignore)
(fset 'doom-docs--toggle-read-only-h #'ignore)

(after! eshell
  (setopt eshell-input-filter #'eshell-input-filter-default)
  (setopt eshell-scroll-to-bottom-on-input nil)
  ;; Give me access to emacs --help
  (fmakunbound #'eshell/emacs)
  ;; I prefer it pick a recent buffer
  (setopt +eshell-enable-new-shell-on-split nil))

(after! esh-mode
  (keymap-set eshell-mode-map "C-l" #'recenter-top-bottom))

(after! dired
  (keymap-set dired-mode-map "q" #'kill-current-buffer))

(after! ws-butler
  ;; Having nil jibes badly with `auto-save-visited-mode'. Bug report:
  ;; https://github.com/doomemacs/doomemacs/issues/7516
  (setopt ws-butler-keep-whitespace-before-point t))

(remove-hook 'dired-mode-hook #'dired-omit-mode) ;; Don't hide any files
(remove-hook 'term-mode-hook #'hide-mode-line-mode)
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; FIXME: This snippet makes the setting t before and after init but not during
;; init, which is where I want it to be t.  Guess I'll just have to make a
;; habit of launching emacs every time with "doom sync && emacs" while I'm
;; developing a package.
(setopt load-prefer-newer t) ;; don't spend another minute confused by this
(general-after-init
  (setopt load-prefer-newer t))
