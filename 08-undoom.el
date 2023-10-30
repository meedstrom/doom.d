;; -*- lexical-binding: t; -*-
;; In this file are some reversions of Doom defaults.


;; I want readable backup names since I rename files all the time.
(advice-remove #'make-backup-file-name-1 #'doom-make-hashed-backup-file-name-a)

;; I find customize a handy exploration tool
(put 'customize-themes 'disabled nil)
(put 'customize-group 'disabled nil)
(put 'customize-changed 'disabled nil)
(put 'customize-face 'disabled nil)
(put 'customize-variable 'disabled nil)

;; I'll do M-x dlnm RET when I want it (a couple of occasions per year)
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  #'display-line-numbers-mode)

(after! vertico
  (keymap-unset vertico-map "<backspace>" t))

(after! org
  ;; Having exactly two states makes for comfy toggling.
  (setopt org-todo-keywords '((sequence "TODO" "DONE"))))

;; IDK why, but I find this doom-docs-mode just gets in my way.  Nice idea tho.
(fset 'doom-docs-org-mode #'ignore)
(fset 'doom-docs--toggle-read-only-h #'ignore)

(after! eshell
  (setopt eshell-input-filter #'eshell-input-filter-default)
  (setopt eshell-scroll-to-bottom-on-input nil)
  (setopt eshell-scroll-to-bottom-on-output nil)
  ;; Give me access to emacs --help
  (fmakunbound #'eshell/emacs)
  ;; I prefer it pick a recent buffer
  (setopt +eshell-enable-new-shell-on-split nil))

(after! esh-mode
  (keymap-set eshell-mode-map "C-l" #'recenter-top-bottom))

(after! ws-butler
  ;; Bug IMO.  Having it nil jibes badly with auto-save-visited-mode.
  ;; https://github.com/doomemacs/doomemacs/issues/7516
  (setopt ws-butler-keep-whitespace-before-point t))

(remove-hook 'dired-mode-hook #'dired-omit-mode)
