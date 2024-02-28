;; Reverse some Doom Emacs defaults -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Martin Edström
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



;; Yep... slower init please!
(add-hook 'emacs-startup-hook
          (defun my-eager-startup ()
            (run-hooks 'doom-first-input-hook)
            (run-hooks 'doom-first-buffer-hook)
            (run-hooks 'doom-first-file-hook)))

;; I want readable backup names because I rename files and dirs all the time.
(advice-remove #'make-backup-file-name-1 #'doom-make-hashed-backup-file-name-a)

;; I find customize a handy tool for exploring and experimenting
(put 'customize-themes 'disabled nil)
(put 'customize-group 'disabled nil)
(put 'customize-changed 'disabled nil)
(put 'customize-face 'disabled nil)
(put 'customize-variable 'disabled nil)

;; Doom puts eww-bookmarks in doomemacs/.local/cache, which I find dangerous
;; since I may unthinkingly wipe that entire folder.  Put it where I won't
;; delete it: my own .doom.d.  Do same for abbrev.
;; Bookmarks are NOT simply cache.
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

;; org-crypt really slows saving on large Org buffers
;; NOTE: Doesn't work, so just disabled org-crypt in packages.el.
;; (add-hook 'org-mode-hook
;;           (defun my-remove-crypt-hook ()
;;             (remove-hook 'before-save-hook 'org-encrypt-entries))
;;           98)

;; I find this doom-docs-mode mainly gets in my way.  Nice idea tho.
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
  ;; Having nil jibes badly with auto-save-visited-mode.
  ;; Bug report: https://github.com/doomemacs/doomemacs/issues/7516
  (setopt ws-butler-keep-whitespace-before-point t))

;; Don't hide dotfiles or any file
(remove-hook 'dired-mode-hook #'dired-omit-mode)
