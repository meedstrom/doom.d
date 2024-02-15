;; -*- lexical-binding: t; -*-
;; Copyright (C) 2020-2024 Martin Edström
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

;; (general-unbind exwm-mode-map "C-c")
;; (general-def exwm-mode-map "M-s q" #'exwm-input-send-next-key)


;; not good
;; (setopt exwm-workspace-minibuffer-position 'bottom)
;; (add-hook 'exwm-init-hook #'exwm-workspace-attach-minibuffer)


;; (setq exwm-replace t)
(setq exwm-input-simulation-keys '(([?\s-a] . [home])
                                   ([?\s-b] . [left])
                                   ([?\s-d] . [delete])
                                   ([?\s-e] . [end])
                                   ([?\s-f] . [right])
                                   ([?\s-g] . [escape])
                                   ([?\s-k] . [S-end delete])
                                   ([?\s-m] . [return])
                                   ([?\s-n] . [down])
                                   ([?\s-p] . [up])
                                   ([?\s-s] . [C-f])
                                   ([?\s-t] . [S-right C-x left C-v])
                                   ([?\s-v] . [next])
                                   ([?\s-w] . [C-x])
                                   ([?\s-y] . [C-v])
                                   ([?\s-/] . [C-z])
                                   ([?\M-w] . [C-c])
                                   ([?\M-d] . [C-S-right delete])
                                   ([?\M-t] . [C-S-right C-x C-left C-v])
                                   ([f8]    . [menu])
                                   ([XF86Back]    . [prior])
                                   ([XF86Forward]    . [next])
                                   ))
(setq exwm-input-prefix-keys '(?\s-1 ?\s-2 ?\s-x ?\s-c menu f1 f2 f3 f5 f7 f10 f11 f12 katakana henkan
                               ;; (vconcat "<katakana>")
                               ))
(setq exwm-input-global-keys `((,(kbd "C-M-<delete>") . exwm-reset)
                               (,(kbd "M-<f4>") . kill-current-buffer) ;; y u no work?
                               ;; (,(kbd "A-<f4>") . kill-current-buffer)
                               (,(kbd "<XF86MonBrightnessDown>") . my-backlight-dec)
                               (,(kbd "<XF86MonBrightnessUp>") . my-backlight-inc)))

(after! exwm-core
  (keymap-set exwm-mode-map "M-q" (keymap-lookup exwm-mode-map "C-c"))
  (keymap-unset exwm-mode-map "C-c"))

(defun my-golden-ratio-if-exwm (&optional _arg)
  (when (eq major-mode 'exwm-mode)
    (golden-ratio)))

(add-hook 'window-buffer-change-functions #'my-golden-ratio-if-exwm)

;; (add-hook 'focus-out-hook #'my-golden-ratio-if-exwm)
(add-hook 'exwm-update-class-hook #'my-exwm-rename-buffer)
(add-hook 'exwm-update-title-hook #'my-exwm-rename-buffer)

;; https://github.com/ieure/exwm-firefox
;; Assume tabdetach extension is installed. Now you get:
;; C-c C-f  history-forward
;; C-c C-b  history-back
;; C-c C-n  split-window: open new ff window in an emacs split
;; C-c C-d  split-detach: detach current tab into an emacs split  ;; DOESNT WORK!
;; C-c C-g  merge: merge a detached tab back into its "parent" window
;; (exwm-firefox-mode)

;; (when (eq window-system 'x)
;;   (use-package! exwm
;;     :config
;;     ;; NOTE: exwm-enable just adds exwm-init on various hooks which will
;;     ;; be called later; don't count on exwm at the end of this sexp.
;;     (exwm-enable)))
