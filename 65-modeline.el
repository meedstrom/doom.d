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


(use-package! awesome-tray
  :config
  ;; (setq awesome-tray-date-format "%H:%M")

  ;; wishlist: defun to check if message plus tray would exceed frame width, and
  ;; hide tray while that message is shown
  (let ((widest-tray (+ awesome-tray-file-name-max-length 11)))
    (when (> (+ (length (current-message)) widest-tray) (frame-width))
      (awesome-tray-disable)))

  (setq awesome-tray-active-modules '("location" "belong" "file-path" "battery"))
  (setq awesome-tray-refresh-idle-delay nil)
  (setq awesome-tray-update-interval nil)
  ;; Patch to always show the percentage, even if plugged in.  No decimal.
  (el-patch-defun awesome-tray-module-battery-info ()
    (let ((current-seconds (awesome-tray-current-seconds)))
      (if (> (- current-seconds awesome-tray-battery-status-last-time) awesome-tray-battery-update-duration)
          (let* ((battery-info (funcall battery-status-function))
                 (battery-type (battery-format "%L" battery-info))
                 battery-status)
            (setq awesome-tray-battery-status-last-time current-seconds)
            (cond ((member battery-type '("on-line" "AC"))
                   (setq battery-type "AC")
                   (setq battery-status
                         (replace-regexp-in-string
                          "\\.[[:digit:]]*" ""
                          (battery-format "%3p%%" battery-info))))
                  ((member battery-type '("off-line" "BAT" "Battery"))
                   (setq battery-type "")
                   (setq battery-status
                         (replace-regexp-in-string
                          "\\.[[:digit:]]*" ""
                          (battery-format "%3p%% %t" battery-info)))))
            ;; Update battery cache.
            (setq awesome-tray-battery-status-cache (concat battery-type battery-status)))
        awesome-tray-battery-status-cache)))
  (add-hook 'emacs-startup-hook #'awesome-tray-mode))


(setopt mode-line-percent-position nil)
(setopt mode-line-modes nil)

;; (setq-default mode-line-format nil)
;; (global-hide-mode-line-mode)

(use-package! mini-modeline :disabled
              :hook  (after-init . mini-modeline-mode))

;; (use-package! feebleline :config (feebleline-mode))

;; (use-package! smarttabs
;;               :config
;;               (smart-tabs-add-language-support ess-r ess-r-mode-hook
;;                 ())
;;               (smart-tabs-insinuate 'ess))


;; (setopt mini-modeline-display-gui-line nil)
;; (setopt mini-modeline-face-attr '(:background "#001100" :foreground "pale green"))
;; (after! mini-modeline
;;   (set-face-background 'mini-modeline-mode-line "#001100")
;;   (column-number-mode)
;;   (require 'dash)
;;   (setopt mini-modeline-r-format
;;           (-difference mini-modeline-r-format
;;                        '((:eval (string-trim (format-mode-line mode-line-modes)))
;;                          mode-line-mule-info))))

;; (after! feebleline
;;   (defface feebleline-norm-face '((t :foreground "pale green"))
;;     nil :group 'feebleline)
;;   (set-face-attribute 'feebleline-dir-face nil :foreground "forest green")
;;   (setopt window-divider-default-bottom-width 2)
;;   (setopt feebleline-msg-functions
;;           '((feebleline-line-number :face feebleline-norm-face :post "" :fmt "%5s")
;;             (feebleline-column-number :face feebleline-norm-face :pre ":" :fmt "%-2s")
;;             (feebleline-file-directory :face feebleline-dir-face :post "")
;;             (feebleline-file-or-buffer-name :face feebleline-norm-face :post "")
;;             (feebleline-file-modified-star :face font-lock-warning-face :post "")
;;             (feebleline-git-branch :face feebleline-git-face :pre " - ")
;;             ;; ((lambda () battery-mode-line-string) :face feebleline-dir-face)
;;             ((lambda () org-mode-line-string) :face feebleline-norm-face)
;;             ((lambda () org-pomodoro-mode-line) :face font-lock-warning-face)
;;             )))

(after! doom-modeline
  (setopt doom-modeline-buffer-encoding nil)
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
  (size-indication-mode 0))
