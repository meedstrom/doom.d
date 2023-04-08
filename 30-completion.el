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


;; Prevent all-the-icons from fattening text line heights on small terminus font

(defun my-adjust-scale-1 ()
  (text-scale-decrease 1))

(defun my-adjust-scale-2 ()
  (text-scale-set -2)
  ;; (text-scale-decrease 2)
  )

;; my first action after startup is often to reach for virtual buffers, so preload please
(recentf-mode)

(add-hook 'minibuffer-setup-hook #'my-adjust-scale-2)
(add-hook 'dired-mode-hook #'my-adjust-scale-2)


;; TODO: Avoid matching on initialisms during plaintext search (consult-line,
;; consult-grep).
;;
;; How?
;;
;; With the Doom layer, it can be done manually by prepending or appending a =
;; sign to the search term
;;
;; But it should always be the case.
;;
;; completion-category-overrides has no category for simple text search.
;;
;; vertico-multiform-mode can know it's using consult-line, and it can control
;; completion-styles or orderless-matching-styles.





;; This worked in Emacs 28.  Why 29 causes warning?

;; Partial fix: only the first component should match on initialisms.  Then I
;; only need to append a = on the first search term, and the rest will anyway
;; not be analyzed for initialisms.
(defun my-orderless-first-piece-may-be-initialism (pattern index total)
  (if (= index 0)
      (or (+vertico-orderless-dispatch pattern index total) ;; so the usual `=@! still work
          '(orderless-initialism orderless-literal orderless-regexp))
    nil))

(after! orderless
  (add-to-list 'orderless-style-dispatchers #'my-orderless-first-piece-may-be-initialism))

(setopt orderless-matching-styles '(orderless-literal
                                    orderless-regexp))





;; embark
(setopt y-or-n-p-use-read-key t)

;; (setopt consult-line-start-from-top t)


;;   (fset 'multi-occur #'consult-multi-occur)
;;   :config
;;   (setq consult-narrow-key "<")

;;   ;; Don't spin up LSP/repls when previewing virtual buffers.
;;   (setq consult-preview-raw-size 0)

;;   ;; Make narrowing help available in the minibuffer.  Do this if I turn off
;;   ;; which-key one day.
;;   ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)


;; completion-at-point, TAB -> corfu-complete
;; RET -> corfu-insert
;; M-g -> corfu-show-location
;; M-h -> corfu-show-documentation
(use-package! corfu
  :init
  (setopt corfu-auto t)
  (setopt corfu-auto-delay 0.35)
  (setopt corfu-excluded-modes '(org-mode))
  (setopt tab-always-indent 'complete) ;; or use M-TAB to complete
  :config
  (setopt completion-cycle-threshold 3)
  ;; invoke corfu for dabbrev instead of the opportunistic expand
  (keymap-set global-map "M-/" #'dabbrev-completion)
  (keymap-set global-map "C-M-/" #'dabbrev-expand)
  (global-corfu-mode)

  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell.
The idea is to avoid pressing RET twice; see README at
https://github.com/minad/corfu."
    (cond
     ((and (derived-mode-p 'eshell-mode)
           (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)
           (fboundp 'comint-send-input))
      (comint-send-input))))

  (advice-add #'corfu-insert :after #'corfu-send-shell)

  (defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico is not active."
  (unless (bound-and-true-p vertico--input)
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  )

;; NOTE: probably fixed in Emacs 29
(use-package! cape
  :after corfu
  :config
  ;; Fix eshell: https://github.com/minad/corfu/issues/61
  ;; (the pcomplete capf does not behave as a proper capf by default)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
