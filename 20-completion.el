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

;; wishlist: buffer preview
(after! helm
  (setopt helm-ff-DEL-up-one-level-maybe t)
  (when (modulep! helm)
    (define-key global-map [remap switch-to-buffer] #'helm-mini)))

(advice-remove 'embark-completing-read-prompter
               '+vertico--embark-which-key-prompt-a)

(which-key-mode 0)

(defun embark-act-with-completing-read (&optional arg)
  (interactive "P")
  (let* ((embark-prompter 'embark-completing-read-prompter)
         (act (propertize "Act" 'face 'highlight))
         (embark-indicator (lambda (_keymap targets) nil)))
    (embark-act arg)))

(after! vertico
  ;; (vertico-buffer-mode)
  ;; (keymap-set vertico-map "<tab>" #'embark-act-with-completing-read)
  )


;; my first action after startup is often to reach for virtual buffers, so preload please
(recentf-mode)

;; (add-hook 'minibuffer-setup-hook #'my-adjust-scale-2)
;; (add-hook 'dired-mode-hook #'my-adjust-scale-2)


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


;; Partial fix: only the first component should match on initialisms.  Then I
;; only need to append a = on the first search term, and the rest will anyway
;; not be analyzed for initialisms.

;; This worked in Emacs 28.  Why 29 causes warning?
(defun my-orderless-first-piece-may-be-initialism (pattern index total)
  (if (= index 0)
      (or (+vertico-orderless-dispatch pattern index total) ;; so the usual `=@! still work
          '(orderless-initialism orderless-literal orderless-regexp))
    nil))

(after! orderless
  (add-to-list 'orderless-style-dispatchers #'my-orderless-first-piece-may-be-initialism))

;; (setopt orderless-matching-styles '(orderless-literal orderless-regexp))


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

(use-package! cape
  :config
  ;; https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-super-capf
                       #'eglot-completion-at-point
                       ;; #'tempel-expand
                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf))

;; https://github.com/minad/corfu/wiki#same-key-used-for-both-the-separator-and-the-insertion
(defun my-complete-on-double-space ()
  "Makes sense only with `corfu-separator' 32 (space)."
  (interactive)
  (if current-prefix-arg
      ;;we suppose that we want leave the word like that, so do a space
      (progn
        (corfu-quit)
        (insert " "))
    (if (and (= (char-before) corfu-separator)
             (or
              ;; check if space, return or nothing after
              (not (char-after))
              (= (char-after) ?\s)
              (= (char-after) ?\n)))
        (progn
          (corfu-insert)
          (insert " "))
      (corfu-insert-separator))))

;; completion-at-point, TAB -> corfu-complete
;; RET -> corfu-insert
;; M-g -> corfu-show-location
;; M-h -> corfu-show-documentation
(use-package! corfu
  :init
  (setopt corfu-auto t)
  (setopt corfu-auto-delay 0.35)
  (setopt tab-always-indent 'complete) ;; or use M-TAB to complete
  :config
  (setopt completion-cycle-threshold 3)
  ;; invoke corfu for dabbrev instead of its own dabbrev-expand
  ;; (keymap-set [remap hippie-expand])
  ;; (global-corfu-mode)

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
      (corfu-mode)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (define-key corfu-map (kbd "SPC") #'my-complete-on-double-space)

  ;; Complete on punctuation
  ;; https://github.com/minad/corfu/wiki#tab-and-go-completion
  (dolist (c (list (cons "." ".")
                   (cons "," ",")
                   (cons ":" ":")
                   (cons ")" ")")
                   (cons "}" "}")
                   (cons "]" "]")))
    (define-key corfu-map (kbd (car c)) `(lambda ()
                                           (interactive)
                                           (corfu-insert)
                                           (insert ,(cdr c)))))

  )

;; ;; NOTE: probably fixed in Emacs 29
;; (use-package! cape
;;   :after corfu
;;   :config
;;   ;; Fix eshell: https://github.com/minad/corfu/issues/61
;;   ;; (the pcomplete capf does not behave as a proper capf by default)
;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
