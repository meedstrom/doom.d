;; -*- lexical-binding: t; -*-

;; Don't spin up LSP/repls when previewing virtual buffers.
;; (setq consult-preview-raw-size 0)


(after! ivy
  (setc ivy-re-builders-alist '((t . ivy--regex-plus))) ; undoom
  (setc ivy-use-virtual-buffers t)
  (setc counsel-find-file-at-point t)
  (setc counsel-switch-buffer-preview-virtual-buffers nil)) ; L A G

;; i go for virtual buffers right away on startup
;; FIXME still doesn't work
(require 'recentf)


;; (use-package! embark
;;   :defer
;;   :init
;;   ;; (setc embark-prompter #'embark-completing-read-prompter)
;;   )

;; (use-package! embark-consult
;;   :after '(embark consult))

;; (use-package! ctrlf :disabled
;;   :defer
;;   :init
;;   (ctrlf-mode) ;; Has smart autoloads
;;   :config
;;   (add-hook 'Info-mode-hook (defun my-turn-off-ctrlf () (ctrlf-local-mode 0))))

;; (use-package! orderless
;;   :custom (completion-styles '(orderless))
;;   :config
;;   (setc orderless-matching-styles '(orderless-literal
;;                                     orderless-regexp
;;                                     orderless-initialism))
;;   ;; Optional performance optimization
;;   ;; by highlighting only the visible candidates.
;;   (setq orderless-skip-highlighting (lambda () selectrum-is-active))
;;   (setq selectrum-highlight-candidates-function #'orderless-highlight-matches))


;; (use-package selectrum
;;   :defer
;;   :init
;;   (selectrum-mode) ;; Has smart autoloads
;;   :config
;;   (require 'consult-selectrum)

;;   ;; (selectrum-prescient-mode)  ;; performance hit
;;   ;; (prescient-persist-mode)
;;   (marginalia-mode)
;;   ;; (selectrum-helm-mode)
;;   ;; (mini-frame-mode)
;;   ;; (global-set-key (kbd "<f5> r") #'selectrum-repeat)

;;   (define-key minibuffer-local-map (kbd "C-l") #'embark-act)
;;   (define-key selectrum-minibuffer-map (kbd "<next>") #'scroll-up-command)
;;   (define-key selectrum-minibuffer-map (kbd "<prior>") #'scroll-down-command)

;;   (setc file-name-shadow-properties '(invisible t))
;;   (file-name-shadow-mode)
;;   (set-face-background 'highlight "dark green")

;;   (autoload 'ffap-guesser "ffap")
;;   (setq minibuffer-default-add-function
;;         (defun minibuffer-default-add-function+ ()
;;           (with-selected-window (minibuffer-selected-window)
;;             (delete-dups
;;              (delq nil
;;                    (list (thing-at-point 'symbol)
;;                          (thing-at-point 'list)
;;                          (ffap-guesser)
;;                          (thing-at-point-url-at-point)))))))

;;   (autoload 'ffap-file-at-point "ffap")
;;   (add-hook 'completion-at-point-functions
;;             (defun complete-path-at-point+ ()
;;               (let ((fn (ffap-file-at-point))
;;                     (fap (thing-at-point 'filename)))
;;                 (when (and (or fn
;;                                (equal "/" fap))
;;                            (save-excursion
;;                              (search-backward fap (line-beginning-position) t)))
;;                   (list (match-beginning 0)
;;                         (match-end 0)
;;                         #'completion-file-name-table)))) 'append))

;; (use-package consult
;;   :bind (("C-x M-:" . consult-complex-command)
;;          ("C-c h" . consult-history)
;;          ("C-c m" . consult-mode-command)
;;          ([remap switch-to-buffer] . consult-buffer)
;;          ("C-x 4 b" . consult-buffer-other-window)
;;          ("C-x 5 b" . consult-buffer-other-frame)
;;          ("C-x r x" . consult-register)
;;          ("C-x r b" . consult-bookmark)
;;          ("M-g g" . consult-goto-line)
;;          ("M-g M-g" . consult-goto-line)
;;          ("M-g o" . consult-outline)     ;; "M-s o" is a good alternative.
;;          ("M-g l" . consult-line)        ;; "M-s l" is a good alternative.
;;          ("M-g m" . consult-mark)        ;; I recommend to bind Consult navigation
;;          ("M-g k" . consult-global-mark) ;; commands under the "M-g" prefix.
;;          ("M-g i" . consult-imenu)
;;          ("M-g e" . consult-error)
;;          ("M-s m" . consult-multi-occur)
;;          ("M-y" . consult-yank-pop)
;;          ("<help> a" . consult-apropos))
;;   :init
;;   (fset 'multi-occur #'consult-multi-occur)
;;   :config
;;   (setq consult-narrow-key "<")

;;   ;; Don't spin up LSP/repls when previewing virtual buffers.
;;   (setq consult-preview-raw-size 0)

;;   ;; Make narrowing help available in the minibuffer.  Do this if I turn off
;;   ;; which-key one day.
;;   ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

;;   ;; Optional configure a view library to be used by `consult-buffer'.
;;   ;; The view library must provide two functions, one to open the view by name,
;;   ;; and one function which must return a list of views as strings.
;;   ;; Example: https://github.com/minad/bookmark-view/
;;   ;; (setq consult-view-open-function #'bookmark-jump
;;   ;;       consult-view-list-function #'bookmark-view-names)

;;   ;; (consult-preview-mode)
;;   )

;; (after! mini-frame
;;   ;; for GNOME Shell users
;;   (setc x-gtk-resize-child-frames 'resize-mode))
