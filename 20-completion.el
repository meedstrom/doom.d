;; Completion systems (helm/vertico) -*- lexical-binding: t; -*-

;; wishlist
;; - Some commands should not sort by length. e.g. M-x recentf (not that I use
;;   that one).  how to config such thing?

;; - Marginalia annotations go off-screen when one of the files have long name
;;   (especially when you use vertico-buffer-mode so you only get half screen
;;   width).  How to fix?

;; wishlist: buffer preview
(after! helm
  (setopt helm-ff-DEL-up-one-level-maybe t)
  (when (modulep! :completion helm)
    (define-key global-map [remap switch-to-buffer] #'helm-mini)))

(advice-remove 'embark-completing-read-prompter
               '+vertico--embark-which-key-prompt-a)

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


;; my first action after startup is often to reach for virtual buffers, so
;; preload please
(recentf-mode)


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
  (add-to-list 'orderless-style-dispatchers
               #'my-orderless-first-piece-may-be-initialism))

;; (setopt orderless-matching-styles '(orderless-literal orderless-regexp))

;; for embark
(setopt y-or-n-p-use-read-key t)

;; (setopt consult-line-start-from-top t)

;; "Drop obsolete =consult-multi-occur=. Alternative: Built-in =multi-occur=,
;;   =multi-occur-in-matching-buffers= or =consult-line-multi=."
;;   (fset 'multi-occur #'consult-multi-occur)


;;   :config
;;   (setq consult-narrow-key "<")

;;   ;; Don't spin up LSP/repls when previewing virtual buffers.
;;   (setq consult-preview-raw-size 0)

;;   ;; Make narrowing help available in the minibuffer.  Do this if I turn off
;;   ;; which-key one day.
;;   ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

