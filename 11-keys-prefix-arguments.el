;; Fix the prefix arguments -*- lexical-binding: t; -*-

;; Don't waste good keys (C-123456890) on digit-argument.  In exchange, make it
;; more convenient to access them in other ways.
;;
;; - instead of C-u, let C-=, M-=, s-= be universal argument
;; - Let C--, M--, s-- be negative argument
;; - Let - and = be neg. and univ. argument when any hydra is open
;; - Let - and = be neg. and univ. argument when any prefix argument has been called and awaiting next input
;; - Allow typing M-= M-9 M-d, much better than M-= 9 M-d

(keymap-unset global-map "C-u" t)
(keymap-unset universal-argument-map "C-u" t)

(after! hydra
  (define-key hydra-base-map (kbd "C-u") nil)
  (define-key hydra-base-map (kbd "=") #'hydra--universal-argument)
  (define-key hydra-base-map (kbd "-") #'hydra--negative-argument))

(let ((modifiers '("C-" "M-" "s-" "H-" "A-"))
      (digits (split-string "1234567890" "" t)))
  (dolist (mod modifiers)
    ;; Some modes bind e.g. M-- (org-mode with org-replace-disputed-keys t), so
    ;; override everywhere.  Actually even if we haven't discovered any
    ;; conflicts it makes sense to encode that this must work everywhere.
    ;; However we may run into a problem where it also overrides hydra-base-map...
    ;;
    ;; TODO: Don't rely on general (perhaps provide a way in
    ;; deianira-mass-remap.el, although just Emacs internals would be great).
    ;; You'll note dei--known-keymaps does NOT include hydra-base-map as it's
    ;; not a mode map.  In other words transient maps like that will work as
    ;; intended.  Elegant.  Does general override have the same elegance?
    (define-key general-override-mode-map (kbd (concat mod "=")) #'universal-argument)
    (define-key general-override-mode-map (kbd (concat mod "-")) #'negative-argument)

    (define-key global-map (kbd (concat mod "=")) #'universal-argument)
    (define-key global-map (kbd (concat mod "-")) #'negative-argument)
    (define-key universal-argument-map (kbd (concat mod "=")) #'universal-argument-more)
    ;; necessary?
    (after! hydra
      (define-key hydra-base-map (kbd (concat mod "=")) #'hydra--universal-argument)
      (define-key hydra-base-map (kbd (concat mod "-")) #'hydra--negative-argument))
    (dolist (d digits)
      (define-key global-map (kbd (concat mod d)) nil)
      (define-key universal-argument-map (kbd (concat mod d)) #'digit-argument)
      ;; REVIEW: does it mess with nonum hydras?
      (after! hydra
        (define-key hydra-base-map (kbd (concat mod d)) #'hydra--digit-argument)))))
