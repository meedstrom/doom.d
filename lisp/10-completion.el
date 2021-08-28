(setc vertico-count 10)

;; Prevent all-the-icons from fattening text line heights on small terminus font

(defun my-adjust-scale-1 ()
  (text-scale-decrease 1))

(defun my-adjust-scale-2 ()
  (text-scale-decrease 2))

(add-hook 'minibuffer-setup-hook #'my-adjust-scale-2)
(add-hook 'dired-mode-hook #'my-adjust-scale-2)

(setc orderless-matching-styles '(orderless-literal
                                  orderless-regexp
                                  orderless-initialism))

(general-def global-map "M-g o" #'consult-outline)     ;; "M-s o" is a good alternative.
(general-def global-map "M-g l" #'consult-line)        ;; "M-s l" is a good alternative.
(general-def global-map "M-g m" #'consult-mark)        ;; I recommend to bind Consult navigation
(general-def global-map "M-g k" #'consult-global-mark) ;; commands under the "M-g" prefix.
(general-def global-map "M-g i" #'consult-imenu)
(general-def global-map "M-g e" #'consult-error)
(general-def global-map "M-s m" #'consult-multi-occur)

;; per doom: embark-act on C-;
