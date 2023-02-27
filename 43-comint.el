;; -*- lexical-binding: t; -*-

;; Emacs 28 fixed this
;; ;; Reuse current window on M-x shell, like with eshell.
;; (add-to-list 'display-buffer-alist
;;              (cons "\\*shell\\*" display-buffer--same-window-action))

;; Limit scrollback because gcc and R can spit out enough to slow my system.
;; Good values:
;; 2^12 on Latitude E7250.
;; 2^10 on Thinkpad X200.
(setopt comint-buffer-maximum-size (^ 2 10))
(add-hook 'comint-output-filter-functions #'my-truncate-buffer-and-move-excess)

;; This shouldn't affect eshell, but does...  Maybe it's been fixed since.
;; (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)
