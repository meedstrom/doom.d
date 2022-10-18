;; -*- lexical-binding: t; -*-

;; Reuse current window on M-x shell, like with eshell.
(add-to-list 'display-buffer-alist
             (cons "\\*shell\\*" display-buffer--same-window-action))

;; Limit scrollback because gcc and R can spit out enough to slow my system.
(setopt comint-buffer-maximum-size (^ 2 12))
(add-hook 'comint-output-filter-functions #'my-truncate-buffer-and-move-excess)

;; This shouldn't affect eshell, but does...  Maybe it's been fixed since.
;; (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)
