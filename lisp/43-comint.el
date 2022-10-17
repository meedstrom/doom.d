;; -*- lexical-binding: t; -*-

;; Tell pop-to-buffer to reuse current window on M-x shell.
(add-to-list 'display-buffer-alist
             (cons "\\*shell\\*" display-buffer--same-window-action))

;; Limit scrollback because gcc and R can spit out enough to slow my system.
(setc comint-buffer-maximum-size (^ 2 12))
(add-hook 'comint-output-filter-functions #'my-truncate-buffer-and-move-excess)
