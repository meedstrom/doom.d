;; -*- lexical-binding: t; -*-

;; Tell pop-to-buffer to reuse current window, so M-x shell does too.
(add-to-list 'display-buffer-alist
             (cons "\\*shell\\*" display-buffer--same-window-action))

;; Limit scrollback, because gcc and R can spit out enough to slow my system.
(setq comint-buffer-maximum-size (^ 2 12))
(add-hook 'comint-output-filter-functions #'my-truncate-buffer-and-move-excess)

;; (make-comint "lel" (start-process "fake" (current-buffer) "cat"))

