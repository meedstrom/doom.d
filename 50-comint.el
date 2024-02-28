;; -*- lexical-binding: t; -*-

;; Limit scrollback because gcc and R can spit out enough to slow my system.
;; Good values:
;; 2^12 on Latitude E7250.
;; 2^10 on Thinkpad X200.
(setopt comint-buffer-maximum-size (^ 2 10))
(add-hook 'comint-output-filter-functions #'my-truncate-buffer-and-move-excess)

;; This shouldn't affect eshell, but does...  Maybe it's been fixed since.
;; (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)
