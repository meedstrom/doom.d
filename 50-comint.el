;; -*- lexical-binding: t; -*-

;; Limit scrollback because gcc and R can spit out enough to slow my system.
;; Good values:
;; 2^12 on Latitude E7250.
;; 2^10 on Thinkpad X200.
(setopt comint-buffer-maximum-size (^ 2 10))
(add-hook 'comint-output-filter-functions #'my-truncate-buffer-and-move-excess)

;; for ess it would be useful to autoscroll the R console when sending
;; expressions from an R file. Maybe not great while I'm inside a shell buffer
;; or other comint buffer directly
(setopt comint-scroll-to-bottom-on-input t)
