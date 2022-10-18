;; -*- lexical-binding: t; -*-

;; (general-unbind exwm-mode-map "C-c")
;; (general-def exwm-mode-map "M-s q" #'exwm-input-send-next-key)

(setq exwm-replace t)
(setq exwm-input-simulation-keys '(([?\s-a] . [home])
                                   ([?\s-b] . [left])
                                   ([?\s-d] . [delete])
                                   ([?\s-e] . [end])
                                   ([?\s-f] . [right])
                                   ([?\s-g] . [escape])
                                   ([?\s-k] . [S-end delete])
                                   ([?\s-m] . [return])
                                   ([?\s-n] . [down])
                                   ([?\s-p] . [up])
                                   ([?\s-s] . [C-f])
                                   ([?\s-v] . [next])
                                   ([?\s-w] . [C-c])
                                   ([?\s-y] . [C-v])
                                   ([?\s-/] . [C-z])
                                   ([?\M-d] . [C-S-right delete])
                                   ;; ([f8]    . [menu])
                                   ))
(setq exwm-input-prefix-keys '(?\M-1 ?\M-2 menu f1 f2 f3 f4 f5 f7 f10 f11 f12))
(setq exwm-input-global-keys `((,(kbd "C-M-<delete>") . exwm-reset)
                               (,(kbd "s-<delete>") . exwm-reset)
                               (,(kbd "M-<f4>") . kill-current-buffer)
                               (,(kbd "<XF86MonBrightnessDown>") . my-backlight-dec)
                               (,(kbd "<XF86MonBrightnessUp>") . my-backlight-inc)))

(defun my-golden-ratio-if-exwm (&optional _arg)
  (when (eq major-mode 'exwm-mode)
    (golden-ratio)))

;; FIXME: does not work beyond the first time the program appears
(add-hook 'exwm-manage-finish-hook #'my-golden-ratio-if-exwm)
(add-hook 'window-selection-change-functions #'my-golden-ratio-if-exwm)
(add-hook 'window-buffer-change-functions #'my-golden-ratio-if-exwm)

;; (add-hook 'focus-out-hook #'my-golden-ratio-if-exwm)
(add-hook 'exwm-update-class-hook #'my-exwm-rename-buffer)
(add-hook 'exwm-update-title-hook #'my-exwm-rename-buffer)

;; https://github.com/ieure/exwm-firefox
;; Assume tabdetach extension is installed. Now you get:
;; C-c C-f  history-forward
;; C-c C-b  history-back
;; C-c C-n  split-window: open new ff window in an emacs split
;; C-c C-d  split-detach: detach current tab into an emacs split  ;; DOESNT WORK!
;; C-c C-g  merge: merge a detached tab back into its "parent" window
;; (exwm-firefox-mode)

(use-package! exwm
  :config
  ;; NOTE: exwm-enable just adds exwm-init on various hooks, so the actual exwm-init
  ;; doesn't happen at this stage of the emacs startup.
  (exwm-enable))

;; not good
;; (setopt exwm-workspace-minibuffer-position 'bottom)
;; (add-hook 'exwm-init-hook #'exwm-workspace-attach-minibuffer)
