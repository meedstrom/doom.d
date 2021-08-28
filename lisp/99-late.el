;; -*- lexical-binding: t; -*-
;; REVIEW: Maybe just wrap all this in a function for after-init-hook?

;; (after! secretary
;;   (general-after-init
;;     (secretary-present-diary)))

(require 'l)
;; (auto-save-visited-mode) ;; blanks echo area after 5 secs!
(add-hook 'doom-switch-buffer-hook (l'save-some-buffers t))
(add-hook 'doom-switch-window-hook (l'save-some-buffers t))

(use-package! gif-screencast
  :commands (gif-screencast-start-or-stop))

;; NOTE: don't bother to use keyfreq-excluded-commands, all commands still
;; gonna run the keyfreq-pre-command-hook.
(use-package! keyfreq
  :config
  (keyfreq-mode)
  (keyfreq-autosave-mode))

(use-package! eva
  :init
  ;; must be set early
  (setq eva-va-name "Alfred")
  (setq eva-fallback-to-emacs-idle t)

  ;; best set early, but not strictly breaking if not
  (setq eva-user-name "Martin")
  (setq eva-user-birthday "1991-12-07")
  (setq eva-debug t)
  (setq eva-presumptive nil)
  (setq eva-idle-log-path         "/home/kept/Self_data/idle.tsv")
  (setq eva-buffer-focus-log-path "/home/kept/Self_data/buffer-focus.tsv")
  (setq eva-buffer-info-path      "/home/kept/Self_data/buffer-info.tsv")
  (setq eva-main-ledger-path      "/home/kept/Journal/Finances/l.ledger")
  (setq eva-main-datetree-path    "/home/kept/Journal/diary.org")

  :config

  (require 'eva-builtin)

  ;; (setq org-journal-dir)
  ;; (setq org-journal-file-format "%F.org")


  (add-hook 'eva-after-load-vars-hook #'eva-check-dangling-clock)
  (add-hook 'eva-after-load-vars-hook #'eva-check-org-variables)

  ;; HINT: you can even use the same object multiple times in the queue, you'll
  ;; just have to assign the output of (eva-item-create) to an external
  ;; variable and refer to it.
  (setq eva-items
        (list
         (eva-item-create :fn #'eva-greet
                          :min-hours-wait 1)

         (eva-item-create :fn #'eva-query-mood-numeric
                          :dataset "/home/kept/Self_data/mood.tsv"
                          :min-hours-wait 1)

         (eva-item-create :fn #'eva-present-diary
                          :max-successes-per-day 1)

         (eva-item-create :fn #'eva-query-weight
                          :dataset "/home/kept/Self_data/weight.tsv"
                          :max-entries-per-day 1)

         (eva-item-create :fn #'eva-plot-weight
                          :max-entries-per-day 1)

         (eva-item-create :fn #'eva-query-sleep
                          :dataset "/home/kept/Self_data/sleep.tsv"
                          :min-hours-wait 5
                          :lookup-posted-time t)

         ;; (eva-item-create :fn #'eva-present-ledger-report)

         ;; (eva-item-create :fn #'eva-present-org-agenda)

         (eva-item-create :fn #'eva-query-activity
                          :dataset "/home/kept/Self_data/activities.tsv"
                          :min-hours-wait 1)

         (eva-item-create
          :fn (eva-defun my-bye ()
                (message (eva-emit "All done for now."))
                (bury-buffer (eva-buffer-chat)))
          :min-hours-wait 0)))

  (transient-replace-suffix 'eva-dispatch '(0)
    '["General actions"
      ("q" "Quit" bury-buffer)
      ("l" "View Ledger report" eva-present-ledger-report)
      ("f" "View Ledger file" eva-present-ledger-file)
      ("a" "View Org agenda" org-agenda-list)])

  (define-key eva-chat-mode-map (kbd "l") #'eva-present-ledger-report)
  (define-key eva-chat-mode-map (kbd "a") #'org-agenda-list)

  (setq eva-activity-list
        (list (eva-activity-create :name "sleep"
                                   :cost-false-pos 3
                                   :cost-false-neg 3)

              (eva-activity-create :name "studying"
                                   :cost-false-pos 8
                                   :cost-false-neg 8)

              (eva-activity-create :name "coding"
                                   :cost-false-pos 5
                                   :cost-false-neg 5)

              (eva-activity-create :name "unknown"
                                   :cost-false-pos 0
                                   :cost-false-neg 0)))

  (eva-mode)
  )

(defvar my-auto-save--timer nil)
(define-minor-mode my-auto-save-visited-mode
  "Toggle automatic saving to file-visiting buffers on or off.

Unlike `auto-save-mode', this mode will auto-save buffer contents
to the visited files directly and will also run all save-related
hooks.  See Info node `Saving' for details of the save process."
  :group 'auto-save
  :global t
  (when my-auto-save--timer (cancel-timer my-auto-save--timer))
  (setq my-auto-save--timer
        (when my-auto-save-visited-mode
          (run-with-idle-timer
           5 :repeat
           (lambda ()
             (quiet!
               (save-some-buffers
                :no-prompt
                (lambda ()
                  (and buffer-file-name
                       (not (and buffer-auto-save-file-name
                                 auto-save-visited-file-name)))))))))))

(use-package! eva-config :disabled
  :init
  ;; must be set early
  (setq eva-ai-name "Alfred")
  (setq eva-fallback-to-emacs-idle t)

  ;; best set early, but not strictly breaking if not
  (setq eva-user-short-title "sir")
  (setq eva-user-name "Martin")
  (setq eva-user-birthday "1991-12-07")
  (setq eva-debug t)
  (setq eva-presumptive nil)
  (setq eva-idle-log-path         "/home/kept/Self_data/idle.tsv")
  (setq eva-buffer-focus-log-path "/home/kept/Self_data/buffer-focus.tsv")
  (setq eva-buffer-info-path      "/home/kept/Self_data/buffer-info.tsv")
  (setq eva-main-ledger-path      "/home/kept/Journal/Finances/l.ledger")
  (setq eva-main-datetree-path    "/home/kept/Journal/diary.org")
  ;; (setq org-journal-dir)
  ;; (setq org-journal-file-format "%F.org")
  )

(setc mouse-wheel-scroll-amount '(1)) ;; emacs28 default

;; timer every 0.01 secs really?
;; (use-package! good-scroll
;;   :config
;;   (good-scroll-mode))

(use-package! iscroll
  :config
  (add-hook 'text-mode-hook #'iscroll-mode))

;; great for pageup/down, but turns mouse wheel retarded
;; also, counteracts Doom's line-by-line scroll when cursor at edge
;; (use-package! sublimity
;;   :config
;;   (require 'sublimity-scroll)
;;   (sublimity-mode)
;;   (setc sublimity-scroll-drift-length 1)
;;   (setc sublimity-scroll-weight 6))
