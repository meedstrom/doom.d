(defvar my-wlr-idle-tracker nil)

;; (start-process "swayidle-eva" nill "swayidle" "timeout" "60" "")
;; (my-idle-secs-wlr)
(setopt eva-debug t)
;; (setopt eva-fallback-to-emacs-idle t)
(setopt eva-init-r nil)
(setopt eva-user-birthday "1991-12-07")
(setopt eva-idle-log-path         "/home/kept/self-data/idle.tsv")
(setopt eva-buffer-focus-log-path "/home/kept/self-data/buffer-focus.tsv")
(setopt eva-buffer-info-path      "/home/kept/self-data/buffer-info.tsv")
;; (setopt eva-main-ledger-path      "~/finances.ledger")
;; (setopt eva-main-datetree-path    "/foo")
(setopt ess-ask-for-ess-directory nil) ;; Prevent annoying ESS startup prompt.
(use-package! eva :disabled
              :config
              (require 'eva-builtin)

              (setopt eva--idle-secs-fn
                      (defun my-idle-secs-wlroots ()
                        "Return idle-time in seconds, rounded to nearest 60.
Should work for Wayland compositors implementing ext-idle-notify.
In other words, if the swayidle program works, this should too."
                        (if (process-live-p my-wlr-idle-tracker)
                            (if (file-exists-p "/tmp/idle")
                                (string-to-number (f-read "/tmp/idle"))
                              0)
                          (message (eva-emit "Restarting the daemon that counts idle-time"))
                          (setq my-wlr-idle-tracker
                                (start-process "~/track-idle2.sh" nil "~/track-idle2.sh")
                                ;; (file-name-concat (file-name-directory (find-library-name "eva"))
                                ;;                    "track-idle2.sh")
                                )
                          0)))

              ;; These are looked up by `eva-present-diary', but org-journal is not needed.
              (setq org-journal-dir "/home/kept/roam/daily/")
              ;; (setq org-journal-file-format "%F.org")

              (add-hook 'eva-after-load-vars-hook #'eva-check-dangling-clock)
              (add-hook 'eva-after-load-vars-hook #'eva-check-org-vars)

              (setq eva-items
                    (list
                     (eva-item-create :fn #'eva-greet
                                      :min-hours-wait 1)

                     (eva-item-create :fn #'eva-query-mood
                                      :dataset "/home/kept/self-data/mood.tsv"
                                      :min-hours-wait 1)

                     (eva-item-create :fn #'eva-present-diary
                                      :max-successes-per-day 1)

                     (eva-item-create :fn #'eva-query-sleep
                                      :dataset "/home/kept/self-data/sleep.tsv"
                                      :min-hours-wait 5
                                      :lookup-posted-time t)

                     ;; (eva-item-create :fn #'eva-present-ledger-report)

                     ;; May be slow
                     ;; (eva-item-create :fn #'eva-present-org-agenda-log-archive)
                     (eva-item-create :fn #'eva-present-org-agenda-log)

                     ;; (eva-item-create :fn #'eva-query-ingredients
                     ;;                  :dataset "~/self-data/ingredients.tsv"
                     ;;                  :min-hours-wait 5)

                     ;; (eva-item-create :fn #'eva-query-cold-shower
                     ;;                  :dataset "~/self-data/cold.tsv"
                     ;;                  :max-entries-per-day 1)

                     (eva-item-create :fn #'eva-query-activity
                                      :dataset "/home/kept/self-data/activities.tsv"
                                      :min-hours-wait 1)

                     ;; you can inline define the functions too
                     (eva-item-create
                      :fn (eva-defun my-bye ()
                            (message (eva-emit "All done for now."))
                            (bury-buffer (eva-buffer-chat)))
                      :min-hours-wait 0)))

              ;; Hotkeys in the chat buffer

              (transient-replace-suffix 'eva-dispatch '(0)
                '["General actions"
                  ("q" "Quit the chat" bury-buffer)
                  ("l" "View Ledger report" eva-present-ledger-report)
                  ("f" "View Ledger file" eva-present-ledger-file)
                  ("a" "View Org agenda" org-agenda-list)])

              (define-key eva-chat-mode-map (kbd "l") #'eva-present-ledger-report)
              (define-key eva-chat-mode-map (kbd "f") #'eva-present-ledger-file)
              (define-key eva-chat-mode-map (kbd "a") #'org-agenda-list)

              ;; Activities (for `eva-query-activity').  These are cl objects for forward
              ;; compatibility; right now only :name is used, to fill out completion
              ;; candidates.
              (setq eva-activity-list
                    (list (eva-activity-create :name "sleep")
                          (eva-activity-create :name "studying")
                          (eva-activity-create :name "coding")
                          (eva-activity-create :name "unknown")))

              ;; (eva-mode)
              )
