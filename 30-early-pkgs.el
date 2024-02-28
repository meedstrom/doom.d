;; -*- lexical-binding: t; -*-

(use-package! snitch
  :disabled
  :config
  (require 'snitch)
  (setq snitch-network-policy 'deny)
  (setq snitch-network-whitelist
        (list (cons #'snitch-filter-src-pkg '(eww))
              (cons #'snitch-filter-src-pkg '(magit))
              (cons #'snitch-filter-src-pkg '(forge))))
  (setq snitch-log-policy '(blocked blacklisted))
  ;; (setq snitch-log-policy '(allowed blocked whitelisted blacklisted))
  ;; (setq snitch-log-verbose nil)
  ;; (setq snitch-log-buffer-max-lines)
  ;; (setq snitch-enable-notifications t)
  (setq snitch-trace-timers nil)
  (snitch-mode))
