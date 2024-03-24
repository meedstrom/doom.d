;; -*- lexical-binding: t; -*-

;;; PERFORMANCE (Org-roam has some tarpits)

;; Don't seek "roam:" links (slows saving)
(setopt org-roam-link-auto-replace nil)

;; I didn't realize for the longest time, but Logseq's logseq/ subdirectory has
;; a lot of backups ending in ".org", adding to the sync time.
(after! org-roam
  (add-to-list 'org-roam-file-exclude-regexp "logseq/")
  (add-to-list 'org-roam-file-exclude-regexp "archive.org"))

;; Speed up `org-roam-db-sync'
;; NOTE: Counterproductive on Windows
;; NOTE: `setopt' breaks it, use `setq'
(setq org-roam-db-gc-threshold (* 4 1024 1024 1024)) ;; 4 GiB

;; Make the commands `org-roam-node-find' & `org-roam-node-insert' instant.
;; Drawback: new notes won't be visible until it auto-refreshes the cached
;; value after 10s of idle.  TODO: Integrate with consult-org-roam.

(defun my-vulpea-memo-refresh ()
  (memoize-restore #'vulpea-db-query)
  (memoize         #'vulpea-db-query)
  (vulpea-db-query nil))

(let ((timer (timer-create)))
  (defun my-vulpea-memo-schedule (&rest _)
    "Schedule a re-caching for when the user is idle."
    (cancel-timer timer)
    (setq timer (run-with-idle-timer 4 nil #'my-vulpea-memo-refresh))))

;; Perf lifesaver  https://github.com/d12frosted/vulpea
(use-package vulpea
  :disabled
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable))
  :bind (([remap org-roam-node-find] . vulpea-find)
         ([remap org-roam-node-insert] . vulpea-insert))
  :config
  ;; why the FUCK can't i just eval the use-package form and trust in the :bind
  ;; clause to work?  seems it has no effect if the package has already loaded.
  ;; and to bind the bindings anyway in a running session, i have to type so
  ;; much to turn my :bind clauses into proper define-key expressions. this
  ;; shit is the worst thing about use-package.
  ;;
  ;; i need to be able to eval it repeatedly while hacking on it, it should
  ;; never be a no-op!  or it should at least message in loud letters "I DIDNT
  ;; ACTUALLY DO ANYTHING, please call my manager and have me fired"!  stupid
  ;;
  ;; the no-op behavior would be acceptable if it applied to :init/:config
  ;; since you can just place point on those sexps and eval them manually, but
  ;; you CANNOT eval the :bind clause or the :hook clause or...  but ironically
  ;; it seems that :config does execute but :bind doesn't...
  ;;
  ;; (define-key global-map [remap org-roam-node-find] #'vulpea-find)
  ;; (define-key global-map [remap org-roam-node-insert] #'vulpea-insert)
  (use-package memoize :demand)
  (memoize #'vulpea-db-query)
  (advice-add 'org-roam-db-update-file :after #'my-vulpea-memo-schedule))


;;; The same memoize solution without vulpea

(defun my-roam-memo-refresh ()
  (memoize-restore #'org-roam-node-read--completions)
  (memoize #'org-roam-node-read--completions)
  (org-roam-node-read--completions nil nil))

(let ((timer (timer-create)))
  (defun my-roam-memo-schedule (&rest _)
    "Schedule a re-caching for when the user is idle."
    (cancel-timer timer)
    (setq timer (run-with-idle-timer 10 nil #'my-roam-memo-refresh))))

(after! org-roam
  ;; Org-roam source ends up below recentf in consult-buffer, no me gusta.
  ;; (consult-org-roam-mode)
  (use-package memoize :demand)
  (memoize #'org-roam-node-read--completions)
  (advice-add 'org-roam-db-update-file :after #'my-roam-memo-schedule))


;;; Stuff

(define-key global-map [remap org-open-at-point] #'my-org-open-at-point-as-maybe-roam-ref)
(add-hook 'org-roam-capture-new-node-hook #'my-org-add-:CREATED:)
(add-hook 'org-roam-buffer-postrender-functions #'magit-section-show-level-2)
(add-hook 'org-insert-heading-hook #'my-org-id-get-create-and-copy)
(setopt org-roam-directory "/home/kept/roam/")
(setopt org-roam-extract-new-file-path "${slug}.org")
(setopt org-roam-ui-browser-function #'my-browse-url-chromium-kiosk)

(when os-guix
  (add-to-list 'browse-url-chromium-arguments "--no-sandbox"))

;; Use my own slug style (see `my-slugify')
(after! org-roam-node
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (my-slugify (org-roam-node-title node))))

(after! org-roam
  (add-hook 'doom-load-theme-hook
            (defun my-theme-mod-org ()
              ;; instead of red todo, i use green IDEA
              (let ((green (face-foreground 'success))
                    (grey (face-foreground 'org-drawer)))
                (set-face-foreground 'org-todo green)
                (set-face-foreground 'org-done grey))
              ;; for backlinks buffer
              (set-face-attribute 'org-roam-title nil :height 1.5)))
  (my-theme-mod-org))

;; When I have a fresh thought, avoid distraction by any earlier stuff I
;; wrote (ADHD is a bitch)
(after! org-roam-dailies
  (advice-add 'org-roam-dailies-capture-today :after
              (defun my-recenter-top (&rest args)
                (recenter 0)
                args)))

(setopt org-roam-dailies-capture-templates
        `(("d" "default" entry "* %<%H:%M>\n%?" :if-new
           (file+head "%<%Y-%m-%d>.org"
                      ,(lines "#+title: %<%Y-%m-%d>"
                              "#+filetags: :noexport:daily:"))
           :immediate-finish t
           :jump-to-captured t)))

(setopt org-roam-capture-templates
        `(("d" "default" plain "%?" :if-new
           (file+head "${slug}.org"
                      ,(lines "#+title: ${title}"
                              "#+filetags: :noexport:stub:"
                              "#+date: "))
           :immediate-finish t
           :jump-to-captured t)

          ("i" "instantly create this node" plain "%?" :if-new
           (file+head "${slug}.org"
                      ,(lines "#+title: ${title}"
                              "#+filetags: :noexport:stub:"
                              "#+date: "))
           :immediate-finish t)

          ("a" "acquaintance" plain "%?" :if-new
           (file+head "${slug}.org"
                      ,(lines "#+title: ${title}"
                              "#+filetags: :stub:acquaintance:shrink:"
                              "#+date: "
                              ":noexport:"
                              "- Email :: "
                              "- Phone :: "
                              "- Address :: "
                              ":end:"
                              "- Location :: "
                              "- Birthday :: "
                              "- Interests :: "
                              "- How we met :: "))
           :immediate-finish t
           :jump-to-captured t)))

(add-hook 'delve-mode-hook #'delve-compact-view-mode)
(after! delve
  ;; It normally inherits from org-roam-title, which I find too big
  (set-face-attribute 'delve-title-face () :inherit 'org-document-title))
