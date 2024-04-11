;; -*- lexical-binding: t; -*-

;;; PERFORMANCE (Org-roam has some tarpits)

;; Don't seek "roam:" links (slows saving)
(setopt org-roam-link-auto-replace nil)
(setopt org-roam-db-update-on-save nil)

;; I didn't realize for the longest time, but Logseq's logseq/ subdirectory has
;; a lot of backups ending in ".org", adding to the sync time.
(after! org-roam
  (add-to-list 'org-roam-file-exclude-regexp "logseq/")
  (add-to-list 'org-roam-file-exclude-regexp "archive.org"))

;; Speed up `org-roam-db-sync'
;; NOTE: Counterproductive on Windows
;; NOTE: `setopt' breaks it, use `setq'
(setq org-roam-db-gc-threshold (* 4 1024 1024 1024)) ;; 4 GiB

(after! org-roam
  ;; Org-roam completion source ends up shown below the massive recentf source
  ;; (consult-org-roam-mode)
  
  (org-roam-db-autosync-mode)


  ;; Because `org-roam-db-autosync-mode' is very slow saving large files,
  ;; set up only the other relevant things that it would have set up.
  ;; (add-hook 'org-mode-hook
  ;;           (defun my-roam-setup ()
  ;;             (when (org-roam-file-p)
  ;;               (org-roam--register-completion-functions-h)
  ;;               (add-hook 'post-command-hook #'org-roam-buffer--redisplay-h 0 t)
  ;;               ;; (add-hook 'after-save-hook #'my-roam-memo-schedule 0 t)
  ;;               )))
  )



;;; Stuff

(define-key global-map [remap org-open-at-point] #'my-org-open-at-point-as-maybe-roam-ref)
(add-hook 'org-roam-capture-new-node-hook #'my-org-add-:CREATED:)
(add-hook 'org-roam-buffer-postrender-functions #'magit-section-show-level-2)
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
        `(("d" "default" plain "%?"
           :target (file+head "${slug}.org"
                              ,(concat "#+title: ${title}"
                                       "\n#+filetags: :noexport:stub:"
                                       "\n#+date: "))
           :immediate-finish t
           :jump-to-captured t)

          ("i" "instantly create this node" plain "%?"
           :target (file+head "${slug}.org"
                              ,(concat "#+title: ${title}"
                                       "\n#+filetags: :noexport:stub:"
                                       "\n#+date: "))
           :immediate-finish t)

          ("l" "Unsorted LW" plain
           ,(concat "* ${title}"
                    "\n:PROPERTIES:"
                    "\n:ID:       %(org-id-uuid)"
                    "\n:CREATED:  [%<%F>]"
                    "\n:ROAM_REFS: %^{ROAM_REFS}"
                    "\n:END:"
                    "\n%i%?")
           :target (node "87724b00-7083-4884-8f1d-6210868679ff")
           :immediate-finish t
           :jump-to-captured t)

          ("a" "acquaintance" plain "%?"
           :if-new (file+head "${slug}.org"
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
