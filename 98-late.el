(save-place-mode)
(auto-save-visited-mode)
(display-battery-mode)

;; From reading Emacs 29 news
;; - image-dired is faster now
;; - image-dired slideshow on S
;; - in image-dired, marking an image in the display buffer shows the next
;;   image
;; - new hotkeys in help buffers n(next) p(prev) e(edit)
;; - leuven-dark
;; - describe-buffer-bindings no longer prints "Prefix Command"
;; - new fn: setopt (like setq!/setc/csetq)
;; - new fn: buffer-match-p and match-buffers
;; - new fn: key-valid-p
;; - new fn: key-parse that always returns vector output unlike kbd, but
;;   (key-parse "<TAB>") and (key-parse "TAB") still differ
;; - keymap-set instsead of define-key
;; - keymap-lookup instead of lookup-key and key-binding
;; - new fn: define-keymap and defvar-keymap
;; - M-x shortdoc RET keymaps RET
;; - new fn: org-get-title
(when (version<= "29" emacs-version)
  (pixel-scroll-precision-mode)
  (add-to-list 'default-frame-alist '(alpha-background . 90))
  ;; (setopt help-enable-variable-value-editing t)
  ;; (add-hook 'org-cycle-hook #'org-cycle-display-inline-images)
  )
;; From reading Emacs 28 news
;; - C-x x a new keymap for "buffer actions"
;; - eval-last-sexp now actually does re-eval defvars!
;; - C-x 5 5: other-frame-prefix!  no more multitude of *-other-frame.  is
;;   there an embark "postfix" instead of prefix?
;; - C-x t t: similar to above but for other tab
;; - describe-keymap useful for e.g. image-dired-map or ess-mode-map
;; - toggle-truncate-lines now disables visual-line-mode
;; - M-o prefix gone
;; - 'shell' now uses 'pop-to-buffer-same-window' consistent w/ eshell
;; - eshell-mode-map works normally
(when (version<= "28" emacs-version)
  (context-menu-mode)
  (repeat-mode)
  ;; (setq use-short-answers t)
  (setq abbrev-suggest t)
  )

;; Don't filter the buffer list when cycling.  How do these people actually find
;; the filtered buffers when they want them?
(assoc-delete-all 'buffer-predicate default-frame-alist) ;; undoom
(set-frame-parameter nil 'buffer-predicate nil) ;; undoom
(setopt iflipb-ignore-buffers nil)

;; FWIW, might be worth knowing the command `unbury-buffer' and using that
;; instead -- but would be great if there was a visual effect associated with a
;; buffer being buried as opposed to just switched-away-from.
;; Note that `unbury-buffer' is just a debatably-named wrapper for
;; (switch-to-buffer (last-buffer)).  So maybe you could use it all the time in
;; place of `iflipb-previous-buffer'/`iflipb-next-buffer'.
(fset 'bury-buffer #'ignore)
(fset 'bury-buffer-internal #'ignore)

(setopt helpful-max-buffers nil) ;; wats the point of killing buffers
(setopt iflipb-wrap-around t)
(setopt ranger-map-style 'emacs)
(setopt which-key-idle-delay 0.2)
(setopt rainbow-x-colors nil) ;; only colorize hex strings
(setopt calibredb-root-dir "~/Calibre Library/")
(setopt calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
(setopt calibredb-format-width 8)

(setopt +doom-dashboard-functions
        '(doom-dashboard-widget-shortmenu
          doom-dashboard-widget-loaded))

;; NOTE: you can use ~/.authinfo instead of putting your user name and password here
(setopt mediawiki-site-alist
        '(("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" nil "Main Page")
          ("WikEmacs" "http://wikemacs.org/" "username" "password" nil "Main Page")))

(setopt mediawiki-site-default "WikEmacs")

(after! ws-butler
  ;; Undoom. Was this a Vimism? If this is nil while auto-save-visited-mode is
  ;; active, the result is incredibly annoying.
  (setopt ws-butler-keep-whitespace-before-point t)
  ;; fix guix.el
  (add-to-list 'ws-butler-global-exempt-modes #'minibuffer-inactive-mode)
  ;; because org-element-cache (runs in background) throws warnings now (culprit Roam?)
  ;; probably fixed in emacs 29
  (add-to-list 'ws-butler-global-exempt-modes #'org-mode))

(use-package! form-feed
  :config
  (global-form-feed-mode)
  (add-hook 'emacs-lisp-compilation-mode-hook #'form-feed-mode))

(use-package! goggles
  :hook ((prog-mode text-mode) . goggles-mode))

(use-package! iscroll
  :hook ((text-mode elfeed-show-mode eww-mode shr-mode) . iscroll-mode))

(use-package! objed
  :commands objed-ipipe)

(use-package! crux
  :config
  ;; Reopen as root specifically when exiting read-only-mode on a root-owned file.
  ;; (add-hook 'read-only-mode-hook
  ;;           (defun my-sudo-edit-maybe ()
  ;;             "Call `crux-sudo-edit' if the file is unwritable."
  ;;             (if buffer-read-only
  ;;                 nil
  ;;               (when (and (buffer-file-name)
  ;;                          (not (file-writable-p (buffer-file-name)))
  ;;                          (crux-sudo-edit))))))
  )

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode))

(which-key-mode 0)

(use-package! deianira-mass-remap
  :config
  (general-auto-unbind-keys 'undo) ;; ensure it works with and without general
  (add-hook 'window-buffer-change-functions #'dei-record-keymap-maybe -70)
  (add-hook 'dei-keymap-found-hook #'dei-define-super-like-ctl)
  (add-hook 'dei-keymap-found-hook #'dei-homogenize-all-keymaps)
  (setq dei-homogenizing-winners
        '(("C-x C-s" . global-map)
          ("C-x C-f" . global-map)
          ("C-x C-q" . global-map)
          ("C-x C-;" . global-map)
          ("C-x C-l" . global-map)
          ("C-c C-c")
          ("C-c C-," . org-mode-map))))

(use-package! deianira
  :config
  (after! hydra
    (define-key hydra-base-map (kbd "<f5>") #'hydra-repeat))
  (setq asyncloop-debug-level 1)
  (setq dei-invisible-leafs
        (seq-difference dei-invisible-leafs '("<menu>" "SPC")))
  ;; (deianira-mode)
  )

(use-package! apheleia
  :config
  (apheleia-global-mode))

(use-package! nameless
  :hook (emacs-lisp-mode . nameless-mode)
  :init
  ;; swung dash ⁓ tilde op ∼ sine ∿ almost eq ≈
  ;; four dot mark ⁛ lock 🔒 ⊝ ◯ ⁐ ○ ⚞⁖ ⋐⚟⤳〜
  (setopt nameless-prefix "⁓")
  (setopt nameless-private-prefix t)
  (setopt nameless-affect-indentation-and-filling nil)
  (add-hook 'nameless-mode-hook #'my-adjust-scale-2)
  :config
  (set-face-attribute 'nameless-face nil :inherit 'unspecified))

;; try to fix JS/TS buffers freezing.  Seems the issue is that prism-mode collides badly with RJSX mode.
;; (setq tide-server-max-response-length )
(after! rjsx-mode
  (remove-hook 'rjsx-mode-hook #'rainbow-delimiters-mode))

(after! typescript-mode
  ;; NOTE: typescript-tsx-mode is actually defined in ~/doomemacs/modules/lang/javascript/config.el
  (remove-hook 'typescript-tsx-mode-hook #'rainbow-delimiters-mode))

(use-package! prism
  :init
  (setopt prism-parens t)
  (setopt prism-desaturations '(0 20 60))
  :config
  ;; Replace rainbow-delimiters (it's on a dozen hooks in Doom so this is easiest).
  (fset 'rainbow-delimiters-mode #'prism-mode)
  (add-hook 'doom-load-theme-hook #'prism-set-colors))

(use-package! elfeed
  :defer
  :config
  (setopt rmh-elfeed-org-files
          (list (expand-file-name "elfeed.org" doom-private-dir)))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :entry-title (rx (or "MCMXXX"
                                                     "A&R"))
                                :add 'junk))
  (setopt elfeed-curl-max-connections 1)
  (setopt elfeed-search-filter "@2-months-ago -junk +unread +fav")
  ;; (ignore-errors (elfeed-org)) ;; does not work
  )

;; It sounds like Hyperbole is a sort of greybeard's Embark, and it packs a lot
;; more "batteries included".  A drawback is that Hyperbole uses different UI
;; metaphors than does Embark, metaphors that could feel natural to an
;; Ido/icomplete user, but not so much to Vertico/Ivy/Helm users.  That
;; shouldn't be too hard to get over though.  A feature that caught my interest:
;; lots of premade "buttons" that Embark lacks.
;;
;; I haven't even used Embark much, as I also struggle with its UI metaphor.
;; Could be worth trying Hyperbole.  While it has a hand-rolled UI, that also
;; means it's thought-through.  Every Embark popup is full of suggestions I
;; don't need.  I think to learn Hyperbole, I'd like all possible
;; targets/buttons colorized for me during a training period.  I'd also like the
;; Hyperbole prompts to come in a form utilizable by Vertico instead of that
;; ascetic oneline minibuffer -- not that the ascetic minibuffer is a bad idea,
;; but it feels less inviting as it breaks the paradigms I'm used to, and it's
;; less discoverable so I must put in more effort to discover what does what
;; (names like "hyrolo" certainly tell me nothing).
;;
;; To be clear where I'm coming from, I don't regard Embark and Hyperbole as
;; competing for popularity. (I'm the kind of user who uses Vertico for some
;; things and Helm for other things.)  It may well be that when one gets more
;; popular, so does the other.  Not zero-sum.  But I observe that people on the
;; subreddit seem much more ready to adopt Embark than they ever were ready to
;; adopt Hyperbole, so there could be a risk it goes the way of Icicles where
;; there's a handful of faithful users who don't publish anything about their
;; workflows and the rest of us have no idea if it contains any lessons to carry
;; over to the wider ecosystem and will not bother learning it in order to see,
;; because it's too strange.  I don't know the full scope of Hyperbole so stop
;; me if I'm wrong but would it be possible to reimplement Hyperbole to be only
;; a large collection of commands that only use completing-read as UI, together
;; with recommendations for where to bind them and how to configure Embark and
;; maybe some other packages?
(use-package! hyperbole
  :commands hkey-either)

;; FIXME: need to hook prism-set-colors...
;; (use-package! circadian
;;   :config
;;   (setq circadian-themes '(("8:00"   . doom-storage-tube-green)
;;                            ("18:00"  . doom-storage-tube-amber-2)))
;;   (circadian-setup))

(defvar my-auto-commit-dirs
  '("/home/kept/roam/"
    "/home/kept/emacs/conf-doom/"))

;; TODO: Change to hourly commits?
(defun my-auto-commit-maybe ()
  "Create a new commit if the last was on a different day.
Otherwise just amend today's commit.

Only operate if the repo root directory is a member of
`my-auto-commit-dirs'.

If there are untracked files, do nothing and print a message.

Suitable on `after-save-hook'."
  (require 'magit)
  (require 'project)
  (when (and (project-current)
             (member (project-root (project-current)) my-auto-commit-dirs))
    (let ((last-commit-date (my-process-output-to-string
                             "git" "log" "-n" "1" "--pretty=format:%cs"))
          (last-commit-msg (my-process-output-to-string
                             "git" "log" "-n" "1" "--pretty=format:%s")))
      (if (string-search "Fatal" last-commit-date)
          (message "Git failed, probably not a Git repo: %s" default-directory)
        ;; Special case for Org-Roam: auto-stage new notes, bc it happens often
        (and (equal "org" (file-name-extension (buffer-file-name)))
             (string-search org-roam-directory default-directory)
             (magit-run-git "add" (buffer-file-name)))

        (if (magit-untracked-files)
            (message "Won't auto-commit.  Stage untracked files or edit .gitignore")
          (if (and (equal last-commit-date (format-time-string "%F"))
                   (equal last-commit-msg "Auto-commit"))
              ;; Same day, so amend today's autocommit
              (magit-commit-amend '("--all" "--reuse-message=HEAD"))
            ;; New day, new autocommit
            (magit-commit-create '("--all" "--message=Auto-commit"))))))))

(add-hook 'after-save-hook #'my-auto-commit-maybe)

;; often it's a binary file, so prevent accidental edits
(add-hook 'so-long-mode-hook #'read-only-mode)

;; It's insane to put data-syncs on kill-emacs-hook.  Most of the time my emacs
;; goes down, it happens in a non-clean way -- why would I intentionally shut
;; off Emacs if everything is fine?  As a result, I'm missing some data
;; every time I start Emacs: I can't find org notes by org-id, recentf
;; suffers partial amnesia, and so on.  This has been annoying me for years.
(defun my-write-data ()
  "Write histories and caches to disk.
This runs many members of `kill-emacs-hook' so we don't have to
rely on that hook.  You may put this on a repeating timer."
  (let ((hooks (seq-intersection
                ;; any more items of interest in your `kill-emacs-hook', add them here
                #'(bookmark-exit-hook-internal
                   savehist-autosave
                   transient-maybe-save-history
                   org-id-locations-save
                   save-place-kill-emacs-hook
                   recentf-save-list
                   recentf-cleanup
                   doom-cleanup-project-cache-h
                   doom-persist-scratch-buffers-h)
                kill-emacs-hook)))
    (run-hooks 'hooks)))

;; THIS is how you do data sync.  You can't rely on takedown logic.
(setq my-write-data-timer (run-with-idle-timer (* 3 60) t #'my-write-data))
