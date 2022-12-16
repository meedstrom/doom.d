(save-place-mode)
(auto-save-visited-mode)
(display-battery-mode)

;; From reading Emacs 29 news
;; - image-dired is fast now
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
;; buffer being buried
(fset 'bury-buffer #'ignore)

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

;; NOTE: you can use .authinfo instead of putting your user name and password here
(setopt mediawiki-site-alist
        '(("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" nil "Main Page")
          ("WikEmacs" "http://wikemacs.org/" "username" "password" nil "Main Page")))

(setopt mediawiki-site-default "WikEmacs")

(after! ws-butler
  ;; Undoom. Was this a Vimism?
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

(use-package! deianira-mass-remap
  :config
  (setq dei-mass-remap-debug-level 1)
  (general-auto-unbind-keys 'undo) ;; ensure it works with and without general
  (add-hook 'window-buffer-change-functions #'dei-record-keymap-maybe -70)
  (add-hook 'dei-keymap-found-hook #'dei-define-super-like-ctl-everywhere)
  (add-hook 'dei-keymap-found-hook #'dei-homogenize-all-keymaps)
  (setq dei-homogenizing-winners
        '(("C-x C-s" . global-map)
          ("C-x C-f" . global-map)
          ("C-x C-q" . global-map)
          ("C-x C-;" . global-map)
          ("C-x C-l" . global-map)
          ("C-c C-c" . org-mode-map)
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

(use-package! prism
  :init
  (setopt prism-parens t)
  (setopt prism-desaturations '(0 20 60))
  :config
  ;; Replace rainbow-delimiters (it's on a dozen hooks in Doom).
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

;; It sounds like Hyperbole is not only a sort of greybeard Embark, it has lots
;; of premade "buttons" (what are those?) that Embark lacks, for one thing.  I
;; think to learn it, I'd like all possible targets/buttons colorized for me
;; during a training period.  I'd also like the Hyperbole prompts to conform to
;; Vertico style instead of that ascetic oneliner.
(use-package! hyperbole
  :commands hkey-either)
