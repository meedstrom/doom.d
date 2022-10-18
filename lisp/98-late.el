(save-place-mode)
(auto-save-visited-mode)
(display-battery-mode)

;; embark
(setopt y-or-n-p-use-read-key t)

(use-package! goggles
  :hook ((prog-mode text-mode) . goggles-mode))

(use-package! objed
  :commands objed-ipipe)

;; From reading Emacs 29 news
;; - image-dired is fast now
;; - image-dired slideshow on S
;; - in image-dired, marking an image in the display buffer shows the next
;; - image
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
(when (version<= "29" emacs-version)
  (pixel-scroll-precision-mode)
  (add-to-list 'default-frame-alist '(alpha-background . 90))
  ;; (setopt help-enable-variable-value-editing t)
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

;; it's starting to annoy me the delay in the first eshell, dired, and org-roam
;; buffers. Either design a :defer-incrementally list for each, or do some
;; general "load everything" during idle so that we effectively don't have ANY
;; autoloaded things...

;; (use-package! eshell
;;   :defer-incrementally
;;   ())

;; (use-package! dired
;;   :defer-incrementally
;;   ())

;; (use-package! org-roam
;;   :defer-incrementally
;;   ())

;; neat
(setopt browse-url-chromium-arguments '("--app=http://localhost:35901"))
(setopt org-roam-ui-browser-function #'browse-url-chromium)

;; for Guix System
(add-to-list 'browse-url-chromium-arguments "--no-sandbox")

(after! ws-butler
  ;; fix guix.el
  (add-to-list 'ws-butler-global-exempt-modes #'minibuffer-inactive-mode)
  ;; because org-element-cache (runs in background) throws warnings now (culprit Roam?)
  (add-to-list 'ws-butler-global-exempt-modes #'org-mode))

(use-package! iscroll
  :hook ((text-mode elfeed-show-mode eww-mode shr-mode) . iscroll-mode))

(require 'form-feed)
(global-form-feed-mode)
(add-hook 'emacs-lisp-compilation-mode-hook #'form-feed-mode)

(use-package! nameless
  :hook (emacs-lisp-mode . nameless-mode)
  :init
  ;; swung dash ⁓ tilde op ∼ sine ∿ almost eq ≈
  ;; four dot mark ⁛ lock 🔒 ⊝ ◯ ⁐ ○ ⚞⁖ ⋐⚟⤳〜
  (setopt nameless-prefix "⁓")
  ;; (setopt nameless-private-prefix "〜")
  (setopt nameless-private-prefix t)
  (setopt nameless-affect-indentation-and-filling nil)
  (add-hook 'nameless-mode-hook #'my-adjust-scale-2)
  :config
  (set-face-attribute 'nameless-face nil :inherit 'unspecified))

(use-package! prism
  :custom (;; (prism-comments nil)
           ;; (prism-lightens '(0))
           (prism-parens t)
           (prism-desaturations '(0 20 60)))
  :hook (prog-mode . prism-mode)
  :config
  (add-hook 'doom-load-theme-hook #'prism-set-colors)
  ;; Disable rainbow-delimiters.  Doom bug: the following advice doesn't run late enough to have a meaningful effect.
  ;; (advice-add #'prism-mode :after
  ;;             (defun my-turn-off-rainbow-delimiters (&rest _args)
  ;;               (rainbow-delimiters-mode 0)))
  (fset 'rainbow-delimiters-mode #'ignore)
  ;; (remove-hook! (racket-mode
  ;;                scheme-mode
  ;;                sh-mode
  ;;                clojure-mode
  ;;                purescript-mode
  ;;                rjsx-mode
  ;;                typescript-mode
  ;;                typescript-tsx-mode
  ;;                TeX-update-style
  ;;                zig
  ;;                java-mode
  ;;                c-mode-common
  ;;                csharp-mode
  ;;                lisp-mode)
  ;;   #'rainbow-delimiters-mode)
  )
(setopt rmh-elfeed-org-files
        (list (expand-file-name "elfeed.org" doom-private-dir)))

(use-package! elfeed
  :defer
  :config
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :entry-title (rx (or "MCMXXX" "A&R"))
                                :add 'junk))
  (setopt elfeed-curl-max-connections 1)
  (setopt elfeed-search-filter "@2-months-ago -junk +unread +fav")
  ;; (ignore-errors (elfeed-org)) ;; does not work
  )

;; NOTE: this mode sometimes messes things up. You could just manually
;; call M-x crux-sudo-edit when you need it (initialism: M-x cse).
(use-package! crux
  :config (crux-reopen-as-root-mode))

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package! deianira
  :config (deianira-mode))

;; It sounds like Hyperbole is not just a sort of greybeard Embark, it has lots
;; of premade "buttons" (what are those?) that Embark lacks, for one thing.  I
;; think to properly learn it, I'd like all possible targets/buttons colorized
;; for me during a training period.  I'd also like the Hyperbole prompts to
;; conform to completing-read.
(use-package! hyperbole
  :defer
  :commands hkey-either
  :config (hyperbole-mode))
