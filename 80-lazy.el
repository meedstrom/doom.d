;; Configure lazy external packages -*- lexical-binding: t; -*-

(setopt helpful-max-buffers nil) ;; what's the point of killing buffers
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
(setopt mediawiki-site-default "WikEmacs")
(setopt mediawiki-site-alist
        '(("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" nil "Main Page")
          ("WikEmacs" "http://wikemacs.org/" "username" "password" nil "Main Page")))

(after! multiple-cursors
  ;; auto-save every 5 seconds, destroys what youre doing!
  (add-to-list 'mc/unsupported-minor-modes 'auto-save-visited-mode)
  (add-to-list 'mc/unsupported-minor-modes 'nameless-mode))

(after! inline-anki
  (setopt inline-anki-send-tags '(not
                                  "noexport"
                                  "ARCHIVE"
                                  "stub"
                                  "eyes_partner"
                                  "eyes_friend"
                                  "eyes_therapist"))
  (add-to-list 'inline-anki-fields '("Online mirror" . my-anki-field-for-webpage))
  (add-to-list 'inline-anki-ignore-file-regexps "/daily/")
  (add-to-list 'inline-anki-ignore-file-regexps "/lesswrong/")
  (after! org
    (add-to-list 'org-structure-template-alist '("f" . "flashcard"))))

(after! ws-butler
  ;; Fix problem with guix.el
  (add-to-list 'ws-butler-global-exempt-modes #'minibuffer-inactive-mode)
  ;; Fix for org because the org-element parser throws hella warnings since 9.5
  (add-to-list 'ws-butler-global-exempt-modes #'org-mode))

(after! deianira
  ;; (fset 'which-key-mode #'ignore)
  (after! hydra
    (define-key hydra-base-map (kbd "<f5>") #'hydra-repeat))
  (setq dei-ignore "C-")
  (setq dei-invisible-leafs
        (seq-difference dei-invisible-leafs '("<menu>" "SPC"))))

;; visual flash on deletion, paste etc
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode))

;; better image scroll
(use-package iscroll
  :hook ((text-mode elfeed-show-mode eww-mode shr-mode) . iscroll-mode))

;; (use-package iedit
;;   ;; default is C-;
;;   :init (setq iedit-toggle-key-default nil)
;;   :commands iedit-mode)

(use-package objed
  :commands objed-ipipe)

(use-package hyperbole
  :commands hkey-either)

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package copilot
  :disabled
  :defer
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package nameless
  :hook (emacs-lisp-mode . nameless-mode)
  :init
  (setopt nameless-prefix "⁓")
  (setopt nameless-private-prefix t)
  (setopt nameless-affect-indentation-and-filling nil)
  :config
  (set-face-attribute 'nameless-face nil :inherit 'unspecified))

(use-package elfeed
  :disabled
  :defer
  :config
  (setopt rmh-elfeed-org-files
          (list (expand-file-name "elfeed.org" doom-private-dir)))
  ;; filter into junk
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :entry-title (rx (or "MCMXXX"
                                                     "A&R"))
                                :add 'junk))
  (setopt elfeed-curl-max-connections 1)
  (setopt elfeed-search-filter "@2-months-ago -junk +unread +fav")
  ;; (ignore-errors (elfeed-org)) ;; does not work
  )
