;; -*- lexical-binding: t; -*-
;; Copyright (C) 2020-2023 Martin Edström
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(defvar my-stim-collection (my-stim-collection-generate))

;; doom (or modern emacs?) already provides
;; (require 'epa-file)
;; (epa-file-enable)
;; (setq epg-pinentry-mode 'loopback)
(setq epg-debug t)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; Ask for a symmetric cipher passphrase by default.  Needed to interop with Beorg.
(after! epa-file
  (setq-default epa-file-encrypt-to nil))

;; Workaround a problem in GPG 2.4.1+ where trying to save the file hangs
;; forever.
;; https://stackoverflow.com/questions/76388376/emacs-org-encrypt-entry-hangs-when-file-is-modified
;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2023-08/msg00511.html
(fset 'epg-wait-for-status 'ignore) ;; Uncertain if there are any side effects.

;; (save-place-mode)  ;; doom provides
(auto-save-visited-mode)
(display-battery-mode)

;; From reading Emacs 29 news
;; https://edstrom.dev/tsdt/notable-emacs-news/
(when (version<= "29" emacs-version)
  (pixel-scroll-precision-mode)
  ;; (add-to-list 'default-frame-alist '(alpha-background . 90))
  ;; (setopt mouse-drag-and-drop-region-cross-program t)
  ;; (add-hook 'org-cycle-hook #'org-cycle-display-inline-images)
  (setopt show-paren-context-when-offscreen t)
  (setopt help-enable-variable-value-editing t)
  (setopt proced-enable-color-flag t))
(when (version<= "28" emacs-version)
  (context-menu-mode)
  (repeat-mode)
  ;; (setq use-short-answers t) ;; doom sets
  (setq abbrev-suggest t))

;; Don't filter the buffer list when cycling.  How do these people actually find
;; the filtered buffers when they want them?
(assoc-delete-all 'buffer-predicate default-frame-alist) ;; undoom
(set-frame-parameter nil 'buffer-predicate nil) ;; undoom
(setopt iflipb-ignore-buffers (lambda () t))

;; Never bury buffers, so the buffer list is truly chronological and
;; unsurprising to cycle thru.
;;
;; FWIW, might be worth knowing the command `unbury-buffer' and using that
;; instead -- but would prefer if there was a visual effect when a
;; buffer gets buried as opposed to just switched out.
;; Note that `unbury-buffer' is just a debatably-named wrapper for
;; (switch-to-buffer (last-buffer)).  So maybe you could use it all the time in
;; place of `iflipb-previous-buffer'/`iflipb-next-buffer'.
(fset 'bury-buffer #'ignore)
(fset 'bury-buffer-internal #'ignore)

(setopt backtrace-on-redisplay-error t)
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
(setopt mediawiki-site-alist
        '(("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" nil "Main Page")
          ("WikEmacs" "http://wikemacs.org/" "username" "password" nil "Main Page")))

(setopt mediawiki-site-default "WikEmacs")

(use-package! inline-anki
  :config
  (setq inline-anki-send-tags '(not
                                "noexport"
                                "ARCHIVE"
                                "stub"
                                "eyes_partner"
                                "eyes_friend"
                                "eyes_therapist"))
  (add-to-list 'inline-anki-fields '("Online mirror" . my-anki-webpage-field))
  (add-to-list 'inline-anki-ignore-file-regexps "/daily/")
  (add-to-list 'inline-anki-ignore-file-regexps "/lesswrong/")
  (after! org 
    (add-to-list 'org-structure-template-alist '("f" . "flashcard"))))

(after! ws-butler
  ;; Fix guix.el
  (add-to-list 'ws-butler-global-exempt-modes #'minibuffer-inactive-mode)
  ;; Because org-element-cache (runs in background) throws warnings now
  (add-to-list 'ws-butler-global-exempt-modes #'org-mode))

(use-package! form-feed
  :config
  (global-form-feed-mode)
  (add-hook 'emacs-lisp-compilation-mode-hook #'form-feed-mode))

;; flash effect on deletion, paste etc
(use-package! goggles
  :hook ((prog-mode text-mode) . goggles-mode))

(use-package! iscroll
  :hook ((text-mode elfeed-show-mode eww-mode shr-mode) . iscroll-mode))

(use-package! iedit
  ;; default is C-;
  :init (setq iedit-toggle-key-default nil)
  :commands iedit-mode)

(use-package! objed
  :commands objed-ipipe)

(use-package! crux)

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package! beginend
  :config
  (beginend-global-mode))

(use-package! deianira
  :commands deianira-mode
  :config
  ;; (fset 'which-key-mode #'ignore)
  (setq dei-ignore "C-")
  (after! hydra
    (define-key hydra-base-map (kbd "<f5>") #'hydra-repeat))
  (setq dei-invisible-leafs
        (seq-difference dei-invisible-leafs '("<menu>" "SPC")))
  ;; (deianira-mode)
  )

(use-package! apheleia
  :config
  (setopt apheleia-log-debug-info t)
  (apheleia-global-mode))

(use-package! nameless
  :hook (emacs-lisp-mode . nameless-mode)
  :init
  ;; swung dash ⁓ tilde op ∼ sine ∿ almost eq ≈
  ;; four dot mark ⁛ lock 🔒 ⊝ ◯ ⁐ ○ ⚞⁖ ⋐⚟⤳〜
  (setopt nameless-prefix "⁓")
  ;; (setopt nameless-prefix "⚟")
  (setopt nameless-private-prefix t)
  (setopt nameless-affect-indentation-and-filling nil)
  ;; (add-hook 'nameless-mode-hook #'my-adjust-scale-2)
  :config
  (set-face-attribute 'nameless-face nil :inherit 'unspecified))

;; try to fix JS/TS buffers freezing.  Seems the issue is that prism-mode collides badly with RJSX mode.
;; (setq tide-server-max-response-length )
;; (after! rjsx-mode
;; (remove-hook 'rjsx-mode-hook #'rainbow-delimiters-mode))

;; (after! typescript-mode
;;   ;; NOTE: typescript-tsx-mode is actually defined in ~/doomemacs/modules/lang/javascript/config.el
;;   (remove-hook 'typescript-tsx-mode-hook #'rainbow-delimiters-mode))

(use-package! prism
  :disabled
  :init
  (setopt prism-comments nil)
  ;; The default (40 50 60) is disorienting when turning prism on and off.
  (setopt prism-desaturations '(0 20 60))
  ;; (setopt prism-desaturations '(0))
  ;; note, another odd default is that (in lisp) the parens enclosing a sexp
  ;; are a diff color from the symbols inside -- people arent used to this
  ;; either, it makes em stand out too much imo
  :config
  ;; Replace rainbow-delimiters (it's on a dozen hooks in Doom, so this method is easiest).
  (fset 'rainbow-delimiters-mode #'prism-mode)
  ;; (require 'prism)
  ;; (add-hook 'doom-load-theme-hook #'prism-set-colors)

  (add-hook 'typescript-mode-hook #'prism-mode)
  (add-hook 'typescript-tsx-mode-hook #'prism-mode)
  (add-hook 'js-base-mode-hook #'prism-mode)
  ;; (add-hook 'web-mode-hook #'prism-mode) ;; infinite loop in .svelte files
  )

(use-package! elfeed
  :disabled
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

;; (use-package! circadian
;;   :config
;;   (add-hook 'circadian-after-load-theme-hook #'prism-set-colors)
;;   (setq circadian-themes '(("8:00"   . doom-storage-tube-green)
;;                            ("18:00"  . doom-storage-tube-amber-2)))
;;   (circadian-setup))



;; (setq counsel-ffdata-database-path "/home/me/.mozilla/firefox/wrki7yvc.dev-edition-default/places.sqlite")
;; (setq helm-firefox-bookmark-user-directory "/home/me/.mozilla/firefox/wrki7yvc.dev-edition-default/")

;; ;; Fix
;; (defun helm-get-firefox-user-init-dir (directory)
;;   "Guess the default Firefox user directory name."
;;   (with-temp-buffer
;;     (insert-file-contents
;;      (expand-file-name "profiles.ini" directory))
;;     (goto-char (point-min))
;;     (search-forward "Default=1")
;;     (search-backward "Path=")
;;     (file-name-as-directory (expand-file-name
;;                              (buffer-substring-no-properties
;;                               (match-end 0) (point-at-eol))
;;                              directory))))


(use-package! smart-tabs-mode
  :config
  (smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python
                        'ruby 'nxml))
