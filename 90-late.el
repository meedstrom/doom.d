;; -*- lexical-binding: t; -*-

(hook-do 'doom-after-init-hook
         (setq my-stim-collection (my-stim-collection-generate)))

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

(after inline-anki
  (add-to-list 'inline-anki-fields '("Online mirror" . my-anki-field-for-webpage))
  (add-to-list 'inline-anki-ignore-file-regexps "/daily/")
  (add-to-list 'inline-anki-ignore-file-regexps "/lesswrong/")
  (after org
    (add-to-list 'org-structure-template-alist '("f" . "flashcard"))))

(setopt inline-anki-send-tags '(not
                                "noexport"
                                "ARCHIVE"
                                "stub"
                                "eyes_partner"
                                "eyes_friend"
                                "eyes_therapist"))

;; (use-package inline-anki
;;   :config
;;   (setq inline-anki-send-tags '(not
;;                                 "noexport"
;;                                 "ARCHIVE"
;;                                 "stub"
;;                                 "eyes_partner"
;;                                 "eyes_friend"
;;                                 "eyes_therapist"))
;;   (add-to-list 'inline-anki-fields '("Online mirror" . my-anki-webpage-field))
;;   (add-to-list 'inline-anki-ignore-file-regexps "/daily/")
;;   (add-to-list 'inline-anki-ignore-file-regexps "/lesswrong/")
;;   (after! org
;;     (add-to-list 'org-structure-template-alist '("f" . "flashcard"))))

(after ws-butler
  ;; Fix guix.el
  (add-to-list 'ws-butler-global-exempt-modes #'minibuffer-inactive-mode)
  ;; Because org-element-cache (runs in background) throws warnings now
  (add-to-list 'ws-butler-global-exempt-modes #'org-mode))

(after deianira
  ;; (fset 'which-key-mode #'ignore)
  (after! hydra
    (define-key hydra-base-map (kbd "<f5>") #'hydra-repeat))
  (setq dei-ignore "C-")
  (setq dei-invisible-leafs
        (seq-difference dei-invisible-leafs '("<menu>" "SPC"))))


;; (use-package circadian
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

