;; -*- no-byte-compile: t; -*-

;; Need during init
(package! named-timer)
(package! l) ;; fn literal so i can write (l'rainbow-mode 0)
(package! defrepeater)
(package! el-patch)
(package! compat)
(package! dash)

;; My own packages
(package! asyncloop :type 'local :recipe (:host github :repo "meedstrom/asyncloop" :depth full))
(package! massmapper :type 'local :recipe (:host github :repo "meedstrom/massmapper" :depth full))
(package! deianira :type 'local :recipe (:host github :repo "meedstrom/deianira" :depth full))
(package! inline-anki :type 'local :recipe (:host github :repo "meedstrom/inline-anki" :depth full))
(package! eva
  :type 'local
  :recipe (:host github
           :repo "meedstrom/eva"
           :depth full
           :files (:defaults "assets" "renv" "*.R" "*.gnuplot")))

;; Disable Doom-installed stuff
(package! org-crypt :disable t) ;; slowwww
(package! helpful :disable t) ;; slowwww
(package! doom-snippets :disable t) ;; doom's yasnippets

;; The rest
(package! academic-phrases)
(package! apheleia)
(package! app-launcher :recipe (:host github :repo "SebastienWae/app-launcher"))
(package! artbollocks-mode)
(package! backup-walker)
(package! beginend)
(package! bm)
(package! chatgpt-shell)
(package! bui)
(package! buttercup)
(package! calibredb)
(package! cape)
(package! circadian)
(package! consult)
(package! consult-ffdata :recipe (:host github :repo "chen-chao/consult-ffdata"))
(package! math-delimiters :recipe (:host github :repo "oantolin/math-delimiters"))
(package! copy-as-format)
(package! corfu)
(package! crux)
(package! cycle-buffer) ;; last updated 1997, but more useful commands than iflipb
(package! define-repeat-map :recipe (:host nil :repo "https://tildegit.org/acdw/define-repeat-map.el"))
(package! director)
(package! dired-git-info)
(package! dired-hacks)
(package! dired-hist :recipe (:host github :repo "karthink/dired-hist"))
(package! disable-mouse)
(package! disk-usage)
(package! dmenu)
(package! doom-themes)
(package! elisp-format)
(package! elisp-autofmt)
(package! emacs-piper :recipe (:host gitlab :repo "howardabrams/emacs-piper"))
(package! embark)
(package! ess-rproj :recipe (:host github :repo "chainsawriot/ess-rproj"))
(package! esup)
(package! firefox-bookmarks :recipe (:host github :repo "tangxinfa/firefox-bookmarks"))
(package! flycheck-package)
(package! form-feed)
(package! gif-screencast)
(package! git-messenger)
(package! git-timemachine)
(package! goggles)
(package! hacker-typer)
(package! helm-firefox)
(package! help-find)
;; (package! copilot)
(package! consult-org-roam)
(package! pocket-reader)
(package! hyperbole)
(package! format-all)
(package! iedit)
(package! iflipb) ;; vs cycle-buffer?
(package! iscroll)
(package! kbd-mode :recipe (:host github :repo "kmonad/kbd-mode"))
(package! keymap-utils) ;; prefix kmu-*
(package! mastodon)
(package! mediawiki)
(package! memoize)
(package! mw-thesaurus)
(package! nameless)
(package! objed) ;; for objed-ipipe
(package! package-lint)
(package! peep-dired)
(package! persist)
(package! pfuture)
(package! pinboard)
(package! pinboard-popular)
(package! prism)
(package! screencast)
(package! shelldon)
(package! ef-themes)
(package! ement)
(package! snitch)
(package! snow)
(package! tempel)
(package! elfeed)
(package! elfeed-org)
(package! tempel-collection)
(package! ts)
(package! unpackaged :recipe (:host github :repo "alphapapa/unpackaged.el"))
(package! vc-msg)
(package! vimgolf)
(package! visual-regexp)
(package! wgrep)
(package! xr)
(package! awesome-tray :recipe (:host github :repo "manateelazycat/awesome-tray"))

;; Org
(package! vulpea)
(package! org-anki)
(package! org-transclusion)
(package! org-roam)
(package! htmlize)
(package! delve :recipe (:host github :repo "publicimageltd/delve"))
;; (package! org-recent-headings)
;; (package! org-roam-bibtex) ;; yes still relevant for org 9.5
(package! org-roam-ui)
;; (package! org-tanglesync)
;; (package! ox-rss)

;; Much copypasta from Doom module (the complexity is to permit downloading a
;; shallow clone, normally you can't build Org from a shallow clone).
(package! org
  :recipe (:host github
           :repo "emacs-straight/org-mode"
           :files (:defaults "etc")
           :depth 1
           :build t
           :pre-build
           (progn
             (with-temp-file "org-loaddefs.el")
             (with-temp-file "org-version.el"
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents (doom-path "lisp/org.el") nil 0 1024)
                        (if (re-search-forward "^;; Version: \\([^\n-]+\\)" nil t)
                            (match-string-no-properties 1)
                          "Unknown"))))
                 (insert (format "(defun org-release () %S)\n" version)
                         (format "(defun org-git-version (&rest _) \"%s-??-%s\")\n"
                                 version (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
                         "(provide 'org-version)\n"))))))
(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib"))


;; (package! maple-minibuffer :recipe (:host github :repo "honmaple/emacs-maple-minibuffer"))
;; (package! sway)
;; (package! eot :recipe (:host github :repo ""))
;; (package! affe)
;; (package! exwm)
;; (package! exwm-edit)
;; (package! exwm-firefox :recipe (:host github :repo "ieure/exwm-firefox"))
;; (package! golden-ratio)
;; (package! helm-selector)
;; (package! key-assist)
;; (package! nov)
;; (package! popper)
;; (package! beancount :recipe (:host github :repo "cnsunyour/beancount.el"))
;; (package! forge)
;; (package! magit)
;; (package! repeaters :recipe (:host github :repo "mmarshall540/repeaters"))
;; (package! xah-elisp-mode) ;; try xah-elisp-prettify-root-sexp
;; (package! subed :recipe (:host github :repo "rndusr/subed"))
;; (package! cycle-region)
;; (package! jammer)
;; (package! frames-only-mode)
;; (package! frame-mode)
;; (package! fullframe)
;; (package! mini-frame)
;; (package! cdlatex)
;; (package! docker-tramp)
;; (package! emms)
;; (package! good-scroll)
;; (package! helm-bibtex)
;; (package! helm-navi)
;; (package! helm-org-recent-headings)
;; (package! literate-calc-mode)
;; (package! prodigy)
;; (package! weechat)

;;(package! taxy
;;  :recipe (:host github
;;           :repo "alphapapa/taxy.el"
;;           :files (:defaults "deffy.el" "*.el")))

;; Not tried out

;; (package! org-timeline)
;; (package! embrace)
;; (package! pulseaudio-control)
;; (package! suggest)
;; (package! osm)
;; (package! transmission)
;; (package! trashed)
;; (package! eshell-prompt-extras)
;; (package! navi-mode)
;; (package! sx)
