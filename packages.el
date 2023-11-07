;; -*- no-byte-compile: t; -*-

;; Need for init

(package! named-timer)
(package! l) ;; anonymous fn literal: (l'concat %) is (lambda (%) (concat %))
(package! defrepeater)
(package! el-patch)

;; Pkg dev stuff

(package! pfuture)
(package! persist)
(package! bui)
(package! ts)
(package! flycheck-package)
(package! package-lint)
(package! buttercup)
(package! nameless)
(package! gif-screencast)
(package! screencast)
(package! keymap-utils) ;; prefix kmu-*
(package! compat)

;; The rest

(package! backup-walker)
(package! kbd-mode :recipe (:host github :repo "kmonad/kbd-mode"))
(package! circadian)
(package! academic-phrases)
;; (package! affe)
(package! apheleia)
(package! copy-as-format)
(package! memoize)
(package! smart-tabs-mode)
;; (package! anki-editor)
;; (package! org-anki)
(package! app-launcher :recipe (:host github :repo "SebastienWae/app-launcher"))
(package! artbollocks-mode)
;; (package! firefox-bookmarks :recipe (:host github :repo "tangxinfa/firefox-bookmarks"))
(package! consult-ffdata :recipe (:host github :repo "chen-chao/consult-ffdata"))
(package! helm-firefox)
(package! bm)
(package! calibredb)
(package! cape)
(package! corfu)
(package! embark)
(package! crux)
(package! cycle-buffer) ;; last updated 1997, but has more useful commands than iflipb
(package! define-repeat-map :recipe (:host nil :repo "https://tildegit.org/acdw/define-repeat-map.el"))
(package! director)
(package! dired-hacks)
(package! disable-mouse)
(package! disk-usage)
(package! dmenu)
(package! doom-snippets :ignore t) ;; disable doom's yasnippets
(package! elisp-format)
(package! emacs-piper :recipe (:host gitlab :repo "howardabrams/emacs-piper"))
(package! ess-rproj :recipe (:host github :repo "chainsawriot/ess-rproj"))
(package! esup)
(package! tempel)
(package! tempel-collection)
(package! massmapper :recipe (:host github :repo "meedstrom/massmapper"))
(package! deianira :recipe (:host github :repo "meedstrom/deianira"))
(package! inline-anki :recipe (:host github :repo "meedstrom/inline-anki"))
(package! exwm)
(package! exwm-edit)
(package! pinboard-popular)
(package! pinboard)
(package! exwm-firefox :recipe (:host github :repo "ieure/exwm-firefox"))
(package! form-feed)
(package! gif-screencast)
(package! git-messenger)
(package! goggles)
;; (package! golden-ratio)
(package! hacker-typer)
;; (package! helm-selector)
(package! help-find)
(package! iedit)
(package! iflipb) ;; vs cycle-buffer?
(package! iscroll)
(package! key-assist)
(package! key-chord)
(package! mediawiki)
(package! mw-thesaurus)
;; (package! ox-rss)
;; (package! nov)
(package! objed) ;; for objed-ipipe
;; (package! org-drill)
;; (package! org-recent-headings)
;; (package! org-roam-bibtex) ;; yes still relevant for org 9.5
;; (package! org-roam-ui)
;; (package! org-tanglesync)
(package! peep-dired)
(package! prism)
(package! shelldon)
(package! sicp)
(package! stan-mode)
(package! sway)
(package! vc-msg)
(package! vimgolf)
(package! visual-regexp)
(package! wgrep)
(package! xr)
(package! dired-hist :recipe (:host github :repo "karthink/dired-hist"))

;; (package! which-key :ignore t)
;; (package! github-review :ignore t) ;; causes errors

(package! org-roam)
(package! org-transclusion)
(package! htmlize)

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
;;
;; (package! mini-frame)
;; (package! cdlatex)
;; (package! docker-tramp)
;; (package! emms)
;; (package! good-scroll)
;; (package! helm-bibtex)
;; (package! helm-navi)
;; (package! helm-org-recent-headings)
;; (package! literate-calc-mode)
;; (package! matrix-client)
;; (package! mu4e-dashboard)
;; (package! prodigy)
;; (package! unpackaged :recipe (:host github :repo "alphapapa/unpackaged.el"))
;; (package! weechat)

(package! eva
  :recipe (:host github
           :repo "meedstrom/eva"
           :files (:defaults "assets" "renv" "*.R" "*.gnuplot")))

;;(package! taxy
;;  :recipe (:host github
;;           :repo "alphapapa/taxy.el"
;;           :files (:defaults "deffy.el" "*.el")))

;; Not tried out

;; (package! org-timeline)
;; (package! embrace)
;; (package! pulseaudio-control)
;; (package! delve :recipe (:host github :repo "publicimageltd/delve"))
(package! hyperbole)
;; (package! suggest)
;; (package! osm)
;; (package! transmission)
;; (package! trashed)
(package! snitch)
;; (package! eshell-prompt-extras)
;; (package! navi-mode)
(package! snow)
;; (package! sx)

;; Minimalist modelines that merge with echo area
;; (package! maple-minibuffer :recipe (:host github :repo "honmaple/emacs-maple-minibuffer"))
;; (package! mini-modeline)
;; (package! feebleline)
(package! awesome-tray :recipe (:host github :repo "manateelazycat/awesome-tray"))
