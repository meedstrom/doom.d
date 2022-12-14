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
(package! package-lint)
(package! buttercup)
(package! nameless)
(package! gif-screencast)
(package! screencast)
(package! keymap-utils) ;; prefix kmu-*

;; The rest

(package! circadian)
(package! academic-phrases)
;; (package! affe)
(package! apheleia)
(package! smart-tabs-mode)
(package! anki-editor)
(package! app-launcher :recipe (:host github :repo "SebastienWae/app-launcher"))
(package! artbollocks-mode)
(package! backup-walker)
(package! bm)
(package! calibredb)
(package! cape)
(package! corfu)
(package! embark)
(package! crux)
(package! cycle-buffer) ;; vs iflipb?
(package! define-repeat-map :recipe (:host nil :repo "https://tildegit.org/acdw/define-repeat-map.el"))
(package! director)
(package! dired-hacks)
(package! disable-mouse)
(package! disk-usage)
(package! dmenu)
(package! doom-snippets :ignore t) ;; disable doom's yasnippets
(package! elisp-format)
(package! emacs-piper :recipe (:host gitlab :repo "howardabrams/emacs-piper"))
(package! ess_rproj :recipe (:host github :repo "chainsawriot/ess_rproj"))
(package! esup)
(package! asyncloop :recipe (:host github :repo "meedstrom/asyncloop"))
(package! deianira :recipe (:host github :repo "meedstrom/deianira"))
(package! kbd-mode :recipe (:host github :repo "kmonad/kbd-mode"))
(package! exwm)
(package! exwm-edit)
(package! exwm-firefox :recipe (:host github :repo "ieure/exwm-firefox"))
(package! form-feed)
(package! gif-screencast)
(package! git-messenger)
(package! goggles)
(package! golden-ratio)
(package! hacker-typer)
(package! helm-selector)
(package! help-find)
(package! iedit)
(package! iflipb) ;; vs cycle-buffer?
(package! iscroll)
(package! key-assist)
(package! key-chord)
(package! keyfreq)
(package! mediawiki)
(package! mw-thesaurus)
(package! nov)
(package! objed) ;; for objed-ipipe
(package! org-drill)
(package! org-recent-headings)
(package! org-roam-bibtex) ;; yes still relevant
(package! org-roam-ui)
(package! org-transclusion)
(package! org-tanglesync)
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
(package! ctrlf :recipe (:host github :repo "radian-software/ctrlf"))
(package! dired-hist :recipe (:host github :repo "karthink/dired-hist"))
;; (package! popper)
;; (package! beancount :recipe (:host github :repo "cnsunyour/beancount.el"))
;; (package! feebleline)
;; (package! forge)
;; (package! magit)
;; (package! mini-modeline)
;; (package! org-fc :recipe (:repo "https://git.sr.ht/~l3kn/org-fc"))
;; (package! repeaters :recipe (:host github :repo "mmarshall540/repeaters"))
;; (package! xah-elisp-mode) ;; try xah-elisp-prettify-root-sexp
;; (package! subed :recipe (:host github :repo "rndusr/subed"))
;; (package! cycle-region)
;; (package! jammer)
;; (package! frames-only-mode)
;; (package! mini-frame)
;; (package! maple-minibuffer :recipe (:host github :repo "honmaple/emacs-maple-minibuffer"))
;; (package! frame-mode)
;; (package! cdlatex)
;; (package! docker-tramp)
;; (package! emms)
;; (package! fullframe)
(package! github-review :ignore t) ;; causes errors
;; (package! good-scroll)
;; (package! helm-bibtex)
;; (package! helm-navi)
;; (package! helm-org-recent-headings)
;; (package! literate-calc-mode)
;; (package! matrix-client)
;; (package! mini-frame)
;; (package! mu4e-dashboard)
;; (package! prodigy)
;; (package! unpackaged :recipe (:host github :repo "alphapapa/unpackaged.el"))
;; (package! weechat)

(package! eva
  :recipe (:host github
           :repo "meedstrom/eva"
           :files (:defaults "assets" "renv" "*.R" "*.gnuplot")))

(package! taxy
  :recipe (:host github
           :repo "alphapapa/taxy.el"
           :files (:defaults "deffy.el" "*.el")))

;; Not tried out

;; (package! org-timeline)
;; (package! embrace)
;; (package! pulseaudio-control)
(package! delve :recipe (:host github :repo "publicimageltd/delve"))
;; (package! hyperbole)
;; (package! suggest)
;; (package! sly)
;; (package! osm)
;; (package! transmission)
;; (package! trashed)
(package! snitch)
;; (package! eshell-prompt-extras)
;; (package! navi-mode)
;; (package! snow)
;; (package! sx)
