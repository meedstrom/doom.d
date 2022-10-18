;; -*- no-byte-compile: t; -*-
;; Need for init
(package! named-timer)
(package! l) ;; anonymous fn literal: (l'concat %) is (lambda (%) (concat %))
(package! defrepeater)
(package! el-patch)

;; Pkg dev stuff
(package! pfuture)
;; (package! concurrent)
;; (package! deferred)
(package! ts)
(package! package-lint)
(package! buttercup)
(package! nameless)
(package! gif-screencast)
(package! screencast)
;; (package! loopy)
;; (package! loopy-dash)
;; (package! aio)
(package! keymap-utils) ;; prefix kmu-*

;; Should be mainlined imo
(package! form-feed)
(package! prism)
(package! goggles)

;; The rest
(package! doom-snippets :ignore t) ;; disable doom's yasnippets
(package! help-find)
(package! shelldon)
(package! transmission)
(package! affe)
(package! iedit)
(package! osm)
(package! visual-regexp)
(package! helm-selector)
(package! sly)
(package! trashed)
(package! popper)
(package! eshell-prompt-extras)
(package! golden-ratio)
(package! academic-phrases)
(package! vimgolf)
(package! anki-editor)
(package! artbollocks-mode)
(package! backup-walker)
(package! bm)
(package! nov)
(package! calibredb)
(package! crux)
(package! director)
(package! dired-hacks)
(package! peep-dired)
(package! disable-mouse)
(package! disk-usage)
(package! dmenu)
;; (package! magit)
;; (package! forge)
(package! embrace)
(package! esup)
(package! fish-mode)
(package! gif-screencast)
(package! git-messenger)
(package! hacker-typer)
(package! hyperbole)
(package! iscroll)
(package! key-assist)
(package! key-chord)
(package! keyfreq)
(package! mediawiki)
(package! mw-thesaurus)
(package! objed) ;; just for objed-ipipe
(package! org-drill)
(package! org-recent-headings)
(package! org-roam-ui)
(package! org-roam-bibtex) ;; yes?
;; (package! citar-org-roam) ;; not needed
(package! org-tanglesync)
;; (package! org-ref)
(package! org-timeline)
(package! elisp-format)
(package! pulseaudio-control)
(package! sicp)
(package! snitch)
(package! stan-mode)
(package! suggest)
(package! vc-msg)
(package! wgrep)
(package! feebleline)
(package! mini-modeline)
(package! corfu)
(package! cape)
(package! xr)
;; (package! ctrlf :recipe (:host github :repo "radian-software/ctrlf"))
;; (package! ctrlf)
(package! sway)
(package! cycle-buffer)
(package! define-repeat-map :recipe (:host nil :repo "https://tildegit.org/acdw/define-repeat-map.el"))
(package! app-launcher :recipe (:host github :repo "SebastienWae/app-launcher"))
(package! beancount :recipe (:host github :repo "cnsunyour/beancount.el"))
(package! emacs-piper :recipe (:host gitlab :repo "howardabrams/emacs-piper"))
(package! ess_rproj :recipe (:host github :repo "chainsawriot/ess_rproj"))
(package! org-fc :recipe (:repo "https://git.sr.ht/~l3kn/org-fc"))
(package! aweshell :recipe (:host github :repo "manateelazycat/aweshell"))
(package! delve :recipe (:host github :repo "publicimageltd/delve"))
(package! repeaters :recipe (:host github :repo "mmarshall540/repeaters"))
(package! eva
  :recipe (:host github
           :repo "meedstrom/eva"
           :files (:defaults "assets" "renv" "*.R" "*.gnuplot")))
(package! taxy
  :recipe (:host github
           :repo "alphapapa/taxy.el"
           :files (:defaults "deffy.el" "*.el")))

;; (package! xah-elisp-mode) ;; try xah-elisp-prettify-root-sexp
(package! exwm)
(package! exwm-edit)
(package! exwm-firefox :recipe (:host github :repo "ieure/exwm-firefox"))
;; (package! subed :recipe (:host github :repo "rndusr/subed"))
;; (package! cycle-region)
;; (package! jammer)
;; (package! frames-only-mode)
;; (package! mini-frame)
;; (package! maple-minibuffer :recipe (:host github :repo "honmaple/emacs-maple-minibuffer"))
;; (package! frame-mode)

;; Doom provides
;; (package! rainbow-mode)
;; (package! synosaurus)
;; (package! org-download)
;; (package! org-journal)
;; (package! org-noter)
;; (package! org-pomodoro)
;; (package! org-roam)
;; (package! citar)
;; (package! eshell-z)

;; Not tried out
;; (package! navi-mode)
;; (package! snow)
;; (package! sx)

;; (package! cdlatex)
;; (package! docker-tramp)
;; (package! emms)
;; (package! forge)
;; (package! fullframe)
;; (package! github-review :ignore t) ;; causes errors
;; (package! good-scroll)
;; (package! helm-bibtex)
;; (package! helm-navi)
;; (package! helm-org-recent-headings)
;; (package! literate-calc-mode)
;; (package! magit) ;; because magit-todos is B R O K E N
;; (package! matrix-client)
;; (package! mini-frame)
;; (package! mu4e-dashboard)
;; (package! prodigy)
;; (package! unpackaged :recipe (:host github :repo "alphapapa/unpackaged.el"))
;; (package! vterm :ignore t) ;; let guix install it
;; (package! weechat)

;; Copypasta from https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/packages.el
;; (package! org
;;   :recipe (:host github
;;            ;; Install cutting-edge version of org, and from a mirror because
;;            ;; code.orgmode.org's uptime is worse than Github's, and
;;            ;; emacs-straight/org is smaller and, therefore, quicker to download.
;;            :repo "emacs-straight/org"
;;            :files (:defaults "etc")
;;            ;; HACK A necessary hack because org requires a compilation step
;;            ;;      after being cloned, and during that compilation a
;;            ;;      org-version.el is generated with these two functions, which
;;            ;;      return the output of a 'git describe ...' call in the repo's
;;            ;;      root. Of course, this command won't work in a sparse clone,
;;            ;;      and initiating these compilation step is a hassle, so...
;;            :build t
;;            :pre-build
;;            (with-temp-file "org-version.el"
;;              (insert "(defun org-release () \"9.5\")\n"
;;                      (format "(defun org-git-version (&rest _) \"9.5-%s\")\n"
;;                              (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
;;                      "(provide 'org-version)\n"))))
;; (package! org-contrib
;;   :recipe (:host nil :repo "https://git.sr.ht/~bzg/org-contrib"))

