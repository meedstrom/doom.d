;; Need for init
(package! named-timer)
(package! l) ;; anon fn literal: (l'foo %)
(package! defrepeater)
(package! el-patch)

;; Pkg dev stuff
(package! pfuture)
(package! concurrent)
(package! deferred)
(package! ts)
(package! package-lint)
(package! buttercup)
(package! nameless)

;; Should be mainlined
(package! form-feed)
(package! prism)


(package! academic-phrases)
(package! anki-editor)
(package! artbollocks-mode)
(package! backup-walker)
(package! beancount :recipe (:host github :repo "cnsunyour/beancount.el"))
(package! bm)
(package! calibredb)
(package! crux)
(package! director)
(package! dired-hacks)
(package! disable-mouse)
(package! disk-usage)
(package! dmenu)
(package! doom-snippets :ignore t) ;; disable doom's yasnippets
;; (package! magit-todos :ignore t) ;; loads org on init, so no thanks
(package! magit)
(package! forge)
(package! emacs-piper :recipe (:host gitlab :repo "howardabrams/emacs-piper"))
(package! embrace)
(package! eshell-z)
(package! ess_rproj :recipe (:host github :repo "chainsawriot/ess_rproj"))
(package! esup)
(package! eva :recipe (:host github :repo "meedstrom/eva"
                       :files (:defaults "assets" "renv" "*.R" "*.gnuplot")))
(package! exwm)
(package! exwm-edit)
(package! exwm-firefox  :recipe (:host github :repo "ieure/exwm-firefox"))
(package! fish-mode)
(package! gif-screencast)
(package! git-messenger)
(package! guix)
(package! hacker-typer)
(package! hyperbole)
(package! iscroll)
(package! key-assist)
(package! key-chord)
(package! keyfreq)
(package! mediawiki)
(package! mw-thesaurus)
(package! objed)
(package! org-drill)
(package! org-recent-headings)
(package! org-ref)
;; (package! consult-bibtex)
(package! org-roam-bibtex)
(package! org-tanglesync)
(package! org-timeline)
(package! websocket) ;; needed for org-roam-ui
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui"
                               :files ("*.el" "out")))
(package! rainbow-mode)
(package! sicp)
(package! snitch)
(package! stan-mode)
(package! subed :recipe (:host github :repo "rndusr/subed"))
(package! suggest)
(package! synosaurus)
(package! vc-msg)
(package! wgrep)
(package! winner :ignore t)

;; Doom provides
;; (package! org-download)
;; (package! org-journal)
;; (package! org-noter)
;; (package! org-pomodoro)
;; (package! org-roam)


;; Not tried out
;; (package! delve) ;; not yet roam v2
;; (package! elcontext)
;; (package! navi-mode)
;; (package! snow)
;; (package! sx)

;; (package! unfill)
;; (package! ctrlf)
;; (package! cdlatex)
;; (package! docker-tramp)
;; (package! emms)
;; (package! feebleline) ;; bug
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
;; (package! mini-modeline) ;; intensive repeating timer :/
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
