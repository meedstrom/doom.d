
(package! disk-usage)
(package! docker-tramp)
;; (package! vterm :ignore t) ;; let guix install it
(package! org-roam)
(package! org-roam-server)
(package! org-roam-bibtex)
(package! org-ref)
(package! fish-mode)
(package! org-noter)
(package! bm)
(package! crux)
(package! el-patch)
;; (package! auto-compile)
(package! org-tanglesync)
(package! org-download)
(package! helm-bibtex)
(package! helm)
(package! ivy-bibtex)
(package! guix)
(package! stan-mode)
(package! academic-phrases)
(package! rainbow-blocks)
(package! company-org-roam)
(package! beancount :recipe (:host github :repo "cnsunyour/beancount.el"))
(package! artbollocks-mode)
(package! sicp)
(package! hyperbole)
(package! disable-mouse)
(package! eshell-z)
;; (package! exwm-mff)
(package! key-assist)
(package! backup-walker)
(package! cdlatex)
(package! literate-calc-mode)
(package! elcontext)
(package! prodigy)
(package! synosaurus)
(package! emacs-piper :recipe (:host gitlab :repo "howardabrams/emacs-piper"))
(package! vc-msg)
(package! git-messenger)
(package! feebleline)
(package! mini-modeline)
(package! unfill)
(package! hacker-typer)
(package! emms)
(package! doom-snippets :ignore t) ;; disable doom's yasnippets
;; (package! github-review :ignore t) ;; causes errors
(package! winner :ignore t)
(package! ts)
(package! org-pomodoro)
;; (package! unpackaged :recipe (:host github :repo "alphapapa/unpackaged.el"))
(package! navi-mode)
(package! helm-navi)
;; (package! delve)
(package! org-recent-headings)
;; (package! helm-org-recent-headings)
(package! org-drill)
(package! mediawiki)
(package! ctrlf)
(package! selectrum)
;(package! magit) ;; because magit-todos is B R O K E N
;(package! forge)
(package! deferred)
(package! concurrent)
(package! consult)
(package! marginalia)
(package! embark)
;; (package! mini-frame)
(package! wgrep)
(package! exwm)
(package! exwm-edit)
(package! exwm-firefox  :recipe (:host github :repo "ieure/exwm-firefox"))
;; (package! snow)
(package! sx)
(package! suggest)
(package! nameless)
(package! package-lint)
(package! named-timer)
(package! key-chord)
(package! fullframe)
(package! form-feed)
(package! org-journal)
(package! esup)
(package! rainbow-mode)
(package! hercules)
(package! objed)
(package! org-timeline)
(package! embrace)
(package! orderless)
(package! anki-editor)
(package! llama)
(package! subed :recipe (:host github :repo "rndusr/subed"))
(package! matrix-client)
(package! weechat)
(package! dmenu)
;; (package! mu4e-dashboard)
(package! mw-thesaurus)
(package! pfuture)
(package! ess_rproj :recipe (:host github :repo "chainsawriot/ess_rproj"))

;; Copypasta from https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/packages.el
(package! org
  :recipe (:host github
           ;; Install cutting-edge version of org, and from a mirror because
           ;; code.orgmode.org's uptime is worse than Github's, and
           ;; emacs-straight/org is smaller and, therefore, quicker to download.
           :repo "emacs-straight/org"
           :files (:defaults "etc")
           ;; HACK A necessary hack because org requires a compilation step
           ;;      after being cloned, and during that compilation a
           ;;      org-version.el is generated with these two functions, which
           ;;      return the output of a 'git describe ...' call in the repo's
           ;;      root. Of course, this command won't work in a sparse clone,
           ;;      and initiating these compilation step is a hassle, so...
           :build t
           :pre-build
           (with-temp-file "org-version.el"
             (insert "(defun org-release () \"9.5\")\n"
                     (format "(defun org-git-version (&rest _) \"9.5-%s\")\n"
                             (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
                     "(provide 'org-version)\n"))))
(package! org-contrib
  :recipe (:host nil :repo "https://git.sr.ht/~bzg/org-contrib"))
