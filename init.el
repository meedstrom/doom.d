;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (vertico +icons)

       :ui
       doom
       ;; doom-dashboard
       ;; modeline
       hl-todo
       (ligatures +extra)
       vc-gutter
       ;; window-select ;; good on big monitors, otherwise no
       ;; (popup +defaults) ;; retarded org capture window size
       ;; (emoji +github +unicode)

       :editor
       fold
       multiple-cursors
       format
       snippets
       word-wrap

       :emacs
       (dired +icons)
       vc

       :term
       vterm
       eshell
       shell
       term

       :checkers
       syntax
       grammar

       :lang
       data
       emacs-lisp
       ess
       (go +lsp)
       json
       ledger
       markdown
       (org +dragndrop +pomodoro +roam2 +pretty)
       (scheme +guile)
       (sh +fish)
       yaml

       :email
       mu4e

       :app
       calendar
       (rss +org)
       ;; everywhere ;; try this if im on neither exwm nor sway

       :tools
       biblio
       (magit +forge)
       (lsp +eglot)
       tree-sitter

       :config
       (default +bindings +smartparens)
       )
