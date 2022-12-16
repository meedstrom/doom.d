;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       ;; (helm +childframe +icons)
       (vertico +icons)

       :ui
       doom
       ;; doom-dashboard
       ;; modeline
       hl-todo
       ;; (ligatures +extra)
       ligatures
       vc-gutter
       ;; window-select ;; good on big monitors, otherwise no
       ;; (popup +defaults) ;; retarded org capture window size
       ;; (emoji +github +unicode)

       :editor
       fold
       multiple-cursors
       ;; format ;; check if the upstream started using apheleia
       ;; snippets
       word-wrap

       :emacs
       ;;(dired +icons)
       dired
       vc

       :term
       vterm
       ;; eshell
       shell
       term

       :checkers
       syntax
       grammar

       :lang
       data
       emacs-lisp
       ess
       go
       javascript
       web
       ;; (go +tree-sitter)
       ;; (javascript +tree-sitter)
       ;; (web +tree-sitter)
       rust
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
       ;; (lsp +eglot)
       ;; tree-sitter
       pdf

       :config
       (default +bindings +smartparens)
       )
