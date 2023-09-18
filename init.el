;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       ;; (helm +childframe +icons)
       (vertico +icons)
       ;; (vertico +childframe +icons)
       company

       :ui
       doom
       unicode
       (emoji +unicode)
       ;; doom-dashboard
       modeline
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
       ;; snippets
       ;; format ;; check if the upstream started using apheleia
       word-wrap

       :emacs
       dired
       vc

       :term
       vterm
       ;; eshell
       shell
       term

       :checkers
       ;; syntax
       grammar
       ;; spell

       :lang
       data
       emacs-lisp
       ess
       (go +lsp +tree-sitter)
       web
       nix
       ;; (web +tree-sitter)
       ;; javascript
       (javascript +lsp +tree-sitter)   ;; tsserver freezes Emacs, maybe edit tide-server-max-response-length
       rust
       json
       ledger
       markdown
       ;; (org +dragndrop +pomodoro +roam2 +pretty)
       (scheme +guile)
       (sh +fish)
       yaml

       :email
       mu4e

       :app
       calendar
       (rss +org)
       ;; everywhere ;; try this if im on neither exwm nor wayland

       :tools
       biblio
       (magit +forge)
       (lsp +eglot)
       ;; lsp
       tree-sitter
       pdf

       :config
       (default +bindings +smartparens)
       )
