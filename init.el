;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       ;; (helm +childframe +icons)
       (vertico +icons)
       ;; (vertico +childframe +icons)
       ;; company

       :ui
       doom
       unicode
       (emoji +unicode)
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
       ;; fold
       multiple-cursors
       ;; snippets
       ;; format ;; check if the upstream started using apheleia
       word-wrap

       :emacs
       dired
       vc

       ;; :term
       ;; vterm
       ;; eshell
       shell
       ;; term

       ;; :checkers
       ;; syntax
       grammar
       ;; spell

       :lang
       emacs-lisp
       data
       ess
       nix
       (go +tree-sitter)
       (web +tree-sitter)
       (javascript +tree-sitter)
       ;; javascript
       rust
       json
       ledger
       markdown
       ;; (org +dragndrop +pomodoro +roam2 +pretty)
       ;; (org +roam2)
       (scheme +guile)
       (sh +fish)
       yaml

       :email
       mu4e

       :tools
       ;; biblio
       (magit +forge)
       (lsp +eglot)
       tree-sitter
       pdf

       :config
       (default +bindings +smartparens))
