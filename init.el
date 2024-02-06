;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       ;; (helm +icons)
       (vertico +icons)
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
       ;; (popup +defaults) ;; terrible org capture window size...

       :editor
       ;; fold
       multiple-cursors
       ;; snippets
       ;; format ;; check if the upstream started using apheleia
       word-wrap

       :emacs
       dired
       vc

       :term
       ;; vterm
       ;; eshell
       shell
       ;; term

       :checkers
       ;; syntax
       grammar
       ;; spell

       :lang
       emacs-lisp
       common-lisp
       data
       ess
       nix
       (go +tree-sitter +lsp)
       (web +tree-sitter +lsp)
       (javascript +tree-sitter +lsp)
       ;; javascript
       rust
       json
       ledger
       ;; markdown
       ;; (org +dragndrop +pomodoro +roam2 +pretty)
       (org +roam2)
       ;; (scheme +guile)
       (sh +fish)
       yaml

       :email
       ;; mu4e

       :tools
       ;; biblio
       (magit +forge)
       (lsp +eglot)
       tree-sitter
       pdf

       :config
       (default +bindings +smartparens))
