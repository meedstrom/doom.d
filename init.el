;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       ;; (helm +icons)
       (vertico +icons)
       ;; company

       :ui
       doom
       vc-gutter
       hl-todo
       ;; unicode
       ;; (emoji +unicode)
       ;; doom-dashboard
       ;; modeline
       ;; (ligatures +extra)
       ligatures
       ;; window-select ;; good on big monitors, otherwise no
       ;; (popup +defaults) ;; terrible org capture window size...

       :editor
       ;; fold
       ;; multiple-cursors
       ;; snippets
       ;; format ;; check if the upstream started using apheleia
       ;; word-wrap

       :emacs
       dired
       ;; vc

       :term
       ;; vterm
       ;; eshell
       ;; shell
       ;; term

       :checkers
       ;; syntax
       ;; grammar
       ;; spell

       :lang
       emacs-lisp
       common-lisp
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
       ;; markdown
       (org +dragndrop +roam2) ;; gotta fix the way it affects `my-publish'
       ;; (scheme +guile)
       (sh +fish)
       yaml

       :email
       ;; mu4e

       :tools
       ;; biblio
       (magit +forge)
       ;; (lsp +eglot)
       tree-sitter
       pdf

       :config
       (default +bindings +smartparens))
