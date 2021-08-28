;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (vertico +icons)
       company

       :ui
       doom
       hl-todo
       (ligatures +extra)

       :editor
       ;; file-templates
       fold
       format
       snippets
       word-wrap

       :emacs
       (dired +icons)
       vc

       :term
       ;; vterm ;; broken

       :checkers
       syntax
       grammar

       :tools
       docker
       editorconfig
       eval
       lsp
       ;; (magit +forge)
       pdf

       :lang
       data
       emacs-lisp
       ess
       (go +lsp)
       json
       ledger
       markdown
       rst
       (org +dragndrop +pomodoro +journal +roam2 +noter) ;; slowwww
       (scheme +guile)
       (sh +fish +lsp)
       yaml

       :email
       mu4e

       :app
       (rss +org)

       :config
       (default +bindings +smartparens))
