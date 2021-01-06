;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       company

       :ui
       doom
       doom-dashboard
       hl-todo
       (ligatures +extra)

       :editor
       file-templates
       fold
       format
       snippets
       word-wrap

       :emacs
       dired
       vc

       :term
       eshell

       :checkers
       grammar

       :tools
       docker
       editorconfig
       eval
       lsp
       magit
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
       ;; (org +dragndrop +pomodoro +journal +pretty) ;; slowwww
       (sh +fish +lsp)
       yaml

       :email
       mu4e

       :config
       (default +bindings +smartparens))
