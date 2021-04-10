;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
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
       dired
       vc

       :term
       ;; vterm ;; broken

       :checkers
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
       ;; (org +dragndrop +pomodoro +journal +pretty) ;; slowwww
       ;; org
       (sh +fish +lsp)
       yaml

       :email
       mu4e

       :app
       rss

       :config
       (default +bindings +smartparens))
