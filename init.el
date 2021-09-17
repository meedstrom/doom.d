;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (vertico +icons)
       ;; (helm +icons +childframe)
       ;; (ivy +icons +childframe)
       company

       :ui
       doom
       hl-todo
       (ligatures +extra)
       vc-gutter

       ;; window-select ;; good on big monitors, otherwise no
       ;; (popup +defaults) ;; retarded org capture window size
       ;; (emoji +github +unicode)
       ;; unicode
       ;; deft
       ;; treemacs
       ;; hydra

       :editor
       ;; file-templates
       fold
       ;; multiple-cursors
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

       :tools
       docker
       rgb
       ;; biblio
       ;; (lookup +dictionary +offline)
       editorconfig
       eval
       lsp
       pdf
       debugger
       direnv
       ;; make

       :lang
       data
       emacs-lisp
       ess
       (go +lsp)
       json
       ledger
       markdown
       rst
       (org +dragndrop +pomodoro +journal +roam2 +noter)
       (scheme +guile)
       (sh +fish)
       yaml

       :email
       mu4e

       :app
       calendar
       (rss +org)
       ;; everywhere ;; try this outside exwm ... but doesnt work in sway

       :config
       (default +bindings +smartparens))
