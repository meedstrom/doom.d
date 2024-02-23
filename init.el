;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (vertico +icons)
       company

       :ui
       (vc-gutter +diff-hl +pretty)
       hl-todo
       ligatures

       :emacs
       dired

       :term
       shell

       :lang
       emacs-lisp
       data
       (ess +tree-sitter +stan)
       (go +tree-sitter)
       (web +tree-sitter)
       (javascript +tree-sitter)
       json
       (org +dragndrop +roam2)
       (sh +fish)

       :tools
       tree-sitter
       (magit +forge)
       pdf

       :app
       irc
       emms
       rss

       :email mu4e
       :config (default +bindings +smartparens))
