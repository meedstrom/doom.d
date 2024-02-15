;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (vertico +icons)
       ;; (helm +icons)
       company

       :ui
       (vc-gutter +diff-hl +pretty)
       hl-todo
       ligatures

       :emacs
       dired

       :lang
       emacs-lisp
       common-lisp
       data
       (ess +tree-sitter +stan)
       nix
       (go +tree-sitter)
       (web +tree-sitter)
       (javascript +tree-sitter)
       rust
       json
       ledger
       (org +dragndrop +roam2)
       (sh +fish)
       (yaml +tree-sitter)

       :tools
       tree-sitter
       (magit +forge)
       pdf

       :config
       (default +bindings +smartparens))
