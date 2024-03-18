;;; init.el -*- lexical-binding: t; -*-

(doom! :completion (vertico +icons)
       :emacs dired
       :term shell
       :checkers spell
       :app irc
       :email mu4e
       :config (default +bindings +smartparens)

       :ui
       (vc-gutter +diff-hl +pretty)
       hl-todo
       ligatures

       :lang
       emacs-lisp
       data
       (ess +tree-sitter +stan +lsp)
       (go +tree-sitter)
       (web +tree-sitter)
       (javascript +tree-sitter)
       json
       ledger
       (sh +fish)

       :tools
       tree-sitter
       editorconfig
       lsp
       (magit +forge)
       pdf)
