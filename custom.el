(put 'customize-themes 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'customize-variable 'disabled nil)
(put 'projectile-grep 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'customize-changed 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'customize-set-variable 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'View-quit 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apheleia-formatters
   '((ess-r "R" "-qs" "--no-save" "--no-restore" "-e" "styler::style_text(readLines(file('stdin')))")
     (astyle "astyle"
      (apheleia-formatters-locate-file "--options" ".astylerc"))
     (asmfmt "asmfmt")
     (bean-format "bean-format")
     (beautysh "beautysh"
      (apheleia-formatters-indent "--tab" "--indent-size" 'sh-basic-offset)
      "-")
     (black "black"
      (when
          (apheleia-formatters-extension-p "pyi")
        "--pyi")
      (apheleia-formatters-fill-column "--line-length")
      "-")
     (brittany "brittany")
     (buildifier "buildifier")
     (caddyfmt "caddy" "fmt" "-")
     (clang-format "clang-format" "-assume-filename"
      (or
       (apheleia-formatters-local-buffer-file-name)
       (apheleia-formatters-mode-extension)
       ".c"))
     (cljfmt "cljfmt" "fix" "-")
     (cmake-format "cmake-format" "-")
     (crystal-tool-format "crystal" "tool" "format" "-")
     (css-beautify "css-beautify" "--file" "-" "--end-with-newline"
      (apheleia-formatters-indent "--indent-with-tabs" "--indent-size"))
     (dart-format "dart" "format")
     (denofmt "deno" "fmt" "-")
     (denofmt-js "deno" "fmt" "-" "--ext" "js")
     (denofmt-json "deno" "fmt" "-" "--ext" "json")
     (denofmt-jsonc "deno" "fmt" "-" "--ext" "jsonc")
     (denofmt-jsx "deno" "fmt" "-" "--ext" "jsx")
     (denofmt-md "deno" "fmt" "-" "--ext" "md")
     (denofmt-ts "deno" "fmt" "-" "--ext" "ts")
     (denofmt-tsx "deno" "fmt" "-" "--ext" "tsx")
     (docformatter "apheleia-docformatter" inplace)
     (dprint "dprint" "fmt" "--stdin" filepath)
     (elm-format "elm-format" "--yes" "--stdin")
     (fish-indent "fish_indent")
     (fourmolu "fourmolu")
     (gawk "gawk" "-f" "-" "--pretty-print=-")
     (gofmt "gofmt")
     (gofumpt "gofumpt")
     (goimports "goimports")
     (google-java-format "google-java-format" "-")
     (hclfmt "hclfmt")
     (html-beautify "html-beautify" "--file" "-" "--end-with-newline"
      (apheleia-formatters-indent "--indent-with-tabs" "--indent-size"))
     (html-tidy "tidy" "--quiet" "yes" "--tidy-mark" "no" "--vertical-space" "yes" "-indent"
      (when
          (derived-mode-p 'nxml-mode)
        "-xml")
      (apheleia-formatters-indent "--indent-with-tabs" "--indent-spaces")
      (apheleia-formatters-fill-column "-wrap"))
     (isort "isort" "-")
     (js-beautify "js-beautify" "--file" "-" "--end-with-newline"
      (apheleia-formatters-indent "--indent-with-tabs" "--indent-size"))
     (jq "jq" "." "-M"
      (apheleia-formatters-indent "--tab" "--indent"))
     (lisp-indent . apheleia-indent-lisp-buffer)
     (ktlint "ktlint" "--log-level=none" "--stdin" "-F" "-")
     (latexindent "latexindent" "--logfile=/dev/null")
     (mix-format "apheleia-from-project-root" ".formatter.exs" "mix" "format" "-")
     (nixfmt "nixfmt")
     (ocamlformat "ocamlformat" "-" "--name" filepath "--enable-outside-detected-project")
     (ormolu "ormolu")
     (perltidy "perltidy" "--quiet" "--standard-error-output"
      (apheleia-formatters-indent "-t" "-i")
      (apheleia-formatters-fill-column "-l"))
     (pgformatter "pg_format"
      (apheleia-formatters-indent "--tabs" "--spaces" 'tab-width)
      (apheleia-formatters-fill-column "--wrap-limit"))
     (phpcs "apheleia-phpcs")
     (prettier "apheleia-npx" "prettier" "--stdin-filepath" filepath
      (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))
     (prettier-css "apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=css"
      (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))
     (prettier-html "apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=html"
      (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))
     (prettier-graphql "apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=graphql"
      (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))
     (prettier-javascript "apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=babel-flow"
      (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))
     (prettier-json "apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=json"
      (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))
     (prettier-markdown "apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=markdown"
      (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))
     (prettier-ruby "apheleia-npx" "prettier" "--stdin-filepath" filepath "--plugin=@prettier/plugin-ruby" "--parser=ruby"
      (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))
     (prettier-scss "apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=scss"
      (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))
     (prettier-svelte "apheleia-npx" "prettier" "--stdin-filepath" filepath "--plugin=prettier-plugin-svelte" "--parser=svelte"
      (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))
     (prettier-typescript "apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=typescript"
      (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))
     (prettier-yaml "apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=yaml"
      (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))
     (purs-tidy "apheleia-npx" "purs-tidy" "format")
     (pyang "pyang" "--ignore-errors" "-f" "yang")
     (robotidy "robotidy" "--no-color" "-"
      (apheleia-formatters-indent nil "--indent")
      (apheleia-formatters-fill-column "--line-length"))
     (python3-json "python3" "-m" "json.tool"
      (apheleia-formatters-indent "--tab" "--indent"))
     (rubocop "rubocop" "--stdin" filepath "--auto-correct" "--stderr" "--format" "quiet" "--fail-level" "fatal")
     (ruby-standard "standardrb" "--stdin" filepath "--fix" "--stderr" "--format" "quiet" "--fail-level" "fatal")
     (ruby-syntax-tree "apheleia-from-project-root" ".streerc" "stree" "format" filepath)
     (ruff "ruff" "format" "--silent"
      (apheleia-formatters-fill-column "--line-length")
      "--stdin-filename" filepath "-")
     (ruff-isort "ruff" "check" "-n" "--select" "I" "--fix" "--fix-only" "--stdin-filename" filepath "-")
     (shfmt "shfmt" "-filename" filepath "-ln"
      (cl-case
          (bound-and-true-p sh-shell)
        (sh "posix")
        (t "bash"))
      (when apheleia-formatters-respect-indent-level
        (list "-i"
              (number-to-string
               (cond
                (indent-tabs-mode 0)
                ((boundp 'sh-basic-offset)
                 sh-basic-offset)
                (t 4)))))
      "-")
     (rufo "rufo" "--filename" filepath "--simple-exit")
     (stylua "stylua" "-")
     (rustfmt "rustfmt" "--quiet" "--emit" "stdout")
     (terraform "terraform" "fmt" "-")
     (treefmt "treefmt" "--stdin" filepath)
     (xmllint "xmllint" "--format" "-")
     (yapf "yapf")
     (yq-csv "yq" "--prettyPrint" "--no-colors" "--input-format" "csv" "--output-format" "csv")
     (yq-json "yq" "--prettyPrint" "--no-colors" "--input-format" "json" "--output-format" "json"
      (apheleia-formatters-indent nil "--indent"))
     (yq-properties "yq" "--prettyPrint" "--no-colors" "--input-format" "props" "--output-format" "props")
     (yq-tsv "yq" "--prettyPrint" "--no-colors" "--input-format" "tsv" "--output-format" "tsv")
     (yq-xml "yq" "--prettyPrint" "--no-colors" "--input-format" "xml" "--output-format" "xml"
      (apheleia-formatters-indent nil "--indent"))
     (yq-yaml "yq" "--prettyPrint" "--no-colors" "--no-doc" "--input-format" "yaml" "--output-format" "yaml"
      (apheleia-formatters-indent nil "--indent"))))
 '(elfeed-feeds
   '("https://edstrom.dev/posts.atom" "https://edstrom.dev/feed"))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-agenda-files
   '("/home/sync-phone/beorg/may-do.org" "/home/kept/roam/noagenda/archive.org" "/home/kept/roam/grismartin/pages/agreements-about-the-relationship.org" "/home/kept/roam/grismartin/pages/april-break.org" "/home/kept/roam/grismartin/pages/camping-prep-list.org" "/home/kept/roam/grismartin/pages/certificates-etc.org" "/home/kept/roam/grismartin/pages/commitments-martin.org" "/home/kept/roam/grismartin/pages/contents.org" "/home/kept/roam/grismartin/pages/conversations-we-had.org" "/home/kept/roam/grismartin/pages/expectations-of-each-other.org" "/home/kept/roam/grismartin/pages/how-to-use.org" "/home/kept/roam/grismartin/pages/job-cover-letters.org" "/home/kept/roam/grismartin/pages/martins-ADHD.org" "/home/kept/roam/grismartin/pages/may-do.org" "/home/kept/roam/grismartin/pages/measures-for-a-healthy-lifestyle.org" "/home/kept/roam/grismartin/pages/movies-we-want-to-watch.org" "/home/kept/roam/grismartin/pages/our-bucket-list.org" "/home/kept/roam/grismartin/pages/our-history.org" "/home/kept/roam/grismartin/pages/our-wishlists.org" "/home/kept/roam/grismartin/pages/reassurance-list.org" "/home/kept/roam/grismartin/pages/shopping-list.org" "/home/kept/roam/grismartin/pages/situation-checklist.org" "/home/kept/roam/grismartin/pages/situation-tap.org" "/home/kept/roam/grismartin/pages/summer-trip.org" "/home/kept/roam/grismartin/pages/tea-date-standard-preamble.org" "/home/kept/roam/grismartin/pages/tea-date-topics.org" "/home/kept/roam/grismartin/pages/things-that-hurt-griselda.org" "/home/kept/roam/grismartin/pages/what-we-can-work-on.org" "/home/me/.doom.d/elfeed.org"))
 '(org-fold-core-style 'overlays)
 '(safe-local-variable-values
   '((coding-system . utf-8-unix)
     (nameless-current-name . "my")
     (org-confirm-babel-evaluate)
     (org-refile-targets quote
      (("/home/kept/roam/noagenda/2021-08-27-somedaymaybe.org" :maxlevel . 3)))
     (org-drill-scope . directory)
     (require-final-newline . t)
     (require-final-newline))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
