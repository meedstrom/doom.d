;; -*- lexical-binding: t; -*-


(after! org
  (setq! org-babel-load-languages '((R . t) (emacs-lisp . t) (calc . t)
                                    (ditaa . t) (sqlite . t)))
  ;; Because I'm not using Doom's Org, I must do this myself
  ;; Adaptive LaTeX preview scale
  (my-change-latex-scale)
  (add-hook 'text-scale-mode-hook #'my-change-latex-scale))


;; Prettify code-snippets in exported pdf.
(after! ox-latex
  (csetq org-latex-listings t)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "booktabs")))



;; Bib stuff
(setq org-latex-pdf-process '("%latex -interaction nonstopmode -output-directory %o %f"
                              "%bib"
                              "%latex -interaction nonstopmode -output-directory %o %f"
                              "%latex -interaction nonstopmode -output-directory %o %f"))
(setq reftex-default-bibliography    '("/home/kept/Knowledge_base/references/library_biblatex.bib"))
(setq org-ref-default-bibliography   '("/home/kept/Knowledge_base/references/library_biblatex.bib"))
(setq org-ref-bibliography-notes       "/home/kept/Knowledge_base/references/notes.org")
(setq org-ref-pdf-directory            "/home/kept/Knowledge_base/references/")
(setq bibtex-completion-library-path '("/home/kept/Knowledge_base/references/"))
(setq bibtex-completion-notes-path     "/home/kept/Knowledge_base/references/notes.org")
(setq bibtex-completion-bibliography   "/home/kept/Knowledge_base/references/library_biblatex.bib")
(setq bibtex-files                   '("/home/kept/Knowledge_base/references/library_biblatex.bib"))
(setq bibtex-completion-pdf-field      "file")

(setq org-roam-directory "/home/kept/Knowledge_base/roam/")
(after! (company company-org-roam)
  (push 'company-org-roam company-backends))

(setq!
 org-ctrl-k-protect-subtree t
 org-datetree-add-timestamp t
 org-export-creator-string ""
 org-hide-emphasis-markers t ; hide the *, =, and / markers
 org-image-actual-width '(200) ; use #ATTR if available, else 300 px
 org-latex-compiler "xelatex"  ; allow unicode (åäö] in VERBATIM blocks
 org-latex-listings-options '(("basicstyle" "\\small")) ; small code font
 org-log-done 'time
 org-modules '(org-id org-bibtex org-info)
 org-log-into-drawer t ; hide spam
 org-pretty-entities t
 org-edit-src-content-indentation 0
 ;; org-replace-disputed-keys t
 org-use-speed-commands t
 org-ellipsis "⤵"
 org-insert-heading-respect-content t
 )

(setq my-org-prettify-alist '(("[" . "［")
                              ("]" . "］")
                              ("-" . "－")
                              ("=" . "＝")
                              ;; ("\\ne" . "＝")
                              ("+" . "＋")
                              ("\\vdots" . "︙")
                              ("\\implies" . "⟹")
                              ("\\sqrt" . "√")
                              ("\\ldots" . "…")))

(defun my-org-prettify ()
  (mapc (lambda (x) (push x prettify-symbols-alist))
        my-org-prettify-alist))

(add-hook 'org-mode-hook #'my-org-prettify)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'text-mode-hook (defun my-kill-smartparens () (smartparens-mode 0)))
(add-hook 'text-mode-hook
          (defun my-kill-fill ()
            (setq-default fill-column most-positive-fixnum)
            (setq-default adaptive-fill-mode nil)
            ))
