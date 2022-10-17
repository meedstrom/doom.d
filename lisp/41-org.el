;; -*- lexical-binding: t; -*-

(require 'my-lib-external)

;; undoom
(after! org
  (setc org-todo-keywords '((sequence "TODO" "DONE"))))

;; (after! org-journal
;;   ;; Workaround https://github.com/bastibe/org-journal/issues/298: open in
;;   ;; org-mode, not org-journal-mode (weird paragraph formatting)
;;   ;; (defun org-journal-is-journal () nil)
;;   )

;; ;; Integrate org-journal with roam dailies.  I don't want to use capture
;; ;; functions for the dailies, it's a weird fit, better to use the org-journal
;; ;; commands.
;; (setc org-journal-dir "/home/kept/roam/daily")
;; (setc org-journal-file-format "%F.org")
;; (setc org-journal-file-header
;;       (lambda (_time)
;;         (concat ":PROPERTIES:\n:ID:       " (org-id-uuid) "\n:END:" )))
;; ;; (setc org-journal-file-header (format ":PROPERTIES:\n:ID:%s\n:END:" (org-id-uuid)))
;; (setc org-journal-date-prefix "#+title: ")
;; (setc org-journal-date-format "[%F]")
;; (setc org-journal-time-prefix "* ")

;; TODO insert a front matter in my blog posts with title from #+TITLE
(defun my-md-filter-thingy (plist backend)
  "For org-export-filter-options-functions."
  (if (eq backend 'md)
      (let ((title (plist-get plist :title)))
        (when title
          (message "yes, TITLE option is among the plist")
          ;; we probably need a different hook
          (goto-char (point-min))
          (insert "---")
          (insert "title: " title)
          (insert "---")))
    )
  plist)

(defun my-md-export-hook (backend)
  (when (eq backend 'md)
    (let ((title (save-excursion
                   (goto-char (point-min))
                   (when (search-forward "#+title: " nil t)
                     (buffer-substring (point) (line-end-position)))))
          (planted-date (save-excursion
                   (goto-char (point-min))
                   (when (search-forward "#+date: " nil t)
                     (buffer-substring (point) (line-end-position))))))
      (when title
        (save-excursion
          (goto-char (point-min))
          (re-search-forward (rx bol (not (any "#" ":"))))
          (if (save-excursion (re-search-backward (rx bol "#+begin") (point-min) t))
              (message "my-md-export-hook: not sure where I am, so not messing with output")
            (goto-char (line-beginning-position))
            (open-line 1)
            (insert "#+BEGIN_EXPORT md")
            (newline)
            (insert "---")
            (newline)
            (insert "title: " title)
            (newline)
            (insert "date: " (format-time-string "%F"))
            (newline)
            (insert "---")
            (newline)
            (insert "#+END_EXPORT")
            (when planted-date
              (insert "Planted " planted-date))
            (newline)))))))

(remove-hook 'org-export-before-parsing-hook #'my-md-export-hook)
(add-hook 'org-export-before-processing-hook #'my-md-export-hook)

;; (after! ox
  ;; (add-to-list 'org-export-filter-options-functions #'my-md-filter-thingy))

;; Roam
(after! org-roam-node
  ;; Override the slug to use hyphens rather than underscores
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s) (string-glyph-compose
                                              (apply #'string
                                                     (seq-remove #'nonspacing-mark-p
                                                                 (string-glyph-decompose s)))))
                 (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
                        ("--*" . "-")                   ;; remove sequential dashes
                        ("^-" . "")                     ;; remove starting dash
                        ("-$" . "")))                   ;; remove ending dash
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (downcase slug))))))

(add-hook 'org-roam-buffer-postrender-functions #'magit-section-show-level-2)
(setc org-roam-directory "/home/kept/roam/")
(setc org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%H:%M>\n%?" :if-new
         (file+head "%<%Y-%m-%d>.org" "#+title: [%<%Y-%m-%d>]\n#+filetags: :personal:\n")
         :immediate-finish t
         :jump-to-captured t)))

;; (setc org-roam-dailies-capture-templates
;;       '(("d" "default" entry "* %<%H:%M>\n%?" :if-new
;;          (file+head "%<%Y-%m-%d>.org" "#+title: [%<%Y-%m-%d>]\n#+filetags: :personal:\n")
;;          :unnarrowed t)))

;; (setq org-roam-db-node-include-function (l'not (member "drill" (org-get-tags))))
;; (setc org-roam-db-node-include-function
;;       (defun my-db-node-include-function ()
;;         (let ((tags (org-get-tags)))
;;           (and
;;            (not (member "drill" tags))
;;            (not (member "fc" tags))
;;            (not (member "anki" tags))
;;            (not (member "noexport" tags))
;;            (not (member "exclude" tags))
;;            ;; (not (s-contains? "daily" (buffer-file-name (buffer-base-buffer))))
;;            ))))

;; Bibliography stuff
;; (setc org-latex-pdf-process '("%latex -interaction nonstopmode -output-directory %o %f"
;;                               "%bib"
;;                               "%latex -interaction nonstopmode -output-directory %o %f"
;;                               "%latex -interaction nonstopmode -output-directory %o %f"))
;; (setc reftex-default-bibliography    '("/home/kept/roam/refs/library_biblatex.bib"))
;; (setc org-ref-default-bibliography   '("/home/kept/roam/refs/library_biblatex.bib"))
;; (setc org-ref-bibliography-notes       "/home/kept/roam/refs/notes.org")
;; (setc org-ref-pdf-directory            "/home/kept/roam/refs/")
;; (setc bibtex-completion-library-path '("/home/kept/roam/refs/"))
;; (setc bibtex-completion-notes-path     "/home/kept/roam/refs/notes.org")
;; (setc bibtex-completion-bibliography   "/home/kept/roam/refs/library_biblatex.bib")
;; (setc bibtex-files                   '("/home/kept/roam/refs/library_biblatex.bib"))
;; (setc bibtex-completion-pdf-field      "file")
;; See also <f1> d m :tools biblio
;; NOTE: Org 9.5 has a native org-cite thing. Deprecate the rest?  Do I still need ol-bibtex in org-modules?
(setc citar-bibliography '("/home/kept/roam/refs/library_biblatex.bib"))
;; Citation completion (for Vertico and similar)
;; Doom provides it in :tools biblio
;; (use-package! citar
;;   :no-require
;;   :custom
;;   (org-cite-global-bibliography '("/home/kept/roam/references/library_biblatex.bib"))
;;   (org-cite-insert-processor 'citar)
;;   (org-cite-follow-processor 'citar)
;;   (org-cite-activate-processor 'citar)
;;   (citar-bibliography org-cite-global-bibliography)
;;   ;; optional: org-cite-insert is also bound to C-c C-x C-@
;;   :bind
;;   (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

;; Noter
(add-hook 'org-noter-notes-mode-hook #'abbrev-mode)
(add-hook 'org-noter-notes-mode-hook (l'rainbow-delimiters-mode 0))

;; errooooooooooooooooooooooors
;; (remove-hook 'after-save-hook #'org-roam-db-autosync--try-update-on-save-h)
;; (remove-hook 'org-roam-find-file-hook #'org-roam-buffer-toggle)

(add-hook 'org-mode-hook #'my-org-prettify)
;;(add-hook 'org-mode-hook #'org-resolve-clocks 95)
;;(add-hook 'org-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'org-mode-hook #'org-clock-persistence-insinuate)
(add-hook 'org-clock-in-hook #'org-clock-save)
;;(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'text-mode-hook (defun my-kill-smartparens () (smartparens-mode 0)))
;; (add-hook 'text-mode-hook
;;           (defun my-kill-fill () ;; i find doom weird about fill
;;             ;; (setc fill-column most-positive-fixnum)
;;             (setc adaptive-fill-mode nil)))


;; (org-publish-file "/home/kept/roam/blog/2022-01-08-how_i_live.org" '("blag" :base-directory "/home/kept/roam/blog/" :publishing-directory "/home/kept/Blog/meedstrom.github.io/_posts/" :publishing-function org-md-publish-to-md) t)

;; TODO: Rename the exported file as a Jekyll-compatible slug, so I don't need the original filename to be any particular way
(setc org-publish-project-alist
      '(("blag"
         :base-directory "/home/kept/roam/blog/"
         :publishing-directory "/home/kept/Blog/meedstrom.github.io/_posts/"
         :publishing-function org-md-publish-to-md
         )))

;; (org-publish-projects (list '("blag"
;;          :base-directory "/home/kept/roam/blog/"
;;          :publishing-directory "/home/kept/Blog/meedstrom.github.io/_posts/"
;;          :recursive nil
;;          :publishing-function #'org-md-publish-to-md
;;          )))

;; FIXME: there is an issue with possibly the include regexp
;; (org-publish-get-base-files '("blag"
;;                              :base-directory "/home/kept/roam/"
;;                              :publishing-directory "/home/kept/Blog/meedstrom.github.io/_posts/"
;;                              :exclude ".*"
;;                              ;; :base-extension (rx (or "org" "jpg" "svg"))
;;                              :recursive nil
;;                              ;; YYYY-MM-DD- in name (most roam notes have YYYYMMDD so will not match)
;;                              ;; :include "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}-"
;;                              :include "....-..-..-"
;;                              :publishing-function #'org-md-publish-to-md
;;                              ))

;; (setc org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
;;                                  (todo . " %i %-32b") ;; have breadcrumbs
;;                                  (tags . " %i %-12:c")
;;                                  (search . " %i %-12:c")))
;; (setc org-archive-location "/home/kept/Journal/diary.org::datetree/")
(setc org-agenda-custom-commands '(("b" todo "NEXT")
                                   ("w" todo "WAITING")
                                   ("p" todo "PROCRASTINATING")
                                   ("c" tags-todo "+active")))
(setc org-archive-save-context-info '(time file itags olpath))
(setc org-agenda-tag-filter-preset '("-exclude"))
(setc org-agenda-todo-list-sublevels nil)
(setc org-agenda-todo-ignore-scheduled t)
(setc org-agenda-files '("/home/kept/Journal/diary.org"
                         ;; for always caching the org-id locations
                         "/home/kept/Emacs/conf-doom/elfeed.org"
                         "/home/kept/roam/gtd.org"
                         "/home/kept/roam/2021-08-27-someday_maybe.org"))

(setc org-pomodoro-play-sounds nil)
(setc org-clock-out-remove-zero-time-clocks t)
(setc org-clock-persist t)
(setc org-clock-idle-time 5)
(setc org-hide-leading-stars nil)
(setc org-clock-mode-line-total 'today)
(setc org-clock-auto-clock-resolution 'always)
(setc org-clock-in-resume t)
(after! org (setc org-startup-folded 'fold)) ;; FIXME: doom(?) doesn't respect this
(setc org-catch-invisible-edits 'smart)
(setc org-ctrl-k-protect-subtree t)
(setc org-agenda-include-diary t)
(setc org-cycle-separator-lines 3)
(setc org-datetree-add-timestamp t)
(setc org-edit-src-content-indentation 0)
(setc org-ellipsis "⤵")
(setc org-export-creator-string "")
(setc org-hide-emphasis-markers t) ; hide the *, =, and / markers
(setc org-image-actual-width '(200)) ; use #ATTR if available, else 200 px
(setc org-insert-heading-respect-content t)
;; (setc org-latex-compiler "xelatex") ; allow unicode (åäö) in VERBATIM blocks
(setc org-log-done 'time)
(setc org-log-into-drawer t) ; hide spam
(setc org-modules '(org-id ol-info))
(setc org-pretty-entities t)
(setc org-use-speed-commands t)
(setc org-clock-x11idle-program-name (or (executable-find "xprintidle") "x11idle"))
;; (setc org-replace-disputed-keys t)

;; improve org performance
(global-auto-composition-mode 0)
(setc bidi-display-reordering nil)

(setc my-org-prettify-alist '(
                              ;; Nice for writing/reading equations in plaintext
                              ;; ("[" . "［")
                              ;; ("]" . "］")
                              ;; ("-" . "－")
                              ;; ("=" . "＝")
                              ;; ("+" . "＋")
                              ("\\vdots" . "⋮")
                              ("\\implies" . "⟹")
                              ("\\sqrt" . "√")
                              ("\\ldots" . "…")))

(defun my-org-prettify ()
  (setq prettify-symbols-alist
        (cl-union prettify-symbols-alist my-org-prettify-alist))
  ;; (mapc (lambda (x) (pushnew! prettify-symbols-alist x))
  ;; my-org-prettify-alist)
  )

(after! org
  (require 'named-timer) ;; an indispensable 70-line library
  (named-timer-run :my-clock-reminder nil 600
                   (defun my-clock-remind ()
                     (when (org-clock-is-active)
                       (message (concat "Currently working on: "
                                        org-clock-current-task))))))

;; remove some of doom's defaults
;; (remove-hook 'org-load-hook #'+org-init-capture-defaults-h)

(after! org-roam
  (add-hook 'doom-load-theme-hook
            (defun my-theme-mod-org ()
              (set-face-attribute 'org-roam-title nil :height 1.5)))
  (my-theme-mod-org))

(defun my-org-roam-extract-subtree ()
  "Variant of org-roam-extract-subtree.
It skips prompting, and inserts the metadata I want."
  (interactive)
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (when (bobp) (user-error "Already a top-level node"))
    (let ((parent-node (org-roam-node-at-point)))
      (org-id-get-create)
      (save-buffer)
      (org-roam-db-update-file)
      (let* ((template-info nil)
             (node (org-roam-node-at-point))
             (template (org-roam-format-template
                        (string-trim (org-capture-fill-template org-roam-extract-new-file-path))
                        (lambda (key default-val)
                          (let ((fn (intern key))
                                (node-fn (intern (concat "org-roam-node-" key)))
                                (ksym (intern (concat ":" key))))
                            (cond
                             ((fboundp fn)
                              (funcall fn node))
                             ((fboundp node-fn)
                              (funcall node-fn node))
                             (t (let ((r (read-from-minibuffer (format "%s: " key) default-val)))
                                  (plist-put template-info ksym r)
                                  r)))))))
             (file-path
              (expand-file-name template org-roam-directory)))
        (when (file-exists-p file-path)
          (user-error "%s exists. Aborting" file-path))
        (org-cut-subtree)
        (open-line 1)
        (insert "- " (org-link-make-string
                      (concat "id:" (org-roam-node-id node))
                      (org-roam-node-formatted node)))
        (save-buffer)
        (find-file file-path)
        (org-paste-subtree)
        (while (> (org-current-level) 1) (org-promote-subtree))
        (save-buffer)
        (org-roam-promote-entire-buffer)
        (goto-char (point-min))
        (search-forward "#+title")
        (goto-char (line-beginning-position))
        (delete-char -1)
        (forward-line 1)
        (open-line 2)
        (insert "#+date: [" (format-time-string "%F") "]")

        ;; Insert a link to the parent (DEPRECATED, I changed my mind)
        ;; (when parent-node
        ;;   (newline 2)
        ;;   (insert (org-link-make-string
        ;;            (concat "id:" (org-roam-node-id parent-node))
        ;;            (org-roam-node-formatted parent-node)) ","))

        (save-buffer)))))

(after! org-roam
  (setc org-roam-extract-new-file-path "%<%Y-%m-%d>-${slug}.org")
  (setc org-roam-capture-templates
        `(("d" "default" plain "%?" :if-new
           (file+head "%<%Y-%m-%d>-${slug}.org"
                      "#+title: ${title}\n#+date: \[%<%Y-%m-%d>\]\n#+filetags: :stub:\n")
           :unnarrowed t
           :immediate-finish t
           :jump-to-captured t)

          ("i" "instantly create this node" plain "%?" :if-new
           (file+head "%<%Y-%m-%d>-${slug}.org"
                      "#+title: ${title}\n#+date: \[%<%Y-%m-%d>\]\n#+filetags: :stub:\n")
           :unnarrowed t
           :immediate-finish t)
          
          )))

;; has to happen after load bc doom sets capture templates at load time.
;; incidentally also means we cannot use custom-file to config them
(after! org
  (setc org-capture-templates
        `(
          ("p" "Person of history" entry
           (file "/home/kept/roam/2021-08-27-historical-people.org")
           ,(lines "* %^{Name} :stub:"
                   ":PROPERTIES:"
                   ":ID:       %(org-id-uuid)"
                   ":ROAM_ALIASES: \"%\\1 (%\\2)\""
                   ":END:"
                   "%^{Years of birth and death (YYYY--YYYY)}"
                   "%?"))

          ;; TODO: Number it by a counter
          ("u" "untitled note" plain
           (file "/home/kept/roam/untitled-notes.org")
           "* \[%<%Y-%m-%d %T>\]
:PROPERTIES:
:ID:  %(org-id-uuid)
:DATE: \[%<%Y-%m-%d>\]\n
:END:
%?")

          ("e" "Emacs idea" entry (file+headline "/home/kept/roam/2021-08-27-someday_maybe.org" "Ideas"))
          ("q" "Statistics question" entry (file+headline "/home/kept/roam/stats.org" "Questions"))
          ("t" "Statistics header" entry (file+headline "/home/kept/roam/stats.org" "Statistics"))
          ("s" "Someday/Maybe" entry (file+headline "/home/kept/roam/2021-08-27-someday_maybe.org" "Unsorted"))
          ("m" "A Virtual Assistant function")
          ("mw" "weight" plain (function eva-session-new) :immediate-finish t)
          ("mf" "visit Ledger file" plain (function eva-present-ledger-file) :immediate-finish t)

          ;; ("ln" "From Nordea" plain (file "/home/kept/Journal/Finances/clean_start.ledger")
          ;;  ,(lines "%<%Y-%m-%d> * \"\""
          ;;          "    Expenses   %?"
          ;;          "    Assets:Nordea:Personkonto")
          ;;  :empty-lines 1
          ;;  :jump-to-captured t)

          ;; ("lk" "From Komplett" plain (file "/home/kept/Journal/Finances/clean_start.ledger")
          ;;  ,(lines "%<%Y-%m-%d> * \"\""
          ;;          "    Expenses   %?"
          ;;          "    Liabilities:Komplett")
          ;;  :empty-lines 1
          ;;  :jump-to-captured t)

          ;; ("li" "InvestNotSpend" plain (file "/home/kept/Journal/Finances/clean_start.ledger")
          ;;  ,(lines "%<%Y-%m-%d> ! \"\""
          ;;          "    [Assets:Lysa:InvestNotSpend]   %?"
          ;;          "    Assets:Nordea:Personkonto")
          ;;  :empty-lines 1
          ;;  :jump-to-captured t)

          ("r" "Retroactive clock" entry (file+olp+datetree "/home/kept/Journal/diary.org")
           ,(lines "* %^{Activity|School|Piano|Signing|Coding}"
                   "CLOCK: %^{Time at start}U--%^{Time at finish}U => %^{Rough time spent (sorry, the program is dumb), H:MM}")
           :immediate-finish t)

          )))

;; Because capfs don't do what I want in Roam
(add-hook 'org-mode-hook #'my-corfu-turn-off)

(after! org
  (unless after-init-time
    (warn "Org loaded during init, I don't want this"))
  (setc org-babel-load-languages '((R . t)
                                   (emacs-lisp . t)
                                   (calc . t)
                                   (ditaa . t)
                                   (sqlite . t)
                                   (dot . t)))
  ;; Stuff to do if I'm not using Doom's Org
  (unless (fboundp '+org-init-org-directory-h)
    ;; Upscale the LaTeX preview.
    (my-change-latex-scale)
    ;; Adapt LaTeX preview scale to the font zoom.
    (add-hook 'text-scale-mode-hook #'my-change-latex-scale)))

;; Prettify code-snippets in exported pdf.
(after! ox-latex
  (setc org-latex-listings t)
  (setc org-latex-listings-options '(("basicstyle" "\\small"))) ; small code font
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "booktabs")))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 90)
