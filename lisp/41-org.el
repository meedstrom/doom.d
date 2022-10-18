;; -*- lexical-binding: t; -*-

(require 'my-lib-external)

(after! org
  (setopt org-startup-folded 'fold)
  ;; Undoom. Having exactly two states makes for comfy toggling.
  (setopt org-todo-keywords '((sequence "TODO" "DONE"))))

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

(add-hook 'org-export-before-processing-hook #'my-md-export-hook)

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
(setopt org-roam-directory "/home/kept/roam/")
(setopt org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%H:%M>\n%?" :if-new
         (file+head "%<%Y-%m-%d>.org" "#+title: [%<%Y-%m-%d>]\n#+filetags: :personal:\n")
         :immediate-finish t
         :jump-to-captured t)))

;; See also C-h d m :tools biblio
(setopt citar-bibliography '("/home/kept/roam/refs/library_biblatex.bib"))


(add-hook 'org-noter-notes-mode-hook #'abbrev-mode)
(add-hook 'org-noter-notes-mode-hook (l'rainbow-delimiters-mode 0))
(add-hook 'org-mode-hook #'my-org-prettify)
;;(add-hook 'org-mode-hook #'org-resolve-clocks 95)
;;(add-hook 'org-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'org-mode-hook #'org-clock-persistence-insinuate)
(add-hook 'org-clock-in-hook #'org-clock-save)
(add-hook 'text-mode-hook (defun my-kill-smartparens () (smartparens-mode 0)))

;; TODO: Rename the exported file as a Jekyll-compatible slug, so I don't need
;; the original filename to be any particular way.
(setopt org-publish-project-alist
        '(("blag"
           :base-directory "/home/kept/roam/blog/"
           :publishing-directory "/home/kept/blog/meedstrom.github.io/_posts/"
           :publishing-function org-md-publish-to-md
           )))

;; (setopt org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
;;                                    (todo . " %i %-32b") ;; have breadcrumbs
;;                                    (tags . " %i %-12:c")
;;                                    (search . " %i %-12:c")))
;; (setopt org-agenda-custom-commands '(("b" todo "NEXT")
;;                                      ("w" todo "WAITING")
;;                                      ("p" todo "PROCRASTINATING")
;;                                      ("c" tags-todo "+active")))
(setopt org-agenda-tag-filter-preset '("-exclude"))
(setopt org-agenda-todo-list-sublevels nil)
(setopt org-agenda-todo-ignore-scheduled t)
(setopt org-agenda-files '("/home/kept/archive/journal/diary.org"
                           ;; to always cache the org-id locations
                           "/home/kept/emacs/conf-doom/elfeed.org"
                           "/home/kept/roam/gtd.org"
                           "/home/kept/roam/2021-08-27-someday_maybe.org"))

;; (setopt org-archive-location "/home/kept/archive/journal/diary.org::datetree/")
(setopt org-archive-save-context-info '(time file itags olpath))
(setopt org-pomodoro-play-sounds nil)
(setopt org-clock-out-remove-zero-time-clocks t)
(setopt org-clock-persist t)
(setopt org-clock-idle-time 5)
(setopt org-hide-leading-stars nil)
(setopt org-clock-mode-line-total 'today)
(setopt org-clock-auto-clock-resolution 'always)
(setopt org-clock-in-resume t)
(setopt org-catch-invisible-edits 'smart)
(setopt org-ctrl-k-protect-subtree t)
(setopt org-agenda-include-diary t)
(setopt org-cycle-separator-lines 3)
(setopt org-datetree-add-timestamp t)
(setopt org-edit-src-content-indentation 0)
(setopt org-ellipsis "⤵")
(setopt org-export-creator-string "")
(setopt org-hide-emphasis-markers t) ; hide the *, =, and / markers
(setopt org-image-actual-width '(200)) ; use #ATTR if available, else 200 px
(setopt org-insert-heading-respect-content t)
;; (setopt org-latex-compiler "xelatex") ; allow unicode (åäö) in VERBATIM blocks
(setopt org-log-done 'time)
(setopt org-log-into-drawer t) ; hide spam
(setopt org-modules '(org-id ol-info))
(setopt org-pretty-entities t)
(setopt org-use-speed-commands t)
(setopt org-clock-x11idle-program-name (or (executable-find "xprintidle") "x11idle"))
(setopt org-replace-disputed-keys t)

;; Improve org performance
(global-auto-composition-mode 0)
(setopt bidi-display-reordering nil)

(setopt my-org-prettify-alist '(
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

(after! org-roam
  (add-hook 'doom-load-theme-hook
            (defun my-theme-mod-org ()
              (set-face-attribute 'org-roam-title nil :height 1.5)))
  (my-theme-mod-org))

(after! org-roam
  (setopt org-roam-extract-new-file-path "%<%Y-%m-%d>-${slug}.org")
  (setopt org-roam-capture-templates
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
;; incidentally also means we cannot use custom-file to config them.
;; to remove the offender, do
;; (remove-hook 'org-load-hook #'+org-init-capture-defaults-h)
(after! org
  (setopt org-capture-templates
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

          ("ln" "From Nordea" plain (file "/home/kept/self-data/clean_start.ledger")
           ,(lines "%<%Y-%m-%d> * \"\""
                   "    Expenses   %?"
                   "    Assets:Nordea:Personkonto")
           :empty-lines 1
           :jump-to-captured t)

          ("li" "InvestNotSpend" plain (file "/home/kept/self-data/clean_start.ledger")
           ,(lines "%<%Y-%m-%d> ! \"\""
                   "    [Assets:Lysa:InvestNotSpend]   %?"
                   "    Assets:Nordea:Personkonto")
           :empty-lines 1
           :jump-to-captured t)

          ("r" "Retroactive clock" entry (file+olp+datetree "/home/kept/archive/journal/diary.org")
           ,(lines "* %^{Activity|School|Piano|Signing|Coding}"
                   "CLOCK: %^{Time at start}U--%^{Time at finish}U => %^{Rough time spent (sorry, the program is dumb), H:MM}")
           :immediate-finish t)

          )))

;; Because capfs don't do what I want in Roam
(add-hook 'org-mode-hook #'my-corfu-turn-off)

(after! org
  (unless after-init-time
    (warn "Org loaded during init, I don't want this"))
  (setopt org-babel-load-languages '((R . t)
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
  (setopt org-latex-listings t)
  (setopt org-latex-listings-options '(("basicstyle" "\\small"))) ; small code font
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "booktabs")))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 90)
