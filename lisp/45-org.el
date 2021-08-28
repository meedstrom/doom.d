;; -*- lexical-binding: t; -*-

(after! org
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

;; Bib
(setc org-latex-pdf-process '("%latex -interaction nonstopmode -output-directory %o %f"
                              "%bib"
                              "%latex -interaction nonstopmode -output-directory %o %f"
                              "%latex -interaction nonstopmode -output-directory %o %f"))
(setc reftex-default-bibliography    '("/home/kept/Knowledge_base/references/library_biblatex.bib"))
(setc org-ref-default-bibliography   '("/home/kept/Knowledge_base/references/library_biblatex.bib"))
(setc org-ref-bibliography-notes       "/home/kept/Knowledge_base/references/notes.org")
(setc org-ref-pdf-directory            "/home/kept/Knowledge_base/references/")
(setc bibtex-completion-library-path '("/home/kept/Knowledge_base/references/"))
(setc bibtex-completion-notes-path     "/home/kept/Knowledge_base/references/notes.org")
(setc bibtex-completion-bibliography   "/home/kept/Knowledge_base/references/library_biblatex.bib")
(setc bibtex-files                   '("/home/kept/Knowledge_base/references/library_biblatex.bib"))
(setc bibtex-completion-pdf-field      "file")

;; Roam
(setc org-roam-directory "/home/kept/Knowledge_base/roam/")
(after! (company company-org-roam)
  (push 'company-org-roam company-backends))

;; Noter
(add-hook 'org-noter-notes-mode-hook #'abbrev-mode)
(add-hook 'org-noter-notes-mode-hook
          (defun my-kill-rainbow-delimiters () (rainbow-delimiters-mode 0)))

(add-hook 'org-mode-hook #'my-org-prettify)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'org-clock-persistence-insinuate)
(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'text-mode-hook (defun my-kill-smartparens () (smartparens-mode 0)))
(add-hook 'text-mode-hook
          (defun my-kill-fill () ;; i find doom weird about fill
            ;; (setc fill-column most-positive-fixnum)
            (setc adaptive-fill-mode nil)))
(remove-hook 'org-mode-hook #'rainbow-delimiters-mode)

(setc org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                 (todo . " %i %-32b")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))
(setc org-archive-location "/home/kept/Journal/diary2.org::datetree/")
(setc org-agenda-custom-commands '(("b" todo "NEXT")
                                    ("w" todo "WAITING")
                                    ("p" todo "PROCRASTINATING")
                                    ("c" tags-todo "+active")))
(setc org-archive-save-context-info '(time file itags olpath))
(setc org-pomodoro-play-sounds nil)
(setc org-clock-out-remove-zero-time-clocks t)
(setc org-clock-persist t)
(setc org-hide-leading-stars nil)
(setc org-clock-mode-line-total 'today)
(setc org-clock-auto-clock-resolution 'always)
(setc org-clock-in-resume t)
(setc org-startup-folded 'fold) ;; FIXME: doom(?) doesn't respect this
(setc org-catch-invisible-edits 'smart)
(setc org-ctrl-k-protect-subtree t)
(setc org-agenda-include-diary t)
(setc org-journal-dir "/home/kept/Diary")
(setc org-journal-date-format "%F %A")
(setc org-journal-file-format "%Y%m%d.org")
(setc org-cycle-separator-lines 3)
(setc org-datetree-add-timestamp t)
(setc org-edit-src-content-indentation 0)
(setc org-ellipsis "⤵")
(setc org-export-creator-string "")
(setc org-hide-emphasis-markers t) ; hide the *, =, and / markers
(setc org-image-actual-width '(200)) ; use #ATTR if available, else 200 px
(setc org-insert-heading-respect-content t)
(setc org-latex-compiler "xelatex") ; allow unicode (åäö) in VERBATIM blocks
(setc org-log-done 'time)
(setc org-log-into-drawer t) ; hide spam
(setc org-modules '(org-id org-bibtex))
(setc org-pretty-entities t)
(setc org-use-speed-commands t)
(setc org-clock-x11idle-program-name "xprintidle")
;; (setc org-replace-disputed-keys t)
(setc org-agenda-files '("/home/kept/Journal/diary2.org"
                         "/home/kept/Journal/measurable.org"
                         "/home/kept/Emacs/common/emacs-todo.org"))

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


(setc org-capture-templates
 `(

   ("b" "Blog post" entry (file+headline "/home/kept/Blog/open.org" "Posts")
    ,(lines "** %u"
            "%?")
    :empty-lines-after 1)

   ("d" "Diary entry" plain (file+olp+datetree "/home/kept/Journal/diary2.org")
    ,(lines "-----"
            "%?") :empty-lines 1)

   ("e" "Emacs idea" entry (file+headline "/home/kept/Emacs/common/emacs-todo.org" "Ideas")
    ,(lines "** %?") :empty-lines 1)

   ("s" "Study journal" entry (file+olp+datetree "/home/kept/Journal/diary2.org")
    ,(lines "* Study journal"
            "%?"))

   ("q" "Statistics question" entry (file+headline "/home/kept/Knowledge_base/stats.org" "Questions")
    ,(lines "** %?") :empty-lines 1 :prepend t)

   ("t" "Statistics header" entry (file+headline "/home/kept/Knowledge_base/stats.org" "Statistics")
    ,(lines "** %?") :empty-lines 1)

   ("f" "Diana fact" item (file+headline "/home/kept/Diary/200907-diana-facts.org" "Etc")
    ,(lines "- %?") :empty-lines- 1)

   ("l" "Ledger entry")

   ("ln" "From Nordea" plain (file "/home/kept/Journal/Finances/clean_start.ledger")
    ,(lines "%<%Y-%m-%d> * \"\""
            "    Expenses   %?"
            "    Assets:Nordea:Personkonto")
    :empty-lines 1
    :jump-to-captured t)

   ("lk" "From Komplett" plain (file "/home/kept/Journal/Finances/clean_start.ledger")
    ,(lines "%<%Y-%m-%d> * \"\""
            "    Expenses   %?"
            "    Liabilities:Komplett")
    :empty-lines 1
    :jump-to-captured t)

   ("li" "InvestNotSpend" plain (file "/home/kept/Journal/Finances/clean_start.ledger")
    ,(lines "%<%Y-%m-%d> ! \"\""
            "    [Assets:Lysa:InvestNotSpend]   %?"
            "    Assets:Nordea:Personkonto")
    :empty-lines 1
    :jump-to-captured t)

   ("r" "Retroactive clock" entry (file+olp+datetree "/home/kept/Journal/diary2.org")
    ,(lines "* %^{Activity|School|Piano|Signing}"
            "CLOCK: %^{Time at start}U--%^{Time at finish}U => %^{Rough time spent (sorry, the program is dumb), H:MM}")
    :immediate-finish t)

   ("m" "Secretary.el queries and presentations")
   ("mw" "weight" plain (function #'sc-query-weight)
    :immediate-finish t)

   ))


(setq bh/keep-clock-running nil)

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "2416adaa-9baa-40a8-a16b-85579699c149")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)
