;; -*- lexical-binding: t; -*-

(after! org-journal
  ;; Workaround https://github.com/bastibe/org-journal/issues/298
  ;; (defun org-journal-is-journal () nil)
  )

;; Integrate org-journal with roam dailies (I don't want to use capture
;; functions for the dailies, it's a weird fit)
(setc org-journal-dir "/home/kept/roam/daily")
(setq org-journal-file-format "%F.org")
(setc org-journal-file-header
      (lambda (_time)
        (concat ":PROPERTIES:\n:ID:       " (org-id-uuid) "\n:END:" )))
;; (setc org-journal-file-header (format ":PROPERTIES:\n:ID:%s\n:END:" (org-id-uuid)))
(setc org-journal-date-prefix "#+title: ")
(setc org-journal-date-format "%F %A")
(setq org-journal-time-prefix "* ")

;; Roam
(setc org-roam-directory "/home/kept/roam/")
(setc org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%H:%M>\n%?" :if-new
         (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d %A>\n")
         :empty-lines 1 :unnarrowed t)))

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

;; Noter
(add-hook 'org-noter-notes-mode-hook #'abbrev-mode)
(add-hook 'org-noter-notes-mode-hook (l'rainbow-delimiters-mode 0))

(add-hook 'org-mode-hook #'my-org-prettify)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'org-clock-persistence-insinuate)
(add-hook 'org-clock-in-hook #'org-clock-save)
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
;; (setc org-archive-location "/home/kept/Journal/diary.org::datetree/")
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
(setc org-modules '(org-id org-habit ol-bibtex))
(setc org-pretty-entities t)
(setc org-use-speed-commands t)
(setc org-clock-x11idle-program-name "xprintidle")
;; (setc org-replace-disputed-keys t)
(setc org-agenda-files '("/home/kept/Journal/diary.org"
                         "/home/kept/Journal/gtd3.org"
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

(after! org
  (require 'named-timer) ;; an indispensable 70-line library
  (named-timer-run :my-clock-reminder nil 600
                   (defun my-clock-remind ()
                     (when (org-clock-is-active)
                       (message (concat "Currently working on: "
                                        org-clock-current-task))))))

(setq org-roam-db-node-include-function (l'not (member "drill" (org-get-tags))))

;; remove some of doom's defaults
;; (remove-hook 'org-load-hook #'+org-init-capture-defaults-h)

;; has to be on after-load bc doom also sets capture templates at that time.
;; incidentally this means we cannot use custom-file to config them
(after! org 
  (setc org-capture-templates
        `(
          ("b" "Blog post" entry (file+headline "/home/kept/Blog/open.org" "Posts")
           ,(lines "** %u"
                   "%?")
           :empty-lines-after 1)

          ("e" "Emacs idea" entry (file+headline "/home/kept/roam/20210826233815-emacs.org" "Ideas"))
          ("q" "Statistics question" entry (file+headline "/home/kept/Knowledge_base/stats.org" "Questions"))
          ("t" "Statistics header" entry (file+headline "/home/kept/Knowledge_base/stats.org" "Statistics"))
          ("f" "Diana fact" item (file+headline "/home/kept/Diary/2020-109-07-diana-facts.org" "Etc"))
          ("s" "someday" item (file+headline "/home/kept/roam/20210827184025-someday_maybe.org"))

          ("l" "Ledger")
          ("lf" "visit file" plain (function eva-present-ledger-file)
           :immediate-finish t)

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

          ("m" "Call the VA")
          ("mw" "weight" plain (function eva-session-new)
           :immediate-finish t)
          )))


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

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 90)
