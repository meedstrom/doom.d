;; -*- lexical-binding: t; -*-

;; Improve org performance
(global-auto-composition-mode 0)
(setopt bidi-display-reordering nil)

(setopt org-timestamp-custom-formats '("%Y-%b-%d" . "%Y-%m-%d %a %H:%M"))
(setopt org-pretty-entities t)
(setopt org-archive-location "/home/kept/roam/noagenda/archive.org::datetree/")
(setopt org-clock-persist t)
(setopt org-clock-auto-clock-resolution t)
(setopt org-startup-folded 'nofold)
(setopt org-agenda-include-diary t)
(setopt citar-bibliography '("/home/kept/roam/refs/library_biblatex.bib"))
(setopt org-agenda-todo-list-sublevels nil)
(setopt org-agenda-todo-ignore-scheduled t)
(setopt org-agenda-dim-blocked-tasks nil) ;; perf
(setopt org-agenda-use-tag-inheritance '(todo search)) ;; perf
(setopt org-agenda-ignore-properties '(stats)) ;; perf
(setopt org-agenda-inhibit-startup t) ;; perf
(setopt org-archive-save-context-info '(time file itags olpath))
(setopt org-attach-id-dir "static/")
(setopt org-pomodoro-play-sounds nil)
(setopt org-export-backends '(html latex odt texinfo))
(setopt org-export-with-toc nil)
(setopt org-clock-out-remove-zero-time-clocks t)
(setopt org-clock-idle-time 5)
(setopt org-hide-leading-stars nil)
(setopt org-clock-mode-line-total 'today)
(setopt org-clock-in-resume t)
(setopt org-catch-invisible-edits 'smart)
(setopt org-ctrl-k-protect-subtree t)
(setopt org-cycle-separator-lines 3)
(setopt org-datetree-add-timestamp nil)
(setopt org-edit-src-content-indentation 0)
;; (setopt org-ellipsis "⤵")
(setopt org-ellipsis "…")
(setopt org-hide-emphasis-markers t) ; hide the *, =, and / markers
(setopt org-image-actual-width '(200)) ; use #ATTR if available, else 200 px
;; (setopt org-latex-compiler "xelatex") ; allow unicode (åäö) in VERBATIM blocks
(setopt org-log-done 'time)
(setopt org-log-into-drawer t) ; hide spam
(setopt org-modules '(org-id ol-info ol-eww)) ;; `org-eww-copy-for-org-mode'
(setopt org-use-speed-commands t)
(setopt org-clock-x11idle-program-name (or (executable-find "xprintidle") "x11idle"))
(setopt org-replace-disputed-keys t)
(setopt org-tags-column 0)

;; (setq-default org-display-custom-times t) ;; could it cause org-element bugs due to daily page titles?
(setopt org-agenda-files
        (-filter #'file-exists-p '(
                                   ;; "/home/kept/roam/"   ;; slowww
                                   ;; "/home/kept/roam/daily/" ;; sloww
                                   ;; "/home/kept/roam/refs/"
                                   ;; "/home/kept/roam/frozen/"
                                   "/home/kept/roam/grismartin/pages/"
                                   ;; to always cache the org-id locations
                                   "/home/me/.doom.d/elfeed.org")))

;; (setopt org-babel-load-languages '((R . t)
;;                                    (emacs-lisp . t)
;;                                    (calc . t)
;;                                    (ditaa . t)
;;                                    (sqlite . t)
;;                                    (dot . t)))

;; (setopt org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
;;                                    (todo . " %i %-32b") ;; have breadcrumbs
;;                                    (tags . " %i %-12:c")
;;                                    (search . " %i %-12:c")))
;; (setopt org-agenda-custom-commands '(("b" todo "NEXT")
;;                                      ("w" todo "WAITING")
;;                                      ("p" todo "PROCRASTINATING")
;;                                      ("c" tags-todo "+active")))
;; (setopt org-agenda-tag-filter-preset '("-exclude"))


;; For inline-anki: override Org's underlines to represent cloze deletions and
;; make them look appropriate for that.
;; (set-face-attribute 'underline () :box t)
(defface my-cloze '((t . (:box t))) "Cloze face")
(setq org-emphasis-alist '(("*" bold)
                           ("/" italic)
                           ("_" my-cloze)
                           ("=" org-verbatim verbatim)
                           ("~" org-code verbatim)
                           ("+" (:strike-through t))))

(add-hook 'org-noter-notes-mode-hook #'abbrev-mode)
(add-hook 'org-noter-notes-mode-hook (l'rainbow-delimiters-mode 0))
(add-hook 'org-mode-hook #'my-org-setup-prettify)
;; (add-hook 'org-mode-hook #'org-resolve-clocks 95)
;; (add-hook 'org-mode-hook #'org-clock-persistence-insinuate)
(add-hook 'org-clock-in-hook #'org-clock-save)
(add-hook 'org-clock-out-hook #'bh/clock-out-maybe 90)
(add-hook 'text-mode-hook #'turn-off-smartparens-mode)

(defun my-org-setup-prettify ()
  (setq prettify-symbols-alist
        (cl-union prettify-symbols-alist
                  '(;; Still waiting for the Year of AsciiMath on Org-mode...
                    ("\\vdots" . "⋮")
                    ("\\implies" . "⟹")
                    ("\\sqrt" . "√")
                    ("\\ldots" . "…")))))

(after! org
  (unless after-init-time
    (setq debug-on-error t)
    (error (message "Org loaded during init, I don't want this")))
  (require 'named-timer)
  (named-timer-run :my-clock-reminder nil 600
                   (defun my-clock-remind ()
                     (when (org-clock-is-active)
                       (message (concat "Currently working on: "
                                        org-clock-current-task)))))
  ;; If using Doom's Org
  (if (modulep! :lang org)
      ;; fix interference with org-transclusion
      (advice-remove 'org-link-search '+org--recenter-after-follow-link-a)
    ;; if not using Doom's org
    ;; (require 'org-indent)
    ;; (add-hook 'org-mode-hook #'org-indent-mode)
    (my-change-latex-scale) ;; Bigger LaTeX preview
    ;; Adapt LaTeX preview scale to the font zoom
    (add-hook 'text-scale-mode-hook #'my-change-latex-scale)))

(after! ox-latex
  ;; Prettify code-snippets in exported pdf.
  (setopt org-latex-listings t)
  (setopt org-latex-listings-options '(("basicstyle" "\\small"))) ; small code font
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "booktabs"))
  ;; Add letter class so I can... write a cover letter. yup, my life
  (add-to-list 'org-latex-classes
               '("letter"
                 "\\documentclass[11pt]{letter}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(after! org-transclusion
  (set-face-background 'org-transclusion "#222")
  (setopt org-transclusion-exclude-elements '(property-drawer comment keyword)))


;;; Workaround the tide of org-element parser bugs since 9.5 rewrite

(setq org-element-use-cache nil)
