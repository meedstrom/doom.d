;; -*- lexical-binding: t; -*-
;; Copyright (C) 2020-2023 Martin Edström
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(require 'my-lib-external)

(after! ox-latex
  (add-to-list 'org-latex-classes
               '("letter"
                 "\\documentclass[11pt]{letter}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(defun my-org-open-at-point (&optional arg)
  (interactive "P")
  (if-let ((url (thing-at-point 'url)))
      ;; TODO: only goto if an actually known reflink
      (let ((all-refs (org-roam-db-query
                       [:select [ref id title]
                        :from refs
                        :left-join nodes
                        :on (= refs:node-id nodes:id)])))
        ;; (org-roam-ref-find url)

        )

    (if arg
        (org-open-at-point arg)
      (org-open-at-point))))

;; (defun my-logseq-mode (_))
;; (defvar-local my-logseq-mode nil)

;; TODO: Maybe this can become a buffer-local mode, if instead of setting faces,
;; it just removes the faces from the local syntax table.  Or use
;; `face-remap-add-relative'.
(defvar my-org-default-faces nil)
(define-minor-mode my-logseq-mode
  "De-fontify Org headings.
Must unfortunately be a global mode because faces cannot be set
per-buffer."
  :global t
  (when (featurep 'org)
    (if my-logseq-mode
        (dolist (x '(org-level-1
                     org-level-2
                     org-level-3
                     org-level-4
                     org-level-5
                     org-level-6
                     org-level-7
                     org-level-8))
          (setf (alist-get x my-org-default-faces)
                (cons (face-bold-p x)
                      (face-foreground x)))
          (set-face-bold x nil)
          (set-face-foreground x (face-foreground 'default)))
      (dolist (x '(org-level-1
                   org-level-2
                   org-level-3
                   org-level-4
                   org-level-5
                   org-level-6
                   org-level-7
                   org-level-8))
        (set-face-bold x (car (alist-get x my-org-default-faces)))
        (set-face-foreground x (cdr (alist-get x my-org-default-faces)))))))

(add-hook 'delve-mode-hook #'delve-compact-view-mode)

;; (add-hook 'lister-mode-hook #'View-exit)
(after! delve
  ;; It normally inherits from org-roam-title, which I find too big
  (set-face-attribute 'delve-title-face () :inherit 'org-document-title))

;; undoom
(after! org
  ;; IDK why but I find this doom-docs-mode just gets in my way
  (remove-hook 'read-only-mode-hook 'doom-docs--toggle-read-only-h))
;; Crude but guaranteed to work
(fset 'doom-docs-org-mode #'ignore)
(fset 'doom-docs--toggle-read-only-h #'ignore)

(after! org
  (org-recent-headings-mode)
  ;; (setopt org-startup-folded 'fold)
  ;; Undoom. Having exactly two states makes for comfy toggling.
  (setopt org-todo-keywords '((sequence "TODO" "DONE"))))

(after! org-roam-node
  ;; Use my own slug style
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (my-slugify (org-roam-node-title node))))

(defun my-last-daily-file ()
  (interactive)
  (require 'org-roam-dailies)
  (find-file (car (last (org-roam-dailies--list-files)))))

(after! org
  (dolist (x '(org-level-1
               org-level-2
               org-level-3
               org-level-4
               org-level-5
               org-level-6
               org-level-7
               org-level-8))
    (set-face-bold x nil)))

(add-hook 'org-roam-buffer-postrender-functions #'magit-section-show-level-2)
(setopt org-roam-directory "/home/kept/roam/")
(setopt org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%H:%M>\n%?" :if-new
         (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: :noexport:daily:\n#+date: [%<%Y-%m-%d>]\n")
         :immediate-finish t
         :jump-to-captured t)))

;; See also C-h d m :tools biblio
(setopt citar-bibliography '("/home/kept/roam/refs/library_biblatex.bib"))

(setopt org-roam-db-node-include-function
        (lambda ()
          (not (string-search "lw" default-directory))))

(add-hook 'org-noter-notes-mode-hook #'abbrev-mode)
(add-hook 'org-noter-notes-mode-hook (l'rainbow-delimiters-mode 0))
(add-hook 'org-mode-hook #'my-org-prettify)
;;(add-hook 'org-mode-hook #'org-resolve-clocks 95)
;;(add-hook 'org-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'org-mode-hook #'org-clock-persistence-insinuate)
(add-hook 'org-clock-in-hook #'org-clock-save)
(add-hook 'text-mode-hook (defun my-kill-smartparens () (smartparens-mode 0)))

;; Open Org-roam UI in Chromium's kiosk mode.
;; I don't use Chromium for anything else.
(setopt browse-url-chromium-arguments '("--app=http://localhost:35901"))
(setopt org-roam-ui-browser-function #'browse-url-chromium)
(when guix
  (add-to-list 'browse-url-chromium-arguments "--no-sandbox"))

;; (setopt org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
;;                                    (todo . " %i %-32b") ;; have breadcrumbs
;;                                    (tags . " %i %-12:c")
;;                                    (search . " %i %-12:c")))
;; (setopt org-agenda-custom-commands '(("b" todo "NEXT")
;;                                      ("w" todo "WAITING")
;;                                      ("p" todo "PROCRASTINATING")
;;                                      ("c" tags-todo "+active")))
;; (setopt org-agenda-tag-filter-preset '("-exclude"))
(setopt org-agenda-todo-list-sublevels nil)
(setopt org-agenda-todo-ignore-scheduled t)
(setopt org-agenda-files (-filter #'file-exists-p
                                  '("/home/kept/archive/journal/diary.org"
                                    ;; to always cache the org-id locations
                                    "/home/me/.doom.d/elfeed.org"
                                    "/home/kept/roam/"
                                    "/home/kept/roam/daily/"
                                    "/home/kept/roam/refs/"
                                    "/home/kept/roam/frozen/"
                                    "/home/kept/roam/grismartin/pages/"
                                    )))
(setopt org-archive-location "/home/kept/roam/archive.org::datetree/")
(setopt org-archive-save-context-info '(time file itags olpath))
(setopt org-attach-id-dir "attachments/") ;; doom prolly overrides
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
(setopt org-hide-emphasis-markers t) ; hide the *, =, and / markers
(setopt org-image-actual-width '(200)) ; use #ATTR if available, else 200 px
(setopt org-insert-heading-respect-content t)
;; (setopt org-latex-compiler "xelatex") ; allow unicode (åäö) in VERBATIM blocks
(setopt org-log-done 'time)
(setopt org-log-into-drawer t) ; hide spam
(setopt org-modules '(org-id ol-info ol-eww)) ;;org-eww-copy-for-org-mode
(setopt org-pretty-entities t)
(setopt org-use-speed-commands t)
(setopt org-clock-x11idle-program-name (or (executable-find "xprintidle") "x11idle"))
(setopt org-replace-disputed-keys t)
(setopt org-time-stamp-custom-formats '("<%Y-%b-%d>" . "<%Y-%m-%d %a %H:%M>"))
(setq-default org-display-custom-times t)

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
  (require 'org-protocol) ;; for org capture from firefox

  (require 'named-timer) ;; an indispensable 70-line library
  (named-timer-run :my-clock-reminder nil 600
                   (defun my-clock-remind ()

                     (when (org-clock-is-active)
                       (message (concat "Currently working on: "
                                        org-clock-current-task))))))

;; WIP
;; I don't think this will work. I think the code that looks up values entered by user, has to run after, not in the template function.
;;
;;To go with a capture template that goes like this
;; ("p" "Person" plain "%?" :if-new
;;    (file+head "%<%Y-%m-%d>-${slug}.org")
;;    (function my-person-template)
;;    :unnarrowed t
;;    :immediate-finish t
;;    :jump-to-captured t)
(defun my-person-template ()
  "Do a capture to two places: an usual new note and a link
to the new note in the \"timeline\" note."
  ;; untested
  (with-current-buffer (org-id-open "3r342-id-for-the-timeline-note")
    (insert "- 1991-2022 Person Name"))
  ;; untested
  (org-set-property "ROAM_ALIASES"
                    (concat (ndk/org-current-buffer-get-title)
                            " ("
                            (read-string "Years of birth and death (YYYY--YYYY)")
                            ")"))
  "#+title: ${title}\n#+filetags: :person:stub:\n#+date: \[%<%Y-%m-%d>\]\n")

(after! org-roam
  (add-hook 'doom-load-theme-hook
            (defun my-theme-mod-org ()
              (set-face-attribute 'org-roam-title nil :height 1.5)))
  (my-theme-mod-org)
  (setopt org-roam-extract-new-file-path "%<%Y-%m-%d>-${slug}.org")
  (setopt org-roam-capture-templates
        `(("d" "default" plain "%?" :if-new
           (file+head "%<%Y-%m-%d>-${slug}.org"
                      "#+title: ${title}\n#+filetags: :noexport:stub:\n#+date: \[%<%Y-%m-%d>\]\n")
           :unnarrowed t
           :immediate-finish t
           :jump-to-captured t)
          ("i" "instantly create this node" plain "%?" :if-new
           (file+head "%<%Y-%m-%d>-${slug}.org"
                      "#+title: ${title}\n#+filetags: :stub:\n#+date: \[%<%Y-%m-%d>\]\n")
           :unnarrowed t
           :immediate-finish t)
          ("a" "acquaintance" plain "%?" :if-new
           (file+head "%<%Y-%m-%d>-${slug}.org"
                      ,(lines "#+title: ${title}\n#+filetags: :stub:partner:therapist:acquaintance:\n#+date: \[%<%Y-%m-%d>\]\n"
                              ":noexport:"
                               "- Email :: "
                               "- Phone :: "
                               "- Address :: "
                               ":end:"
                               "- Location :: "
                               "- Birthday :: "
                               "- Interests :: "
                               "- How we met :: "))
           :unnarrowed t
           :immediate-finish t
           :jump-to-captured t)
          )))

(defun my-insert-heading-with-id ()
  (interactive)
  (org-insert-heading)
  (org-id-get-create))

;; has to happen after load bc doom sets capture templates at load time.
;; incidentally also means we cannot use custom-file to config them.
;; to remove the offender, do (remove-hook 'org-load-hook #'+org-init-capture-defaults-h)
;; (after! org
;;   (setopt org-capture-templates
;;         `(
;;           ("p" "Person of history" entry
;;            (file "/home/kept/roam/2021-08-27-historical-people.org")
;;            ,(lines "* %^{Name} :stub:"
;;                    ":PROPERTIES:"
;;                    ":ID:       %(org-id-uuid)"
;;                    ":ROAM_ALIASES: \"%\\1 (%\\2)\""
;;                    ":END:"
;;                    "%^{Years of birth and death (YYYY--YYYY)}"
;;                    "%?"))

;;           ;; TODO: Number it by a counter
;;           ("u" "untitled note" plain
;;            (file "/home/kept/roam/untitled-notes.org")
;;            "* \[%<%Y-%m-%d %T>\]
;; :PROPERTIES:
;; :ID:  %(org-id-uuid)
;; :END:
;; :DATE: \[%<%Y-%m-%d>\]
;; %i%?
;; %a")
;;           (";" "firefox capture" plain
;;            (file "/tmp/captures.org")
;;            "* %a
;; :PROPERTIES:
;; :ID:  %(org-id-uuid)
;; :END:
;; :DATE: \[%<%Y-%m-%d>\]
;; %i%?")

;;           ("e" "Emacs idea" entry (file+headline "/home/kept/roam/2021-08-27-someday_maybe.org" "Ideas"))
;;           ("q" "Statistics question" entry (file+headline "/home/kept/roam/stats.org" "Questions"))
;;           ("t" "Statistics header" entry (file+headline "/home/kept/roam/stats.org" "Statistics"))
;;           ("s" "Someday/Maybe" entry (file+headline "/home/kept/roam/2021-08-27-someday_maybe.org" "Unsorted"))
;;           ("m" "A Virtual Assistant function")
;;           ("mw" "weight" plain (function eva-session-new) :immediate-finish t)
;;           ("mf" "visit Ledger file" plain (function eva-present-ledger-file) :immediate-finish t)

;;           ("ln" "From Nordea" plain (file "/home/kept/self-data/clean_start.ledger")
;;            ,(lines "%<%Y-%m-%d> * \"\""
;;                    "    Expenses   %?"
;;                    "    Assets:Nordea:Personkonto")
;;            :empty-lines 1
;;            :jump-to-captured t)

;;           ("li" "InvestNotSpend" plain (file "/home/kept/self-data/clean_start.ledger")
;;            ,(lines "%<%Y-%m-%d> ! \"\""
;;                    "    [Assets:Lysa:InvestNotSpend]   %?"
;;                    "    Assets:Nordea:Personkonto")
;;            :empty-lines 1
;;            :jump-to-captured t)

;;           ("r" "Retroactive clock" entry (file+olp+datetree "/home/kept/archive/journal/diary.org")
;;            ,(lines "* %^{Activity|School|Piano|Signing|Coding}"
;;                    "CLOCK: %^{Time at start}U--%^{Time at finish}U => %^{Rough time spent (sorry, the program is dumb), H:MM}")
;;            :immediate-finish t)

;;           )))

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
