;; -*- lexical-binding: t; -*-
;;; Commentary:

;; One day, I might relocate these settings to be more context-relevant, then
;; consult the total list with (general-describe-keybindings), although that
;; one is cluttered by Doom's settings, or bind-key's
;; (describe-personal-keybindings). These outputs could even be exported to a
;; table for the curious in case I publish my config. But I enjoy handling them
;; this way, and the upshot is that I still have all my bindings when half my
;; init has broken.

;;; Code:

(require 'my-lib-unprefixed)
(require 'my-lib)
(require 'defrepeater) ;; not needed if i would just remember to call `repeat'!
(require 'define-repeat-map)

;; Backport from Emacs 29 in case I'm on 28
(unless (version<= "29" emacs-version)
  (require 'general)
  (defmacro keymap-unset (a b &optional _c)
    `(general-unbind ,a ,b))
  (defmacro keymap-set (&rest args)
    `(general-def ,@args)))


;;; Clean house a bit

;; Nice commands I discovered
;; "C-]" ;; abort-recursive-edit
;; C-x i ;; insert-file

;; Unbind commands I never used or won't use anymore.
(keymap-unset global-map "<f2>" t) ;; 2C-command
(keymap-unset global-map "<f5>" t) ;; NOTE: which-key-paging-key is this by default
(keymap-unset global-map "<f6>" t)
(keymap-unset global-map "<f7>" t)
(keymap-unset global-map "<f8>" t)
(keymap-unset global-map "<f9>" t)
(keymap-unset global-map "<f10>" t) ;; menu-bar-open
(keymap-unset global-map "<insert>" t) ;; overwrite-mode
(keymap-unset global-map "C-o" t) ;; open-line
(keymap-unset global-map "C-z" t) ;; suspend-frame
(keymap-unset global-map "C-\\" t) ;; toggle-input-method
(keymap-unset global-map "M-." t) ;; xref-find-definitions
(keymap-unset global-map "M-`" t) ;; tmm-menubar
(keymap-unset global-map "M-i" t) ;; tab-to-tab-stop
(keymap-unset global-map "M-j" t) ;; default-indent-new-line
(keymap-unset global-map "M-m" t) ;; back-to-indentation
(keymap-unset global-map "M-o" t) ;; facemenu-keymap
(keymap-unset global-map "M-r" t) ;; move-to-window-line-top-bottom
(keymap-unset global-map "M-z" t) ;; zap-to-char
(keymap-unset global-map "M-~" t) ;; not-modified
(keymap-unset global-map "C-x k" t) ;; Discourage unproductive behavior
(keymap-unset global-map "C-x C-z" t)
(keymap-unset global-map "C-x z" t)
(keymap-unset global-map "C-x (" t)
(keymap-unset global-map "C-x )" t)
(keymap-unset global-map "C-x *" t)
(keymap-unset global-map "C-x DEL" t) ;; use M-- M-k

;; Unbind keys whose commands I'll be putting on new keys anyway.
(keymap-unset global-map "C-u" t) ;; universal-argument
(keymap-unset global-map "C-q" t) ;; quoted-insert
(keymap-unset global-map "M-q" t) ;; fill-paragraph
(keymap-unset global-map "<f3>" t) ;; kmacro-start-macro-or-insert-counter
(keymap-unset global-map "<f4>" t) ;; kmacro-end-or-call-macro
(keymap-unset global-map "C-x SPC" t)
(keymap-unset global-map "C-x C-SPC" t)
(keymap-unset global-map "C-SPC" t)

;; these unbindings hurt too much until I have more modal editing (deianira)
;; (general-unbind "<f1>")
;; (general-unbind "<down>")
;; (general-unbind "<left>")
;; (general-unbind "<next>")
;; (general-unbind "<prior>")
;; (general-unbind "<right>")
;; (general-unbind "<up>")
;; (general-unbind "RET")

;; removing these has all kinds of consequences, why I'll use super instead of ctl one day
;; (general-unbind "C-g") ;; keyboard-quit
;; (general-unbind "C-j") ;; newline
;; (general-unbind "C-i")
;; (general-unbind "C-]")
;; (general-unbind "C-m")


;;; Fix prefix arguments

(keymap-unset universal-argument-map "C-u" t)
(keymap-set universal-argument-map "-" #'negative-argument)

;; Don't waste good keys (C-123456890) on digit-argument.
;; But make it more convenient to access them in other ways.
(let ((modifiers '("C-" "M-" "s-" "H-" "A-"))
      (digits (split-string "1234567890" "" t)))
  (dolist (d digits)
    (keymap-set universal-argument-map (kbd d) #'digit-argument))
  (dolist (mod modifiers)
    (keymap-set global-map (concat mod "-") #'negative-argument)
    (keymap-set global-map (concat mod "=") #'universal-argument)
    (keymap-set universal-argument-map (concat mod "=") #'universal-argument-more)
    (dolist (d digits)
      (define-key global-map (kbd (concat mod d)) nil) ;; clear space
      (define-key universal-argument-map (kbd (concat mod d)) #'digit-argument))))


;;; More repeaters

;; Here's how to plug commands into Emacs 28's repeat-mode so they
;; automatically will be repeaters without having to make a separate command
;; with `defrepeater'. Basically, first make a keymap (this is the typing-heavy
;; part that define-repeat-mode helps you with).  Then set a symbol property
;; `repeat-map' on the appropriate command, to point to this keymap. For `forward-page':
;;
;; (defvar page-navigation-repeat-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "]" #'forward-page)
;;     (define-key map "[" #'backward-page)
;;     map)
;;   "Keymap to repeat page navigation key sequences.  Used in `repeat-mode'.")
;;
;; (put 'forward-page 'repeat-map 'page-navigation-repeat-map)
;; (put 'backward-page 'repeat-map 'page-navigation-repeat-map)

;; Great little package, see https://tildegit.org/acdw/define-repeat-map.el
(after! define-repeat-map
  (define-repeat-map my-buffer-thumbing
    ("<right>"   next-buffer
     "C-<right>" next-buffer
     "<left>"   previous-buffer
     "C-<left>" previous-buffer))

  (define-repeat-map my-case
    ("c" capitalize-word
     "u" upcase-word
     "l" downcase-word)
    (:continue "f" forward-word
               "b" backward-word)
    (:enter downcase-dwim
            upcase-dwim
            capitalize-dwim)))

;; While we're at it, enhance the classic `repeat'.  Note that it's totally
;; separate from the Emacs 28 repeat-map system.

;; Let me type a digit such as 5 after a `repeat' to repeat another 5 times.
(advice-add #'repeat :after #'my-enable-post-repeat-transient-map)


;;; Main

(defvar my-abbrev-minor-mode-map (make-sparse-keymap))
(add-to-list 'minor-mode-map-alist (cons 'abbrev-mode my-abbrev-minor-mode-map))

(defvar my-anki-editor-mode-map (make-sparse-keymap))
(add-to-list 'minor-mode-map-alist (cons 'anki-editor-mode my-anki-editor-mode-map))

;; (run-hooks 'my-before-keybinds-hook)

(when (boundp 'doom-version)
  (keymap-unset global-map "M--" t)
  (keymap-unset global-map "M-=" t)
  (keymap-unset global-map "C-'" t) ;; imenu
  (setq doom-leader-alt-key "<f3>")
  (setq doom-localleader-alt-key "<f4>"))

(when (boundp 'spacemacs-version)
  (keymap-unset elisp-slime-nav-mode-map "M-," t)
  (keymap-unset elisp-slime-nav-mode-map "M-." t)
  (keymap-unset evil-emacs-state-map "C-z" t))

(after! key-chord
  (key-chord-define-global "cd" #'calc-dispatch))

(after! ranger
  ;; Don't take my M-1234567890
  (let ((digits (split-string "1234567890" "" t)))
    (dolist (d digits)
      (keymap-unset ranger-normal-mode-map (concat "M-" d) t)
      (keymap-unset ranger-emacs-mode-map (concat "M-" d) t))))

(after! magit
  ;; Don't take my M-1234567890
  (keymap-unset magit-section-mode-map "M-1" t)
  (keymap-unset magit-section-mode-map "M-2" t)
  (keymap-unset magit-section-mode-map "M-3" t)
  (keymap-unset magit-section-mode-map "M-4" t))

(after! geiser-mode
  (keymap-unset geiser-mode-map "M-," t)
  (keymap-unset geiser-mode-map "M-." t)
  (keymap-unset geiser-mode-map "M-`" t))

(after! geiser-repl
  (keymap-unset geiser-repl-mode-map "M-," t)
  (keymap-unset geiser-repl-mode-map "M-." t)
  (keymap-unset geiser-repl-mode-map "M-`" t))

(after! cus-edit
  (keymap-set custom-mode-map "q" #'kill-current-buffer))

(after! ess-mode
  (keymap-set ess-mode-map "<f1> <f2>" #'ess-abort)
  (keymap-set ess-mode-map "<f1> <f3>" #'ess-interrupt)
  (keymap-set ess-mode-map "C-<return>" #'ess-eval-line))

(after! eww
  (keymap-set eww-mode-map "q" #'kill-current-buffer))

(after! vertico
  (keymap-unset vertico-map "<backspace>" t) ;; undoom
  (keymap-set vertico-map "M-<backspace>" #'vertico-directory-up)
  (keymap-set vertico-map "<next>" #'scroll-up-command)
  (keymap-set vertico-map "<prior>" #'scroll-down-command))

(after! ledger-mode
  (keymap-set ledger-mode-map "M-<return>" #'crux-duplicate-current-line-or-region))

(after! shell
  (keymap-set shell-mode-map "C-S-n" #'my-new-shell))

(after! ctrlf
  (keymap-set ctrlf-minibuffer-mode-map "<down>"   #'ctrlf-forward-default)
  (keymap-set ctrlf-minibuffer-mode-map "M-<down>" #'ctrlf-forward-alternate)
  (keymap-set ctrlf-minibuffer-mode-map "<up>"     #'ctrlf-backward-default)
  (keymap-set ctrlf-minibuffer-mode-map "M-<up>"   #'ctrlf-backward-alternate))

;; Ensure that the key physically labelled "Caps Lock" works as my M-x.  Aside
;; from this lisp, we also need the xkb option caps:menu so it emits <menu>.
;; TIP: it also unlocks the comfortable key M-<menu>.
(keymap-set global-map "<menu>" #'execute-extended-command)
(keymap-set global-map "M-<menu>" #'embark-act)

;; Grand list
(keymap-set key-translation-map "<f7> a" (kbd "α"))
(keymap-set key-translation-map "<f7> b" (kbd "β"))
(keymap-set key-translation-map "<f7> c" (kbd "γ"))
(keymap-set key-translation-map "<f7> d" (kbd "δ"))
(keymap-set key-translation-map "<f7> e" (kbd "ε"))
(keymap-set key-translation-map "<f7> f" (kbd "φ"))
(keymap-set key-translation-map "<f7> g" (kbd "θ"))
(keymap-set key-translation-map "<f7> l" (kbd "λ"))
(keymap-set key-translation-map "<f7> m" (kbd "μ"))
(keymap-set key-translation-map "<f7> p" (kbd "π"))
(keymap-set key-translation-map "<f7> r" (kbd "ρ"))
(keymap-set key-translation-map "<f7> s" (kbd "σ"))
(keymap-set key-translation-map "<f7> t" (kbd "τ"))
(keymap-set key-translation-map "<f7> o" (kbd "ω"))
(keymap-set key-translation-map "<f7> x" (kbd "ξ"))
(keymap-set key-translation-map "<f7> A" (kbd "Α"))
(keymap-set key-translation-map "<f7> B" (kbd "Β"))
(keymap-set key-translation-map "<f7> C" (kbd "Γ"))
(keymap-set key-translation-map "<f7> D" (kbd "Δ"))
(keymap-set key-translation-map "<f7> E" (kbd "Ε"))
(keymap-set key-translation-map "<f7> F" (kbd "Φ"))
(keymap-set key-translation-map "<f7> G" (kbd "Θ"))
(keymap-set key-translation-map "<f7> L" (kbd "Λ"))
(keymap-set key-translation-map "<f7> M" (kbd "Μ"))
(keymap-set key-translation-map "<f7> P" (kbd "Π"))
(keymap-set key-translation-map "<f7> R" (kbd "Ρ"))
(keymap-set key-translation-map "<f7> S" (kbd "Σ"))
(keymap-set key-translation-map "<f7> T" (kbd "Τ"))
(keymap-set key-translation-map "<f7> O" (kbd "Ω"))
(keymap-set key-translation-map "<f7> X" (kbd "Ξ"))
(keymap-set isearch-mode-map "M-s n"  #'isearch-repeat-forward)
(keymap-set isearch-mode-map "<down>" #'isearch-repeat-forward)
(keymap-set isearch-mode-map "M-s p"  #'isearch-repeat-backward)
(keymap-set isearch-mode-map "<up>"   #'isearch-repeat-backward)
(keymap-set my-abbrev-minor-mode-map "`" #'expand-abbrev)
(keymap-set embark-general-map "C-\\"   #'hkey-either)
(keymap-set global-map "<f5>"               #'repeat)
(keymap-set global-map "C-\\"                 #'embark-act)
(keymap-set global-map "C-<next>"                   #'next-buffer)
(keymap-set global-map "C-<prior>"                  #'previous-buffer)
(keymap-set global-map "C-g"                        #'keyboard-quit)
(keymap-set global-map "M-0"                        #'hippie-expand)
(keymap-set global-map "M-1"                        #'switch-to-buffer)
(keymap-set global-map "M-2"                        #'other-window)
(keymap-set global-map "M-3"                        #'unexpand-abbrev)
(keymap-set global-map "M-5"                        #'my-prev-file-in-dir)
(keymap-set global-map "M-6"                        #'my-next-file-in-dir)
(keymap-set global-map "M-8"                        #'kill-whole-line)
(keymap-set global-map "M-9"                        #'crux-duplicate-current-line-or-region)
(keymap-set global-map "M-<backspace>"              #'sp-backward-unwrap-sexp)
(keymap-set global-map "M-<delete>"                 #'sp-unwrap-sexp)
(keymap-set global-map "M-<f4>"                     #'kill-current-buffer)
(keymap-set global-map "M-<insert>"                 #'sp-rewrap-sexp)
(keymap-set global-map "M-|"                        #'my-shell-command-replace-region)
(keymap-set global-map "TAB"                        #'my-tab-command)
(keymap-set global-map "<f10> p" #'my-pipe)
(keymap-set global-map "<f2> <next>" (defrepeater #'my-next-buffer-of-same-mode))
(keymap-set global-map "<f2> <prior>" (defrepeater #'my-previous-buffer-of-same-mode))
(keymap-set global-map "<f10> r v" #'my-replace-var-at-point-with-value)
(keymap-set global-map "<f10> r p" (defrepeater #'my-cycle-path-at-point))
(keymap-set global-map "<f10> r c" #'my-copy-region-to-variable)
(keymap-set global-map "<f10> r w" #'my-copy-region-or-rest-of-line-to-other-window)
(keymap-set global-map "<f10> r e" #'my-eval-and-replace-print)
(keymap-set global-map "<f10> a"                    #'my-save-buffer-and-amend)
(keymap-set global-map "<f10> d"                    #'org-download-yank)
(keymap-set global-map "<f10> e"                    #'eww)
(keymap-set global-map "<f10> g"                    #'guix-popup)
(keymap-set global-map "<f10> k"                    #'gif-screencast-start-or-stop)
(keymap-set global-map "<f10> l"                    #'mw-thesaurus-lookup)
(keymap-set global-map "<f10> n"                    #'my-normie-toggle)
(keymap-set global-map "<f10> s"                    #'my-save-buffer-and-commit)
(keymap-set global-map "<f2> 1"        (defrepeater #'my-insert-other-buffer-file-name-and-cycle))
(keymap-set global-map "<f2> 2"        (defrepeater #'my-toggle-selective-display))
(keymap-set global-map "<f2> 3"                     #'elfeed)
(keymap-set global-map "<f2> 5"                     #'my-lookup-word)
(keymap-set global-map "<f2> <f1>"                  #'my-describe-last-key)
(keymap-set global-map "<f2> <f2>"                  #'vc-msg-show)
(keymap-set global-map "<f2> <f3>"                  #'git-messenger:popup-message)
(keymap-set global-map "<f2> b"                     #'backup-walker-start)
(keymap-set global-map "<f2> c"                     #'org-roam-capture)
(keymap-set global-map "<f2> d"                     #'my-insert-today)
(keymap-set global-map "<f2> e d"                   #'eval-defun)
(keymap-set global-map "<f2> e e"                   #'eval-last-sexp)
(keymap-set global-map "<f2> e p"                   #'eval-print-last-sexp)
(keymap-set global-map "<f2> e r"                   #'eval-region)
(keymap-set global-map "<f2> e l"                   #'load-library)
(keymap-set global-map "<f2> e s"                   #'ess-eval-region-or-function-or-paragraph-and-step) ;; ess everywhere
(keymap-set global-map "<f2> e x"                   #'eval-expression)
(keymap-set global-map "<f2> f d"                   #'crux-delete-file-and-buffer)
(keymap-set global-map "<f2> f f"                   #'toggle-frame-fullscreen)
(keymap-set global-map "<f2> f m"                   #'toggle-frame-maximized)
(keymap-set global-map "<f2> f r"                   #'crux-rename-file-and-buffer)
(keymap-set global-map "<f2> g"                     #'git-timemachine)
(keymap-set global-map "<f2> i"                     #'my-suggest-sub)
(keymap-set global-map "<f2> j"                     #'project-find-file)
(keymap-set global-map "<f2> k"                     #'+vertico/project-search)
(keymap-set global-map "<f2> l"                     #'helm-locate)
(keymap-set global-map "<f2> m"                     #'my-show-my-files)
(keymap-set global-map "<f2> p"                     #'my-spawn-process)
(keymap-set global-map "<f2> \("                    #'app-launcher-run-app)
(keymap-set global-map "<f2> r"                     #'vertico-repeat)
(keymap-set global-map "<f2> s"                     #'helm-selector-shell)
(keymap-set global-map "<f2> w"                     #'sp-rewrap-sexp)
(keymap-set global-map "<f2> x"                     #'execute-extended-command)
(keymap-set global-map "<f2> z"                     #'my-sleep)
(keymap-set global-map "C-x C-\;"      (defrepeater #'comment-line))
(keymap-set global-map "C-h q"                      #'quoted-insert)
(keymap-set global-map "M-g a a"       (defrepeater #'avy-pop-mark))
(keymap-set global-map "M-g a c"                    #'avy-goto-char-2)
(keymap-set global-map "M-g a g c"                  #'avy-goto-char-2)
(keymap-set global-map "M-g a g e"                  #'avy-goto-end-of-line)
(keymap-set global-map "M-g a g l"                  #'avy-goto-line)
(keymap-set global-map "M-g a g o"                  #'avy-goto-word-or-subword-1)
(keymap-set global-map "M-g a g q"                  #'avy-goto-subword-1)
(keymap-set global-map "M-g a g s"                  #'avy-goto-symbol-1)
(keymap-set global-map "M-g a g w"                  #'avy-goto-word-1)
(keymap-set global-map "M-r"                        #'goto/body)
(keymap-set global-map "M-g a k"                    #'avy-kill-region)
(keymap-set global-map "M-g a m l"                  #'avy-move-line)
(keymap-set global-map "M-g a m r"                  #'avy-move-region)
(keymap-set global-map "M-g a n"                    #'avy-next)
(keymap-set global-map "M-g a o"                    #'avy-goto-symbol)
(keymap-set global-map "M-g a p"                    #'avy-prev)
(keymap-set global-map "M-g a r"                    #'avy-resume)
(keymap-set global-map "M-g a s"                    #'avy-isearch)
(keymap-set global-map "M-g a w"                    #'avy-kill-ring-save-region)
(keymap-set global-map "M-g c"                      #'avy-goto-char-timer)
(keymap-set global-map "M-g z"                      #'avy-goto-word-or-subword-1)
(keymap-set global-map "M-m g"         (defrepeater #'pop-global-mark)) ;; was C-x C-SPC
(keymap-set global-map "M-m m"                      #'set-mark-command) ;; was C-SPC
(keymap-set global-map "M-m p"         (defrepeater #'pop-to-mark-command))
(keymap-set global-map "M-m r"                      #'rectangle-mark-mode) ;; was C-x SPC
(keymap-set global-map "M-m x"                      #'exchange-point-and-mark) ;; also on C-x C-x
(keymap-set global-map "M-o ="                      #'doom/increase-font-size)
(keymap-set global-map "M-s 5"                      #'query-replace)
(keymap-set global-map "M-s f"         (defrepeater #'my-fill-unfill-respect-double-space))
(keymap-set global-map "M-s r"                      #'isearch-backward)
(keymap-set global-map "M-s s"                      #'isearch-forward)
(keymap-set global-map "<f2> n"                     #'org-roam-dailies-capture-today)

;; Smartparens guide: https://gist.github.com/pvik/8eb5755cc34da0226e3fc23a320a3c95
;; Author's config: https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
;; Xah's simplification: https://old.reddit.com/r/emacs/comments/3sfmkz/could_this_be_a_pareditsmartparens_killer/cwxocld/
(after! smartparens
  (keymap-set smartparens-strict-mode-map ";" #'sp-comment)
  (keymap-set global-map "C-'" #'sp-mark-sexp)
  (keymap-set global-map "C-;" #'sp-comment)
  (keymap-set global-map "C-<left>" #'sp-forward-barf-sexp)
  (keymap-set global-map "C-<right>" #'sp-forward-slurp-sexp)
  (keymap-set global-map "C-M-<left>" #'sp-backward-slurp-sexp)
  (keymap-set global-map "C-M-<right>" #'sp-backward-barf-sexp)
  (keymap-set global-map "M-<backspace>" #'sp-backward-unwrap-sexp)
  (keymap-set global-map "M-<delete>" #'sp-unwrap-sexp)
  (keymap-set global-map "s-<SPC>" #'sp-mark-sexp)
  (keymap-set global-map "s-<left>" #'sp-backward-slurp-sexp)
  (keymap-set global-map "s-<right>" #'sp-backward-barf-sexp)
  (keymap-set global-map "s-<delete>" #'sp-splice-sexp-killing-forward)
  (keymap-set global-map "s-<backspace>" #'sp-splice-sexp-killing-backward)
  (keymap-set global-map "s-a" #'sp-backward-down-sexp)
  (keymap-set global-map "s-b" #'sp-backward-sexp)
  (keymap-set global-map "s-d" #'sp-down-sexp)
  (keymap-set global-map "s-e" #'sp-up-sexp)
  (keymap-set global-map "s-f" #'sp-forward-sexp)
  (keymap-set global-map "s-h" #'sp-mark-sexp)
  (keymap-set global-map "s-k" #'sp-kill-sexp)
  (keymap-set global-map "s-n" #'sp-next-sexp)
  (keymap-set global-map "s-p" #'sp-previous-sexp)
  (keymap-set global-map "s-t" #'sp-transpose-sexp)
  (keymap-set global-map "s-u" #'sp-backward-up-sexp)
  (keymap-set global-map "M-[" #'sp-wrap-round)
  ;; TODO: use keymap-substitute?
  (define-key global-map [remap kill-whole-line] #'sp-kill-whole-line))

;; Unassimilated Smartparens commands to try out.
;; (keymap-set "C-2 a" #'sp-join-sexp)
;; (keymap-set "C-2 b" #'sp-select-next-thing)
;; (keymap-set "C-2 c" #'sp-beginning-of-sexp)
;; (keymap-set "C-2 d" #'sp-beginning-of-next-sexp)
;; (keymap-set "C-2 e" #'sp-end-of-sexp)
;; (keymap-set "C-2 f" #'sp-add-to-next-sexp)
;; (keymap-set "C-2 g" #'sp-add-to-previous-sexp)
;; (keymap-set "C-2 h" #'sp-split-sexp)
;; (keymap-set "C-2 i" #'sp-splice-sexp)
;; (keymap-set "C-2 j" #'sp-emit-sexp)
;; (keymap-set "C-2 k" #'sp-absorb-sexp)
;; (keymap-set "C-2 l" #'sp-convolute-sexp)
;; (keymap-set "C-2 m" #'sp-forward-symbol)
;; (keymap-set "C-2 n" #'sp-backward-symbol)
;; (keymap-set "C-2 o" #'sp-wrap)
;; (keymap-set "C-2 p" #'sp-backward-up-sexp)
;; (keymap-set "C-2 q" #'sp-up-sexp)
;; (keymap-set "C-2 r" #'sp-select-next-thing-exchange)
;; (keymap-set "C-2 s" #'sp-select-previous-thing)

(after! hydra
  (defhydra goto (:color blue :hint nil)
    "
Goto:
^Char^              ^Word^                ^org^                    ^search^
^^^^^^^^---------------------------------------------------------------------------
_c_: 2 chars        _w_: word by char     _h_: headline in buffer  _o_: helm-occur
_C_: char           _W_: some word        _a_: heading in agenda   _p_: helm-swiper
_L_: char in line   _s_: subword by char  _q_: swoop org buffers   _f_: search forward
^  ^                _S_: some subword     ^ ^                      _b_: search backward
-----------------------------------------------------------------------------------
_B_: helm-buffers       _l_: avy-goto-line
_m_: helm-mini          _i_: ace-window
_R_: helm-recentf

_n_: Navigate           _._: mark position _/_: jump to mark
"
    ("c" avy-goto-char-2)
    ("C" avy-goto-char)
    ("L" avy-goto-char-in-line)
    ("w" avy-goto-word-1)
    ;; jump to beginning of some word
    ("W" avy-goto-word-0)
    ;; jump to subword starting with a char
    ("s" avy-goto-subword-1)
    ;; jump to some subword
    ("S" avy-goto-subword-0)

    ("l" avy-goto-line)
    ("i" ace-window)

    ("h" helm-org-headlines)
    ("a" helm-org-agenda-files-headings)
    ("q" helm-multi-swoop-org)

    ("o" helm-occur)
    ("p" swiper-helm)

    ("f" isearch-forward)
    ("b" isearch-backward)

    ("." org-mark-ring-push :color red)
    ("/" org-mark-ring-goto :color blue)
    ("B" helm-buffers-list)
    ("m" helm-mini)
    ("R" helm-recentf)
    ("n" hydra-navigate/body)))

(my-normie:abnormalize)

;; (run-hooks 'my-after-keybinds-hook)
