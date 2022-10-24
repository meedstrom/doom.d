;; -*- lexical-binding: t; -*-
;;; Commentary:

;; One day, I might relocate these settings to be more context-relevant, then
;; consult the total list with (general-describe-keybindings), although that one
;; is cluttered by Doom's settings, or bind-key's
;; (describe-personal-keybindings). These outputs could even be exported to a
;; table for the curious in case I publish my config. But I enjoy handling them
;; this way, and the upshot with setting the bindings this early is that I still
;; have them all when half my init has broken.

;;; Code:

(require 'defrepeater) ;; not needed if i would just remember to call `repeat'!
(require 'define-repeat-map)


;;; Clean house a bit

;; Nice commands I discovered
;; "C-]" ;; abort-recursive-edit
;; C-x i ;; insert-file

;; Unbind commands on too good locations (the risk is that I get used to them).
(keymap-unset global-map "<f2>" t) ;; 2C-command
(keymap-unset global-map "<f3>" t) ;; kmacro-start-macro-or-insert-counter
(keymap-unset global-map "<f4>" t) ;; kmacro-end-or-call-macro
(keymap-unset global-map "<f5>" t) ;; NOTE: which-key-paging-key is here by default
(keymap-unset global-map "<f6>" t)
(keymap-unset global-map "<f7>" t)
(keymap-unset global-map "<f8>" t)
(keymap-unset global-map "<f9>" t)
(keymap-unset global-map "<f10>" t) ;; menu-bar-open
(keymap-unset global-map "<insert>" t) ;; overwrite-mode
(keymap-unset global-map "C-SPC" t)
(keymap-unset global-map "C-\\" t) ;; toggle-input-method
(keymap-unset global-map "C-o" t) ;; open-line
(keymap-unset global-map "C-q" t) ;; quoted-insert
(keymap-unset global-map "C-x (" t)
(keymap-unset global-map "C-x )" t)
(keymap-unset global-map "C-x *" t)
(keymap-unset global-map "C-x C-SPC" t)
(keymap-unset global-map "C-x C-z" t)
(keymap-unset global-map "C-x DEL" t) ;; bro just use M-- M-k
(keymap-unset global-map "C-x SPC" t)
(keymap-unset global-map "C-x k" t) ;; Discourage unproductive behavior
(keymap-unset global-map "C-x z" t)
(keymap-unset global-map "C-z" t) ;; suspend-frame
(keymap-unset global-map "M-." t) ;; xref-find-definitions
(keymap-unset global-map "M-`" t) ;; tmm-menubar
(keymap-unset global-map "M-i" t) ;; tab-to-tab-stop
(keymap-unset global-map "M-j" t) ;; default-indent-new-line
(keymap-unset global-map "M-m" t) ;; back-to-indentation
(keymap-unset global-map "M-o" t) ;; facemenu-keymap
(keymap-unset global-map "M-q" t) ;; fill-paragraph
(keymap-unset global-map "M-r" t) ;; move-to-window-line-top-bottom
(keymap-unset global-map "M-z" t) ;; zap-to-char
(keymap-unset global-map "M-~" t) ;; not-modified

;; Unbinding these has all kinds of consequences, why I'll migrate to Super one
;; day and deprecate Control, using Control only outside Emacs and use Super
;; only inside Emacs.  Same idea employed by Mac OS, but I include modern GUI
;; apps -- all apps other than Emacs -- in the "legacy" category of things to
;; be operated with Control.  That will make EXWM run like a dream.
;;
;; (general-unbind "C-g") ;; keyboard-quit
;; (general-unbind "C-j") ;; newline
;; (general-unbind "C-i")
;; (general-unbind "C-]")
;; (general-unbind "C-m")

;; these unbindings hurt too much until I have more modal editing (deianira)
;; (general-unbind "<f1>")
;; (general-unbind "<down>")
;; (general-unbind "<left>")
;; (general-unbind "<next>")
;; (general-unbind "<prior>")
;; (general-unbind "<right>")
;; (general-unbind "<up>")
;; (general-unbind "<return>")

(when (boundp 'doom-version)
  (keymap-unset global-map "C-'" t) ;; imenu
  (keymap-unset global-map "M--" t)
  (keymap-unset global-map "M-=" t))

(when (boundp 'spacemacs-version)
  (keymap-unset elisp-slime-nav-mode-map "M-," t)
  (keymap-unset elisp-slime-nav-mode-map "M-." t)
  (keymap-unset evil-emacs-state-map "C-z" t))


;;; Fix prefix arguments

(keymap-unset global-map "C-u" t)
(keymap-unset universal-argument-map "C-u" t)
(keymap-set universal-argument-map "-" #'negative-argument)

;; Don't waste good keys (C-123456890) on digit-argument.  But make it more
;; convenient to access them in other ways.
(let ((modifiers '("C-" "M-" "s-" "H-" "A-"))
      (digits (split-string "1234567890" "" t)))
  (dolist (d digits)
    (keymap-set universal-argument-map d #'digit-argument))
  (dolist (mod modifiers)
    (keymap-set global-map (concat mod "-") #'negative-argument)
    (keymap-set global-map (concat mod "=") #'universal-argument)
    (keymap-set universal-argument-map (concat mod "=") #'universal-argument-more)
    (dolist (d digits)
      (keymap-unset global-map (concat mod d) t)
      (keymap-set universal-argument-map (concat mod d) #'digit-argument))))


;;; More repeaters! Repeaters are love and life.

;; https://tildegit.org/acdw/define-repeat-map.el
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


;;; Create minor mode maps for modes that lack them

(defvar my-abbrev-minor-mode-map (make-sparse-keymap))
(add-to-list 'minor-mode-map-alist (cons 'abbrev-mode my-abbrev-minor-mode-map))

(defvar my-anki-editor-mode-map (make-sparse-keymap))
(add-to-list 'minor-mode-map-alist (cons 'anki-editor-mode my-anki-editor-mode-map))


;;; Main

;; (run-hooks 'my-before-keybinds-hook)

(setopt doom-leader-alt-key "<f3>")
(setopt doom-localleader-alt-key "<f4>")

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

(after! dired-hist
  (keymap-set dired-mode-map "l" #'dired-hist-go-back)
  (keymap-set dired-mode-map "L" #'dired-hist-go-forward))

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

(after! dired
  ;; Dired default unbound keys: `, b, E, J, K, r, z, <backspace>
  ;; Dired useless keys: h, 1234567890

  ;; Easy swapping between dired and eshell
  (keymap-set dired-mode-map "r"  #'my-eshell-here))

(after! shell
  (keymap-set shell-mode-map "C-S-n" #'my-new-shell))

(after! em-hist
  ;; be docile like M-x shell (don't "hijack" point)
  (keymap-unset eshell-hist-mode-map "<up>")
  (keymap-unset eshell-hist-mode-map "<down>"))

(after! eshell
  (keymap-set eshell-mode-map "C-S-n" #'my-new-eshell)
  (keymap-set eshell-mode-map "<f4> n" (defrepeater #'my-esh-narrow-dwim)))

(after! ctrlf
  (keymap-set ctrlf-minibuffer-mode-map "<down>"   #'ctrlf-forward-default)
  (keymap-set ctrlf-minibuffer-mode-map "M-<down>" #'ctrlf-forward-alternate)
  (keymap-set ctrlf-minibuffer-mode-map "<up>"     #'ctrlf-backward-default)
  (keymap-set ctrlf-minibuffer-mode-map "M-<up>"   #'ctrlf-backward-alternate))

;; Smartparens guide: https://gist.github.com/pvik/8eb5755cc34da0226e3fc23a320a3c95
;; Author's config: https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
;; Xah's simplification: https://old.reddit.com/r/emacs/comments/3sfmkz/could_this_be_a_pareditsmartparens_killer/cwxocld/
(after! smartparens
  (keymap-set smartparens-strict-mode-map ";" #'sp-comment)
  (keymap-set "C-'" #'sp-mark-sexp)
  (keymap-set "C-;" #'sp-comment)
  (keymap-set "C-<left>" #'sp-forward-barf-sexp)
  (keymap-set "C-<right>" #'sp-forward-slurp-sexp)
  (keymap-set "C-M-<left>" #'sp-backward-slurp-sexp)
  (keymap-set "C-M-<right>" #'sp-backward-barf-sexp)
  (keymap-set "M-<backspace>" #'sp-backward-unwrap-sexp)
  (keymap-set "M-<delete>" #'sp-unwrap-sexp)
  (keymap-set "s-<SPC>" #'sp-mark-sexp)
  (keymap-set "s-<left>" #'sp-backward-slurp-sexp)
  (keymap-set "s-<right>" #'sp-backward-barf-sexp)
  (keymap-set "s-<delete>" #'sp-splice-sexp-killing-forward)
  (keymap-set "s-<backspace>" #'sp-splice-sexp-killing-backward)
  (keymap-set "s-a" #'sp-backward-down-sexp)
  (keymap-set "s-b" #'sp-backward-sexp)
  (keymap-set "s-f" #'sp-forward-sexp)
  (keymap-set "s-d" #'sp-down-sexp)
  (keymap-set "s-e" #'sp-up-sexp)
  (keymap-set "s-h" #'sp-mark-sexp)
  (keymap-set "s-k" #'sp-kill-sexp)
  (keymap-set "s-n" #'sp-next-sexp)
  (keymap-set "s-p" #'sp-previous-sexp)
  (keymap-set "s-t" #'sp-transpose-sexp)
  (keymap-set "s-u" #'sp-backward-up-sexp)
  (keymap-set "M-[" #'sp-wrap-round)
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

(after! dired
  (keymap-set dired-mode-map "b" #'dired-up-directory)
  (keymap-set dired-mode-map ")" #'dired-git-info-mode)
  (keymap-set dired-mode-map "M-<up>" #'dired-up-directory)
  (keymap-set dired-mode-map "s-RET" #'my-dired-open-file-with-default-tool)
  ;; undoom
  (keymap-set dired-mode-map "q" #'kill-current-buffer))

(after! general
  ;; dafuq is this set for?
  (general-unbind general-override-mode-map "M-x")
  (general-unbind general-override-mode-map "A-x")
  ;; guess I should take a page from their book
  (general-def general-override-mode-map "<menu>" #'execute-extended-command))

;; Civilize Emacs
(keymap-set input-decode-map "<escape>" "C-g")
(keymap-set input-decode-map "C-g" "s-g") ;; to unlearn

;; ;; Swap F1 and Tab.
;; (keymap-set key-translation-map "TAB" "<f1>")
;; (keymap-set key-translation-map "<tab>" "<f1>")
;; (keymap-set key-translation-map "<f1>" "TAB")

;; ;; Use the key physically labelled "Tab" as my M-x.  It should've been Caps
;; ;; Lock, but there is no Xkb option to make it usable on Chromebook, and Wayland
;; ;; lacks an equivalent to xmodmap.
;; (keymap-set "<f1>" #'execute-extended-command)

;; Use the key physically labelled "Caps Lock" as my M-x.  Aside
;; from this lisp, we also need the Xkb option caps:menu so it emits <menu>.
;; TIP: it also unlocks the comfortable combo M-<menu>.
(when (eq 'window-system 'x)
  (my-exec "setxkbmap" "-option" "caps:menu"))
(keymap-set "<menu>" #'execute-extended-command)
(keymap-set "M-<menu>" #'embark-act)

;; Grand list

(keymap-set view-mode-map "e"            #'my-view-exit-and-reopen-as-root)
(keymap-set "<f10> a"                    #'my-save-buffer-and-amend)
(keymap-set "<f10> d"                    #'org-download-yank)
(keymap-set "<f10> e"                    #'eww)
(keymap-set "<f10> g"                    #'guix-popup)
(keymap-set "<f10> k"                    #'gif-screencast-start-or-stop)
(keymap-set "<f10> l"                    #'mw-thesaurus-lookup)
(keymap-set "<f10> n"                    #'my-normie-toggle)
(keymap-set "<f10> p"                    #'my-pipe)
(keymap-set "<f10> r c"                  #'my-copy-region-to-variable)
(keymap-set "<f10> r e"                  #'my-eval-and-replace-print)
(keymap-set "<f10> r p"     (defrepeater #'my-cycle-path-at-point))
(keymap-set "<f10> r v"                  #'my-replace-var-at-point-with-value)
(keymap-set "<f10> r w"                  #'my-copy-region-or-rest-of-line-to-other-window)
(keymap-set "<f10> s"                    #'my-save-buffer-and-commit)
(keymap-set "<f2> 1"        (defrepeater #'my-insert-other-buffer-file-name-and-cycle))
(keymap-set "<f2> 2"        (defrepeater #'my-toggle-selective-display))
(keymap-set "<f2> 3"                     #'elfeed)
(keymap-set "<f2> 5"                     #'my-lookup-word)
(keymap-set "<f2> <f2>"                  #'vc-msg-show)
(keymap-set "<f2> <f3>"                  #'git-messenger:popup-message)
(keymap-set "<f2> <next>"   (defrepeater #'my-next-buffer-of-same-mode))
(keymap-set "<f2> <prior>"  (defrepeater #'my-previous-buffer-of-same-mode))
(keymap-set "<f2> ("                     #'app-launcher-run-app)
(keymap-set "<f2> b"                     #'backup-walker-start)
(keymap-set "<f2> c"                     #'org-roam-capture)
(keymap-set "<f2> d"                     #'my-insert-today)
(keymap-set "<f2> e d"                   #'eval-defun)
(keymap-set "<f2> e e"                   #'eval-last-sexp)
(keymap-set "<f2> e l"                   #'load-library)
(keymap-set "<f2> e p"                   #'eval-print-last-sexp)
(keymap-set "<f2> e r"                   #'eval-region)
(keymap-set "<f2> e s"                   #'ess-eval-region-or-function-or-paragraph-and-step) ;; ess everywhere
(keymap-set "<f2> e x"                   #'eval-expression)
(keymap-set "<f2> f"                     #'helpful-callable)
(keymap-set "<f2> g"                     #'git-timemachine)
(keymap-set "<f2> h"                     #'consult-find)
(keymap-set "<f2> i"                     #'my-suggest-sub)
(keymap-set "<f2> j"                     #'+vertico/find-file-in)
(keymap-set "<f2> k"                     #'+vertico/project-search)
(keymap-set "<f2> l"                     #'helm-locate)
(keymap-set "<f2> m"                     #'my-show-my-files)
(keymap-set "<f2> n"                     #'org-roam-dailies-capture-today)
(keymap-set "<f2> o"                     #'helpful-symbol)
(keymap-set "<f2> p"                     #'my-spawn-process)
(keymap-set "<f2> r"                     #'vertico-repeat)
(keymap-set "<f2> s"                     #'helm-selector-shell)
(keymap-set "<f2> v"                     #'helpful-variable)
(keymap-set "<f2> w"                     #'sp-rewrap-sexp)
(keymap-set "<f2> x"                     #'execute-extended-command)
(keymap-set "<f2> z"                     #'my-sleep)
(keymap-set "<f5>"                       #'repeat)
(keymap-set "C-<next>"                   #'next-buffer)
(keymap-set "C-<prior>"                  #'previous-buffer)
(keymap-set "C-;"                        #'embark-act) ;; per doom, but globally
(keymap-set "C-\\"                       #'embark-act)
(keymap-set "C-g"                        #'keyboard-quit)
(keymap-set "C-h C-h"                    #'my-describe-last-key)
(keymap-set "C-h q"                      #'quoted-insert)
(keymap-set "C-x C-\;"      (defrepeater #'comment-line))
(keymap-set "M-0"                        #'hippie-expand)
(keymap-set "M-1"                        #'switch-to-buffer)
(keymap-set "M-2"                        #'other-window)
(keymap-set "M-3"                        #'unexpand-abbrev)
(keymap-set "M-5"                        #'my-prev-file-in-dir)
(keymap-set "M-6"                        #'my-next-file-in-dir)
(keymap-set "M-8"                        #'kill-whole-line)
(keymap-set "M-9"                        #'crux-duplicate-current-line-or-region)
(keymap-set "M-<backspace>"              #'sp-backward-unwrap-sexp)
(keymap-set "M-<delete>"                 #'sp-unwrap-sexp)
(keymap-set "M-<f4>"                     #'kill-current-buffer)
(keymap-set "M-<insert>"                 #'sp-rewrap-sexp)
(keymap-set "M-g a a"       (defrepeater #'avy-pop-mark))
(keymap-set "M-g a c"                    #'avy-goto-char-2)
(keymap-set "M-g a g c"                  #'avy-goto-char-2)
(keymap-set "M-g a g e"                  #'avy-goto-end-of-line)
(keymap-set "M-g a g l"                  #'avy-goto-line)
(keymap-set "M-g a g o"                  #'avy-goto-word-or-subword-1)
(keymap-set "M-g a g q"                  #'avy-goto-subword-1)
(keymap-set "M-g a g s"                  #'avy-goto-symbol-1)
(keymap-set "M-g a g w"                  #'avy-goto-word-1)
(keymap-set "M-g a k"                    #'avy-kill-region)
(keymap-set "M-g a m l"                  #'avy-move-line)
(keymap-set "M-g a m r"                  #'avy-move-region)
(keymap-set "M-g a n"                    #'avy-next)
(keymap-set "M-g a o"                    #'avy-goto-symbol)
(keymap-set "M-g a p"                    #'avy-prev)
(keymap-set "M-g a r"                    #'avy-resume)
(keymap-set "M-g a s"                    #'avy-isearch)
(keymap-set "M-g a w"                    #'avy-kill-ring-save-region)
(keymap-set "M-g c"                      #'avy-goto-char-timer)
(keymap-set "M-g e"                      #'consult-error)
(keymap-set "M-g i"                      #'consult-imenu)
(keymap-set "M-g k"                      #'consult-global-mark)
(keymap-set "M-g l"                      #'consult-line)
(keymap-set "M-g m"                      #'consult-mark)
(keymap-set "M-g o"                      #'consult-outline)
(keymap-set "M-g z"                      #'avy-goto-word-or-subword-1)
(keymap-set "M-m g"         (defrepeater #'pop-global-mark)) ;; was C-x C-SPC
(keymap-set "M-m m"                      #'set-mark-command) ;; was C-SPC
(keymap-set "M-m p"         (defrepeater #'pop-to-mark-command))
(keymap-set "M-m r"                      #'rectangle-mark-mode) ;; was C-x SPC
(keymap-set "M-m x"                      #'exchange-point-and-mark) ;; also on C-x C-x
(keymap-set "M-o ="                      #'doom/increase-font-size)
(keymap-set "M-s 5"                      #'query-replace)
(keymap-set "M-s f"         (defrepeater #'my-fill-unfill-respect-double-space))
(keymap-set "M-s m"                      #'consult-multi-occur)
(keymap-set "M-s r"                      #'isearch-backward)
(keymap-set "M-s s"                      #'isearch-forward)
(keymap-set "M-|"                        #'my-shell-command-replace-region)
(keymap-set "TAB"                        #'my-tab-command)
(keymap-set embark-general-map "C-\\"    #'hkey-either)
(keymap-set embark-general-map "C-;"     #'hkey-either)
(keymap-set help-map "M"                 #'describe-mode)
(keymap-set help-map "m"                 #'consult-minor-mode-menu)
(keymap-set isearch-mode-map "<down>"    #'isearch-repeat-forward)
(keymap-set isearch-mode-map "<up>"      #'isearch-repeat-backward)
(keymap-set isearch-mode-map "M-s n"     #'isearch-repeat-forward)
(keymap-set isearch-mode-map "M-s p"     #'isearch-repeat-backward)
(keymap-set my-abbrev-minor-mode-map "`"   #'expand-abbrev)
;; (keymap-set ""                        #'consult-focus-lines)
;; (keymap-set ""                        #'consult-imenu-multi)
;; (keymap-set ""                        #'consult-kmacro)
;; (keymap-set ""                        #'browse-url)

;; Some Greek letters
(keymap-set key-translation-map "<f7> a" "α") ;;alpha
(keymap-set key-translation-map "<f7> b" "β") ;;beta
(keymap-set key-translation-map "<f7> c" "χ") ;;chi
(keymap-set key-translation-map "<f7> d" "δ") ;;delta
(keymap-set key-translation-map "<f7> e" "ε") ;;epsilon
(keymap-set key-translation-map "<f7> f" "φ") ;;phi
(keymap-set key-translation-map "<f7> g" "γ") ;;gamma
(keymap-set key-translation-map "<f7> h" "θ") ;;theta
(keymap-set key-translation-map "<f7> i" "ι") ;;iota
;; (keymap-set key-translation-map "<f7> j" "")
(keymap-set key-translation-map "<f7> k" "κ") ;;kappa
(keymap-set key-translation-map "<f7> l" "λ") ;;lambda
(keymap-set key-translation-map "<f7> m" "μ") ;;mu
(keymap-set key-translation-map "<f7> n" "η") ;;eta
(keymap-set key-translation-map "<f7> o" "ω") ;;omega
(keymap-set key-translation-map "<f7> p" "π") ;;pi
;; (keymap-set key-translation-map "<f7> q" "")
(keymap-set key-translation-map "<f7> r" "ρ") ;;rho
(keymap-set key-translation-map "<f7> s" "σ") ;;sigma
(keymap-set key-translation-map "<f7> t" "τ") ;;tau
(keymap-set key-translation-map "<f7> u" "υ") ;;upsilon
(keymap-set key-translation-map "<f7> v" "ν") ;;nu
;; (keymap-set key-translation-map "<f7> w" "")
(keymap-set key-translation-map "<f7> x" "ξ") ;;xi
;; (keymap-set key-translation-map "<f7> y" "")
(keymap-set key-translation-map "<f7> z" "ζ") ;;zeta

(my-normie:abnormalize)

;; (run-hooks 'my-after-keybinds-hook)
