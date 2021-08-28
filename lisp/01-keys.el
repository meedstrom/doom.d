;;; Commentary:

;; One day, I might relocate these settings to be more context-relevant, then
;; consult the total list with (general-describe-keybindings), although that one
;; is cluttered by Doom's settings, or (describe-personal-keybindings). These
;; outputs could even be exported to a table for the curious in case I publish
;; my config. But I enjoy handling them this way, so there is no hurry.

;;; Code:

(require 'my-lib-unprefixed)
(require 'my-lib)
(require 'general)
(require 'key-chord)
(require 'defrepeater)
;; (require 'l)
(autoload #'objed-ipipe "objed")

;; Favor what I have in the global map on these keys.
(general-unbind geiser-mode-map "M-,")
(general-unbind geiser-mode-map "M-.")
(general-unbind geiser-mode-map "M-`")
(general-unbind geiser-repl-mode-map "M-,")
(general-unbind geiser-repl-mode-map "M-.")
(general-unbind geiser-repl-mode-map "M-`")

;; Nice commands I discovered
;; "C-]" ;; abort-recursive-edit
;; C-x i ;; insert-file

;; Unbind commands I never used or won't use anymore.
(general-unbind "<f2>") ;; 2C-command
(general-unbind "<f5>") ;; NOTE: which-key-paging-key is this by default
(general-unbind "<f6>")
(general-unbind "<f7>")
(general-unbind "<f8>")
(general-unbind "<f9>")
(general-unbind "<f10>") ;; menu-bar-open
(general-unbind "<insert>") ;; overwrite-mode
(general-unbind "C-o") ;; open-line
(general-unbind "C-z") ;; suspend-frame
(general-unbind "C-\\") ;; toggle-input-method
(general-unbind "M-.") ;; xref-find-definitions
(general-unbind "M-`") ;; tmm-menubar
(general-unbind "M-i") ;; tab-to-tab-stop
(general-unbind "M-j") ;; default-indent-new-line
(general-unbind "M-m") ;; back-to-indentation
(general-unbind "M-o") ;; facemenu-keymap
(general-unbind "M-r") ;; move-to-window-line-top-bottom
(general-unbind "M-z") ;; zap-to-char
(general-unbind "M-~") ;; not-modified
(general-unbind "C-x k") ;; Discourage unproductive behavior
(general-unbind "C-x C-z")
(general-unbind "C-x z")
(general-unbind "C-x (")
(general-unbind "C-x )")
(general-unbind "C-x *")
(general-unbind "C-x DEL") ;; use M-- M-k

;; Unbind keys where the commands get new keys.
(general-unbind "C-u") ;; universal-argument
(general-unbind "C-q") ;; quoted-insert
(general-unbind "M-q") ;; fill-paragraph
(general-unbind "<f3>") ;; kmacro-start-macro-or-insert-counter
(general-unbind "<f4>") ;; kmacro-end-or-call-macro
(general-unbind "C-x <left>")
(general-unbind "C-x <right>")
(general-unbind "C-x SPC")
(general-unbind "C-x C-SPC")
(general-unbind "C-SPC")
;; (general-unbind "C-g")

;; these hurt too much until I have more modal editing
;; (general-unbind "<f1>")
;; (general-unbind "<down>")
;; (general-unbind "<left>")
;; (general-unbind "<next>")
;; (general-unbind "<prior>")
;; (general-unbind "<right>")
;; (general-unbind "<up>")
;; (general-unbind "RET")

;; these are hard to remove, why I'll use super instead one day
;; (general-unbind "C-g") ;; keyboard-quit
;; (general-unbind "C-j") ;; newline
;; (general-unbind "C-i")
;; (general-unbind "C-]")
;; (general-unbind "C-m")

(when (boundp 'doom-version)
  (general-unbind "M--")
  (general-unbind "M-=")
  (general-unbind "C-'") ;; imenu
  (setq doom-leader-alt-key "<f3>")
  (setq doom-localleader-alt-key "<f4>"))

(when (boundp 'spacemacs-version)
  (general-unbind elisp-slime-nav-mode-map "M-,")
  (general-unbind elisp-slime-nav-mode-map "M-.")
  (general-unbind evil-emacs-state-map "C-z"))

;;; Fix prefix arguments

;; On my keyboard, it's convenient to type "<insert> -" because <insert> is
;; above - and to the right.
(define-key universal-argument-map (kbd "C-u") nil)
(define-key universal-argument-map (kbd "-") #'negative-argument)

;; Don't waste good keys (C-123456890) on digit-argument.
;; But make it more convenient to access them in other ways.
(let ((modifiers '("C-" "M-" "s-" "H-" "A-"))
      (digits (split-string "1234567890" "" t)))
  (dolist (d digits)
    (define-key universal-argument-map (kbd d) #'digit-argument))
  (dolist (mod modifiers)
    (define-key global-map (kbd (concat mod "-")) #'negative-argument)
    (define-key global-map (kbd (concat mod "=")) #'universal-argument)
    (define-key universal-argument-map (kbd (concat mod "=")) #'universal-argument-more)
    (dolist (d digits)
      (define-key global-map (kbd (concat mod d)) nil) ;; clear space
      (define-key universal-argument-map (kbd (concat mod d)) #'digit-argument))))

;; Fix ranger (don't take my M-1234...!)
(after! ranger
  (let ((digits (split-string "1234567890" "" t)))
    (dolist (d digits)
      (define-key ranger-normal-mode-map (kbd (concat "M-" d)) nil)
      (define-key ranger-emacs-mode-map (kbd (concat "M-" d)) nil))))

;;; Main

(run-hooks 'my-before-keybinds-hook)

;; Easy swapping between dired and eshell
;; Dired default unbound keys: `, b, E, J, K, r, z, <backspace>
;; Dired useless keys: h, 1234567890
(general-def dired-mode-map "r"  #'my-eshell-here)
;; Eshell behaves nonstandardly, so bindings must be done on a hook.
(add-hook 'eshell-mode-hook
          (defun my-eshell-keys ()
            (general-unbind eshell-mode-map "<up>")
            (general-unbind eshell-mode-map "<left>")
            (general-unbind eshell-mode-map "<down>")
            (general-unbind eshell-mode-map "<right>")
            (general-def eshell-mode-map "C-c C-l" #'my-counsel-eshell-history)
            (general-def eshell-mode-map "<escape>" #'crux-switch-to-previous-buffer)
            (general-def eshell-mode-map "C-M-j"    #'my-insert-other-buffer-file-name)
            (general-def eshell-mode-map "C-S-n"    #'my-new-eshell)
            (general-def eshell-mode-map "C-S-n"    #'my-new-eshell)
            (general-def eshell-mode-map "C-c C-l"  #'consult-history)
            ))

(general-def custom-mode-map "q"          #'kill-current-buffer)
(general-def dired-mode-map "q"           #'kill-current-buffer) ;; undoom
(general-def dired-mode-map "b"           #'dired-up-directory)
(general-def dired-mode-map ")"           #'dired-git-info-mode)
(general-def dired-mode-map "M-<up>"      #'dired-up-directory)
(general-def dired-mode-map "M-RET"       #'my-dired-open-file-with-default-tool)
(general-def ess-mode-map "<f1> <f2>"     #'ess-abort)
(general-def ess-mode-map "<f1> <f3>"     #'ess-interrupt)
(general-def ess-mode-map "C-<return>"    #'ess-eval-line)
(general-def eww-mode-map "q"             #'kill-current-buffer)
(general-def vertico-map "<next>"         #'scroll-up-command)
(general-def vertico-map "<prior>"        #'scroll-down-command)
(general-def ledger-mode-map "M-<return>" #'crux-duplicate-current-line-or-region)
(general-def my-abbrev-minor-mode-map "`" #'expand-abbrev)
(general-def shell-mode-map "C-S-n"       #'my-new-shell)
(general-def isearch-mode-map "M-s n"     #'isearch-repeat-forward)
(general-def isearch-mode-map "<down>"    #'isearch-repeat-forward)
(general-def isearch-mode-map "M-s p"     #'isearch-repeat-backward)
(general-def isearch-mode-map "<up>"      #'isearch-repeat-backward)
(general-def ctrlf--keymap "<down>"       #'ctrlf-forward-literal)
(general-def ctrlf--keymap "<up>"         #'ctrlf-backward-literal)
(general-def "<f10> a"                    #'my-save-buffer-and-amend)
(general-def "<f10> d"                    #'org-download-yank)
(general-def "<f10> e"                    #'eww)
(general-def "<f10> g"                    #'guix-popup)
(general-def "<f10> k"                    #'gif-screencast-start-or-stop)
(general-def "<f10> l"                    #'mw-thesaurus-lookup)
(general-def "<f10> n"                    #'my-normie-toggle)
(general-def "<f10> s"                    #'my-save-buffer-and-commit)
(general-def "<f2> 1"                     #'my-insert-other-buffer-file-name-and-cycle)
(general-def "<f2> 2"                     #'my-toggle-selective-display)
(general-def "<f2> 3"                     #'elfeed)
(general-def "<f2> 5"                     #'my-lookup-word)
(general-def "<f2> <f1>"                  #'my-describe-last-key)
(general-def "<f2> <f2>"                  #'vc-msg-show)
(general-def "<f2> <f3>"                  #'git-messenger:popup-message)
(general-def "<f2> b"                     #'backup-walker-start)
(general-def "<f2> c"                     #'org-roam-capture)
(general-def "<f2> d"                     #'my-insert-today)
(general-def "<f2> e d"                   #'eval-defun)
(general-def "<f2> e e"                   #'eval-last-sexp)
(general-def "<f2> e p"                   #'eval-print-last-sexp)
(general-def "<f2> e r"                   #'eval-region)
(general-def "<f2> e l"                   #'load-library)
(general-def "<f2> e s"                   #'ess-eval-region-or-function-or-paragraph-and-step) ;; ess everywhere
(general-def "<f2> e x"                   #'eval-expression)
(general-def "<f2> f d"                   #'crux-delete-file-and-buffer)
(general-def "<f2> f f"                   #'toggle-frame-fullscreen)
(general-def "<f2> f m"                   #'toggle-frame-maximized)
(general-def "<f2> f r"                   #'crux-rename-file-and-buffer)
(general-def "<f2> g"                     #'git-timemachine)
(general-def "<f2> h"                     #'my-file-jump-from-home)
(general-def "<f2> i"                     #'my-suggest-sub)
(general-def "<f2> j"                     #'project-find-file)
(general-def "<f2> k"                     #'consult-ripgrep)
(general-def "<f2> l"                     #'helm-locate)
(general-def "<f2> m"                     #'my-show-my-files)
(general-def "<f2> p"                     #'my-spawn-process)
(general-def "<f2> r"                     #'selectrum-repeat)
(general-def "<f2> s"                     #'my-eshell-here)
(general-def "<f2> w"                     #'sp-rewrap-sexp)
(general-def "<f2> x"                     #'execute-extended-command)
(general-def "<f2> z"                     #'my-sleep)
(general-def "<f5>"                       #'repeat)
(general-def "<menu>"                     #'execute-extended-command)
(general-def "<print>"                    #'embark-act)
(general-def "C-<next>"                   #'next-buffer)
(general-def "C-<prior>"                  #'previous-buffer)
(general-def "C-h q"                      #'quoted-insert)
(general-def "C-g"                        #'keyboard-quit)
(general-def "C-x C-\;"      (defrepeater #'comment-line))
(general-def "M-0"                        #'hippie-expand)
(general-def "M-1"                        #'switch-to-buffer)
(general-def "M-2"                        #'other-window)
(general-def "M-3"                        #'unexpand-abbrev)
(general-def "M-5"                        #'my-prev-file-in-dir)
(general-def "M-6"                        #'my-next-file-in-dir)
(general-def "M-8"                        #'kill-whole-line)
(general-def "M-9"                        #'crux-duplicate-current-line-or-region)
(general-def "M-<backspace>"              #'sp-backward-unwrap-sexp)
(general-def "M-<delete>"                 #'sp-unwrap-sexp)
(general-def "M-<f4>"                     #'kill-current-buffer)
(general-def "M-<insert>"                 #'sp-rewrap-sexp)
(general-def "M-g a a"       (defrepeater #'avy-pop-mark))
(general-def "M-g a c"                    #'avy-goto-char-2)
(general-def "M-g a g c"                  #'avy-goto-char-2)
(general-def "M-g a g e"                  #'avy-goto-end-of-line)
(general-def "M-g a g l"                  #'avy-goto-line)
(general-def "M-g a g o"                  #'avy-goto-word-or-subword-1)
(general-def "M-g a g q"                  #'avy-goto-subword-1)
(general-def "M-g a g s"                  #'avy-goto-symbol-1)
(general-def "M-g a g w"                  #'avy-goto-word-1)
(general-def "M-g a k"                    #'avy-kill-region)
(general-def "M-g a m l"                  #'avy-move-line)
(general-def "M-g a m r"                  #'avy-move-region)
(general-def "M-g a n"                    #'avy-next)
(general-def "M-g a o"                    #'avy-goto-symbol)
(general-def "M-g a p"                    #'avy-prev)
(general-def "M-g a r"                    #'avy-resume)
(general-def "M-g a s"                    #'avy-isearch)
(general-def "M-g a w"                    #'avy-kill-ring-save-region)
(general-def "M-g c"                      #'avy-goto-char-timer)
(general-def "M-g z"                      #'avy-goto-word-or-subword-1)
(general-def "M-m g"         (defrepeater #'pop-global-mark)) ;; was C-x C-SPC
(general-def "M-m m"                      #'set-mark-command) ;; was C-SPC
(general-def "M-m p"         (defrepeater #'pop-to-mark-command))
(general-def "M-m r"                      #'rectangle-mark-mode) ;; was C-x SPC
(general-def "M-m x"                      #'exchange-point-and-mark) ;; also on C-x C-x
(general-def "M-o ="                      #'doom/increase-font-size)
(general-def "M-s 5"                      #'query-replace)
(general-def "M-s f"                      #'my-fill-unfill-respect-double-space)
(general-def "M-s r"                      #'isearch-backward)
(general-def "M-s s"                      #'isearch-forward)
(general-def "M-|"                        #'my-shell-command-replace-region)
(general-def "TAB"                        #'my-tab-command)
(general-def "<f2> n"                  #'org-journal-new-entry)
;; (general-def "<f2> u d"      (defrepeater (l'sk/change-number-at-point -1)))
;; (general-def "<f2> u i"      (defrepeater (l'sk/change-number-at-point 1)))
;;(general-def "<f30>"                    #'execute-extended-command)

;; Smartparens guide: https://gist.github.com/pvik/8eb5755cc34da0226e3fc23a320a3c95
;; Author's config: https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
;; Xah's simplification: https://old.reddit.com/r/emacs/comments/3sfmkz/could_this_be_a_pareditsmartparens_killer/cwxocld/
(general-def smartparens-strict-mode-map ";" #'sp-comment)
(after! smartparens
  (general-def "C-'"                      #'sp-mark-sexp)
  (general-def "C-;"                      #'sp-comment)
  (general-def "C-<left>"                 #'sp-forward-barf-sexp)
  (general-def "C-<right>"                #'sp-forward-slurp-sexp)
  (general-def "C-M-<left>"               #'sp-backward-slurp-sexp)
  (general-def "C-M-<right>"              #'sp-backward-barf-sexp)
  (general-def "M-<backspace>"            #'sp-backward-unwrap-sexp)
  (general-def "M-<delete>"               #'sp-unwrap-sexp)
  ;; (general-def "s-<SPC>"                  #'sp-mark-sexp)
  (general-def "s-<left>" #'sp-backward-slurp-sexp)
  (general-def "s-<right>" #'sp-backward-barf-sexp)
  (general-def "s-<delete>" #'sp-splice-sexp-killing-forward)
  (general-def "s-<backspace>" #'sp-splice-sexp-killing-backward)
  (general-def "s-a" #'sp-backward-down-sexp)
  (general-def "s-b" #'sp-backward-sexp)
  (general-def "s-d" #'sp-down-sexp)
  (general-def "s-e" #'sp-up-sexp)
  (general-def "s-f" #'sp-forward-sexp)
  (general-def "s-h" #'sp-mark-sexp)
  (general-def "s-k" #'sp-kill-sexp)
  (general-def "s-n" #'sp-next-sexp)
  (general-def "s-p" #'sp-previous-sexp)
  (general-def "s-t" #'sp-transpose-sexp)
  (general-def "s-u" #'sp-backward-up-sexp)
  (general-def "M-[" #'sp-wrap-round)
  (general-def [remap kill-whole-line] #'sp-kill-whole-line))

;; Unassimilated Smartparens commands to try out.
;; (general-def "C-2 a" #'sp-join-sexp)
;; (general-def "C-2 b" #'sp-select-next-thing)
;; (general-def "C-2 c" #'sp-beginning-of-sexp)
;; (general-def "C-2 d" #'sp-beginning-of-next-sexp)
;; (general-def "C-2 e" #'sp-end-of-sexp)
;; (general-def "C-2 f" #'sp-add-to-next-sexp)
;; (general-def "C-2 g" #'sp-add-to-previous-sexp)
;; (general-def "C-2 h" #'sp-split-sexp)
;; (general-def "C-2 i" #'sp-splice-sexp)
;; (general-def "C-2 j" #'sp-emit-sexp)
;; (general-def "C-2 k" #'sp-absorb-sexp)
;; (general-def "C-2 l" #'sp-convolute-sexp)
;; (general-def "C-2 m" #'sp-forward-symbol)
;; (general-def "C-2 n" #'sp-backward-symbol)
;; (general-def "C-2 o" #'sp-wrap)
;; (general-def "C-2 p" #'sp-backward-up-sexp)
;; (general-def "C-2 q" #'sp-up-sexp)
;; (general-def "C-2 r" #'sp-select-next-thing-exchange)
;; (general-def "C-2 s" #'sp-select-previous-thing)

(use-package! deianira
  :config
  ;; (esm-xmodmap-reload)
  ;; (esm-xcape-reload)
  ;; (esm-xkbset-enable-sticky-keys)
  ;; Assume that we continuously update relevant bindings and run a hook
  (add-hook 'my-after-keybinds-hook #'dei--get-relevant-bindings)
  (add-hook 'my-after-keybinds-hook #'dei--mass-remap)
  ;; TODO continuously flatten C-c map.
  ;; (add-hook 'window-buffer-change-functions          )
  ;; (after! org
    ;; (dei-restem org-mode-map "c" "C-c " "C-c C-")
    ;; (dei-restem org-mode-map "t" "C-c " "C-c C-")
    )

;; Prep to flatten the keymap.  C-x KEY will win over C-x C-KEY.  Thus if I like
;; C-x a over C-x C-a, I don't need to do anything.  But if I like C-x C-f over
;; C-x f, duplicate the definition here.
;(dolist (leaf '("-" ";" "=" "c" "f" "l" "o" "q" "s" "t" "u" "w" "x"))
;  (dei-restem leaf "C-x " "C-x C-"))

;; (dei-restem kmacro-keymap "k" "" "C-")
;; above same as?
;; (dei-restem "k" "C-x k " "C-x k C-")

(after! which-key
  ;; Don't show keys like C-x C-a, only show simple leafs like C-x a.
  (push '((" .-.") . t) which-key-replacement-alist))

(defvar my-abbrev-minor-mode-map (make-sparse-keymap))
(add-to-list 'minor-mode-map-alist (cons 'abbrev-mode my-abbrev-minor-mode-map))


(defvar my-anki-editor-mode-map (make-sparse-keymap))
(add-to-list 'minor-mode-map-alist (cons 'abbrev-mode my-anki-editor-mode-map))

(after! key-chord
  (key-chord-define-global "cd" #'calc-dispatch))



(my-after-keybinds
 ;; (esm-super-from-ctl global-map)
 (my-abnormalize))

(run-hooks 'my-after-keybinds-hook)
