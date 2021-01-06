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

(run-hooks 'my-before-keybinds-hook)

;; Favor what I have in the global map on these keys.
(general-unbind geiser-mode-map "M-,")
(general-unbind geiser-mode-map "M-.")
(general-unbind geiser-mode-map "M-`")
(general-unbind geiser-repl-mode-map "M-,")
(general-unbind geiser-repl-mode-map "M-.")
(general-unbind geiser-repl-mode-map "M-`")

;; Nice commands I discovered
;; "C-]" ;; abort-recursive-edit

;; Unbind commands I never used or won't use anymore.
(general-unbind global-map "<f2>") ;; 2C-command
(general-unbind global-map "<f5>") ;; ???
(general-unbind global-map "<f6>")
(general-unbind global-map "<f7>")
(general-unbind global-map "<f8>")
(general-unbind global-map "<f9>")
(general-unbind global-map "<f10>") ;; menu-bar-open
(general-unbind global-map "<insert>") ;; overwrite-mode
(general-unbind global-map "C-o") ;; open-line
(general-unbind global-map "C-z") ;; suspend-frame
(general-unbind global-map "C-\\") ;; toggle-input-method
(general-unbind global-map "M-.") ;; xref-find-definitions
(general-unbind global-map "M-`") ;; tmm-menubar
(general-unbind global-map "M-i") ;; tab-to-tab-stop
(general-unbind global-map "M-j") ;; default-indent-new-line
(general-unbind global-map "M-m") ;; back-to-indentation
(general-unbind global-map "M-o") ;; facemenu-keymap
(general-unbind global-map "M-r") ;; move-to-window-line-top-bottom
(general-unbind global-map "M-z") ;; zap-to-char
(general-unbind global-map "M-~") ;; not-modified
(general-unbind global-map "C-x k") ;; Discourage unproductive behavior

;; Unbind keys where the commands get new keys.
(general-unbind global-map "C-u") ;; universal-argument
(general-unbind global-map "C-q") ;; quoted-insert
(general-unbind global-map "M-q") ;; fill-paragraph
(general-unbind global-map "<f3>") ;; kmacro-start-macro-or-insert-counter
(general-unbind global-map "<f4>") ;; kmacro-end-or-call-macro

;; these hurt too much until I have escape-modality
;; (general-unbind global-map "<f1>")
;; (general-unbind global-map "<down>")
;; (general-unbind global-map "<left>")
;; (general-unbind global-map "<next>")
;; (general-unbind global-map "<prior>")
;; (general-unbind global-map "<right>")
;; (general-unbind global-map "<up>")
;; (general-unbind global-map "RET")

;; these are effectively illegal, why I'll use super instead one day
;; (general-unbind global-map "C-g") ;; keyboard-quit
;; (general-unbind global-map "C-j") ;; newline
;; (general-unbind global-map "C-i")
;; (general-unbind global-map "C-]")
;; (general-unbind global-map "C-m")

;; Clear a ton of good keys which were wasted on prefix arguments.
(dolist (x (string-to-list "1234567890-"))
  (global-unset-key (kbd (format "C-%c" x)))
  (global-unset-key (kbd (format "M-%c" x)))
  (global-unset-key (kbd (format "C-M-%c" x))))

;; My alternate setup for prefix arguments. I must start with the universal
;; argument, but holding down modifiers make no difference, so it is possible
;; to type either  <f5> - M-u  or  M-<f5> M-- M-u.  Mind that M-<f5> can be
;; blocked on some OSes.
(dolist (x (string-to-list "1234567890"))
  (define-key universal-argument-map (kbd (format "C-%c" x))   #'digit-argument)
  (define-key universal-argument-map (kbd (format "M-%c" x))   #'digit-argument)
  (define-key universal-argument-map (kbd (format "C-M-%c" x)) #'digit-argument)
  (define-key universal-argument-map (kbd (format "s-%c" x))   #'digit-argument))
;; (general-def global-map     "<f5>" #'universal-argument)
(general-def global-map   "C-<f5>" #'universal-argument)
(general-def global-map   "M-<f5>" #'universal-argument)
(general-def global-map   "s-<f5>" #'universal-argument)
(general-def global-map "C-M-<f5>" #'universal-argument)
(general-def universal-argument-map     "-" #'negative-argument)
(general-def universal-argument-map   "C--" #'negative-argument)
(general-def universal-argument-map   "M--" #'negative-argument)
(general-def universal-argument-map   "s--" #'negative-argument)
(general-def universal-argument-map "C-M--" #'negative-argument)

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

(defvar my-abbrev-minor-mode-map (make-sparse-keymap))
(add-to-list 'minor-mode-map-alist (cons 'abbrev-mode my-abbrev-minor-mode-map))

(after! key-chord
  (key-chord-define-global "cd" #'calc-dispatch))

(general-def "<menu>" (general-key "C-g")) ;; my caps lock
(general-def "<menu>" #'keyboard-quit)  ;; training wheels
(general-unbind "C-g")                  ;; training wheels

(general-def custom-mode-map "q"          #'kill-current-buffer)
(general-def dired-mode-map ")"           #'dired-git-info-mode)
(general-def dired-mode-map "M-<up>"      #'dired-up-directory)
(general-def dired-mode-map "M-RET"       #'my-dired-open-file-with-default-tool)
(general-def eshell-mode-map "<escape>"   #'crux-switch-to-previous-buffer)
(general-def eshell-mode-map "C-M-j"      #'my-insert-other-buffer-file-name)
(general-def eshell-mode-map "C-S-n"      #'my-new-eshell)
(general-def eshell-mode-map "C-S-n"      #'my-new-eshell)
(general-def eshell-mode-map "C-c C-l"    #'consult-history)
(general-def ess-mode-map "<f1> <f2>"     #'ess-abort)
(general-def ess-mode-map "<f1> <f3>"     #'ess-interrupt)
(general-def ess-mode-map "C-<return>"    #'ess-eval-line)
(general-def eww-mode-map "q"             #'kill-current-buffer)
(general-def global-map "<escape>"        #'keyboard-escape-quit) ; ESC ESC ESC => ESC
(general-def global-map "<f10> a"         #'my-save-buffer-and-amend)
(general-def global-map "<f10> d"         #'org-download-yank)
(general-def global-map "<f10> e"         #'eww)
(general-def global-map "<f10> g"         #'guix-popup)
(general-def global-map "<f10> n"         #'my-normie-toggle)
(general-def global-map "<f10> s"         #'my-save-buffer-and-commit)
(general-def global-map "<f2> 1"          #'my-insert-other-buffer-file-name-and-cycle)
(general-def global-map "<f2> 2"          #'my-toggle-selective-display)
(general-def global-map "<f2> 3"          #'elfeed)
(general-def global-map "<f2> 5"          #'my-lookup-word)
(general-def global-map "<f2> <f1>"       #'my-describe-last-key)
(general-def global-map "<f2> <f2>"       #'vc-msg-show)
(general-def global-map "<f2> <f3>"       #'git-messenger:popup-message)
(general-def global-map "<f2> b"          #'backup-walker-start)
(general-def global-map "<f2> d"          #'my-insert-today)
(general-def global-map "<f2> e d"        #'eval-defun)
(general-def global-map "<f2> e x"        #'eval-expression)
(general-def global-map "<f2> e e"        #'eval-last-sexp)
(general-def global-map "<f2> e p"        #'eval-print-last-sexp)
(general-def global-map "<f2> e r"        #'eval-region)
(general-def global-map "<f2> e s"        #'ess-eval-region-or-function-or-paragraph-and-step) ;; ess everywhere
(general-def global-map "<f2> f d"        #'crux-delete-file-and-buffer)
(general-def global-map "<f2> f f"        #'toggle-frame-fullscreen)
(general-def global-map "<f2> f m"        #'toggle-frame-maximized)
(general-def global-map "<f2> f r"        #'crux-rename-file-and-buffer)
(general-def global-map "<f2> g"          #'git-timemachine)
(general-def global-map "<f2> h"          #'my-file-jump-from-home)
(general-def global-map "<f2> i"          #'my-suggest-sub)
(general-def global-map "<f2> j"          #'project-find-file)
(general-def global-map "<f2> k"          #'consult-ripgrep)
(general-def global-map "<f2> l"          #'helm-locate)
(general-def global-map "<f2> m"          #'my-show-my-files)
(general-def global-map "<f2> n"          #'my-today-file)
(general-def global-map "<f2> p"          #'my-spawn-process)
(general-def global-map "<f2> r"          #'selectrum-repeat)
(general-def global-map "<f2> s"          #'my-eshell-here)
(general-def global-map "<f2> w"          #'sp-rewrap-sexp)
(general-def global-map "<f2> x"          #'execute-extended-command)
(general-def global-map "<f2> z"          #'my-sleep)
(general-def global-map "<insert>"        #'embark-act)
(general-def global-map "<menu>"          #'keyboard-quit)
(general-def global-map "C-."             #'repeat)
(general-def global-map "C-<next>"        #'next-buffer)
(general-def global-map "C-<prior>"       #'previous-buffer)
(general-def global-map "M-0"             #'hippie-expand)
(general-def global-map "M-1"             #'switch-to-buffer)
(general-def global-map "M-2"             #'other-window)
(general-def global-map "M-3"             #'unexpand-abbrev)
(general-def global-map "M-4"             #'avy-goto-char-2)
(general-def global-map "M-5"             #'my-prev-file-in-dir)
(general-def global-map "M-6"             #'my-next-file-in-dir)
(general-def global-map "M-8"             #'kill-whole-line)
(general-def global-map "M-9"             #'crux-duplicate-current-line-or-region)
(general-def global-map "M-<backspace>"   #'sp-backward-unwrap-sexp)
(general-def global-map "M-<delete>"      #'sp-unwrap-sexp)
(general-def global-map "M-<insert>"      #'sp-rewrap-sexp)
(general-def global-map "C-h C-q"         #'quoted-insert) ;; frequently used to insert control chars, so good on control map
(general-def global-map "C-h q"           #'quoted-insert)
(general-def global-map "M-o M--"         #'doom/decrease-font-size)
(general-def global-map "M-o -"         #'doom/decrease-font-size)
(general-def global-map "M-o M-="         #'doom/increase-font-size)
(general-def global-map "M-o ="         #'doom/increase-font-size)
(general-def global-map "M-s M-r"         #'isearch-backward)
(general-def global-map "M-s M-s"         #'isearch-forward)
(general-def global-map "M-s f"           #'my-fill-unfill-respect-double-space)
(general-def global-map "M-s q"           #'quoted-insert)
(general-def global-map "M-s r"           #'isearch-backward)
(general-def global-map "M-s s"           #'isearch-forward)
(general-def global-map "M-s z"           #'avy-goto-char-2)
(general-def global-map "M-|"             #'my-shell-command-replace-region)
(general-def ledger-mode-map "M-<return>" #'crux-duplicate-current-line-or-region)
(general-def my-abbrev-minor-mode-map "`" #'expand-abbrev)
(general-def shell-mode-map "C-S-n"       #'my-new-shell)

(general-def smartparens-mode-map "C-;"                      #'sp-comment)
(general-def smartparens-mode-map "C-<left>"                 #'sp-forward-barf-sexp)
(general-def smartparens-mode-map "C-<right>"                #'sp-forward-slurp-sexp)
(general-def smartparens-mode-map "C-M-<left>"               #'sp-backward-slurp-sexp)
(general-def smartparens-mode-map "C-M-<right>"              #'sp-backward-barf-sexp)
(general-def smartparens-mode-map "C-M-a"                    #'sp-backward-down-sexp)
(general-def smartparens-mode-map "C-M-w"                    #'sp-copy-sexp)
(general-def smartparens-mode-map "M-<backspace>"            #'sp-backward-unwrap-sexp)
(general-def smartparens-mode-map "M-<delete>"               #'sp-unwrap-sexp)
(general-def smartparens-mode-map "s-<SPC>"                  #'sp-mark-sexp)
(general-def smartparens-mode-map [remap backward-kill-sexp] #'sp-backward-kill-sexp);DEL
(general-def smartparens-mode-map [remap backward-list]      #'sp-previous-sexp);p
(general-def smartparens-mode-map [remap backward-sexp]      #'sp-backward-sexp);b
(general-def smartparens-mode-map [remap backward-up-list]   #'sp-backward-up-sexp);u
(general-def smartparens-mode-map [remap down-list]          #'sp-down-sexp);d
(general-def smartparens-mode-map [remap forward-list]       #'sp-next-sexp);n
(general-def smartparens-mode-map [remap forward-sexp]       #'sp-forward-sexp);f
(general-def smartparens-mode-map [remap kill-sexp]          #'sp-kill-sexp);k
(general-def smartparens-mode-map [remap kill-whole-line]    #'sp-kill-whole-line)
(general-def smartparens-mode-map [remap mark-sexp]          #'sp-mark-sexp);SPC
(general-def smartparens-mode-map [remap transpose-sexps]    #'sp-transpose-sexp);t

;; Unassimilated Smartparens commands to try out.  Tip: author's config at
;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el (heavy on
;; C-M- though).
(general-def smartparens-mode-map "C-2 a" #'sp-join-sexp)
(general-def smartparens-mode-map "C-2 b" #'sp-select-next-thing)
(general-def smartparens-mode-map "C-2 c" #'sp-beginning-of-sexp)
(general-def smartparens-mode-map "C-2 d" #'sp-beginning-of-next-sexp)
(general-def smartparens-mode-map "C-2 e" #'sp-end-of-sexp)
(general-def smartparens-mode-map "C-2 f" #'sp-add-to-next-sexp)
(general-def smartparens-mode-map "C-2 g" #'sp-add-to-previous-sexp)
(general-def smartparens-mode-map "C-2 h" #'sp-split-sexp)
(general-def smartparens-mode-map "C-2 i" #'sp-splice-sexp)
(general-def smartparens-mode-map "C-2 j" #'sp-emit-sexp)
(general-def smartparens-mode-map "C-2 k" #'sp-absorb-sexp)
(general-def smartparens-mode-map "C-2 l" #'sp-convolute-sexp)
(general-def smartparens-mode-map "C-2 m" #'sp-forward-symbol)
(general-def smartparens-mode-map "C-2 n" #'sp-backward-symbol)
(general-def smartparens-mode-map "C-2 o" #'sp-wrap)
(general-def smartparens-mode-map "C-2 p" #'sp-backward-up-sexp)
(general-def smartparens-mode-map "C-2 q" #'sp-up-sexp)
(general-def smartparens-mode-map "C-2 r" #'sp-select-next-thing-exchange)
(general-def smartparens-mode-map "C-2 s" #'sp-select-previous-thing)

;; (general-def key-translation-map "<f3> a" (kbd "α"))
;; (general-def key-translation-map "<f3> b" (kbd "β"))
;; (general-def key-translation-map "<f3> c" (kbd "γ"))
;; (general-def key-translation-map "<f3> d" (kbd "δ"))
;; (general-def key-translation-map "<f3> e" (kbd "ε"))
;; (general-def key-translation-map "<f3> f" (kbd "φ"))
;; (general-def key-translation-map "<f3> g" (kbd "θ"))
;; (general-def key-translation-map "<f3> l" (kbd "λ"))
;; (general-def key-translation-map "<f3> m" (kbd "μ"))
;; (general-def key-translation-map "<f3> p" (kbd "π"))
;; (general-def key-translation-map "<f3> r" (kbd "ρ"))
;; (general-def key-translation-map "<f3> s" (kbd "σ"))
;; (general-def key-translation-map "<f3> t" (kbd "τ"))
;; (general-def key-translation-map "<f3> o" (kbd "ω"))
;; (general-def key-translation-map "<f3> x" (kbd "ξ"))

;; (general-def key-translation-map "<f3> A" (kbd "Α"))
;; (general-def key-translation-map "<f3> B" (kbd "Β"))
;; (general-def key-translation-map "<f3> C" (kbd "Γ"))
;; (general-def key-translation-map "<f3> D" (kbd "Δ"))
;; (general-def key-translation-map "<f3> E" (kbd "Ε"))
;; (general-def key-translation-map "<f3> F" (kbd "Φ"))
;; (general-def key-translation-map "<f3> G" (kbd "Θ"))
;; (general-def key-translation-map "<f3> L" (kbd "Λ"))
;; (general-def key-translation-map "<f3> M" (kbd "Μ"))
;; (general-def key-translation-map "<f3> P" (kbd "Π"))
;; (general-def key-translation-map "<f3> R" (kbd "Ρ"))
;; (general-def key-translation-map "<f3> S" (kbd "Σ"))
;; (general-def key-translation-map "<f3> T" (kbd "Τ"))
;; (general-def key-translation-map "<f3> O" (kbd "Ω"))
;; (general-def key-translation-map "<f3> X" (kbd "Ξ"))

(my-after-keybinds
 ;; (esm-super-from-ctl global-map)
 (my-abnormalize))

(run-hooks 'my-after-keybinds-hook)
