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

(run-hooks 'my-before-keybinds-hook)

;; Clear some obstacles. Why not just override case-by-case when needed? Because
;; some were prefix keys, like <f2>, the mode-specific keys override global keys
;; I always want access to, and some are just annoying.
(general-unbind global-map "<f2>") ;; prefix
;; (general-unbind global-map "<f3>") ;; prefix
;; (general-unbind global-map "<f5>")
;; (general-unbind global-map "<f6>")
;; (general-unbind global-map "<f7>")
;; (general-unbind global-map "<f8>")
;; (general-unbind global-map "<f9>")
;; (general-unbind global-map "<f10>")
;; (general-unbind global-map "<f11>")

;; allow global keys in favour of these
(general-unbind geiser-mode-map "M-,")
(general-unbind geiser-mode-map "M-.")
(general-unbind geiser-mode-map "M-`")
(general-unbind geiser-repl-mode-map "M-,")
(general-unbind geiser-repl-mode-map "M-.")
(general-unbind geiser-repl-mode-map "M-`")

;; (general-unbind global-map "C-z")
;; (general-unbind global-map "M-#")
;; (general-unbind global-map "M-/")
;; (general-unbind global-map "M-`")
;; (general-unbind global-map "M-~")


(when (boundp 'doom-version)
  (setq doom-leader-alt-key "<f2>")
  (setq doom-localleader-alt-key "<f12>")
  )

(when (boundp 'spacemacs-version)
  (general-unbind elisp-slime-nav-mode-map "M-,")
  (general-unbind elisp-slime-nav-mode-map "M-.")
  (general-unbind evil-emacs-state-map "C-z")
  )

(defvar abbrev-minor-mode-map (make-sparse-keymap))
(add-to-list 'minor-mode-map-alist (cons 'abbrev-mode abbrev-minor-mode-map))
(general-def abbrev-minor-mode-map "`" #'expand-abbrev)

(general-def Info-mode-map "C-s" #'isearch-forward)
(general-def custom-mode-map "q" #'kill-current-buffer)
(general-def dired-mode-map ")" #'dired-git-info-mode)
(general-def dired-mode-map "M-<up>" #'dired-up-directory)
(general-def dired-mode-map "M-RET" #'my-dired-open-file-with-default-tool)
(general-def eshell-mode-map "<escape>" #'crux-switch-to-previous-buffer)
(general-def eshell-mode-map "C-M-j" #'my-insert-other-buffer-file-name)
(general-def eshell-mode-map "C-S-n" #'my-new-eshell)
(general-def eshell-mode-map "C-S-n" #'my-new-eshell)
(general-def eshell-mode-map "C-c C-l" #'my-counsel-eshell-history)
;; (general-def ess-mode-map "<f1> <f2>" #'ess-abort)
;; (general-def ess-mode-map "<f1> <f3>" #'ess-interrupt)
(general-def ess-mode-map "C-<return>"  #'ess-eval-line)
(general-def eww-mode-map "q" #'kill-current-buffer)
(general-def global-map "<f1> <f2>" #'vc-msg-show)
 (general-def global-map "<f1> <f3>" #'git-messenger:popup-message)
 (general-def global-map "<f7> s" #'my-save-buffer-and-commit)
 (general-def global-map "<f7> a" #'my-save-buffer-and-amend)
 (general-def global-map "<escape>" #'keyboard-escape-quit) ;; ESC ESC ESC => ESC
(general-def global-map "<f5> 1"   #'my-insert-other-buffer-file-name-and-cycle)
(general-def global-map "<f5> 2"   #'my-toggle-selective-display)
(general-def global-map "<f5> 3"   #'elfeed)
(general-def global-map "<f5> 5"   #'my-lookup-word)
(general-def global-map "<f5> a"   (if (executable-find "gtk-launch") #'counsel-linux-app #'my-linux-app))
(general-def global-map "<f5> b"   #'backup-walker-start)
(general-def global-map "<f5> d"   #'my-insert-today)
(general-def global-map "<f5> e d" #'eval-defun)
(general-def global-map "<f5> e e" #'eval-expression)
(general-def global-map "<f5> e l" #'eval-last-sexp)
(general-def global-map "<f5> e p" #'eval-print-last-sexp)
(general-def global-map "<f5> e r" #'eval-region)
(general-def global-map "<f5> e s" #'ess-eval-region-or-function-or-paragraph-and-step) ;; ess everywhere
(general-def global-map "<f5> f d" #'crux-delete-file-and-buffer)
(general-def global-map "<f5> f f" #'toggle-frame-fullscreen)
(general-def global-map "<f5> f m" #'toggle-frame-maximized)
(general-def global-map "<f5> f r" #'crux-rename-file-and-buffer)
(general-def global-map "<f5> g"   #'git-timemachine)
(general-def global-map "<f5> h"   #'my-file-jump-from-home)
(general-def global-map "<f5> i"   #'my-suggest-sub)
(general-def global-map "<f5> j"   #'counsel-file-jump)
(general-def global-map "<f5> k"   #'helm-do-grep-ag)
(general-def global-map "<f5> l"   #'counsel-locate)
(general-def global-map "<f5> m"   #'my-show-my-files)
(general-def global-map "<f5> n"   #'my-today-file)
(general-def global-map "<f5> p"   #'my-spawn-process)
(general-def global-map "<f5> r"   #'ivy-resume)
(general-def global-map "<f5> s"   #'my-eshell-here)
(general-def global-map "<f5> x"   #'execute-extended-command)
(general-def global-map "<f5> z"   #'my-sleep)
(general-def global-map "<f6>"     #'calc-dispatch)
(general-def global-map "<f7> d"    #'org-download-yank)
(general-def global-map "<f7> e"   #'eww)
(general-def global-map "<f7> g"   #'guix-popup)
(general-def global-map "<f7> n"   #'my-normie-toggle)
(general-def global-map "C-."      #'repeat)
(general-def global-map "C-<next>" #'next-buffer)
(general-def global-map "C-<prior>" #'previous-buffer)
(general-def global-map "M-0"      #'hippie-expand)
(general-def global-map "M-1"      #'avy-goto-char)
(general-def global-map "M-2"      #'avy-goto-char-2)
(general-def global-map "M-3"      #'unexpand-abbrev)
(general-def global-map "M-4"      #'transpose-lines)
(general-def global-map "M-5"      #'my-prev-file-in-dir)
(general-def global-map "M-6"      #'my-next-file-in-dir)
(general-def global-map "M-8"      #'kill-whole-line)
(general-def global-map "M-9"      #'crux-duplicate-current-line-or-region)
(general-def ledger-mode-map "M-<return>"  #'crux-duplicate-current-line-or-region)
(general-def shell-mode-map "C-S-n"        #'my-new-shell)
;; (general-def global-map "<f5> k"   (if-let (app (seq-find #'executable-find '("rg" "ag" "pt" "ack"))) (intern (concat "counsel-" app))))

(when (boundp 'vanilla)
  ;; sorta following spacemacs leader key
  (general-def global-map "<f12> b b" #'switch-to-buffer)
  (general-def global-map "<f12> b d" #'my-revisit-buffer)
  (general-def global-map "<f12> b k" #'kill-this-buffer)
  (general-def global-map "<f12> e d" #'edebug-defun) ;; https://www.spacemacs.org/doc/DOCUMENTATION
  (general-def global-map "<f12> e l" #'edebug-eval-last-sexp)
  (general-def global-map "<f12> f s" #'save-buffer)
  (general-def global-map "<f12> g a" #'my-save-buffer-and-amend) ;; my addition
  (general-def global-map "<f12> g s" #'magit-status)
  (general-def global-map "<f12> o a" #'org-agenda)
  (general-def global-map "<f12> o c" #'org-capture)
  (general-def global-map "<f12> p v" #'magit-status) ;; the new hotkey...
  (general-def global-map "<f12> l"   #'my-spawn-process)
  )

;; sp
(general-def global-map "M-<backspace>" #'sp-backward-unwrap-sexp)
(general-def global-map "M-<delete>"    #'sp-unwrap-sexp)
(general-def global-map "M-<insert>"    #'sp-rewrap-sexp)
(general-def global-map "<f5> w"    #'sp-rewrap-sexp)
(general-def smartparens-mode-map "C-;"         #'sp-comment)
(general-def smartparens-mode-map "C-<left>"    #'sp-forward-barf-sexp)
(general-def smartparens-mode-map "C-<right>"   #'sp-forward-slurp-sexp)
(general-def smartparens-mode-map "C-M-<left>"  #'sp-backward-slurp-sexp)
(general-def smartparens-mode-map "C-M-<right>" #'sp-backward-barf-sexp)
(general-def smartparens-mode-map "C-M-a"       #'sp-backward-down-sexp)
(general-def smartparens-mode-map "C-2 a"       #'sp-join-sexp)
(general-def smartparens-mode-map "C-2 b"       #'sp-select-next-thing)
(general-def smartparens-mode-map "C-2 c"       #'sp-beginning-of-sexp)
(general-def smartparens-mode-map "C-2 d"       #'sp-beginning-of-next-sexp)
(general-def smartparens-mode-map "C-2 e"       #'sp-end-of-sexp)
(general-def smartparens-mode-map "C-2 f"       #'sp-add-to-next-sexp)
(general-def smartparens-mode-map "C-2 g"       #'sp-add-to-previous-sexp)
(general-def smartparens-mode-map "C-2 h"       #'sp-split-sexp)
(general-def smartparens-mode-map "C-2 i"       #'sp-splice-sexp)
(general-def smartparens-mode-map "C-2 j"       #'sp-emit-sexp)
(general-def smartparens-mode-map "C-2 k"       #'sp-absorb-sexp)
(general-def smartparens-mode-map "C-2 l"       #'sp-convolute-sexp)
(general-def smartparens-mode-map "C-2 m"       #'sp-forward-symbol)
(general-def smartparens-mode-map "C-2 n"       #'sp-backward-symbol)
(general-def smartparens-mode-map "C-2 o"       #'sp-wrap)
(general-def smartparens-mode-map "C-2 p"       #'sp-backward-up-sexp)
(general-def smartparens-mode-map "C-2 q"       #'sp-up-sexp)
(general-def smartparens-mode-map "C-2 r"       #'sp-select-next-thing-exchange)
(general-def smartparens-mode-map "C-2 s"       #'sp-select-previous-thing)
(general-def smartparens-mode-map "C-M-w"         #'sp-copy-sexp)
(general-def smartparens-mode-map "M-<backspace>" #'sp-backward-unwrap-sexp)
(general-def smartparens-mode-map "M-<delete>"    #'sp-unwrap-sexp)
(general-def smartparens-mode-map "s-<SPC>"       #'sp-mark-sexp)
(general-def smartparens-mode-map [remap backward-kill-sexp]  #'sp-backward-kill-sexp);DEL
(general-def smartparens-mode-map [remap backward-list]       #'sp-previous-sexp);p
(general-def smartparens-mode-map [remap backward-sexp]       #'sp-backward-sexp);b
(general-def smartparens-mode-map [remap backward-up-list]    #'sp-backward-up-sexp);u
(general-def smartparens-mode-map [remap down-list]           #'sp-down-sexp);d
(general-def smartparens-mode-map [remap forward-list]        #'sp-next-sexp);n
(general-def smartparens-mode-map [remap forward-sexp]        #'sp-forward-sexp);f
(general-def smartparens-mode-map [remap kill-sexp]           #'sp-kill-sexp);k
(general-def smartparens-mode-map [remap kill-whole-line]     #'sp-kill-whole-line)
(general-def smartparens-mode-map [remap mark-sexp]           #'sp-mark-sexp);SPC
(general-def smartparens-mode-map [remap transpose-sexps]     #'sp-transpose-sexp);t

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
