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
(keymap-unset global-map "<XF86Back>" t) ;; previous-buffer
(keymap-unset global-map "<XF86Forward>" t) ;; next-buffer

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
;; Don't waste good keys (C-123456890) on digit-argument.  But make it more
;; convenient to access them in other ways.
;;
;; - instead of C-u, let C-=, M-=, s-= be universal argument
;; - Let C--, M--, s-- be negative argument
;; - Let - and = be neg. and univ. argument when any hydra is open
;; - Let - and = be neg. and univ. argument when any prefix argument has been called and awaiting next input
;; - Allow typing M-= M-9 M-d, much better than M-= 9 M-t

(keymap-unset global-map "C-u" t)
(keymap-unset universal-argument-map "C-u" t)

(after! hydra
  (define-key hydra-base-map (kbd "C-u") nil)
  (define-key hydra-base-map (kbd "=") #'hydra--universal-argument)
  (define-key hydra-base-map (kbd "-") #'hydra--negative-argument))

(let ((modifiers '("C-" "M-" "s-" "H-" "A-"))
      (digits (split-string "1234567890" "" t)))
  (dolist (mod modifiers)
    ;; Some modes bind e.g. M-- (org-mode with org-replace-disputed-keys t), so
    ;; override everywhere.  Actually even if we haven't discovered any
    ;; conflicts it makes sense to encode that this must work everywhere.
    ;; However we may run into a problem where it also overrides hydra-base-map...
    ;;
    ;; TODO: Don't rely on general (perhaps provide a way in
    ;; deianira-mass-remap.el, although just Emacs internals would be great).
    ;; You'll note dei--known-keymaps does NOT include hydra-base-map as it's
    ;; not a mode map.  In other words transient maps like that will work as
    ;; intended.  Elegant.  Does general override have the same elegance?
    (define-key general-override-mode-map (kbd (concat mod "=")) #'universal-argument)
    (define-key general-override-mode-map (kbd (concat mod "-")) #'negative-argument)

    (define-key global-map (kbd (concat mod "=")) #'universal-argument)
    (define-key global-map (kbd (concat mod "-")) #'negative-argument)
    (define-key universal-argument-map (kbd (concat mod "=")) #'universal-argument-more)
    ;; necessary?
    (after! hydra
      (define-key hydra-base-map (kbd (concat mod "=")) #'hydra--universal-argument)
      (define-key hydra-base-map (kbd (concat mod "-")) #'hydra--negative-argument))
    (dolist (d digits)
      (define-key global-map (kbd (concat mod d)) nil)
      (define-key universal-argument-map (kbd (concat mod d)) #'digit-argument)
      ;; REVIEW: does it mess with nonum hydras?
      (after! hydra
        (define-key hydra-base-map (kbd (concat mod d)) #'hydra--digit-argument)))))


;;; More repeaters! Repeaters are love and life.

;; https://tildegit.org/acdw/define-repeat-map.el
(after! define-repeat-map

  (define-repeat-map my-buffer-thumbing
    ("<right>"   next-buffer
     "C-<right>" next-buffer
     "<left>"   previous-buffer
     "C-<left>" previous-buffer))

  (define-repeat-map my-nav
    ("f" forward-char
     "b" backward-char
     "n" next-line
     "p" previous-line))

  ;; from author of define-repeat-map
  (define-repeat-map my-case
    ("c" capitalize-word
     "u" upcase-word
     "l" downcase-word)
    (:continue "f" forward-word
               "b" backward-word)
    (:enter downcase-dwim
            upcase-dwim
            capitalize-dwim)))

(define-key global-map [remap org-roam-node-random] (defrepeater #'org-roam-node-random))

;; mc/ commands have some magic to avoid asking about re-running themselves once
;; for all cursors ... We need to apply the magic to the repeating version of
;; the command as well.  I considered using define-repeat-map, but it does not
;; make sense to me to bind every mc/ variant inside the same repeat-map.  I
;; want just the single one to become repeatable, although the correct response
;; in this sort of situation is to just remember the `repeat' command.  But
;; correct for who? If I want to type <f3> m n n n n n n n n instead of using
;; repeat, there should bloody well be some convenient elisp to allow it.  And
;; this is the convenient elisp, it just does not work for self-aware commands.

;; (define-key global-map [remap mc/mark-next-like-this] (defrepeater #'mc/mark-next-like-this))
;; (define-key global-map [remap mc/mark-previous-like-this] (defrepeater #'mc/mark-previous-like-this))

;; While we're at it, enhance the classic `repeat'.  Note that it's totally
;; separate from the Emacs 28 repeat-map system.

;; Let me type a digit such as 5 after a `repeat' to repeat another 5 times.
(advice-add #'repeat :after #'my-enable-post-repeat-transient-map)



;;; Create minor mode maps for modes that lack them

(defvar my-abbrev-minor-mode-map (make-sparse-keymap))
(add-to-list 'minor-mode-map-alist (cons 'abbrev-mode my-abbrev-minor-mode-map))

(defvar my-anki-editor-mode-map (make-sparse-keymap))
(add-to-list 'minor-mode-map-alist (cons 'anki-editor-mode my-anki-editor-mode-map))


;;; Key translations
;; TODO:  move this stuff to kmonad or some such external program

;; Civilize GUI Emacs.  It doesn't always work, see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=58808
;; TODO: do this in kmonad, but do it only for eamcs, or use exwm simulation keys to turn escape back into a real escape for other apps.
(define-key function-key-map    (kbd "<escape>") (kbd "C-g"))
(define-key key-translation-map (kbd "<escape>") (kbd "C-g"))
(define-key input-decode-map    (kbd "<escape>") (kbd "C-g"))
(define-key input-decode-map    (kbd "C-g") (kbd "<f35>")) ;; to unlearn it

;; Make my Lenovo Thinkpad like the Dell Latitude I used to have
(define-key key-translation-map (kbd "<XF86Back>"      ) (kbd "<prior>"))
(define-key key-translation-map (kbd "C-<XF86Back>"    ) (kbd "C-<prior>"))
(define-key key-translation-map (kbd "M-<XF86Back>"    ) (kbd "M-<prior>"))
(define-key key-translation-map (kbd "s-<XF86Back>"    ) (kbd "s-<prior>"))
(define-key key-translation-map (kbd "<XF86Forward>"   ) (kbd "<next>"))
(define-key key-translation-map (kbd "C-<XF86Forward>" ) (kbd "C-<next>"))
(define-key key-translation-map (kbd "M-<XF86Forward>" ) (kbd "M-<next>"))
(define-key key-translation-map (kbd "s-<XF86Forward>" ) (kbd "s-<next>"))


;;; Main

(setopt doom-leader-alt-key "<f3>")
(setopt doom-localleader-alt-key "<f4>")

(after! key-chord
  (key-chord-define-global "cd" #'calc-dispatch))

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
  (keymap-set eww-mode-map "q" #'kill-current-buffer)
  (keymap-set eww-bookmark-mode-map "w" #'my-eww-bookmark-copy-url))

(after! vertico
  (keymap-set global-map "M-<backspace>" (lambda()(interactive)(message "no")))
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

(after! esh-mode
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
  (keymap-set global-map "C-'" #'sp-mark-sexp)
  (keymap-set global-map "C-;" #'sp-comment)
  (keymap-set global-map "C-<left>" #'sp-forward-barf-sexp)
  (keymap-set global-map "C-<right>" #'sp-forward-slurp-sexp)
  (keymap-set global-map "C-M-<left>" #'sp-backward-slurp-sexp)
  (keymap-set global-map "C-M-<right>" #'sp-backward-barf-sexp)
  (keymap-set global-map "M-<backspace>" #'sp-backward-unwrap-sexp)
  (keymap-set global-map "M-<delete>" #'sp-unwrap-sexp)
  (keymap-set global-map "C-M-<SPC>" #'sp-mark-sexp)
  (keymap-set global-map "C-M-<left>" #'sp-backward-slurp-sexp)
  (keymap-set global-map "C-M-<right>" #'sp-backward-barf-sexp)
  (keymap-set global-map "C-M-<delete>" #'sp-splice-sexp-killing-forward)
  (keymap-set global-map "C-M-<backspace>" #'sp-splice-sexp-killing-backward)
  (keymap-set global-map "C-M-a" #'sp-backward-down-sexp)
  (keymap-set global-map "C-M-b" #'sp-backward-sexp)
  (keymap-set global-map "C-M-f" #'sp-forward-sexp)
  (keymap-set global-map "C-M-d" #'sp-down-sexp)
  (keymap-set global-map "C-M-e" #'sp-up-sexp)
  (keymap-set global-map "C-M-h" #'sp-mark-sexp)
  (keymap-set global-map "C-M-k" #'sp-kill-sexp)
  (keymap-set global-map "C-M-n" #'sp-next-sexp)
  (keymap-set global-map "C-M-p" #'sp-previous-sexp)
  (keymap-set global-map "C-M-t" #'sp-transpose-sexp)
  (keymap-set global-map "C-M-u" #'sp-backward-up-sexp)
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

(after! embark
  (keymap-set embark-general-map "M-<menu>" #'hkey-either)
  (keymap-set embark-general-map "C-;"     #'hkey-either))

(after! view
  (keymap-set view-mode-map "e" #'my-view-exit-and-reopen-as-root))

(after! org-keys
  (keymap-set org-mode-map "C-c u" #'my-insert-heading-with-id)
  (keymap-set org-mode-map "C-c f"#'org-roam-node-find)
  (keymap-set org-mode-map "C-c i"#'org-roam-node-insert))

;; Use the key physically labelled "Caps Lock" as my new M-x.
;; TIP: it also unlocks the comfy combo M-<menu>.
(when (eq 'window-system 'x)
  (my-exec "setxkbmap" "-option" "caps:menu" "altwin:menu_super"))
(keymap-set global-map "<menu>" #'execute-extended-command)
(keymap-set global-map "M-<menu>" #'embark-act)

;; Grand list

(keymap-set global-map "<f10> a"                    #'my-save-buffer-and-amend)
(keymap-set global-map "<f10> d"                    #'org-download-yank)
(keymap-set global-map "<f10> e"                    #'eww)
(keymap-set global-map "<f10> g"                    #'guix-popup)
(keymap-set global-map "<f10> k"                    #'gif-screencast-start-or-stop)
(keymap-set global-map "<f10> l"                    #'mw-thesaurus-lookup)
(keymap-set global-map "<f10> n"                    #'my-normie-toggle)
(keymap-set global-map "<f10> p"                    #'my-pipe)
(keymap-set global-map "<f10> r c"                  #'my-copy-region-to-variable)
(keymap-set global-map "<f10> r e"                  #'my-eval-and-replace-print)
(keymap-set global-map "<f10> r p"     (defrepeater #'my-cycle-path-at-point))
(keymap-set global-map "<f10> r v"                  #'my-replace-var-at-point-with-value)
(keymap-set global-map "<f10> r w"                  #'my-copy-region-or-rest-of-line-to-other-window)
(keymap-set global-map "<f10> s"                    #'my-save-buffer-and-commit)
(keymap-set global-map "<f2> 1"        (defrepeater #'my-insert-other-buffer-file-name-and-cycle))
(keymap-set global-map "<f2> 2"        (defrepeater #'my-toggle-selective-display))
(keymap-set global-map "<f2> 3"                     #'elfeed)
(keymap-set global-map "<f2> 5"                     #'my-lookup-word)
(keymap-set global-map "<f2> <f2>"                  #'vc-msg-show)
(keymap-set global-map "<f2> <f3>"                  #'git-messenger:popup-message)
(keymap-set global-map "<f2> <next>"   (defrepeater #'my-next-buffer-of-same-mode))
(keymap-set global-map "<f2> <prior>"  (defrepeater #'my-previous-buffer-of-same-mode))
(keymap-set global-map "<f2> ("                     #'app-launcher-run-app)
(keymap-set global-map "<f2> b"                     #'backup-walker-start)
(keymap-set global-map "<f2> c"                     #'org-roam-capture)
(keymap-set global-map "<f2> d"                     #'my-insert-today)
(keymap-set global-map "<f2> e d"                   #'eval-defun)
(keymap-set global-map "<f2> e e"                   #'eval-last-sexp)
(keymap-set global-map "<f2> e l"                   #'load-library)
(keymap-set global-map "<f2> e p"                   #'eval-print-last-sexp)
(keymap-set global-map "<f2> e r"                   #'eval-region)
(keymap-set global-map "<f2> e s"                   #'ess-eval-region-or-function-or-paragraph-and-step) ;; ess everywhere
(keymap-set global-map "<f2> e x"                   #'eval-expression)
(keymap-set global-map "<f2> f"                     #'org-roam-node-find)
(keymap-set global-map "<f2> i"                     #'org-roam-node-insert)
(keymap-set global-map "<f2> g"                     #'git-timemachine)
(keymap-set global-map "<f2> h"                     #'consult-find)
(keymap-set global-map "<f2> j"                     #'+vertico/find-file-in)
(keymap-set global-map "<f2> k"                     #'+vertico/project-search)
(keymap-set global-map "<f2> l"                     #'helm-locate)
(keymap-set global-map "<f2> n"                     #'org-roam-dailies-capture-today)
(keymap-set global-map "<f2> m"                     #'my-last-daily-file)
(keymap-set global-map "<f2> o"                     #'helpful-symbol)
(keymap-set global-map "<f2> p"                     #'my-spawn-process)
(keymap-set global-map "<f2> r"                     #'vertico-repeat)
(keymap-set global-map "<f2> s"                     #'helm-selector-shell)
(keymap-set global-map "<f2> v"                     #'helpful-variable)
(keymap-set global-map "<f2> w"                     #'sp-rewrap-sexp)
(keymap-set global-map "<f2> x"                     #'execute-extended-command)
(keymap-set global-map "<f2> z"                     #'my-sleep)
(keymap-set global-map "<f5>"                       #'repeat)
(keymap-set global-map "C-<prior>"                  #'iflipb-previous-buffer)
(keymap-set global-map "C-<next>"                   #'iflipb-next-buffer)
(keymap-set global-map "C-;"                        #'embark-act) ;; like doom, but global
(keymap-set global-map "C-h C-h"                    #'my-describe-last-key)
(keymap-set global-map "C-h q"                      #'quoted-insert)
(keymap-set global-map "C-x C-\;"      (defrepeater #'comment-line))
(keymap-set global-map "C-0"                        #'hippie-expand)
(keymap-set global-map "C-1"                        #'switch-to-buffer)
(keymap-set global-map "C-2"                        #'other-window)
(keymap-set global-map "C-3"                        #'unexpand-abbrev)
(keymap-set global-map "C-4"                        #'my-stim)
(keymap-set global-map "C-5"                        #'my-prev-file-in-dir)
(keymap-set global-map "C-6"                        #'my-next-file-in-dir)
(keymap-set global-map "C-8"                        #'kill-whole-line)
(keymap-set global-map "C-9"                        #'crux-duplicate-current-line-or-region)
(keymap-set global-map "M-<backspace>"              #'sp-backward-unwrap-sexp)
(keymap-set global-map "M-<delete>"                 #'sp-unwrap-sexp)
(keymap-set global-map "M-<f4>"                     #'kill-current-buffer)
(keymap-set global-map "M-<insert>"                 #'sp-rewrap-sexp)
(keymap-set global-map "M-g a a"       (defrepeater #'avy-pop-mark))
(keymap-set global-map "M-g a c"                    #'avy-goto-char-2)
(keymap-set global-map "M-g a g c"                  #'avy-goto-char-2)
(keymap-set global-map "M-g a g e"                  #'avy-goto-end-of-line)
(keymap-set global-map "M-g a g l"                  #'avy-goto-line)
(keymap-set global-map "M-g a g o"                  #'avy-goto-word-or-subword-1)
(keymap-set global-map "M-g a g q"                  #'avy-goto-subword-1)
(keymap-set global-map "M-g a g s"                  #'avy-goto-symbol-1)
(keymap-set global-map "M-g a g w"                  #'avy-goto-word-1)
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
(keymap-set global-map "M-g e"                      #'consult-error)
(keymap-set global-map "M-g i"                      #'consult-imenu)
(keymap-set global-map "M-g k"                      #'consult-global-mark)
(keymap-set global-map "M-g l"                      #'consult-line)
(keymap-set global-map "M-g m"                      #'consult-mark)
(keymap-set global-map "M-g o"                      #'consult-outline)
(keymap-set global-map "M-g z"                      #'avy-goto-word-or-subword-1)
(keymap-set global-map "M-m g"         (defrepeater #'pop-global-mark)) ;; was C-x C-SPC
(keymap-set global-map "M-m m"                      #'set-mark-command) ;; was C-SPC
(keymap-set global-map "M-m p"         (defrepeater #'pop-to-mark-command))
(keymap-set global-map "M-m r"                      #'rectangle-mark-mode) ;; was C-x SPC
(keymap-set global-map "M-m x"                      #'exchange-point-and-mark) ;; also on C-x C-x
(keymap-set global-map "M-o ="                      #'doom/increase-font-size)
(keymap-set global-map "M-s 5"                      #'query-replace)
(keymap-set global-map "M-s f"         (defrepeater #'my-fill-unfill-respect-double-space))
(keymap-set global-map "M-s m"                      #'consult-multi-occur)
(keymap-set global-map "M-s r"                      #'isearch-backward)
(keymap-set global-map "M-s s"                      #'isearch-forward)
(keymap-set global-map "M-|"                        #'my-shell-command-replace-region)
(keymap-set global-map "TAB"                        #'my-tab-command)
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
(define-key key-translation-map (kbd "<f7> a") (kbd "α")) ;;alpha
(define-key key-translation-map (kbd "<f7> b") (kbd "β")) ;;beta
(define-key key-translation-map (kbd "<f7> c") (kbd "χ")) ;;chi
(define-key key-translation-map (kbd "<f7> d") (kbd "δ")) ;;delta
(define-key key-translation-map (kbd "<f7> e") (kbd "ε")) ;;epsilon
(define-key key-translation-map (kbd "<f7> f") (kbd "φ")) ;;phi
(define-key key-translation-map (kbd "<f7> g") (kbd "γ")) ;;gamma
(define-key key-translation-map (kbd "<f7> h") (kbd "θ")) ;;theta
(define-key key-translation-map (kbd "<f7> i") (kbd "ι")) ;;iota
(define-key key-translation-map (kbd "<f7> k") (kbd "κ")) ;;kappa
(define-key key-translation-map (kbd "<f7> l") (kbd "λ")) ;;lambda
(define-key key-translation-map (kbd "<f7> m") (kbd "μ")) ;;mu
(define-key key-translation-map (kbd "<f7> n") (kbd "η")) ;;eta
(define-key key-translation-map (kbd "<f7> o") (kbd "ω")) ;;omega
(define-key key-translation-map (kbd "<f7> p") (kbd "π")) ;;pi
(define-key key-translation-map (kbd "<f7> r") (kbd "ρ")) ;;rho
(define-key key-translation-map (kbd "<f7> s") (kbd "σ")) ;;sigma
(define-key key-translation-map (kbd "<f7> t") (kbd "τ")) ;;tau
(define-key key-translation-map (kbd "<f7> u") (kbd "υ")) ;;upsilon
(define-key key-translation-map (kbd "<f7> v") (kbd "ν")) ;;nu
(define-key key-translation-map (kbd "<f7> x") (kbd "ξ")) ;;xi
(define-key key-translation-map (kbd "<f7> z") (kbd "ζ")) ;;zeta
;; (keymap-set key-translation-map "<f7> w" "")
;; (keymap-set key-translation-map "<f7> y" "")
;; (keymap-set key-translation-map "<f7> j" "")
;; (keymap-set key-translation-map "<f7> q" "")

(my-normie:abnormalize)
