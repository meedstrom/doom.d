;; Key bindings -*- lexical-binding: t; -*-

(require 'defrepeater) ;; not needed if i would just remember to call `repeat'!
(require 'define-repeat-map)


;;; Create minor mode maps for modes that lack them

(defvar-keymap my-abbrev-minor-mode-map)
(add-to-list 'minor-mode-map-alist (cons 'abbrev-mode my-abbrev-minor-mode-map))


;;; Grand list
;; One day, I might relocate these settings to be more context-relevant
;; (i.e. near use-package forms or such), then consult the total list with
;; `general-describe-keybindings', although that one is cluttered by Doom's
;; settings, or bind-key's `describe-personal-keybindings'.  But I enjoy
;; handling them this way, and the upside with setting the bindings this early
;; is that I still have them all when half my init has broken.

(keymap-set global-map "<f10> a" #'my-save-buffer-and-amend)
(keymap-set global-map "<f10> d" #'org-download-yank)
(keymap-set global-map "<f10> e" #'eww)
(keymap-set global-map "<f10> g" #'guix-popup)
(keymap-set global-map "<f10> k" #'gif-screencast-start-or-stop)
(keymap-set global-map "<f10> l" #'mw-thesaurus-lookup)
(keymap-set global-map "<f10> n" #'my-normie-toggle)
(keymap-set global-map "<f10> p" #'my-pipe)
(keymap-set global-map "<f10> r c" #'my-copy-region-to-variable)
(keymap-set global-map "<f10> r e" #'my-eval-and-replace-print)
(keymap-set global-map "<f10> r p" (defrepeater #'my-cycle-path-at-point))
(keymap-set global-map "<f10> r v" #'my-replace-var-at-point-with-value)
(keymap-set global-map "<f10> r w" #'my-copy-region-or-rest-of-line-to-other-window)
(keymap-set global-map "<f10> s" #'my-save-buffer-and-commit)
(keymap-set global-map "<f2> (" #'app-launcher-run-app)
(keymap-set global-map "<f2> 1" (defrepeater #'my-insert-other-buffer-file-name-and-cycle))
(keymap-set global-map "<f2> 2" (defrepeater #'my-toggle-selective-display))
(keymap-set global-map "<f2> 3" #'elfeed)
(keymap-set global-map "<f2> 5" #'my-lookup-word)
(keymap-set global-map "<f2> <f2>" #'vc-msg-show)
(keymap-set global-map "<f2> <f3>" #'git-messenger:popup-message)
(keymap-set global-map "<f2> <next>" (defrepeater #'my-next-buffer-of-same-mode))
(keymap-set global-map "<f2> <prior>" (defrepeater #'my-previous-buffer-of-same-mode))
(keymap-set global-map "<f2> b" #'backup-walker-start)
(keymap-set global-map "<f2> c" #'org-roam-capture)
(keymap-set global-map "<f2> d" #'my-insert-today)
(keymap-set global-map "<f2> e d" #'eval-defun)
(keymap-set global-map "<f2> e e" #'eval-last-sexp)
(keymap-set global-map "<f2> e l" #'load-library)
(keymap-set global-map "<f2> e p" #'eval-print-last-sexp)
(keymap-set global-map "<f2> e r" #'eval-region)
(keymap-set global-map "<f2> e s" #'ess-eval-region-or-function-or-paragraph-and-step) ;; ess everywhere
(keymap-set global-map "<f2> e x" #'eval-expression)
(keymap-set global-map "<f2> f" #'org-roam-node-find)
(keymap-set global-map "<f2> g" #'git-timemachine)
(keymap-set global-map "<f2> h" #'consult-find)
(keymap-set global-map "<f2> i" #'org-roam-node-insert)
(keymap-set global-map "<f2> j" #'+default/find-file-under-here)
(keymap-set global-map "<f2> k" #'+default/search-project)
(keymap-set global-map "<f2> l" #'helm-locate)
(keymap-set global-map "<f2> m" #'my-last-daily-file)
(keymap-set global-map "<f2> n" #'org-roam-dailies-capture-today)
(keymap-set global-map "<f2> p" #'my-spawn-process)
(keymap-set global-map "<f2> r" #'vertico-repeat)
(keymap-set global-map "<f2> s" #'helm-selector-shell)
(keymap-set global-map "<f2> w" #'sp-rewrap-sexp)
(keymap-set global-map "<f2> x" #'execute-extended-command)
(keymap-set global-map "<f2> z" #'my-sleep)
(keymap-set global-map "<f5>" #'repeat)
(keymap-set global-map "C-0" #'hippie-expand)
(keymap-set global-map "C-1" #'switch-to-buffer)
(keymap-set global-map "C-2" #'my-other-window-any-frame-hyprland)
(keymap-set global-map "C-3" #'unexpand-abbrev)
(keymap-set global-map "C-4" #'my-stim)
(keymap-set global-map "C-5" #'my-prev-file-in-dir)
(keymap-set global-map "C-6" #'my-next-file-in-dir)
(keymap-set global-map "C-8" #'kill-whole-line)
(keymap-set global-map "C-9" #'duplicate-dwim)
(keymap-set global-map "C-;" #'embark-act) ;; like doom, but in all buffers
(keymap-set global-map "C-<next>" #'iflipb-next-buffer)
(keymap-set global-map "C-<prior>" #'iflipb-previous-buffer)
(keymap-set global-map "C-M-/" #'dabbrev-expand)
(keymap-set global-map "C-h C-h" #'my-describe-last-key)
(keymap-set global-map "C-h M" #'describe-mode)
(keymap-set global-map "C-h m" #'consult-minor-mode-menu)
(keymap-set global-map "C-h q" #'quoted-insert)
(keymap-set global-map "C-h r" #'consult-info)
(keymap-set global-map "C-h s" #'find-function)
(keymap-set global-map "C-h t" #'doom/toggle-profiler)
(keymap-set global-map "C-q" #'my-dired-shell-cycle)
(keymap-set global-map "C-x C-\;" (defrepeater #'comment-line))
(keymap-set global-map "C-x k c" #'consult-kmacro)
(keymap-set global-map "M-/" #'dabbrev-completion)
(keymap-set global-map "M-1" #'switch-to-buffer)
(keymap-set global-map "M-2" #'my-other-window-any-frame-hyprland)
(keymap-set global-map "M-<backspace>" #'sp-backward-unwrap-sexp)
(keymap-set global-map "M-<delete>" #'sp-unwrap-sexp)
(keymap-set global-map "M-<f4>" #'kill-current-buffer)
(keymap-set global-map "M-<insert>" #'sp-rewrap-sexp)
(keymap-set global-map "M-g a a" (defrepeater #'avy-pop-mark))
(keymap-set global-map "M-g a c" #'avy-goto-char-2)
(keymap-set global-map "M-g a g c" #'avy-goto-char-2)
(keymap-set global-map "M-g a g e" #'avy-goto-end-of-line)
(keymap-set global-map "M-g a g l" #'avy-goto-line)
(keymap-set global-map "M-g a g o" #'avy-goto-word-or-subword-1)
(keymap-set global-map "M-g a g q" #'avy-goto-subword-1)
(keymap-set global-map "M-g a g s" #'avy-goto-symbol-1)
(keymap-set global-map "M-g a g w" #'avy-goto-word-1)
(keymap-set global-map "M-g a k" #'avy-kill-region)
(keymap-set global-map "M-g a m l" #'avy-move-line)
(keymap-set global-map "M-g a m r" #'avy-move-region)
(keymap-set global-map "M-g a n" #'avy-next)
(keymap-set global-map "M-g a o" #'avy-goto-symbol)
(keymap-set global-map "M-g a p" #'avy-prev)
(keymap-set global-map "M-g a r" #'avy-resume)
(keymap-set global-map "M-g a s" #'avy-isearch)
(keymap-set global-map "M-g a w" #'avy-kill-ring-save-region)
(keymap-set global-map "M-g c" #'avy-goto-char-timer)
(keymap-set global-map "M-g e" #'consult-error)
(keymap-set global-map "M-g i" #'consult-imenu)
(keymap-set global-map "M-g k" #'consult-global-mark)
(keymap-set global-map "M-g l" #'consult-line)
(keymap-set global-map "M-g m" #'consult-mark)
(keymap-set global-map "M-g o" #'consult-outline)
(keymap-set global-map "M-g z" #'avy-goto-word-or-subword-1)
(keymap-set global-map "M-m g" (defrepeater #'pop-global-mark)) ;; was C-x C-SPC
(keymap-set global-map "M-m m" #'set-mark-command) ;; was C-SPC
(keymap-set global-map "M-m p" (defrepeater #'pop-to-mark-command))
(keymap-set global-map "M-m r" #'rectangle-mark-mode) ;; was C-x SPC
(keymap-set global-map "M-m x" #'exchange-point-and-mark) ;; also on C-x C-x
(keymap-set global-map "M-o -" #'doom/decrease-font-size)
(keymap-set global-map "M-o =" #'doom/increase-font-size)
(keymap-set global-map "M-o M--" #'doom/decrease-font-size)
(keymap-set global-map "M-s 5" #'query-replace)
(keymap-set global-map "M-s 6" #'query-replace-regexp)
(keymap-set global-map "M-s d" #'+default/search-cwd)
(keymap-set global-map "M-s f" (defrepeater #'my-fill-unfill-respect-double-space))
(keymap-set global-map "M-s l" #'+default/find-file-under-here)
(keymap-set global-map "M-s m" #'consult-multi-occur)
(keymap-set global-map "M-s p" #'+default/search-project)
(keymap-set global-map "M-s r" #'isearch-backward)
(keymap-set global-map "M-s s" #'isearch-forward)
(keymap-set global-map "M-|" #'my-shell-command-replace-region)
;; (keymap-set global-map "C-q" #'+shell/here)
;; (keymap-set global-map "M-r" #'hkey-either)
;; (keymap-set global-map "TAB" #'my-tab-command)

(keymap-set isearch-mode-map "<down>" #'isearch-repeat-forward)
(keymap-set isearch-mode-map "<up>" #'isearch-repeat-backward)
(keymap-set isearch-mode-map "M-s n" #'isearch-repeat-forward)
(keymap-set isearch-mode-map "M-s p" #'isearch-repeat-backward)
(keymap-set my-abbrev-minor-mode-map "`" #'expand-abbrev)

;; (keymap-set "" #'consult-focus-lines)  ;; Man.  Disturbing command.
;; (keymap-set minibuffer-mode-map "M-g i" #'consult-imenu-multi)
;; (keymap-set "" #'browse-url)

;; Some Greek letters
;; (This sequence doesn't align with the official Greek alphabet)
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

(setopt doom-leader-alt-key "<f3>")
(setopt doom-localleader-alt-key "<f4>")

(after! doom-keybinds
  (keymap-set doom-leader-map "f d" (keymap-lookup doom-leader-map "f D")))

(after! mu4e-main
  ;; .. thinking that I should let massmapper.el translate all capital bindings
  ;; to lowercase letters when the lowercase is unbound anyway
  ;; (keymap-unset mu4e-main-mode-map "C" t)
  ;; (keymap-unset mu4e-main-mode-map "U" t)
  (keymap-set mu4e-main-mode-map "c" #'mu4e-compose-new)
  (keymap-set mu4e-main-mode-map "u" #'mu4e-update-mail-and-index))

(after! dired-hist
  (keymap-set dired-mode-map "l" #'dired-hist-go-back)
  ;; the hell
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
  (keymap-set vertico-map "M-<backspace>" #'vertico-directory-up)
  (keymap-set vertico-map "<next>" #'scroll-up-command)
  (keymap-set vertico-map "<prior>" #'scroll-down-command))

(after! ledger-mode
  (keymap-set ledger-mode-map "M-<return>" #'crux-duplicate-current-line-or-region))

(after! shell
  (keymap-set shell-mode-map "C-S-n" #'my-new-shell))

(after! esh-mode
  (keymap-set eshell-mode-map "C-S-n" #'my-new-eshell)
  (keymap-set eshell-mode-map "<f4> n" (defrepeater #'my-esh-narrow-dwim)))

(after! ctrlf
  (keymap-set ctrlf-minibuffer-mode-map "<down>"   #'ctrlf-forward-default)
  (keymap-set ctrlf-minibuffer-mode-map "M-<down>" #'ctrlf-forward-alternate)
  (keymap-set ctrlf-minibuffer-mode-map "<up>"     #'ctrlf-backward-default)
  (keymap-set ctrlf-minibuffer-mode-map "M-<up>"   #'ctrlf-backward-alternate))

(after! dired
  (keymap-set dired-mode-map "b" #'dired-up-directory)
  (keymap-set dired-mode-map ")" #'dired-git-info-mode)
  (keymap-set dired-mode-map "M-<up>" #'dired-up-directory)
  ;; (keymap-set dired-mode-map "s-<return>" #'my-dired-open-file-with-default-tool)
  (keymap-set dired-mode-map "s-<return>" #'embark-open-externally))

(after! embark
  (keymap-set embark-general-map "C-;" #'hkey-either))

(after! grep
  (keymap-set grep-mode-map "C-x C-q" #'wgrep-change-to-wgrep-mode))

(after! view
  (keymap-set view-mode-map "e" #'my-view-exit-and-reopen-as-root))

(after! org-keys
  (keymap-set org-mode-map "C-c u" #'my-insert-heading-with-id)
  (keymap-set org-mode-map "C-c f" #'org-roam-node-find)
  (keymap-set org-mode-map "C-c i" #'org-roam-node-insert))

;; Override some Doom Org keys
(my-hook-once 'org-load-hook
  (map! :map org-mode-map :localleader "i" #'my-org-id-get-create-and-copy))

(after! dired
  ;; Dired's default unbound keys: `, b, E, J, K, r, z, <backspace>
  ;; Dired's useless keys: h, 1234567890
  )

;; (after! which-key
;;   (keymap-set which-key-mode-map "DEL" #'which-key-undo-key))


;;; Civilize C-g
;; Doesn't always work, see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=58808

;; TODO: move this to kmonad or some such external program.  Possibilities:
;; - Translate keys only when emacs is in focus, if possible
;; - Translate keys globally, and use EXWM simulation keys to translate back
;;   into a real escape for other apps

(define-key function-key-map    (kbd "<escape>") (kbd "C-g"))
(define-key key-translation-map (kbd "<escape>") (kbd "C-g"))
(define-key input-decode-map    (kbd "<escape>") (kbd "C-g"))


;;; More repeaters! Repeaters are love and life.

;; https://tildegit.org/acdw/define-repeat-map.el

(define-repeat-map my-buffer-thumbing
  ("<right>"   next-buffer
   "C-<right>" next-buffer
   "<left>"   previous-buffer
   "C-<left>" previous-buffer))

;; (define-repeat-map my-nav
;;   ("f" forward-char
;;    "b" backward-char
;;    "n" next-line
;;    "p" previous-line))

;; ;; from author of define-repeat-map
;; (define-repeat-map my-case
;;   ("c" capitalize-word
;;    "u" upcase-word
;;    "l" downcase-word)
;;   (:continue "f" forward-word
;;              "b" backward-word)
;;   (:enter downcase-dwim
;;           upcase-dwim
;;           capitalize-dwim))

(define-key global-map [remap org-roam-node-random] (defrepeater #'org-roam-node-random))

;; mc/ commands have some magic to avoid asking about re-running themselves
;; once for all cursors ... We need to apply the magic to the repeating version
;; of the command as well.  I considered using define-repeat-map, but it does
;; not make sense to me to bind every mc/ variant inside the same repeat-map.
;; I want just the single one to become repeatable, although the correct
;; response in this sort of situation is to just remember the `repeat' command.
;; But correct for who? If I want to type <f3> m n n n n n n n n instead of
;; using repeat, there should bloody well be some convenient elisp to allow it.
;; And this is the convenient elisp, it just does not work for self-aware
;; commands.

;; (define-key global-map [remap mc/mark-next-like-this] (defrepeater #'mc/mark-next-like-this))
;; (define-key global-map [remap mc/mark-previous-like-this] (defrepeater #'mc/mark-previous-like-this))

;; While we're at it, enhance the classic `repeat'.  Note that it's totally
;; separate from the Emacs 28 repeat-map system.

;; Let me type a digit such as 5 after a `repeat' to repeat another 5 times.
(advice-add #'repeat :after #'my-enable-post-repeat-transient-map)


;;; Smartparens.  Oh boy.

;; Smartparens guide: https://gist.github.com/pvik/8eb5755cc34da0226e3fc23a320a3c95
;; Author's config: https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
;; Xah's simplification: https://old.reddit.com/r/emacs/comments/3sfmkz/could_this_be_a_pareditsmartparens_killer/cwxocld/
(after! smartparens
  (keymap-set smartparens-strict-mode-map ";" #'comment-sp))

(keymap-set global-map "C-<left>" #'sp-forward-barf-sexp)
(keymap-set global-map "C-<right>" #'sp-forward-slurp-sexp)
(keymap-set global-map "C-M-<left>" #'sp-backward-slurp-sexp)
(keymap-set global-map "C-M-<right>" #'sp-backward-barf-sexp)
(keymap-set global-map "C-M-<left>" #'sp-backward-slurp-sexp)
(keymap-set global-map "C-M-<right>" #'sp-backward-barf-sexp)

;; Still haven't really used these
(keymap-set global-map "C-M-a" #'sp-backward-down-sexp)
(keymap-set global-map "C-M-e" #'sp-up-sexp)
(keymap-set global-map "C-M-u" #'sp-backward-up-sexp)
(keymap-set global-map "C-'" #'sp-mark-sexp)
(keymap-set global-map "C-;" #'sp-comment)
(keymap-set global-map "M-[" #'sp-wrap-round)

;; Already in agreement with doom's (default +smartparens):
;; (keymap-set global-map "M-<backspace>" #'sp-backward-unwrap-sexp)
;; (keymap-set global-map "M-<delete>" #'sp-unwrap-sexp)
;; (keymap-set global-map "C-M-b" #'sp-backward-sexp)
;; (keymap-set global-map "C-M-f" #'sp-forward-sexp)
;; (keymap-set global-map "C-M-d" #'sp-down-sexp)
;; (keymap-set global-map "C-M-n" #'sp-next-sexp)
;; (keymap-set global-map "C-M-p" #'sp-previous-sexp)
;; (keymap-set global-map "C-M-t" #'sp-transpose-sexp)

;; BUG: Smartparens kill-commands do not respect `kill-read-only-ok'.
;; https://github.com/Fuco1/smartparens/issues/1186
;; (keymap-set global-map "C-M-k" #'sp-kill-sexp)
;; (define-key global-map [remap kill-whole-line] #'sp-kill-whole-line)

;; Unassimilated Smartparens commands to try out.
;; (keymap-set global-map "C-M-<delete>" #'sp-splice-sexp-killing-forward)
;; (keymap-set global-map "C-M-<backspace>" #'sp-splice-sexp-killing-backward)
;; (keymap-set global-map "C-2 a" #'sp-join-sexp)
;; (keymap-set global-map "C-2 b" #'sp-select-next-thing)
;; (keymap-set global-map "C-2 c" #'sp-beginning-of-sexp)
;; (keymap-set global-map "C-2 d" #'sp-beginning-of-next-sexp)
;; (keymap-set global-map "C-2 e" #'sp-end-of-sexp)
;; (keymap-set global-map "C-2 f" #'sp-add-to-next-sexp)
;; (keymap-set global-map "C-2 g" #'sp-add-to-previous-sexp)
;; (keymap-set global-map "C-2 h" #'sp-split-sexp)
;; (keymap-set global-map "C-2 i" #'sp-splice-sexp)
;; (keymap-set global-map "C-2 j" #'sp-emit-sexp)
;; (keymap-set global-map "C-2 k" #'sp-absorb-sexp)
;; (keymap-set global-map "C-2 l" #'sp-convolute-sexp)
;; (keymap-set global-map "C-2 m" #'sp-forward-symbol)
;; (keymap-set global-map "C-2 n" #'sp-backward-symbol)
;; (keymap-set global-map "C-2 o" #'sp-wrap)
;; (keymap-set global-map "C-2 p" #'sp-backward-up-sexp)
;; (keymap-set global-map "C-2 q" #'sp-up-sexp)
;; (keymap-set global-map "C-2 r" #'sp-select-next-thing-exchange)
;; (keymap-set global-map "C-2 s" #'sp-select-previous-thing)


;; Use the key physically labelled "Caps Lock" as my new M-x.
;; TIP: it also unlocks the comfy combo M-<menu>.
(when (eq 'window-system 'x)
  (my-exec "setxkbmap" "-option" "caps:menu" "altwin:menu_super"))
(keymap-set global-map "<menu>" #'execute-extended-command)
;; (keymap-set global-map "M-<menu>" #'embark-act)
(after! general
  ;; dafuq is this set for? maybe i want to get rid of M-x, bro?
  (general-unbind general-override-mode-map "M-x")
  (general-unbind general-override-mode-map "A-x")
  ;; guess I should take a page from their book
  (general-def general-override-mode-map "<menu>" #'execute-extended-command))


;; The indispensable Massmapper (screw using emacs without it. it's like emacs
;; had a supermassive scab its own bodyweight and now it's all gone, so nice)
(use-package! massmapper
  :hook (after-init . massmapper-mode))
(add-hook 'massmapper-keymap-found-hook #'massmapper-define-super-like-ctl)
(add-hook 'massmapper-keymap-found-hook #'massmapper-homogenize -50)
;; (add-hook 'massmapper-keymap-found-hook #'massmapper-protect-ret-and-tab -75)
(setopt massmapper-debug-level 2)
(setopt massmapper-homogenizing-winners
        '(("C-x C-s" . global-map)
          ("C-x C-f" . global-map)
          ("C-x C-q" . global-map)
          ("C-x C-;" . global-map)
          ("C-x C-l" . global-map)
          ("C-c C-c")
          ("C-c C-," . org-mode-map)))
;; (setopt massmapper-Cm-Ci-override '((global-map . (("C-m" . nil)
;;                                                   ("C-i" . nil)))))

;; Ensure it works the same with and without general
(general-auto-unbind-keys 'undo)


(my-normie:abnormalize) ;; (see *lib.el)
