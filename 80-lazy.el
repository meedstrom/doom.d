;; flash effect on deletion, paste etc
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode))

(use-package iscroll
  :hook ((text-mode elfeed-show-mode eww-mode shr-mode) . iscroll-mode))

(use-package iedit
  ;; default is C-;
  :init (setq iedit-toggle-key-default nil)
  :commands iedit-mode)

(use-package objed
  :commands objed-ipipe)

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package copilot
  :disabled
  :defer
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package nameless
  :hook (emacs-lisp-mode . nameless-mode)
  :init
  ;; swung dash ⁓ tilde op ∼ sine ∿ almost eq ≈
  ;; four dot mark ⁛ lock 🔒 ⊝ ◯ ⁐ ○ ⚞⁖ ⋐⚟⤳〜
  (setopt nameless-prefix "⁓")
  ;; (setopt nameless-prefix "⚟")
  (setopt nameless-private-prefix t)
  (setopt nameless-affect-indentation-and-filling nil)
  ;; (add-hook 'nameless-mode-hook #'my-adjust-scale-2)
  :config
  (set-face-attribute 'nameless-face nil :inherit 'unspecified))

;; try to fix JS/TS buffers freezing.  Seems the issue is that prism-mode collides badly with RJSX mode.
;; (setq tide-server-max-response-length )
;; (after! rjsx-mode
;; (remove-hook 'rjsx-mode-hook #'rainbow-delimiters-mode))

;; (after! typescript-mode
;;   ;; NOTE: typescript-tsx-mode is actually defined in ~/doomemacs/modules/lang/javascript/config.el
;;   (remove-hook 'typescript-tsx-mode-hook #'rainbow-delimiters-mode))

(use-package prism
  :disabled
  :init
  (setopt prism-comments nil)
  ;; The default (40 50 60) is disorienting when turning prism on and off.
  (setopt prism-desaturations '(0 20 60))
  ;; (setopt prism-desaturations '(0))
  ;; note, another odd default is that (in lisp) the parens enclosing a sexp
  ;; are a diff color from the symbols inside -- people arent used to this
  ;; either, it makes em stand out too much imo
  :config
  ;; Replace rainbow-delimiters (it's on a dozen hooks in Doom, so this method is easiest).
  (fset 'rainbow-delimiters-mode #'prism-mode)
  ;; (require 'prism)
  ;; (add-hook 'doom-load-theme-hook #'prism-set-colors)

  (add-hook 'typescript-mode-hook #'prism-mode)
  (add-hook 'typescript-tsx-mode-hook #'prism-mode)
  (add-hook 'js-base-mode-hook #'prism-mode)
  ;; (add-hook 'web-mode-hook #'prism-mode) ;; infinite loop in .svelte files
  )

(use-package elfeed
  :disabled
  :defer
  :config
  (setopt rmh-elfeed-org-files
          (list (expand-file-name "elfeed.org" doom-private-dir)))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :entry-title (rx (or "MCMXXX"
                                                     "A&R"))
                                :add 'junk))
  (setopt elfeed-curl-max-connections 1)
  (setopt elfeed-search-filter "@2-months-ago -junk +unread +fav")
  ;; (ignore-errors (elfeed-org)) ;; does not work
  )

(use-package hyperbole
  :commands hkey-either)
