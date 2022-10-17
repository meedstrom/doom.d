;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ensure that guix org comes first
;; Tends to break unless i install everything that depends on org thru guix. Wait and see what hlissner does with Nix...
;(use-package org
;  :load-path "/home/me/.guix-profile/share/emacs/site-lisp/")

(setq-default load-prefer-newer t)
(add-load-path!
 ;; (expand-file-name "Emacs/conf-vanilla/lisp" (getenv "MY_FILES"))
 ;; (expand-file-name "Emacs/conf-doom/subed" (getenv "MY_FILES"))
 (expand-file-name "Emacs/conf-common" (getenv "MY_FILES"))
 (expand-file-name "Emacs/conf-doom/lisp" (getenv "MY_FILES"))
 (expand-file-name "Emacs/deianira" (getenv "MY_FILES"))
 (expand-file-name "Emacs/twee-mode" (getenv "MY_FILES")))

;; NOTE: I use load, not load!, and the directory-files FULL argument, so I can
;;       ffap my way to the broken file when init breaks (relative file paths
;;       suck in error messages).

(defadvice! fix-solaire-mode-fix-minibuffer (&optional unset)
  :override #'solaire-mode-fix-minibuffer
  (dolist (buf '(" *Minibuf-0*" " *Minibuf-1*"
                 " *Echo Area 0*" " *Echo Area 1*"))
    (with-current-buffer (get-buffer-create buf)
      (if (or unset (not solaire-global-mode))
          (solaire-mode -1)
        (setq-local evil-buffer-regexps '((".")))
        (when (= (buffer-size) 0)
          (insert " "))
        (add-hook 'kill-buffer-query-functions #'ignore nil 'local)
        (solaire-mode +1)))))

(dolist (module (directory-files "/home/kept/Emacs/conf-common/" t "^[^.#].*.el$"))
  (load module))

(dolist (module (directory-files "/home/kept/Emacs/conf-doom/lisp/" t "^[^.#].*.el$"))
  (load module))
