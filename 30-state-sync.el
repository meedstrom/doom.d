;; Eagerly cache recentf and other data -*- lexical-binding: t -*-

;;; Commentary:

;; Ensure survival of data such as recentf, that normally rely on
;; `kill-emacs-hook'.
;;
;; It's sloppy design to put data-syncs on `kill-emacs-hook'.  Most of the time
;; my Emacs goes down, it's a crash or SIGTERM.  I'm not sure who is the
;; mythical user who regularly types C-x C-c to bring down a fully functional
;; Emacs, even though nothing is broken (why are they bringing it down? I don't
;; get it).  Anyway, I'm missing some data every time I start Emacs: I can't
;; find org notes by org-id, recentf suffers partial amnesia, and so on.  This
;; code fixes all that.

;;; Code:

(defvar my-state-sync-hook nil
  "Dynamic (global) variable.
For some reason, lexical binding does not permit `my-state-sync'
to treat a let-bound variable as a hook, so we use this.")

(defun my-state-sync ()
  "Write histories and caches to disk.
This runs many members of `kill-emacs-hook' so we don't have to
rely on that hook.  Suggested to run from a repeating idle timer."
  (setq my-state-sync-hook
        (seq-intersection
         ;; NOTE: Check your `kill-emacs-hook' for more functions to add here.
         ;; Rule of thumb, almost all its functions should be included, only a
         ;; minority aren't about persisting state at all!  I think upstream
         ;; should actually split this hook up into two hooks... one for state
         ;; sync, one for cleanup.
         '(straight--delete-stderr-file
           bookmark-exit-hook-internal
           savehist-autosave
           transient-maybe-save-history
           org-roam-db--close-all
           org-clock-save
           org-id-locations-save
           org-persist-gc
           org-persist-write-all
           org-persist-clear-storage-maybe
           org-babel-remove-temporary-stable-directory
           org-babel-remove-temporary-directory
           save-place-kill-emacs-hook
           recentf-save-list
           recentf-cleanup
           doom-cleanup-project-cache-h
           doom-persist-scratch-buffers-h)
         kill-emacs-hook))
  (run-hooks 'my-state-sync-hook))

;; Run after 3 minutes of idle.
;; (Will not repeat until user becomes idle again.)
(setq my-state-sync-timer
      (run-with-idle-timer (* 3 60) t #'my-state-sync))

;; Bonus! We can kill/restart emacs much faster!
(advice-add #'kill-emacs :before
            (defun wipe-kill-emacs-hook (&rest _)
              (setq kill-emacs-hook nil)))
