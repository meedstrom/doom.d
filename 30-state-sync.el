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
;; find org notes by org-id, recentf suffers partial amnesia, and so on.
;;
;; This mode fixes all that by preemptively running most of `kill-emacs-hook'
;; while the user is idle.

;;; Code:

(defcustom my-state-sync-functions-to-try
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
  "The subset of `kill-emacs-hook' you want to run regularly.
For `my-state-sync-mode' to call a function regularly, that
function must be in both this list and in `kill-emacs-hook'.
It is fine to add functions that have not yet been defined.

The net effect is that you can fill this list with functions from
packages you no longer use, and they will simply be ignored, plus
that even for the packages that do exist -- let's say org -- will
not do their sync business until you have loaded org, because
that's when it adds its business to `kill-emacs-hook'."
  :type '(repeat sexp))

(defvar my-state-sync-timer (timer-create))

(defvar my-state-sync-hook nil
  "Internal variable, do not set!
Dynamic (global) variable holding hooks to run, used by
`my-state-sync-write'.  Naively, that function could just let-bind
a list of hooks internally, but `run-hooks' will not work on
lexically scoped variables.")

(defun my-state-sync-write ()
  "Write histories and caches to disk."
  (setq my-state-sync-hook
        (seq-intersection kill-emacs-hook my-state-sync-functions-to-try))
  (run-hooks 'my-state-sync-hook))

(define-minor-mode my-state-sync-mode
  "Regularly write caches to disk."
  :global t
  (cancel-timer my-state-sync-timer)
  (when my-state-sync-mode
    (setq my-state-sync-timer
          (run-with-idle-timer (* 3 60) t #'my-state-sync-write))))

(my-state-sync-mode)

;; Bonus! Knowing that the stuff is well-synced already, we can kill/restart
;; emacs MUCH faster!
(advice-add #'kill-emacs :before
            (defun my-wipe-kill-emacs-hook (&rest _)
              (setq kill-emacs-hook nil)))
