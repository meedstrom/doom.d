;; -*- lexical-binding: t -*-

;; Copyright (C) 2023 Martin Edström
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; It's insane to put data-syncs on kill-emacs-hook.  Most of the time my emacs
;; goes down, it happens in a non-clean way -- why would I intentionally shut
;; off Emacs if everything is fine?  As a result, I'm missing some data
;; every time I start Emacs: I can't find org notes by org-id, recentf
;; suffers partial amnesia, and so on.  This has been annoying me for years.

;;; Code:

(defvar my-state-sync-hooks nil
  "Dynamic variable.
For some reason, lexical binding does not permit `my-state-sync'
to just let-bind a variable, so we have this.")

(defun my-state-sync ()
  "Write histories and caches to disk.
This runs many members of `kill-emacs-hook' so we don't have to
rely on that hook.  You may put this on a repeating timer."
  (setq my-state-sync-hooks
        (seq-intersection
         ;; NOTE: Check your `kill-emacs-hook' in case there's more
         ;; you want to add here.
         #'(bookmark-exit-hook-internal
            savehist-autosave
            transient-maybe-save-history
            org-id-locations-save
            save-place-kill-emacs-hook
            recentf-save-list
            recentf-cleanup
            doom-cleanup-project-cache-h
            doom-persist-scratch-buffers-h)
         kill-emacs-hook))
  (run-hooks my-state-sync-hooks))

;; Run after 3 minutes of idle.
(setq my-state-sync-timer
      (run-with-idle-timer (* 3 60) t #'my-state-sync))
