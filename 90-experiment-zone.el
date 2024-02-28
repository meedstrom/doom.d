;; Experiment zone -*- lexical-binding: t; -*-


;;; Fixes for buffer-cycling

;; Don't filter the buffer list when cycling.  How do people actually find the
;; filtered buffers when they want them?  They can't possibly be typing out the
;; name?
(assoc-delete-all 'buffer-predicate default-frame-alist) ;; undoom
(set-frame-parameter nil 'buffer-predicate nil) ;; undoom
(setopt iflipb-ignore-buffers (lambda () t))

;; Never bury buffers, so the buffer list is truly chronological and
;; unsurprising to cycle thru.  FWIW, might be worth knowing the command
;; `unbury-buffer' and using that instead -- but I would prefer if there was a
;; visual effect when a buffer gets buried, if I'm gonna have to keep track of
;; what got buried as opposed to just switched out.
(fset 'bury-buffer #'ignore)
(fset 'bury-buffer-internal #'ignore)



(hookgen doom-after-init-hook
  (setq my-stim-collection (my-stim-collection-generate)))

;; (setq counsel-ffdata-database-path "/home/me/.mozilla/firefox/wrki7yvc.dev-edition-default/places.sqlite")
;; (setq helm-firefox-bookmark-user-directory "/home/me/.mozilla/firefox/wrki7yvc.dev-edition-default/")

;; ;; Fix
;; (defun helm-get-firefox-user-init-dir (directory)
;;   "Guess the default Firefox user directory name."
;;   (with-temp-buffer
;;     (insert-file-contents
;;      (expand-file-name "profiles.ini" directory))
;;     (goto-char (point-min))
;;     (search-forward "Default=1")
;;     (search-backward "Path=")
;;     (file-name-as-directory (expand-file-name
;;                              (buffer-substring-no-properties
;;                               (match-end 0) (point-at-eol))
;;                              directory))))

