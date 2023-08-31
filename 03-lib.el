;; my-lib.el -- a collection of defuns -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Martin Edström
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

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'dash)
(require 'crux)

(autoload #'server-running-p "server")
(autoload #'tramp-time-diff  "tramp")
(autoload #'objed-ipipe "objed")
(autoload #'piper "piper")

(defun my-downcase-all-paths-in-file (base)
  (interactive "MBeginning of string that marks a filename (regexp): ")
  (if (string-empty-p base)
      (message "Beginning of string not provided, doing nothing")
    (save-excursion
      (goto-char (point-min))
      (let ((regexp (rx (regexp base) (* (any alnum "/" "-" "_" ".")))))
        (while (re-search-forward regexp nil t)
          (message "Downcasing: %s" (buffer-substring (match-beginning 0) (match-end 0)))
          (downcase-region (match-beginning 0) (match-end 0))))
      (message "Done downcasing all paths in file"))))

;; TODO: insert a whole Org link, complete with naming it after the node title?
(defun my-insert-known-url ()
  "Insert any URL known by the roam-refs database."
  (interactive)
  (let* ((urls (--map (substring it 2)
                      (-flatten (org-roam-db-query
                                 [:select [ref]
                                  :from refs
                                  :left-join nodes
                                  :on (= refs:node-id nodes:id)]))))
         (choice (completing-read "Insert at point" urls)))
    (when choice
      (insert "https://" choice))))

;; TODO: craft a hyprctl command that only considers emacs frames (if there are
;;       2 emacs frames and 1 firefox frame, stay in emacs).  Remember to
;;       discount frames not on the current workspace.
(defun my-other-window-any-frame-hyprland ()
  (interactive)
  (unless (equal (window-frame)
                 (window-frame (next-window nil 'skip-minibuf 'visible)))
      (my-exec "hyprctl" "dispatch" "cyclenext"))
  (other-window 1))

;; Modified version of `org-roam-node-slug'
(defun my-slugify (title)
  (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                           768 ; U+0300 COMBINING GRAVE ACCENT
                           769 ; U+0301 COMBINING ACUTE ACCENT
                           770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771 ; U+0303 COMBINING TILDE
                           772 ; U+0304 COMBINING MACRON
                           774 ; U+0306 COMBINING BREVE
                           775 ; U+0307 COMBINING DOT ABOVE
                           776 ; U+0308 COMBINING DIAERESIS
                           777 ; U+0309 COMBINING HOOK ABOVE
                           778 ; U+030A COMBINING RING ABOVE
                           779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                           780 ; U+030C COMBINING CARON
                           795 ; U+031B COMBINING HORN
                           803 ; U+0323 COMBINING DOT BELOW
                           804 ; U+0324 COMBINING DIAERESIS BELOW
                           805 ; U+0325 COMBINING RING BELOW
                           807 ; U+0327 COMBINING CEDILLA
                           813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ; U+032E COMBINING BREVE BELOW
                           816 ; U+0330 COMBINING TILDE BELOW
                           817 ; U+0331 COMBINING MACRON BELOW
                           )))
    (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
               (strip-nonspacing-marks (s) (string-glyph-compose
                                            (apply #'string
                                                   (seq-remove #'nonspacing-mark-p
                                                               (string-glyph-decompose s)))))
               (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[[:space:]]+" . "-")
                      ("[^[:alnum:][:digit:]\\/+=-]" . "")
                      ("\\/" . "-")
                      ("--*" . "-")
                      ("^-" . "")
                      ("-$" . "")
                      ("-\\+-" . "+")
                      ("-=-" . "=")
                      ))
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug)))))

;; (my-slugify "A/B testing")
;; (my-slugify "Someday/Maybe whale carcass")
;; (my-slugify "No one can feel a probability that small")
;; (my-slugify "\"But there's still a chance, right?\"")
;; (my-slugify "Löb's Theorem")
;; (my-slugify "How to convince me that 2 + 2 = 3")
;; (my-slugify "C. S. Peirce")
;; (my-slugify "Do one thing at a time")
;; (my-slugify "Are you losing items in recentf, bookmarks, org-id-locations? Solution: Run kill-emacs-hook periodically.")
;; (my-slugify "Slimline/\"pizza box\" computer chassis")

;; NOTE: not used during my publishing process right now, I just run it manually sometimes
(defun my-rename-roam-file-by-title (&optional path title)
  (interactive)
  (unless path
    (setq path (buffer-file-name)))
  (unless (equal "org" (file-name-extension path))
    (error "Unexpected that file doesn't end in .org, halting on: %s" path))
  (unless title
    (with-temp-buffer
      (insert-file-contents path)
      (let ((case-fold-search t))
        (setq title (save-excursion
                      (goto-char (point-min))
                      (when (search-forward "#+title: " nil t)
                        (buffer-substring (point) (line-end-position))))))))
  (let* ((name (file-name-nondirectory path))
         (filename-preamble
          (when (string-match
                 (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "-")
                 name)
            (match-string 0 name)))
         (slugified-path (concat (file-name-directory path)
                                 filename-preamble
                                 (my-slugify title)
                                 ".org"))
         (visiting (find-buffer-visiting path)))
    (unless (equal slugified-path path)
      (if (and visiting (buffer-modified-p visiting))
          (message "Unsaved file, letting it be: %s" path)
        (when visiting
          (kill-buffer visiting))
        (and (file-writable-p path)
             (file-writable-p slugified-path)
             (rename-file path slugified-path))
        (when visiting
          (find-file slugified-path))))))


(defun my-eww-bookmark-copy-url ()
  "Kill the current bookmark."
  (interactive nil eww-bookmark-mode)
  (let* ((start (line-beginning-position))
         (bookmark (get-text-property start 'eww-bookmark))
         (url (plist-get bookmark :url)))
    (unless bookmark
      (user-error "No bookmark on the current line"))
    (forward-line 1)
    (kill-new url)
    (message "Copied %s" url)))

;; TODO: would be cool to use the "motion" program and start it watching me right away.
(defun my-browse-random-lw-post ()
  "Practice something for my Youtube channel."
  (interactive)
  (eww-browse-url
   (seq-random-elt (cl-loop
                    for x in eww-bookmarks
                    as url = (plist-get x :url)
                    when (string-search "wrong.com/" url)
                    collect url)))
  (let ((guvc-running (cl-loop for p in (process-list)
                               when (string-search "guvcview" (process-name p))
                               return t)))
    (unless guvc-running
      (my-spawn-process "guvcview"))))

(defun my-browse-random-eww-bookmark ()
  (interactive)
  (eww-browse-url (plist-get (seq-random-elt eww-bookmarks) :url)))

(defun my-guix-profile ())

(defun my-org-roam-extract-subtree ()
  "Variant of `org-roam-extract-subtree'.
It skips prompting, and inserts the metadata I want."
  (interactive)
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (when (bobp) (user-error "Already a top-level node"))
    (org-id-get-create)
    (save-buffer)
    (org-roam-db-update-file)
    (let* ((template-info nil)
           (node (org-roam-node-at-point))
           (template (org-roam-format-template
                      (string-trim (org-capture-fill-template org-roam-extract-new-file-path))
                      (lambda (key default-val)
                        (let ((fn (intern key))
                              (node-fn (intern (concat "org-roam-node-" key)))
                              (ksym (intern (concat ":" key))))
                          (cond
                           ((fboundp fn)
                            (funcall fn node))
                           ((fboundp node-fn)
                            (funcall node-fn node))
                           (t (let ((r (read-from-minibuffer (format "%s: " key) default-val)))
                                (plist-put template-info ksym r)
                                r)))))))
           (file-path
            (expand-file-name template org-roam-directory))
           (created-date (save-excursion
                           (org-back-to-heading)
                           (when (search-forward ":created: " (org-entry-end-position) t)
                             (prog1 (buffer-substring (point) (line-end-position))
                               (delete-line))))))
      (when (file-exists-p file-path)
        (user-error "%s exists. Aborting" file-path))
      (org-cut-subtree)
      (open-line 1)
      (insert "- " (org-link-make-string
                    (concat "id:" (org-roam-node-id node))
                    (org-roam-node-formatted node)))
      (save-buffer)
      (find-file file-path)
      (org-paste-subtree)
      (while (> (org-current-level) 1) (org-promote-subtree))
      (save-buffer)
      (org-roam-promote-entire-buffer)
      (goto-char (point-min))
      (search-forward "#+title")
      (search-forward "#+filetags" nil t)
      (goto-char (line-beginning-position))
      (delete-char -1)
      (forward-line 1)
      (open-line 2)
      (if created-date
          (insert "#+date: " created-date)
        (insert "#+date: [" (format-time-string "%F") "]"))
      (save-buffer))))

(defun my-truncate-buffer-and-move-excess (&optional _string)
  "A substitute for `comint-truncate-buffer'.
Instead of deleting, move the excess lines to a buffer named
*comint-excess:..., in case you need to look far back."
  (save-mark-and-excursion
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (forward-line (- comint-buffer-maximum-size))
    (goto-char (line-beginning-position))
    (let ((inhibit-read-only t)
          (beg (point-min))
          (end (point)))
      (when (/= beg end)
        (append-to-buffer (concat "*comint-excess: " (buffer-name) "*") beg end)
        (delete-region beg end)))))

(defmacro my-hook-once (hook &rest body)
  "Add temporary actions to HOOK to run only once.
BODY is wrapped in a function run the next time HOOK is
triggered, and the function removes itself from HOOK before
executing BODY."
  (declare (indent defun))
  (let ((funcname (cl-gensym)))
    `(add-hook
      ,hook
      (defun ,funcname (&rest _)
        (remove-hook ,hook #',funcname)
        ;; (fmakunbound ',funcname)
        ;; (unintern (symbol-name ',funcname))
        ,@body))))

(defconst my--ignore-keys-regexp
  (regexp-opt '("mouse" "remap" "scroll-bar" "select" "switch" "help" "state"
                "which-key" "corner" "divider" "edge" "header" "mode-line"
                "tab" "vertical-line" "frame" "open" "menu" "kp-" "iso-")))

(defun my-locate-keys (command &optional keymap)
  (->> (where-is-internal command (when keymap (list keymap)))
       (-map #'key-description)
       (--remove (string-match-p my--ignore-keys-regexp it))))

(defun my-script-1 ())

(defvar my-stim-origin-buffer nil)

(defvar my-stim-buffers nil)

;; See also the concept of user excursions in eva.el.  It could be a library.
(defvar my-stim-transient-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda () (interactive)
                                (switch-to-buffer my-stim-origin-buffer)
                                (cl-loop for buf in my-stim-buffers
                                         do (kill-buffer-if-not-modified buf))))
    map))

(defun my-stim-collection-generate ()
  (let ((documented-commands nil)
        (roam-files
         (append (directory-files "/home/kept/roam/" t ".org$")
                 ;; (directory-files "/home/kept/roam/bloggable/" t ".org$")
                 (directory-files "/home/kept/roam/frozen/" t ".org$")
                 ;; (directory-files "/home/kept/roam/martin/pages/" t ".org$")
                 (directory-files "/home/kept/roam/grismartin/pages/" t ".org$")
                 (directory-files "/home/kept/roam/daily/" t ".org$"))))
    (mapatoms
     (lambda (sym)
       (when (and (commandp sym)
                  (documentation sym t)
                  (null (get sym 'byte-obsolete-info)))
         (push sym documented-commands))))
    (setq roam-files (cl-loop
                      for file in roam-files
                      unless (string-search ".sync-conflict" file)
                      collect file))
    (list
     (cons #'find-file roam-files)
     (cons #'describe-function documented-commands))))

(my-hook-once 'doom-after-init-hook
  (setq my-stim-collection (my-stim-collection-generate)))

(defun my-stim (&optional collection)
  "Show something random.
Hopefully this helps for working while addled by bees \(afflicted
by ADHD).  The user is to feel free to call this command at any
time, however many times they wish.  Pressing q brings back the
buffer that was previously active.

Optional argument COLLECTION defaults to the value of
`my-stim-collection' if not provided.  It must be a list of lists
in this format:

\(\(COMMAND ITEM ITEM ITEM ...)
 \(COMMAND ITEM ITEM ITEM ...)
 ...)

where the ITEMs are things to visit, such as web addresses, and
the COMMAND is the command to use on such an item, such as
`eww-browse-url'."
  (interactive)
  (if (eq last-command #'my-stim)
      (push (current-buffer) my-stim-buffers)
    (setq my-stim-buffers nil)
    (setq my-stim-origin-buffer (current-buffer)))
  (set-transient-map my-stim-transient-map)
  (let ((sublist (seq-random-elt (or collection my-stim-collection))))
    (funcall-interactively (car sublist) (seq-random-elt (cdr sublist)))))

(defvar my-stim-collection-online nil)

(defun my-stim-with-online ()
  "Show something random.
Hopefully this helps for working while addled by bees \(ADHD).
The user is to feel free to call this command at any time,
however many times they wish.  Pressing q brings back the buffer
that was previously active."
  (interactive)
  (require 'eww)
  (unless my-stim-collection-online
    (setq my-stim-collection-online
          (append (my-stim-collection-generate)
                  (list (cons #'eww-browse-url
                              (cl-loop for x in eww-bookmarks
                                       collect (plist-get x :url)))))))
  (my-stim my-stim-collection-online))

;; Wishlist: A command for repeat with universal arg
;; Wishlist: A command for undo then repeat with universal arg
;; TODO: let us repeat many times in a row e.g. 9 9 9 3 2 1...
(defun my-repeat-digit-times ()
  "Repeat the last command an amount of times.
Only up to 9 because this command only works when you bind it to
the keys 0-9 (or some combination like C-1, C-2...) in the
transient `my-post-repeat-transient-map'.  The idea is to allow you to say
\"command, repeat n times\" instead of \"do n times
command\".

To start using it, evaluate the following.

    (advice-add #'repeat :after #'my-enable-post-repeat-transient-map)"
  (interactive)
  ;; bits pasted from `repeat'
  (when (eq last-repeatable-command 'repeat)
    (setq last-repeatable-command repeat-previous-repeated-command))
  (when (eq last-repeatable-command 'my-repeat-digit-times)
    (message "I thought it was impossible to end up here"))
  ;; bits pasted from `digit-argument'
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0)))
    (dotimes (i digit) (call-interactively #'repeat))))

(defvar my-post-repeat-transient-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 10)
      (define-key map (kbd (int-to-string i)) #'my-repeat-digit-times))
    map))

(defun my-enable-post-repeat-transient-map (&rest _args)
  ;; (internal-pop-keymap my-post-repeat-transient-map 'my-post-repeat-transient-map)
  (set-transient-map my-post-repeat-transient-map))

(defun my-corfu-turn-off ()
  (when (bound-and-true-p corfu-mode)
    (corfu-mode 0)))

(defun my-pipe ()
  (interactive)
  (if (region-active-p)
      (objed-ipipe)
    (piper)))

(defun my-copy-buffer-filename ()
  (interactive)
  (kill-new buffer-file-name))

(defun my-save-without-final-newline ()
  "Also strips initial newline"
  (interactive)
  (require 'whitespace)
  (cl-letf ((require-final-newline nil))
    ;; Grabbed from whitespace-cleanup
    (let (overwrite-mode)        ;; enforce no overwrite
      (goto-char (point-min))
      (when (looking-at whitespace-empty-at-bob-regexp)
        (delete-region (match-beginning 1) (match-end 1)))
      (when (re-search-forward
             whitespace-empty-at-eob-regexp nil t)
        (delete-region (match-beginning 1) (match-end 1))))
    (goto-char (point-max))
    (when (looking-back (rx bol) nil)
      (delete-region (1- (point)) (point-max)))
    (save-buffer)))

(defun my-tab-command ()
  (interactive)
  (call-interactively (if (equal (point)
                                 (save-mark-and-excursion
                                   (forward-char)
                                   (beginning-of-defun)
                                   (point)))
                          #'+fold/toggle
                        #'indent-for-tab-command)))

(defun my-shell-command-replace-region ()
  "Run `shell-command-on-region' as if you had supplied a prefix
arg. In addition, use fish if available."
  (interactive)
  (let ((current-prefix-arg 4)
        (shell-file-name (if-let (fish (executable-find "fish"))
                             fish
                           shell-file-name)))
    (call-interactively #'shell-command-on-region)))

(defun my-fill-unfill-respect-double-space ()
  "Toggle filling/unfilling of the current region, or current
    paragraph if no region is active.  Also pretend that
    `sentence-end-double-space' is non-nil to avoid clobbering
    existing double spaces. See `fill-paragraph' for what a prefix
    command will do."
  (interactive)
  (let ((deactivate-mark nil)
        (fill-column (if (eq last-command this-command)
                         (prog1 most-positive-fixnum
                           (setq this-command nil))
                       fill-column))
        (sentence-end-double-space t))
    ;; For whatever reason, `fill-paragraph-function' does not get consulted in
    ;; org buffers, so we have to do this manually, even though we don't have
    ;; to call `lisp-fill-paragraph' explicitly in lisp buffers.
    (if (derived-mode-p 'org-mode)
        (call-interactively #'org-fill-paragraph)
      (call-interactively #'fill-paragraph))))

(defun my-fill-paragraph-noclobber ()
  "Like `fill-paragraph', but pretend that
`sentence-end-double-space' is non-nil to avoid clobbering
existing double spaces."
  (interactive)
  (let ((sentence-end-double-space t))
    (call-interactively #'fill-paragraph)))

(defun my-describe-last-key ()
  (interactive)
  (describe-function winner-last-command))

;; https://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun my-narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first.  Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; These are a little uncreative. You only need one, and then functions for
;; replacing the filename at point with its basename/dirname/truename.
(defun my-insert-buffer-filename ()
  "Insert the filename for the current buffer.
If you're in the minibuffer it will use the other buffer file name."
  (interactive)
  (let ((filename (buffer-file-name (if (window-minibuffer-p)
                                        (window-buffer (previous-window))
                                      (current-buffer)))))
    (when filename (insert (kill-new filename)))))

(defun my-insert-buffer-base-filename ()
  "Insert the base filename for the current buffer.
If you're in the minibuffer it will use the other buffer file
name."
  (interactive)
  (let ((filename (buffer-file-name (if (window-minibuffer-p)
                                        (window-buffer (previous-window))
                                      (current-buffer)))))
    (when filename (insert (kill-new (file-name-base filename))))))

(defvar my-yank-ring '())

(defun my-yank-next ()
  "Yank the next item in the kill ring, letting the previous yank
stand (instead of overwriting it).  This allows a workflow without
`append-next-kill' for those not inclined to plan ahead.  However,
the result will be in reverse order compared to if you had used
`append-next-kill' on every item and yanked once.  When that's a
problem, `my-reverse-yanks' can help."
  (interactive)
  (let ((pos (point)))
    (if (or (equal real-last-command this-command)
            (and (equal real-last-command #'repeat)
                 (equal repeat-previous-repeated-command this-command)))
        (yank 2)
      (setq my-yank-ring '())
      (yank))
    (push (buffer-substring pos (point)) my-yank-ring)))

;; FIXME: Only works once
(defun my-reverse-yanks ()
  "See `my-reverse-region-dwim' for a broader command."
  (interactive)
  (when (or (equal real-last-command this-command)
            (equal real-last-command #'my-yank-next)
            (and (equal real-last-command #'repeat)
                 (equal repeat-previous-repeated-command #'my-yank-next)))
    (undo (length my-yank-count))
    (dolist (item my-yank-ring)
      (insert item))))

(defun my-reverse-region-dwim ()
  (interactive)
  (if (member real-last-command '(my-yank-next my-reverse-yanks))
      (my-reverse-yanks)
    (reverse-region)))

(defun my-copy-region-or-rest-of-line-to-other-window ()
  "Copy the current region to the other window.
Tip: afterwards, use C-w to kill the original."
  (interactive)
  (save-mark-and-excursion
    (if (region-active-p)
        (kill-ring-save (region-beginning) (region-end))
      (kill-ring-save (point) (line-end-position)))
    (other-window 1 'visible)
    (yank)))

;; TODO: A little uncreative.  Add the body of every invocation to a single
;; list variable, then eval the entire list in one named defun.
(defmacro my-before-keybinds (&rest body)
  `(add-hook 'my-before-keybinds-hook (lambda nil ,@body)))

(defmacro my-after-keybinds (&rest body)
  `(add-hook 'my-after-keybinds-hook (lambda nil ,@body)))

(defun my-change-latex-scale ()
  (setq org-format-latex-options
        (plist-put org-format-latex-options
                   :scale (* 1.5 (or 1 (caddr text-scale-mode-remapping))))))

(defun my-wipe-kill-ring ()
  "Sometimes there's a huge object in there that freezes Emacs
when killing/yanking."
  (interactive)
  (setq kill-ring nil))

(defun my-insert-time-interval (input var)
  "Translate the input from ISO 31-11 interval notation, such
as (0,50), to inequality-based notation such as 0 < x < 50, and
insert that at point.  Also prompt for the name of the variable.

Why you'd do this?  It lets you sidestep issues with delimiter
parsing; intervals like (X,Y] and ]X,Y] can confuse some Emacs
packages in some situations."
  (interactive  "MTime interval in (X,Y] notation: \nMName of variable: ")
  (string-match (rx bol (group (any "][(")) (group (*? nonl))
                    (any ",;") (group(*? nonl)) (group (any "][)")))
                input)
  (let ((opener (match-string 1 input))
        (start (match-string 2 input))
        (end (match-string 3 input))
        (closer (match-string 4 input)))
    (let ((lower-inequality (if (string-match-p "\\[" opener)
                           " \\le "
                         " < "))
          (upper-inequality (if (string-match-p "\\]" closer)
                           " \\le " ;; or unicode ≤ ?
                         " < ")))
      (insert (concat start lower-inequality var upper-inequality end)))))

(defmacro my-exec (program &rest program-args)
  "Similar to `async-shell-command', but skips the shell intermediary.

In other words: don't spawn a bash process that runs a bash
command that spawns a grandchild, but spawn PROGRAM as a direct
child of the emacs process. No shell notation such as pipes or
ampersands will work since the bash program is what normally
parses that and carries out actions on your system. Environment
variables like $HOME will also not be understood, but in Lisp
code you can use (env \"HOME\").

PROGRAM and PROGRAM-ARGS are passed on to `start-process'."
  `(start-process ,program nil ,program ,@program-args))

(defmacro my-process-output-to-string (program &rest args)
  "Similar to `shell-command-to-string', but skips the shell intermediary.
 See `my-exec' for deeper explanation.

PROGRAM and ARGS are passed on to `call-process'. Like
`shell-command-to-string', this is synchronous and blocks Emacs
until the program finishes."
  `(with-temp-buffer 
     (call-process ,program nil (current-buffer) nil ,@args)
     (buffer-string)))

(defvar my-spawn-process-hook nil)
(defun my-spawn-process (command)
  (interactive (list (read-shell-command "$ ")))
  (when (bound-and-true-p my-spawn-process-hook)
    (run-hooks 'my-spawn-process-hook))
  (start-process-shell-command command nil command))

(defun my-server-start-maybe ()
  (unless (server-running-p)
    (server-start nil t)
    (my-things-for-primogenitor-emacs)))

(defun my-things-for-primogenitor-emacs ()
  (when (executable-find "updatedb")
    (run-with-timer 5 3600 #'my-index-locatedb))
  (when (executable-find "duc")
    (run-with-timer 10 3600 #'my-index-duc)))

(defun my-index-duc ()
  (start-process "duc" nil "duc" "index" "/home"))

(defun my-index-locatedb ()
  (when (string-match-p "GNU" (my-process-output-to-string "updatedb" "--version"))
    (unless (getenv "FINDOPTIONS")
      (setenv "FINDOPTIONS" (concat " -name node_modules -prune "
                                    " -name packrat -prune "
                                    " -name backup -prune "
                                    " -name backups -prune ")))
    (my-exec "updatedb"
             "--prunepaths='/home/backups /home/me2'"
             "--localpaths=/home"
             (concat  "--output=" (getenv "HOME") "/locate.db"))
    (setenv "LOCATE_PATH" (concat (getenv "HOME") "/locate.db"))))

(defun my-prev-file-in-dir ()
  (interactive)
  (let* ((files (directory-files default-directory t))
         (remainder (reverse (seq-difference files
                                             (member (buffer-file-name) files))))
         (first-relevant-file
          (cl-loop for x in remainder
                   until (not (or (file-directory-p x)
                                  (string-match-p
                                   (rx "." (or "elc" "pdf" "o" "pyc" "so" (seq "so." num)) eol)
                                   x)))
                   finally return x)))
    (if first-relevant-file
        (find-file first-relevant-file)
      (message "No more files in directory"))))

(defun my-next-file-in-dir (&optional literally)
  (interactive "p")
  (let* ((remainder (cdr (member (buffer-file-name)
                                 (directory-files default-directory t))))
         (first-relevant-file
          (cl-loop for x in remainder
                   until (not (or (null x)
                                  (string-match-p
                                   (rx (or "."
                                           ".elc"
                                           ".pdf"
                                           ".o"
                                           ".pyc"
                                           ".so"
                                           (seq ".so." num))
                                       eol) x)
                                  (file-directory-p x)))
                   finally return x)))
    (if first-relevant-file
        (if (= 4 literally)
            (find-file-literally first-relevant-file)
          (find-file first-relevant-file))
      (message "No more files in directory"))))

(defun my-compile-and-drop ()
  "Compile buffer to check for errors, but don't write an .elc.
Original inspiration was to catch malformed sexps like
 (global-set-key \"C-x c\" ...) that would break init, not for
nitpicking things that work, so `byte-compile-warnings' is
temporarily overridden."
  (interactive)
  (when (derived-mode-p #'emacs-lisp-mode)
    (cl-letf ((byte-compile-dest-file-function (lambda (_) (null-device)))
              (inhibit-message t) ;; prevent "Wrote /dev/null"
              ((symbol-value 'byte-compile-warnings) nil))
      (byte-compile-file (buffer-file-name)))))

(defvar my-buffer-ring nil)
(defun my-nth-buffer-of-same-mode (n)
  (require 'dash)
  (unless (member last-command '(my-previous-buffer-of-same-mode
                                 my-next-buffer-of-same-mode))
    (setq my-buffer-ring
          (cl-loop for buf in (buffer-list)
                   if (eq (buffer-local-value 'major-mode buf) major-mode)
                   collect buf)))
  (if (= 1 (length my-buffer-ring))
      (message "No other buffer of same mode.")
    (setq my-buffer-ring
          (-rotate n my-buffer-ring))
    (switch-to-buffer (car my-buffer-ring))))

(defun my-next-buffer-of-same-mode ()
  (interactive)
  (my-nth-buffer-of-same-mode -1))

(defun my-previous-buffer-of-same-mode ()
  (interactive)
  (my-nth-buffer-of-same-mode 1))

(defun my-xsetroot ()
  (start-process "xsetroot" nil
                 "xsetroot" "-solid" (face-background 'default)))

(defun my-reconnect-bluetooth-headphones ()
  (interactive)
  (set-process-sentinel
   (start-process "b" nil "bluetoothctl" "disconnect" "84:D4:C8:01:CE:73")
   (lambda (_x _y)
     (start-process "b" nil "bluetoothctl" "connect" "84:D4:C8:01:CE:73"))))

(defun my-reconnect-bluetooth-headphones* ()
  (interactive)
  (shell-command (concat "bluetoothctl disconnect 84:D4:C8:01:CE:73 "
                         "&& bluetoothctl connect 84:D4:C8:01:CE:73")))

(defvar my--greeting-last nil)
(defun my-greeting ()
  (require 'ts)
  (unless (and (bound-and-true-p my--greeting-last)
               (= (ts-day (ts-now)) (ts-day my--greeting-last)))
    (when (> (ts-hour (ts-now)) 4)
      (setq my--greeting-last (ts-now))
      (message "Good morning, Martin!"))))

(defun my-write-or-rename-file (_arg)
  "If in a file, offer to rename, else act like `write-file'."
  (interactive "p")
  (prefix-command-preserve-state)
  (if buffer-file-name
      (crux-rename-file-and-buffer)
    (call-interactively #'write-file)))

(defvar my-new-buffer-counter-file
  (expand-file-name "my-new-buffer-counter" user-emacs-directory))

;; Basically Notepad
(defun my-new-buffer ()
  "Create a new buffer named \"untitled-X\" where X is an auto-assigned number."
  (interactive)
  (let* ((remembered-count
          (with-temp-buffer
            (insert-file-contents-literally my-new-buffer-counter-file)
            (string-to-number (buffer-substring 1 2))))
         (highest-in-dir
          (string-to-number
           (shell-command-to-string
            "ls | grep untitled | sort -n | tail -1 | tr -cd '[:digit:]'")))
         (count (+ 1 (max remembered-count highest-in-dir))))
    (with-temp-file my-new-buffer-counter-file (insert (int-to-string count)))
    (switch-to-buffer (format "untitled-%d" count))))

(defun my-fix-invalid-backup-settings ()
  "Meant to run on `after-save-hook'.
Should prevent getting into an infinite loop of failing
auto-saves, which can lock you out of Emacs if these saves are
being called by certain hooks, such as
`window-selection-change-functions'."
  (when (buffer-modified-p) ;; i.e. it is STILL in a modified state
    (auto-save-visited-mode 0)
    (global-auto-revert-mode 0)
    (message "Disabled auto-save-visited-mode due to suspected failure to save.")
    (unless (ignore-errors (backup-buffer))
      (warn "Backup failed\; wiping backup-directory-alist to let you save anyway")
      (setq backup-directory-alist nil))))

(defun my-save-all ()
  (interactive)
  (save-some-buffers t))

(defun my-close-minibuffer ()
  (interactive)
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(defun my-die-minibuffer-fringes (frame)
  (when (display-graphic-p)
    (with-selected-frame frame
      (set-window-fringes (minibuffer-window) 0 0))))

(defun my-trunc-lines ()
  (interactive)
  (visual-line-mode 0)
  (setq truncate-lines t))

(defun my-save-buffers-kill-emacs-silently ()
  (interactive)
  (save-buffers-kill-emacs t))

(defun my-view-exit-and-reopen-as-root ()
  "Like `crux-sudo-edit', but only if necessary.
Serves double duty as `View-exit', so can replace the key binding
for that."
  (interactive)
  (unless (crux-reopen-as-root)
    (View-exit)
    (read-only-mode 0)))

(defun my-backlight-inc ()
  (interactive)
  (my-exec "sudo" "light" "-A" "20"))

(defun my-backlight-dec ()
  (interactive)
  (if (> 20 (string-to-number (my-process-output-to-string "light" "-G")))
      (my-exec "sudo" "light" "-S" "1")
    (my-exec "sudo" "light" "-U" "20")))

(defun my-pad-window ()
  "Pad fringes until text area is the width of `fill-column' + 1.
If window is really wide, split it first."
  (interactive)
  (set-window-fringes nil 0 0)
  (when (> (window-body-width) (* 2 fill-column))
    (split-window-right))
  (let* ((width-chars (window-body-width))
         (width-px (window-body-width nil t))
         (ratio (/ width-px width-chars))
         (target-chars (1+ fill-column))
         (target-px (* target-chars ratio))
         (one-fringe-px (/ (- width-px target-px) 2)))
    (set-window-fringes nil one-fringe-px one-fringe-px)))

;; FIXME: Be less destructive
(defun my-three-cols ()
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(defun my-split-window-dwim (&optional arg)
  "An attempt at a single command for window splitting. Split side-by-side
unless it would make the resulting windows narrower than 80 characters, in
which case split above-below instead. With a prefix arg, delete all windows and
split into three balanced windows."
  (interactive "P")
  (if arg
      (my-three-cols)
    (if (< (* 2 80) (window-body-width))
        (split-window-right)
      (split-window-below))))

;; To further refine, see https://www.emacswiki.org/emacs/HippieExpand
(defun my-hippie-config ()
  (setq-local
   hippie-expand-try-functions-list
   (cond ((memq major-mode '(ess-mode
                             inferior-ess-mode
                             shell-mode
                             eshell-mode))
          (list #'try-complete-file-name-partially
                #'try-complete-file-name
                #'try-expand-dabbrev
                #'try-expand-dabbrev-all-buffers
                #'try-expand-dabbrev-from-kill))
         ;; Lisp-friendly hippie expand
         ;; Thanks https://github.com/flyingmachine/emacs-for-clojure
         ((memq major-mode '(emacs-lisp-mode
                             lisp-interaction-mode
                             scheme-mode
                             geiser-repl-mode))
          (list #'try-expand-dabbrev
                #'try-expand-dabbrev-all-buffers
                #'try-expand-dabbrev-from-kill
                #'try-complete-lisp-symbol-partially
                #'try-complete-lisp-symbol))
         (t
          hippie-expand-try-functions-list))))

(defun my-turn-on-comint-history (history-file)
  (setq comint-input-ring-file-name history-file)
  (comint-read-input-ring 'silent))

(defun my-save-buffer-and-commit ()
  (interactive)
  (save-buffer)
  (magit-stage-file (buffer-file-name))
  (magit-commit))

(defun my-save-buffer-and-amend ()
  (interactive)
  (save-buffer)
  (magit-stage-file (buffer-file-name))
  (magit-commit-amend))

(defun my-insert-gpl-maybe ()
  (and (= 1 (point-max))
       (not (file-exists-p buffer-file-name))
       (derived-mode-p 'prog-mode)
       (my-insert-short-gpl)))

(defun my-insert-short-agpl-for-elisp ()
  "Insert the short brief of the GNU AGPL as comment at the top of the file."
  (interactive)
  (goto-char (point-min))
  (insert (concat ";;; " (buffer-name) " -*- lexical-binding: t; -*-
;; Copyright (C) " (format-time-string "%Y") " Martin Edström" "

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(provide '" (string-remove-suffix ".el" (buffer-name)) ")

;;; " (buffer-name) " ends here"))
  (forward-line -3)
  (newline)
  (newline))

(defun my-insert-short-gpl-for-elisp ()
  "Insert the short brief of the GNU GPL as comment at the top of the file."
  (goto-char (point-min))
  (insert (concat ";;; " (buffer-name) " -*- lexical-binding: t; -*-
;; Copyright (C) " (format-time-string "%Y") " Martin Edström" "

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(provide '" (string-remove-suffix ".el" (buffer-name)) ")

;;; " (buffer-name) " ends here"))
  (forward-line -3)
  (newline)
  (newline))

(defun my-insert-short-gpl ()
  "Insert the short brief of the GNU GPL as comment at the top of the file."
  (interactive)
  (if (derived-mode-p 'emacs-lisp-mode)
      (my-insert-short-gpl-for-elisp)
    (goto-char (point-min))
    (insert (buffer-name))
    (insert (concat "
Copyright (C) " (format-time-string "%Y") " Martin Edström" "

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.
"))
    (comment-region (point-min) (point))))

(defun my-toggle-dedicate-window ()
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))

(defvar my-oops-count 0)
(defun my-oops-key (arg)
  "Shield against typing this key unless typed twice.

I used to bind the key \` to a frequent command (switch-buffer).
Now I find myself cluttering \` over all my buffers. This is a
training wheel."
  (interactive "p")
  (if (= my-oops-count 1)
      (progn
        (self-insert-command arg)
        (setq my-oops-count 0))
    (setq my-oops-count 1)
    (beep)
    (message "Did you mean to type that?")
    (run-with-timer 2 nil (lambda () (setq my-oops-count 0)))))

;; Why equake/"quake console" when you can just have this command?
(defun my-eshell-switch ()
  (interactive)
  (if (derived-mode-p 'eshell-mode)
      (switch-to-prev-buffer)
    (eshell)))

(defun my-rainbow-shrink ()
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :height 1.1)
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :height 1.0)
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :height 0.90)
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :height 0.85)
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :height 0.80)
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :height 0.75)
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil :height 0.70)
  (set-face-attribute 'rainbow-delimiters-depth-9-face nil :height 0.65))

;; TODO: Have it also undo when the last command triggered an abbrev
(defun my-suspend-abbrev ()
  (interactive)
  (when abbrev-mode
    (abbrev-mode 0)
    (run-with-timer 1 nil #'abbrev-mode)))

(defun my-eval-current-sexp (arg)
  (interactive "p")
  (save-mark-and-excursion
    (up-list)
    (eval-last-sexp arg)))

(defun my-exchange-point-and-mark ()
  "Like `exchange-point-and-mark' but will not activate the region."
  (interactive)
  (if mark-active (exchange-point-and-mark)
    (exchange-point-and-mark 4)))

(defun my-org-demarcate-block ()
  ;; like org-babel-demarcate-block but for QUOTE etc
  )

(defun my-hippie-expand-or-org-cycle (&optional arg)
  "On Org headline or table do `org-cycle', else `hippie-expand'.
Great on the TAB key!  As an atlernative, you can use corfu
instead of hippie-expand and set `tab-always-indent' to
'complete."
  (interactive)
  (if (eq 'headline (org-element-at-point))
      (org-cycle arg)
    (hippie-expand arg)))

;; why did I call it alpha?...
(defvar my--alpha)
(defvar my--insertion-point)
(defun my-insert-other-buffer-file-name-and-cycle ()
  (interactive)
  (if (eq last-command 'my-insert-other-buffer-file-name-and-cycle)
      (progn (delete-region my--insertion-point (point))
             (setq my--alpha (cdr my--alpha)))
    (setq my--insertion-point (point))
    (setq my--alpha (cl-remove-if-not #'buffer-file-name (buffer-list))))
  (if (eq nil my--alpha)
      (setq my--alpha (cl-remove-if-not #'buffer-file-name (buffer-list))))
  (insert (buffer-file-name (car my--alpha))))

(defun my-insert-buffer-file-name (arg)
  (let ((name (buffer-file-name arg)))
    (when name
      (insert name)
      t)))

(defun my-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun my-turn-off-aggressive-indent ()
  (aggressive-indent-mode 0))

;; For after-init-hook
(defun my-request-executables ()
  (dolist (x my-wanted-executables)
    (unless (executable-find x)
      (message (concat "Executable wanted: " x)))))

;; If you want to theme the minibuffer do
;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       `((default :foreground ,(face-foreground 'mode-line)
           :background ,(face-background 'mode-line)))))

(defun my-minibuffer-setup* ()
  (set (make-local-variable 'face-remapping-alist)
       `((default :background ,(face-background 'fringe)))))

(defun my-sleep ()
  (interactive)
  (or (if-let* ((x (seq-find #'executable-find '("s2ram" "pm-suspend"))))
          (my-exec x))
      (if (executable-find "systemctl")
          (my-exec "systemctl" "suspend"))
      (if (executable-find "loginctl")
          (my-exec "loginctl" "suspend" "-i")))
  (sleep-for 1)
  (discard-input))

;; could be progn instead of save-excursion?
(defun my-lookup-word (word)
  (interactive (list (save-excursion
                       (require 'ispell)
                       (car (ispell-get-word nil)))))
  (browse-url (format "http://en.wiktionary.org/wiki/%s" word)))

(defun my-eww-other-window ()
  (interactive)
  (if (featurep 'eww)
      (let ((newbuf (eww-open-in-new-buffer)))
        (save-excursion
          (other-window 1)
          (switch-to-buffer newbuf)))))

(defun my-capitalize-this-word ()
  (interactive)
  (capitalize-word -1))

;; DEPRECATED: see emacs 29 `eww-auto-rename-buffer'
(defun my-eww-rename-buffer-by-webpage-title ()
  (ignore-errors (rename-buffer
                  (concat "*eww " (plist-get eww-data :title) "*") t)))

(defun my-exwm-rename-buffer ()
  (require 'my-lib-unprefixed)
  (when (derived-mode-p 'exwm-mode)
    (let ((app-name (cond ((equal exwm-class-name "Nightly") "Firefox")
                          (t exwm-class-name))))
      (exwm-workspace-rename-buffer
       (concat "*" (cut-at 35 (concat "EXWM (" app-name ")"
                                      (when exwm-title (concat ": " exwm-title))))
               "*")))))

;; misnamed, really
(defun my-exwm-set-keys-by-app ()
  (when exwm-class-name
    (when (string= exwm-class-name "R_x11")
      ;; FIXME: local-set-key is not buffer-local...
      (local-set-key (kbd "q") #'kill-current-buffer))
    (when (string-match (rx (or "Firefox" "Nightly" "Icecat")) exwm-class-name)
      (let* ((curr-width (window-body-size (selected-window) t t))
             (target-width 800)
             (remainder (- target-width curr-width)))
        (exwm-layout-enlarge-window-horizontally remainder)))))

(defun my-eww-latex-preview ()
  (interactive)
  (when (string-match "\*eww\*" (buffer-name))
    (kill-ring-save (point-min) (point-max))
    (find-file (concat "/tmp/eww-" (format-time-string "%Y%m%d-%H%M%S") ".org"))
    (yank)
    (sleep-for 0 500)
    (org-latex-preview '(16))))

(defun my-revisit-buffer ()
  (interactive)
  (unless (bound-and-true-p exwm-class-name)
    (message "Attempting to revisit buffer"))
  (kill-buffer)
  (my-undo-kill-buffer))

(defun my-undo-kill-buffer ()
  (interactive)
  (let ((active-files (cl-loop for buf in (buffer-list)
                               when (buffer-file-name buf) collect it)))
    (cl-loop for file in recentf-list
             unless (member file active-files) return (find-file file))))

(defmacro my-symconcat (pre x &optional post)
  "Expand a symbol name with strings, without needing the symbol to be quoted
first."
  `(let ((newsym (intern (concat ,pre (symbol-name ,x) ,post))))
     (if (boundp newsym) newsym
       nil)))

(defmacro my-symconcat* (&optional pre x post)
  `(intern (concat ,pre (symbol-name ,x) ,post)))


;; TODO: Make my-normie-p buffer-local and let the toggler restore every buffer.
;;       It still won't normalize new buffers, but I don't plan on using to
;;       that extent, and can always call the toggle twice to bring the new
;;       ones into the fold.
(defun my-normie-toggle ()
  "Toggle between normie-friendly and weirder personal settings.
Note that this function will populate the list of modes to toggle
every time it turns on the \"normal\" settings, so the assumption
is that your initfiles put everything in your preferred
\"abnormal\" state to start with.

Note that the command is only ever meant to be called on one
buffer.  While calling it elsewhere to undo the
changes (i.e. \"abnormalize\" your settings) will successfully
undo global modes such as CUA (see function `cua-mode'), the
local settings in the buffer where it was first called will not
be fixed.

This is meant for a single text buffer to act as
a chat between a deaf and hearing person.  As a workaround, call
\\[my-normie:normalize] on every buffer you want to work together
on.  Note that after that, even calling \\[my-normie:abnormalize]
on each buffer involved may not fully restore settings."
  (interactive)
  (if my-normie-p
      (my-normie:abnormalize)
    (my-normie:normalize)))

(defvar my-normie:buffer-modes nil)

(defun my-normie:normalize ()
  "Disable settings that may confuse someone at my keyboard.
See `my-normie-toggle'."
  (interactive)
  ;; Global
  (setq my-normie-p t)
  (cua-mode)
  (show-paren-mode 0)
  (define-key key-translation-map (kbd "[") nil)
  (define-key key-translation-map (kbd "]") nil)
  (define-key key-translation-map (kbd "(") nil)
  (define-key key-translation-map (kbd ")") nil)

  ;; (if (assoc 5 foo)
  ;;     (map-put! foo 5 'c)
  ;;   (cl-pushnew '(5 . d) foo)
  ;;     )

  ;; Local
  (let ((cell (cons (current-buffer)
                    ;; Check for modes to disable.  Remember which ones were
                    ;; on, so `my-normie:abnormalize' can reenable only those.
                    (->> '(rainbow-delimiters-mode
                           prism-mode
                           aggressive-indent-mode
                           show-smartparens-mode
                           abbrev-mode
                           corfu-mode
                           )
                         (-filter #'fboundp)
                         (-filter #'boundp)
                         (--remove (null (symbol-value it)))))))
    (unless (assoc (car cell) my-normie:buffer-modes)
      (push cell my-normie:buffer-modes))
    (dolist (mode (cdr cell))
      (funcall mode 0)))

  ;; Known working single-buffer solution
  ;; (setq-local my-normie:modes-to-toggle
  ;;             (->> '(show-paren-mode ;; global
  ;;                    rainbow-delimiters-mode
  ;;                    prism-mode
  ;;                    aggressive-indent-mode
  ;;                    show-smartparens-mode
  ;;                    abbrev-mode
  ;;                    corfu-mode
  ;;                    )
  ;;                  (-filter #'fboundp)
  ;;                  (-filter #'boundp)
  ;;                  (--remove (null (symbol-value it)))))
  ;; (dolist (f my-normie:modes-to-toggle)
  ;;   (funcall f 0))
  )

(defun my-normie:abnormalize ()
  "Revert to abnormal mode (turn on settings I personally like).
See `my-normie-toggle' for explanation."
  ;; Global
  (setq my-normie-p nil)
  (cua-mode 0)
  (show-paren-mode)
  (define-key key-translation-map (kbd "[") (kbd "("))
  (define-key key-translation-map (kbd "]") (kbd ")"))
  (define-key key-translation-map (kbd "(") (kbd "["))
  (define-key key-translation-map (kbd ")") (kbd "]"))
  ;; Local
  (dolist (bufinfo my-normie:buffer-modes)
    (with-current-buffer (car bufinfo)
      (dolist (mode (cdr bufinfo))
        ;; Turn on each mode.
        (funcall mode))))

  ;; Known working single-buffer solution
  ;; (dolist (f my-normie:modes-to-toggle)
  ;;   (funcall f))
  )

(defvar my-normie:modes-to-toggle nil
  "Modes for `my-normie-toggle' to track for this buffer.")

(defvar my-normie-p nil
  "State variable, see `my-normie-toggle'.")

(defun my-fix-pdf-midnight-colors ()
  (setq pdf-view-midnight-colors (cons (face-foreground 'default)
                                       (face-background 'default))))

(defface my-unimportant-latex-face
  '((t :height 0.7
       :inherit font-lock-comment-face))
  "Face used on less relevant math commands."
  :group 'my-faces)

(defun my-subdue-latex (mode)
  "Visually subdue some LaTeX math commands, like \\left and
  \\begin{}, in the mode MODE."
  (font-lock-add-keywords
   mode
   `((,(rx (and "\\" (or (any ",.!;\\]\[()")
  (and (or "left" "right"
           ;; "left\\lbrace" "right\\lbrace"
           "big" "Big" "bigg" "Bigg"
           "text" "textrm"
           "begin" "end")
       symbol-end))))
      0 'my-unimportant-latex-face prepend))
   'end))

(defun my-passive-aggressive ()
  "Make aggressive-indent-mode indent on save only.
This works around performance issues in some contexts.  Add this
function to `aggressive-indent-mode-hook'."
  ;; buffer-locally remove the rapid reindentation trigger
  (if (featurep 'aggressive-indent)
      (remove-hook
       'post-command-hook #'aggressive-indent--indent-if-changed 'local)))

;; see also crux-switch-to-previous-buffer, which is different
(defun my-switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun my-theme-mods ()
  (interactive)
  (with-eval-after-load 'org
    (set-face-attribute 'org-block-begin-line () :background 'unspecified :inherit 'region)
    (set-face-attribute 'org-block-end-line   () :background 'unspecified :inherit 'region))
  (with-eval-after-load 'org-habit
    ;; the default red color doesn't end up helping my psyche
    (set-face-attribute 'org-habit-overdue-face () :background (face-foreground 'font-lock-comment-face))
    )
  (with-eval-after-load 'rainbow-delimiters
    (set-face-attribute 'rainbow-delimiters-depth-3-face () :foreground 'unspecified :inherit 'font-lock-builtin-face)
    (set-face-attribute 'rainbow-delimiters-depth-2-face () :foreground 'unspecified :inherit 'default)
    (set-face-attribute 'rainbow-delimiters-depth-8-face () :foreground 'unspecified :inherit 'font-lock-doc-face)
    (set-face-attribute 'rainbow-delimiters-depth-9-face () :foreground 'unspecified :inherit 'font-lock-function-name-face))
  (with-eval-after-load 'eww
    (face-spec-reset-face 'eww-form-text)
    (set-face-attribute 'eww-form-text () :inherit 'widget-field)
    (set-face-attribute 'eww-form-checkbox ()
                        :foreground 'unspecified :background 'unspecified
                        :box 'unspecified :inherit 'org-checkbox))
  (with-eval-after-load 'comint
    ;; (set-face-background 'comint-highlight-prompt (face-background 'fringe))
    (set-face-attribute 'comint-highlight-prompt () :inherit 'fringe)
    (set-face-foreground 'comint-highlight-input "white"))
  (with-eval-after-load 'exwm
    (when exwm-workspace-minibuffer-position
      (set-face-attribute 'minibuffer-prompt () :foreground 'unspecified
                          :inherit 'mode-line)
      (set-frame-parameter (car (minibuffer-frame-list))
                           'background-color (face-background 'mode-line))
      (with-current-buffer " *Echo Area 0*"
        (setq-local face-remapping-alist
                    `((default `(:background ,(face-background 'mode-line))))))
      (with-current-buffer " *Echo Area 1*"
        (setq-local face-remapping-alist
                    `((default `(:background ,(face-background 'mode-line))))))
      ))
  (set-face-attribute 'font-lock-regexp-grouping-construct () :inverse-video t)
  (customize-set-variable 'pdf-view-midnight-colors ;; Match the default face
                          (cons (face-foreground 'default)
                                (face-background 'default)))
  ;; Fix wombat-theme (underlines never look good)
  (set-face-attribute 'highlight () :underline nil)
  (with-eval-after-load 'man
    (set-face-attribute 'Man-underline () :underline nil))
  ;; FIXME
  ;; Blend the modeline with the fringes.
  ;; (if (null (face-background 'fringe nil nil))
  ;;     (set-face-attribute 'fringe () :inherit 'mode-line
  ;;                         :foreground 'unspecified :background 'unspecified)
  ;;   (set-face-attribute 'mode-line () :inherit 'fringe
  ;;                       :foreground 'unspecified :background 'unspecified))
  )

(defun my-appropriate-theme ()
  (let ((hour (nth 2 (decode-time))))
    (if (> 6 hour) 'base16-fallout-red
      (if (> 12 hour) 'base16-fallout-blue ;; Between 6 and 12, use blue
        (if (> 19 hour) 'base16-fallout-green ;; Between 12 and 19, use green
          'base16-fallout-amber)))))

(defun my-insert-today ()
  "Insert today's date."
  (interactive)
  (let ((today (format-time-string "%Y-%m-%d")))
    (if (derived-mode-p 'org-mode)
        (insert (concat "[" today "]"))
      (insert today))))

;; Much better tty colors. Explanation: Bold black text in Emacs translates to
;; "brightblack" in the tty, which is... dark grey. Note that the background
;; would ordinarily become dark grey as well, rendering your text unreadable,
;; but there seems to be a special case in the tty rules translating a "bright"
;; background to non-"bright" despite what you tell it. End result: we have dark
;; grey code comments on black.
;; It could be a master's thesis just to unify the theming across tty, xterm and gui.
(defun my-tty-colors ()
  (interactive)
  (set-face-attribute 'font-lock-comment-face ()
                      :foreground "black"
                      :weight 'bold)
  (set-face-attribute 'font-lock-comment-delimiter-face ()
                      :foreground "black"
                      :weight 'bold)
  (set-face-attribute 'vertical-border ()
                      :foreground "black"
                      :weight 'bold))

(provide 'my-lib)

;;; my-lib.el ends here
