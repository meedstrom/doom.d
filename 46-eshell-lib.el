;;; -*- lexical-binding: t; -*-
;; Copyright (C) 2020-2023 Martin Edström
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

;;; Stuff

(defvar my-eshell-last-cmd-start nil)
(defvar my-eshell-buffer-counter 0)
(defvar my-eshell-backref-counter 0)
(defvar my-eshell-id nil)
(make-variable-buffer-local 'my-eshell-last-cmd-start)
(make-variable-buffer-local 'my-eshell-backref-counter)
(make-variable-buffer-local 'my-eshell-id)

(defun my-eshell-assign-id ()
  (setq-local my-eshell-id (my-alphabetic my-eshell-buffer-counter))
  (cl-incf my-eshell-buffer-counter))

(defun my-eshell-timestamp-update ()
  "When added to `eshell-pre-command-hook', the first string --:-- in the
prompt becomes a timestamp like 13:59 after you run a command."
  (save-excursion
    (forward-line -1)
    (when-let* ((unfilled-timestamp "--:--")
                (end (search-forward unfilled-timestamp nil t))
                (beg (- end (length unfilled-timestamp)))
                (inhibit-read-only t))
      (delete-region beg end)
      (insert (format-time-string "%H:%M"))
      (add-text-properties beg (point) '(font-lock-face eshell-prompt)))))

;; TODO: maybe use a variable like this instead of hardcoding.  but the
;; characters inside have to be unique compared to the rest of the prompt
(defvar my-eshell-prompt-backref-placeholder-string "--")

(defun my-eshell-save-output-into-backref ()
  "Save last output into a variable and echo its name in the prompt."
  (when my-eshell-last-cmd-start
    (let ((output (buffer-substring (eshell-beginning-of-output)
                                    (eshell-end-of-output)))
          (i (my-base36 (prog1 my-eshell-backref-counter
                        (cl-incf my-eshell-backref-counter)))))
      (unless (string-blank-p output)
        (unless my-eshell-id
          (my-eshell-assign-id))
        ;; Save the backref buffer-locally.
        ;; (set (make-local-variable (intern (concat my-eshell-id i))) output)
        ;; Save the backref globally.
        (set (intern (concat my-eshell-id i)) output)
        (eshell-previous-prompt 1)
        ;; NOTE: makes assumption about eshell-prompt-function, and that the
        ;; placeholder is in the last line in case of a multiline prompt.
        (if (search-backward "] --" ;;(line-beginning-position)
                             )
            (progn
              (forward-char 2)
              (let ((beg (point))
                    (inhibit-read-only t)
                    (query-replace-skip-read-only nil)
                    (inhibit-message t))
                (replace-regexp (rx (** 2 3 "-")) "" nil (point) (+ 4 (point)))
                (insert (concat my-eshell-id i))
                (add-text-properties beg (point) '(font-lock-face eshell-prompt
                                                   read-only t)))
              (goto-char (point-max)))
          (message "Eshell: Failed to find backref placeholder"))))))

(defun my-eshell-time-cmd-1 ()
  (setq my-eshell-last-cmd-start (time-to-seconds)))

(defvar my-real-eshell-post-command-hook nil
  "Hook run after a non-blank command.
Functions here have access to the variable
`my-eshell-last-cmd-start', a time represented as seconds since
1970, which can be compared to the current output of
`time-to-seconds', usually a somewhat higher number.")

(defun my-eshell-time-cmd-2 ()
  (run-hooks 'my-real-eshell-post-command-hook)
  ;; Possible bug(?) makes post-command-hook run after no-ops, but pre-command
  ;; hook didn't beforehand.  By setting this variable at pre-command-hook, and
  ;; nulling it now, we know next time if a real command did run.
  (setq my-eshell-last-cmd-start nil))

(defun my-eshell-print-elapsed-maybe ()
  (when my-eshell-last-cmd-start
    (let ((n (- (time-to-seconds) my-eshell-last-cmd-start)))
      (when (> n 1)
        (let ((inhibit-read-only t))
          (save-mark-and-excursion
            (eshell-next-prompt 1) ;; b/c other post-command hooks may have run
            (let ((beg (search-backward "〈 ")))
              (forward-char 1)
              (insert
               " Last command time elapsed: "
               (if (> n 100)
                   (number-to-string (round n))
                 (substring (number-to-string n) 0 4))
               " s\n ")
              (add-text-properties beg (point) '(font-lock-face eshell-prompt
                                                 read-only t)))))))))


;;; Base encoding

(defun my-base36 (num)
  "Encode NUM as base 36 string.
This goes z (meaning 0) to a (meaning 25), then 9 (meaning 26) to
0 (meaning 35).  After that, it's yz (meaning 36) to y0 (meaning
71), then xz to x0, wz to w0 and so on.

Why it's backwards?  Look up `message-number-base36' and reprogram
it if you can, I couldn't."
  (require 'message)
  (message-number-base36 num (length (number-to-string (* 10 (/ num 36))))))

(defun my-alphabetic (num)
  "Encode NUM as base 26, represented by a string of letters.
Useful where you don't want a numeric digit, e.g. the start of a
variable name."
  (my--base26 num (length (number-to-string (* 10 (/ num 26))))))

(defun my--base26 (num len)
  "Backend worker for `my-alphabetic'.
NUM and LEN are as in `message-number-base36'."
  (if (if (< len 0)
          (<= num 0)
        (= len 0))
      ""
    (concat (my--base26 (/ num 26) (1- len))
            (char-to-string (aref "zyxwvutsrqponmlkjihgfedcba"
                                  (% num 26))))))


;;; Narrowing shenanigans

(defun my-eshell-prev-line (&optional arg)
  "Like `previous-line', but auto-narrow if on Eshell output.
If butting up against the edge of a narrow region, widen.

Meant to replace `previous-line'.  ARG is passed on."
  (interactive "p")
  (if (buffer-narrowed-p)
      (my-prev-line-maybe-widen arg)
    (my-eshell-prev-line-maybe-narrow arg)))

(defun my-eshell-next-line (&optional arg)
  "Like `next-line', but auto-narrow if on Eshell output.
If butting up against the edge of a narrow region, widen.

Meant to replace `next-line'.  ARG is passed on."
  (interactive "p")
  (if (buffer-narrowed-p)
      (my-next-line-maybe-widen arg)
    (my-eshell-next-line-maybe-narrow arg)))

(defun my-eshell-prev-line-maybe-narrow (&optional arg)
  "Like `previous-line', but auto-narrow if on Eshell output.
ARG is passed on."
  (interactive "p")
  (previous-line arg)
  (unless (buffer-narrowed-p)
    (let ((prev-prompt-from-above-this-line
           (save-mark-and-excursion
             (eshell-previous-prompt 1)
             (point)))
          (prev-prompt-from-end-of-this-line
           (save-mark-and-excursion
             (goto-char (line-end-position))
             (eshell-next-prompt -1)
             (point))))
      (when (= prev-prompt-from-above-this-line
               prev-prompt-from-end-of-this-line)
        ;; Not on a prompt, but in an output, so let's narrow.
        (my-eshell-narrow-to-output)))))

(defun my-eshell-next-line-maybe-narrow (&optional arg)
  "Like `next-line', but auto-narrow if on Eshell output.
ARG is passed on."
  (interactive "p")
  (next-line arg)
  ;; (setq-local my--esh-scroll-position )
  (unless (buffer-narrowed-p)
    (let ((next-prompt-from-below-this-line
           (save-mark-and-excursion
             (goto-char (line-end-position))
             (eshell-next-prompt 1)
             (point)))
          (next-prompt-from-start-of-this-line
           (save-mark-and-excursion
             (goto-char (line-beginning-position))
             (eshell-next-prompt 1)
             (point))))
      (when (= next-prompt-from-below-this-line
               next-prompt-from-start-of-this-line)
        ;; Not on a prompt, but in an output, so let's narrow.
        (my-eshell-narrow-to-output)))))

(defun my-prev-line-maybe-widen (&optional arg)
  "Like `previous-line', but break out of a narrowed region.
ARG is passed on."
  (interactive "p")
  (when (and (buffer-narrowed-p)
             (not (region-active-p))
             ;; On first line within narrow area
             (not (string-match-p "\n" (buffer-substring (point-min) (point)))))
    (widen)
    (recenter nil t))
  (previous-line arg))

(defun my-next-line-maybe-widen (&optional arg)
  "Like `next-line', but break out of a narrowed region.
ARG is passed on."
  (interactive "p")
  (when (and (buffer-narrowed-p)
             (not (region-active-p))
             ;; On last line within narrow area
             (not (string-match-p "\n" (buffer-substring (point) (point-max)))))
    (widen)
    (recenter nil t))
  (next-line arg))

(defun my-eshell-narrow-to-prompt ()
  "Narrow buffer to prompt at point."
  (interactive)
  (forward-line)
  (narrow-to-region
   (save-mark-and-excursion
     (eshell-previous-prompt 1)
     (line-beginning-position))
   (save-mark-and-excursion
     (eshell-next-prompt 1)
     (re-search-backward eshell-prompt-regexp nil t)
     (point))))

(defun my-eshell-narrow-to-output ()
  "Narrow buffer to output at point."
  (interactive)
  (forward-line)
  (narrow-to-region
   (save-mark-and-excursion
     (eshell-previous-prompt 1)
     (forward-line)
     (point))
   (save-mark-and-excursion
     (eshell-next-prompt 1)
     (re-search-backward eshell-prompt-regexp nil t)
     (point))))

(defun my-eshell-narrow-dwim ()
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (my-eshell-narrow-to-output)))

(defun my-eshell-next-prompt (n)
  (interactive "p")
  (if (buffer-narrowed-p)
      (widen))
  (eshell-next-prompt n))

(defun my-eshell-previous-prompt (n)
  (interactive "p")
  (if (buffer-narrowed-p)
      (widen))
  (eshell-previous-prompt n))

(defun my-eshell-consult-history ()
  (interactive)
  (consult-history (my-eshell-history)))


;;; At-point magic

(defun my-copy-region-to-variable (name)
  (interactive "SVariable name: ")
  (set name (buffer-substring (region-beginning) (region-end))))

(defun my-eval-and-replace-print ()
  "Replace the preceding sexp with its value using prin1.
See also `crux-eval-and-replace'."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun my-replace-envvar-at-point-with-value ()
  (interactive)
  (when-let* ((thing (thing-at-point 'sexp))
              (bounds (bounds-of-thing-at-point 'sexp))
              (value (if (string-match-p (rx bol "$") thing)
                         (getenv (substring thing 1))
                       (getenv thing))))
    (delete-region (car bounds) (cdr bounds))
    (insert value)))

;; TODO: Merge with crux-eval-and-replace TODO: Alternatively, just hack
;; eval-last-sexp to mark the sexp and copy result into kill ring, and
;; additionally be able to "eval" envvars for their value.
(defun my-replace-var-at-point-with-value ()
  "Replace a variable at point with its value.
This preferentially assumes it's an envvar (see `getenv'), with
or without an initial $ sign, otherwise a Lisp variable."
  (interactive)
  (when-let* ((thing (thing-at-point 'sexp))
              (bounds (bounds-of-thing-at-point 'sexp))
              (value (if (string-match-p (rx bos "$") thing)
                         (getenv (substring thing 1))
                       (or (getenv thing)
                           (buffer-local-value (intern thing) (current-buffer))
                           (symbol-value (intern thing))))))
    (delete-region (car bounds) (cdr bounds))
    (insert value)))

(defun my-replace-path-at-point-with-truename ()
  (interactive)
  (when-let* ((foo (thing-at-point 'filename))
              (bounds (bounds-of-thing-at-point 'filename))
              (truename (file-truename (substitute-in-file-name foo))))
    (delete-region (car bounds) (cdr bounds))
    (insert truename)))

;; TODO: make undo work with it...
(defvar my-cycle-path-options nil)
(defun my-cycle-path-at-point ()
  "Replace filename at point with a different way to express it.
This may remove the directory component, use a path relative from
`default-directory', or use a full filesystem path."
  (interactive)
  (require 'dash)
  (require 'subr-x)
  (if-let* ((bounds (bounds-of-thing-at-point 'filename))
            (name (substitute-in-file-name (thing-at-point 'filename)))
            (options (if (eq last-command this-command)
                         my-cycle-path-options
                       (setq my-cycle-path-options
                             (-uniq (list (file-relative-name name)
                                          (file-truename name)
                                          (file-name-nondirectory name)))))))
      ;; (Warning for Lisp-originated brain damage) Try to use only tail of
      ;; list (via `member'), so we cycle thru the entire list instead of just
      ;; flipping between the first two unique items.  If this let-binding is
      ;; simple to you, you are beyond saving.
      (let ((alts (or (remove name (member name options))
                      (remove name options))))
        (if (null alts)
            (message "Couldn't find alternative way to express filename.")
          (delete-region (car bounds) (cdr bounds))
          (insert (car alts))))
    (message "This probably isn't a filename.")))


;;; History, scrollback...

;; TODO: Always timestamp commands in history so they can be truly ordered.
;;       Probably requires hacking eshell-hist-initialize etc. See wip.el.

;; NOTE: This one just appends whole histories, so old commands in one buffer
;; can come before recent commands in another buffer.
(defun my-eshell-history ()
  "Return merged history for passing to `consult-history'.
Takes histories of all currently open eshell buffers."
  (let* ((histories (->> (--map (buffer-local-value 'eshell-history-file-name it)
                                (my-eshell-buffers))
                         (-filter #'f-exists-p)
                         (-filter #'f-readable-p)))
         (histories-oldest-first
          (seq-sort-by (lambda (x)
                         (time-convert (file-attribute-modification-time
                                        (file-attributes x))
                                       'integer))
                       #'>
                       histories)))
    (with-temp-buffer
      (cl-loop for f in histories-oldest-first
               do (insert-file-contents f))
      (nreverse (s-split "\n" (buffer-string) t)))))

(defun my-eshell-here (&optional dir)
  "Open or revisit a shell in DIR or the current directory.
Note: doesn't scan for the shell buffers' `default-directory'
values, but rather just looks at their names, expecting them to
have been set by `my-eshell-rename' on
`eshell-directory-change-hook' and `eshell-mode-hook'."
  (interactive)
  (let* ((dir (or dir default-directory))
         (name (my-generate-eshell-name dir)))
    (if (get-buffer name)
        (switch-to-buffer name)
      (let ((default-directory dir))
        (eshell "new"))
      (setq-local eshell-history-file-name (my-eshell-history-file dir))
      (setq-local my-eshell-scrollback-file (my-eshell-scrollback-file dir))
      (eshell-hist-initialize)
      ;; (my-restore-scrollback)
      )))

(defun my-eshell-buffers ()
  "Return a list of live eshell buffers."
  (cl-loop for buf in (buffer-list)
           when (eq (buffer-local-value 'major-mode buf) #'eshell-mode)
           collect it))

;; wip
;; TODO: test it
;; TODO: put setq-local etc on eshell mode hook and eshell change directory hook
;; (defun my-eshell-here* ()
;;   "Open or revisit a shell in the current directory.
;; Attempts to scan all live eshell buffers."
;;   (interactive)
;;   (let* ((bufs (my-eshell-buffers))
;;          (dir default-directory)
;;          (dirs-bufs-alist
;;           (-zip (--map (buffer-local-value 'default-directory it) bufs)
;;                 bufs)))
;;     (if (member dir (map-keys bufs-dirs-alist))
;;         (switch-to-buffer (map-elt bufs-dirs-alist dir))
;;       (let ((default-directory dir))
;;         (eshell "new"))
;;       (setq-local eshell-history-file-name (my-eshell-history-file dir))
;;       (setq-local my-eshell-scrollback-file (my-eshell-scrollback-file dir))
;;       (eshell-hist-initialize))
;;     ;; (my-restore-scrollback)
;;     ))

(defun my-eshell-history-file (&optional dir)
  (expand-file-name ".eshell-command-history" (or dir default-directory)))

(defun my-eshell-scrollback-file (&optional dir)
  (expand-file-name ".eshell-scrollback" (or dir default-directory)))

(defun my-restore-scrollback* ()
  (when (derived-mode-p 'eshell-mode)
    (insert
     (with-temp-buffer
       (insert-file-contents-literally (or (bound-and-true-p my-eshell-scrollback-file)
                                           (my-eshell-scrollback-file)))
       (princ (buffer-string))))))

(defun my-restore-scrollback ()
  (when (derived-mode-p 'eshell-mode)
    (insert-file-contents (or (bound-and-true-p my-eshell-scrollback-file)
                              (my-eshell-scrollback-file)))))

(defun save-eshell-scrollback* ()
  (let* ((file (or (bound-and-true-p my-eshell-scrollback-file)
                   (my-eshell-scrollback-file)))
         (maybe (get-file-buffer file)))
    (when maybe (kill-buffer maybe))
    (save-mark-and-excursion
      (let ((end (point)))
        (eshell-previous-prompt 1)
        (forward-line 0)
        (let* ((beg (point))
               (savestring (prin1-to-string (buffer-substring beg end))))
          (with-temp-buffer
            (insert savestring)
            (write-region (point-min) (point-max) file
                          'append 'silently)
            ))))))

(defun my-eshell-save-scrollback ()
  (let* ((file (or (bound-and-true-p my-eshell-scrollback-file)
                   (my-eshell-scrollback-file)))
         (maybe (get-file-buffer file)))
    (when (f-writable-p file)
      (when maybe (kill-buffer maybe))
      (save-mark-and-excursion
        (let ((end (point)))
          (eshell-previous-prompt 1)
          (forward-line 0)
          (let ((beg (point)))
            (write-region beg end file
                          'append 'silently)))))))


;;; More stuff

;; i think it was ambrevar who made this. maybe mickeyp?
(defun my-fish-style-path (path max-len)
  "Return a potentially trimmed representation of the directory
path PATH, replacing parent directories with their initial
characters if necessary. Try to get the length of PATH, down to
MAX-LEN, not counting slashes."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

(defun my-generate-eshell-name (dir)
  (concat "*eshell " (my-fish-style-path dir 30) "*"))

(defun my-eshell-rename ()
  (let ((newname (my-generate-eshell-name default-directory)))
    (if (get-buffer newname)
        (message "alrdy exist")
      (rename-buffer newname))))

;; I still haven't used this -- keep forgetting ...
(defun eshell/deb (&rest args)
  (eshell-eval-using-options
   "deb" args
   '((?f "find" t find "list available packages matching a pattern")
     (?i "installed" t installed "list installed debs matching a pattern")
     (?l "list-files" t list-files "list files of a package")
     (?s "show" t show "show an available package")
     (?v "version" t version "show the version of an installed package")
     (?w "where" t where "find the package containing the given file")
     (nil "help" nil nil "show this usage information")
     :show-usage)
   (eshell-do-eval
    (eshell-parse-command
     (cond
      (find
       (format "apt-cache search %s" find))
      (installed
       (format "dlocate -l %s | grep '^.i'" installed))
      (list-files
       (format "dlocate -L %s | sort" list-files))
      (show
       (format "apt-cache show %s" show))
      (version
       (format "dlocate -s %s | egrep '^(Package|Status|Version):'" version))
      (where
       (format "dlocate %s" where))))
    t)))

(defun my-pkg-version (lib)
  (require 'epl)
  (string-join (mapcar #'number-to-string
                       (epl-package-version (epl-package-from-file
                                             (find-library-name lib))))
               "."))

;; Tip: installing xwininfo or xprop lets neofetch know the WM
(defun eshell/neofetch (&rest args)
  (eshell/wait (eshell-external-command "neofetch" args))
  (save-mark-and-excursion
    (eshell-previous-prompt 1)
    (when (re-search-forward "Uptime: " nil t)
      (forward-line 1)
      (goto-char (line-beginning-position))
      (insert "Emacs Uptime: ")
      (add-text-properties
       (line-beginning-position) (point)
       `(face (list :foreground ,(seq-elt ansi-color-names-vector 3)
                    :weight bold)))
      (insert (emacs-uptime))
      (open-line 1))
    (forward-line)
    (goto-char (line-beginning-position))
    (insert "Emacs: ")
    (add-text-properties
       (line-beginning-position) (point)
       `(face (list :foreground ,(seq-elt ansi-color-names-vector 3)
                    :weight bold)))
    (insert emacs-version)
    (open-line 1)
    (when (re-search-forward "Shell: " nil t)
      (delete-region (point) (line-end-position))
      (insert (concat "eshell " (my-pkg-version "eshell"))))
    (when (and (bound-and-true-p exwm-state)
               (= exwm-state 1))
      (when (re-search-forward "WM: " nil t)
        (delete-region (point) (line-end-position))
        (insert (concat "Emacs X Window Manager " (my-pkg-version "exwm")))))
    (when (re-search-forward "Terminal: " nil t)
      (delete-region (point) (line-end-position))
      (insert "no terminal emulation")
      ;; (insert (concat "emacs " emacs-version))
      )))

(defun eshell/gp ()
  (eshell-command "guix pull && guix pull --news"))
