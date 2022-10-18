;; Copy-pasted from other configs -*- lexical-binding: t -*-

(defun ambrevar/find-symbol-at-point ()
  "Find the symbol at point, i.e. go to definition."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if (boundp sym)
        (find-variable sym)
      (find-function sym))))
;;(define-key lisp-mode-shared-map (kbd "M-.") 'find-symbol-at-point)

(defun sachac/describe-random-interactive-function ()
  (interactive)
  "Show the documentation for a random interactive function.
Consider only documented, non-obsolete functions."
  (let (result)
    (mapatoms
     (lambda (s)
       (when (and (commandp s)
                  (documentation s t)
                  (null (get s 'byte-obsolete-info)))
         (setq result (cons s result)))))
    (describe-function (elt result (random (length result))))))

(defmacro sachac/convert-shell-scripts-to-interactive-commands (directory)
  "Make the shell scripts in DIRECTORY available as interactive commands."
  (cons 'progn
        (-map
         (lambda (filename)
           (let ((function-name (intern (concat "my/shell/" (file-name-nondirectory filename)))))
             `(defun ,function-name (&rest args)
                (interactive)
                (cond
                 ((not (called-interactively-p 'any))
                  (shell-command-to-string (mapconcat 'shell-quote-argument (cons ,filename args) " ")))
                 ((region-active-p)
                  (apply 'call-process-region (point) (mark) ,filename nil (if current-prefix-arg t nil) t args))
                 (t
                  (apply 'call-process ,filename nil (if current-prefix-arg t nil) nil args))))))
         (-filter (-not #'file-directory-p)
                  (-filter #'file-executable-p (directory-files directory t))))))

(defun prot/focus-minibuffer ()
  "Focus the active minibuffer.

Bind this to `completion-list-mode-map' to M-v to easily jump
between the list of candidates present in the \\*Completions\\*
buffer and the minibuffer (because by default M-v switches to the
completions if invoked from inside the minibuffer."
  (interactive)
  (let ((mini (active-minibuffer-window)))
    (when mini
      (select-window mini))))

(defun prot/focus-minibuffer-or-completions ()
  "Focus the active minibuffer or the \\*Completions\\*.

If both the minibuffer and the Completions are present, this
command will first move per invocation to the former, then the
latter, and then continue to switch between the two.

The continuous switch is essentially the same as running
`prot/focus-minibuffer' and `switch-to-completions' in
succession."
  (interactive)
  (let* ((mini (active-minibuffer-window))
         (completions (get-buffer-window "*Completions*")))
    (cond ((and mini
                (not (minibufferp)))
           (select-window mini nil))
          ((and completions
                (not (eq (selected-window)
                         completions)))
           (select-window completions nil)))))

;; Technically, this is not specific to the minibuffer, but I define
;; it here so that you can see how it is also used from inside the
;; "Completions" buffer
(defun prot/describe-symbol-at-point (&optional arg)
  "Get help (documentation) for the symbol at point.

With a prefix argument, switch to the *Help* window.  If that is
already focused, switch to the most recently used window
instead."
  (interactive "P")
  (let ((symbol (symbol-at-point)))
    (when symbol
      (describe-symbol symbol)))
  (when arg
    (let ((help (get-buffer-window "*Help*")))
      (when help
        (if (not (eq (selected-window) help))
            (select-window help)
          (select-window (get-mru-window)))))))

(defun prot/completions-kill-save-symbol ()
  "Add symbol-at-point to the kill ring.

Intended for use in the \\*Completions\\* buffer.  Bind this to a
key in `completion-list-mode-map'."
  (interactive)
  (kill-new (thing-at-point 'symbol)))

(defun elmord-exwm-get-firefox-url ()
  (exwm-input--fake-key ?\C-l)
  (sleep-for 0.05)                      ; Wait a bit for the browser to respond.
  (exwm-input--fake-key ?\C-c)
  (sleep-for 0.05)
  (gui-backend-get-selection 'CLIPBOARD 'STRING))

(defun elmord-exwm-org-store-link ()
  (when (and (derived-mode-p  'exwm-mode)
             (member exwm-class-name '("Firefox" "Firefox-esr")))
    (org-link-store-props
     :type "http"
     :link (elmord-exwm-get-firefox-url)
     :description exwm-title)))

;; To use elmord's solution
;; (add-to-list 'org-store-link-functions 'elmord-exwm-org-store-link)

(defun xah/lookup-wikipedia ()
  "Look up the word under cursor in Wikipedia.
If there is a text selection (a phrase), use that.

This command switches to browser."
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (browse-url (concat "http://en.wikipedia.org/wiki/" word))))

(setq bh/keep-clock-running nil)

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (derived-mode-p  'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (derived-mode-p 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "a9d30203-8b86-4e84-a6a5-a77f186b0ee5")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(provide 'my-lib-external)
