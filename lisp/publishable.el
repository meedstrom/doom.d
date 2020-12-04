(defun repeat-digit-times ()
  "Repeat the last command an amount of times, only up to 9
  because this command only works when you bind it to the keys 0-9.
  To do something 10 or more times, use the usual digit arguments."
  (interactive)
  ;; bits pasted from `repeat'
  (when (eq last-repeatable-command 'repeat)
    (setq last-repeatable-command repeat-previous-repeated-command))
  (when (eq last-repeatable-command 'repeat-digit-times)
    (message "I thought it was impossible to end up here"))
  ;; bits pasted from `digit-argument'
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0)))
    (dotimes (i digit) (call-interactively #'repeat))))

(setq post-repeat-map (make-sparse-keymap))
(dotimes (i 10)
  (define-key post-repeat-map (kbd (int-to-string i)) #'repeat-digit-times))

(defun enable-post-repeat-map ()
  (set-transient-map post-repeat-map))

;; (add-function :after #'repeat #'enable-post-repeat-map)

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

(with-eval-after-load 'eshell
  (add-hook 'eshell-pre-command-hook #'my-eshell-timestamp-update)
  (setq eshell-prompt-regexp (rx bol (repeat 7 nonl) " Sir? ")
        eshell-prompt-function (lambda () (concat "[--:--] Sir? "))))
