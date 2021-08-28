;; Experimental stuff, load this file last -*- lexical-binding: t; -*-

(defun repeat-digit-times ()
  "Repeat the last command an amount of times.
Only up to 9 because this command only works when you bind it to
the keys 0-9 (or some combination like C-1, C-2...).  The idea is
to allow you to say \"command, repeat n times\" instead of \"do n
times command\". See `post-repeat-map' and
`enable-post-repeat-map'."
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

(defvar post-repeat-map (make-sparse-keymap))
(dotimes (i 10)
  (define-key post-repeat-map (kbd (int-to-string i)) #'repeat-digit-times))

(defun enable-post-repeat-map ()
  (set-transient-map post-repeat-map))

;; (add-function :after #'repeat #'enable-post-repeat-map)

;; Wishlist: A command for repeat with universal arg
;; Wishlist: A command for undo then repeat with universal arg

