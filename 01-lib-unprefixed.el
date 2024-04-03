;; Functions without "my-" prefix -*- lexical-binding: t; -*-

;; These are largely unused, but some are really neat in the right context.

(require 'cl-lib)
(require 'subr-x)

;; Backports for when I'm on an old Emacs
(when (> 29 emacs-major-version)
  (require 'compat)
  (require 'general)
  (require 'crux)
  (defalias #'setopt #'general-setq)
  (defalias #'duplicate-dwim #'crux-duplicate-current-line-or-region))

(defmacro while-progn (&rest body)
  `(while (progn ,@body)))

;; So you can type (hookgen org-mode-hook (set-face-attribute ...) ...).
;; I'm split between this method and llama.el, leaning towards llama.el.
(defmacro hookgen (hook &rest body)
  (declare (indent defun))
  (let ((fname (cl-gensym)))
    `(add-hook ',hook (defun ,fname () ,@body))))

;; what's a good name?
(defalias 'my-hook #'hookgen)
(defalias 'make-hook #'hookgen)
(defalias 'anon-hook #'hookgen)
(defalias 'captain-hook #'hookgen)

(defmacro time (&rest body)
  "Evaluate BODY and print time elapsed."
  (let ((T (cl-gensym)))
    `(let ((,T (current-time)))
       ,@body
       (message "Elapsed: %fs" (float-time (time-since ,T))))))

(defmacro time-return (&rest body)
  "Evaluate BODY and print time elapsed.
Then return the last value of BODY."
  (let ((T (cl-gensym)))
    `(let ((,T (current-time)))
       (prog1 (progn ,@body)
         (message "Elapsed: %fs" (float-time (time-since ,T)))))))

(defmacro c (fn &rest body)
  "Interactive version of `l' (short anonymous lambda).
FN and BODY as in `l'.  A typical use-case is with `keymap-set'
to make a simple command on the fly without the expression
flowing across two lines."
  (require 'l)
  `(lambda ,(l--arguments body)
     (interactive)
     (,(if (car-safe fn)
           (cadr fn)
         fn)
      ,@body)))

(defmacro run-after (secs &rest body)
  (declare (indent defun))
  `(run-with-timer ,secs nil (lambda () ,@body)))

(defun as-string (x)
  "Return X as a string, even if it was a symbol or character.
Danger: assumes numbers are character codes."
  (cond ((stringp x) x)
        ((symbolp x) (symbol-name x))
        ((characterp x) (char-to-string x))))

(defun lines (&rest strings)
  "Like `concat', but intersperse newlines between the STRINGS.
This allows typing the following, which plays well with
indentation.

(lines \"foo\"
       \"bar\"
       \"baz\")"
  (string-join strings "\n"))

;; Preserved because it was a Lisp lesson to me
(defun ^ (x power)
  (apply #'* (make-list power x)))

(defun cut-at (CUTOFF STRING)
  "Variant of `substring'.
Always cuts from the start. Permits the CUTOFF to exceed the
length of the string, in which case the string is returned
unaltered with no complaints."
  (if (< (length STRING) CUTOFF)
      STRING
    (substring STRING 0 CUTOFF)))

(defmacro add-to-string (string-var &rest additions)
  "Destructive macro that concatenates a variable STRING-VAR
with ADDITIONS and sets that as the variable's new value."
  `(setq ,string-var (concat ,string-var ,@additions)))

;; Lifted from Doom
(unless (boundp 'doom-version)
  (defmacro quiet! (&rest forms)
    "Run FORMS without generating any output.

This silences calls to `message', `load', `write-region' and anything that
writes to `standard-output'. In interactive sessions this inhibits output to the
echo-area, but not to *Messages*."
    `(if init-file-debug
         (progn ,@forms)
       ,(if noninteractive
            `(letf! ((standard-output (lambda (&rest _)))
                     (defun message (&rest _))
                     (defun load (file &optional noerror nomessage nosuffix must-suffix)
                       (funcall load file noerror t nosuffix must-suffix))
                     (defun write-region (start end filename &optional append visit lockname mustbenew)
                       (unless visit (setq visit 'no-message))
                       (funcall write-region start end filename append visit lockname mustbenew)))
               ,@forms)
          `(let ((inhibit-message t)
                 (save-silently t))
             (prog1 ,@forms (message "")))))))

(unless (boundp 'doom-version)
  (defmacro after! (package &rest body)
    "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol or list of them. These are package names, not modes,
functions or variables. It can be:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
  Without :or/:any/:and/:all, :and/:all are implied.

This is a wrapper around `eval-after-load' that:

1. Suppresses warnings for disabled packages at compile-time
2. No-ops for package that are disabled by the user (via `package!')
3. Supports compound package statements (see below)
4. Prevents eager expansion pulling in autoloaded macros all at once"
    (declare (indent defun) (debug t))
    (if (symbolp package)
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              (let ((body (macroexp-progn body)))
                `(if (featurep ',package)
                     ,body
                   ;; We intentionally avoid `with-eval-after-load' to prevent
                   ;; eager macro expansion from pulling (or failing to pull) in
                   ;; autoloaded macros/packages.
                   (eval-after-load ',package ',body))))
      (let ((p (car package)))
        (cond ((not (keywordp p))
               `(after! (:and ,@package) ,@body))
              ((memq p '(:or :any))
               (macroexp-progn
                (cl-loop for next in (cdr package)
                         collect `(after! ,next ,@body))))
              ((memq p '(:and :all))
               (dolist (next (cdr package))
                 (setq body `((after! ,next ,@body))))
               (car body)))))))

(defun my-symbol-name-or-string-as-is (x)
  "Like `symbol-name', but accept string input too."
  (eval `(if (stringp ,x)
             ,x
           (if (symbolp ',x)
               (symbol-name ,x)))))

(defmacro symcat (&rest args)
  "Shorthand for (intern (concat ARGS)), plus magic for the lazy.
Examples:

\(symcat \"counsel-\" \"rg\")
   returns the symbol `counsel-rg'.

\(symcat w3m-search-default-engine \"-search\")
   may return the symbol `duckduckgo-search'.

\(symcat major-mode \"-map\")
   may return the symbol `exwm-mode-map'.

Note that the last two cases are subtly different:
`w3m-search-default-engine' returned the string \"duckduckgo\"
\(not a symbol!), while `major-mode' returned the symbol
`exwm-mode' \(not a string!)."
  `(intern (mapconcat #'my-symbol-name-or-string-as-is ',args nil)))

;; Just an old habit from typing "date" at the terminal. It's how I check the
;; current time, which I do so rarely I can't be bothered to set a hotkey, nor
;; will I clutter the modeline with a clock. M-x date it is.
(defun date ()
  (interactive)
  (message (format-time-string "%F %T %Z (%z) (%A)")))

(provide 'my-lib-unprefixed)

;;; my-unprefixed-lib.el ends here
