;;; my-lib-unprefixed.el -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Martin Edström

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

(require 'cl-lib)
(require 'l)
(require 'subr-x)

(defmacro when-car-fbound (form)
  `(when (fboundp (car #',form)) ,form))

(defmacro call-if-fbound (func &rest args)
  `(when (fboundp #',func) (,func ,@args)))

(defalias 'use #'use-package!)

(defmacro custom (package &rest body)
  (declare (indent defun))
  `(use-package! ,package
     :defer
     :custom (,@body)))

(defmacro c (fn &rest body)
  "Interactive version of `l'.
FN and BODY as in `l'.  Typical use: in `define-key' to make a
simple command on the fly."
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
  "Danger: handles numbers as character codes."
  (cond ((stringp x) x)
        ((symbolp x) (symbol-name x))
        ((characterp x) (char-to-string x))))

;;(char-to-string ?\f)
;;(string-to-char "f")

;; To be deprecated in Emacs 29
(defmacro setc (variable value)
  "Similar to `customize-set-variable', but decline to populate `custom-file'.
In other words, like `setq' but calls the `defcustom' :set
function if present.

Useful if you want `custom-file' to only record what you set
through the interactive Custom interface, and not what you set
through Lisp initfiles."
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

;; Backport
(unless (version<= "29" emacs-version)
  (defalias #'setopt #'setc))

(defun lines (&rest args)
  "Intersperse newlines between the strings in ARGS. The purpose is to allow
typing the following, which plays well with indentation.
(lines \"foo\"
       \"bar\"
       \"baz\")"
  (string-join args "\n"))

;; Preserved because it was a Lisp lesson
(defun ^ (x power)
  (apply (function *) (make-list power x)))

;; FIXME: broken compared to quiet!
(defmacro quietly (&rest forms)
  "Run FORMS without generating output, simplified from Doom's `quiet!'."
  `(if init-file-debug
       (progn ,@forms)
     (let ((inhibit-message t)
           (save-silently t))
       (prog1 ,@forms (message "")))))

(defun cut-at (CUTOFF STRING)
  "Variant of `substring'. Always cuts from the start. Permits
the CUTOFF to exceed the length of the string, in which case the
string is returned unaltered without complaint."
  (if (< (length STRING) CUTOFF)
      STRING
    (substring STRING 0 CUTOFF)))

(defmacro add-to-string (string-var &rest additions)
  "Destructive macro that concatenates a variable STRING-VAR
with ADDITIONS and sets that as the variable's new value."
  `(setq ,string-var (concat ,string-var ,@additions)))

;; Lifted from Doom 2.0.9
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

(defmacro sym (&rest args)
  "Shorthand for the expression (intern (concat ARGS)), plus magic. Examples:

(sym \"counsel-\" \"rg\") 
   returns the symbol `counsel-rg'.

(sym w3m-search-default-engine \"-search\") 
   may return the symbol `duckduckgo-search'.

(sym major-mode \"-map\") 
   may return the symbol `exwm-mode-map'.

Note that `w3m-search-default-engine' returned a string \"duckduckgo\".
Note that `major-mode' returned a symbol `exwm-mode'."
  `(intern (mapconcat #'my-symbol-name-or-string-as-is ',args nil)))

(defun my-symbol-name-or-string-as-is (x)
  "Like `symbol-name', but accept string input too."
  (eval `(if (stringp ,x)
             ,x
           (if (symbolp ',x)
               (symbol-name ,x)))))

;; Just an old habit from typing "date" at the terminal. It's how I check the
;; current time, which I do so rarely I can't be bothered to set a hotkey, nor
;; will I clutter the modeline with a clock. M-x date it is.
(defun date ()
  (interactive)
  (message (format-time-string "%F %T %Z (%z) (%A)")))

(provide 'my-lib-unprefixed)

;;; my-unprefixed-lib.el ends here
