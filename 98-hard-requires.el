(require 'crux)

(require 'beginend)
(beginend-global-mode)

(require 'form-feed)
(global-form-feed-mode)
(add-hook 'emacs-lisp-compilation-mode-hook #'form-feed-mode)

(require 'apheleia)
(apheleia-global-mode)
(setopt apheleia-log-debug-info t)

;; note: "javascript" actually just applies to js2-mode.
;; generally this package needs some updates
(require 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml)
