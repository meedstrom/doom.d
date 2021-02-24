;;; doom-storage-tube-theme.el -*- no-byte-compile: t; -*-
;; Copyright (C) 2021 Martin Edström

;; Author:
;; Maintainer:
;; URL:
;; Keywords: color, theme
;; Version:

;; License: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of
;; the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; I thank Adam Alpern for making "green-phosphor-theme" and hence giving me
;; prior art in the choice of which shades of green should go to which faces.

;;; Code:

(require 'doom-themes)

(def-doom-theme doom-storage-tube
  "Inspired by old Tektronix monitors and PipBoys. GUI only."

  (;; Boilerplate ------------------------------------
   (base0      '("#1B2229" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#202328" "#2e2e2e" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f" "brightblack"  ))
   (base5      '("#5B6268" "#525252" "brightblack"  )) ;; org-done
   (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   (rest       '("brightblack" "brightblack"))
   ;; ------------------------------------------------


   (bg          '("#050511" nil nil))
   (bg-alt      '("#000033" nil nil))
   (fg          '("SkyBlue" "brightwhite" "brightwhite"))
   (fg-alt      '("turquoise" "white" "white"))

   (highlight     (cons "CadetBlue1" rest))
   (vertical-bar  (cons fg rest))
   (selection     (cons "grey" rest))
   (builtin       (cons "DodgerBlue1" rest))          ;; :keys
   (comments      (cons "DarkOliveGreen4" rest))
   (doc-comments  (cons "OliveDrab4" rest)) ;; DEPRECATED
   (constants     (cons "DarkKhaki" rest)) ;; HACK
   (functions     (cons "SeaGreen3" rest))
   (keywords      (cons "DarkOliveGreen1" rest)) ;; REVIEW
   (methods       (cons "dark green" rest))
   (operators     (cons "forest green" rest))
   (type          (cons "DarkOliveGreen1" rest))
   (strings       (cons "light green" rest))    ;; "strings"
   (variables     (cons "DarkKhaki" rest))
   (numbers       (cons "PaleGreen3" rest))
   (region        (cons "#154411" rest))
   (error         (cons "firebrick" rest)) ;; FIXME
   (warning       (cons "gold" rest)) ;; TODO
   (success       (cons "#ffeecc" rest)) ;; NOTE
   (vc-modified   (cons "gold" rest))
   (vc-added      (cons "#ffeecc" rest))
   (vc-deleted    (cons "firebrick" rest))

   ;; These should all be distinct from each other
   (grey      '("DarkOliveGreen4" "#dfdfdf" "white"))
   (red       (cons "yellow" '("red" "red"            )))
   (orange    (cons "DarkKhaki" '("brightred" "brightred"))); ~org~~
   (green     (cons "SeaGreen4" '("green" "green"            ))); rainbow3, =org=, org TODO
   (teal      (cons "PaleGreen3" '("brightgreen" "brightgreen")))
   (yellow    (cons "LightGreen" '("yellow" "yellow"        )))
   (blue      (cons "ForestGreen" '("brightblue" "brightblue"))) ;; rainbow1 apparently
   (dark-blue (cons "DarkOliveGreen1" '("blue" "blue"                   ))); rainbow1 and org-h1, REVIEW
   (magenta   (cons "OliveDrab" '("brightmagenta" "brightmagenta"))); rainbow2 and org-h2
   (violet    (cons "DarkGreen" '("magenta" "magenta"            ))); org-h3
   (cyan      (cons "green" '("brightcyan" "brightcyan")))
   (dark-cyan (cons "LawnGreen" '("cyan" "cyan"            )))

   ;; attempt a conceptual translation...
   ;; (base9)
   ;; (base10)
   ;; (base11-0A)
   ;; (base12-0B)
   ;; (base13-0C)
   ;; (base14-0D)
   ;; (base15-0E)
   ;; (base16-0F)

   ))

;;; Stylistic tips

;; All the rainbow delimiters should be dark/subdued colors.
;;
;; Check if you like the org headings.
;;
;; Check the sharp quoted symbols in Lisp. Uncomment this:
;; #'some-function

;;; Notes

;; For most faces, Doom uses the terminal color names, which indicate the color
;; these things would be on a standard tty. The additional color names
;; primarily apply to prog-mode buffers, and are rarely used as reference.

;; Rainbow delimiters and Org headings each have three color levels in Doom
;; that then loop around.
;; - Rainbow goes 'dark-blue -> 'magenta -> 'green
;; - Org goes     'dark-blue -> 'magenta -> 'violet

;; Some errors get 'red instead of 'error.

;; ~org~ gets 'orange
;; =org= gets 'green
;; : org gets 'orange

;; Dired gets drwx in the order 'blue 'yellow 'error '[green or builtin]
;; Dired also uses 'orange, 'cyan, 'blue, 'violet...

;;; doom-storage-tube-theme.el ends here
