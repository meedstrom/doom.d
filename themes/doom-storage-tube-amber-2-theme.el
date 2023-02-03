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

(def-doom-theme doom-storage-tube-amber-2
  "Limited color palette inspired by old Tektronix monitors."

  (;; Boilerplate ------------------------------------
   (base0      '("#1B2229" "black"   "black"      ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"))
   (base2      '("#202328" "#2e2e2e" "brightblack"))
   (base3      '("#23272e" "#262626" "brightblack"))
   (base4      '("#3f444a" "#3f3f3f" "brightblack"))
   (base5      '("#5B6268" "#525252" "brightblack")); org-done
   (base6      '("#73797e" "#6b6b6b" "brightblack"))
   (base7      '("#9ca0a4" "#979797" "brightblack"))
   (base8      '("#DFDFDF" "#dfdfdf" "white"      ))

   ;; TTY-breaking convenience hack
   (t256 "brightblack" )
   (t8 "brightblack")
   ;; ------------------------------------------------

   ;; TIPS for designing a doom-theme:

   ;; 1. use hex strings, not color names, the latter wont work where your
   ;; theme or the base theme applies doom-blend/doom-darken/doom-lighten.
   ;;
   ;; 2. The 12 primary color names must be thoughtfully assigned first and all
   ;; be distinguishable.  Check the result in org-mode buffers.  The 20
   ;; semantic names are mostly applied in prog-modes.
   ;;
   ;; 3. Look the source for one of the flagship themes for information.
   ;;
   ;; 4. Each color is a list of 3 items. If you don't use the terminal, only
   ;; the first item is of interest, let the rest be grey.
   ;;
   ;; 5. Though it's tempting to reuse colors e.g. reuse the value of "red" for
   ;; the "error" color, this makes it harder to iterate your design.  Directly
   ;; assign all 12+20 colors, and only reuse colors that you somehow know
   ;; you'll never change or distsinguish, if any. I recommend forgetting about
   ;; the possibility.
   ;;
   ;; 6. rainbow-mode defaults end up pretty confusing imo, when names like
   ;; "red" which aren't you assigned to red at all get colored red. To only
   ;; colorize hex strings, (setq rainbow-x-colors nil).


   (bg          '("#251500" nil nil))
   (bg-alt      '("#151000" nil nil)) ;; hl-line, solaire
   (fg          '("dark goldenrod" "white" "white"))
   (fg-alt '("goldenrod" "brightwhite" "brightwhite")) ;; solaire

   ;; These should all be distinct from each other
   (grey       '("#556b2f" "#dfdfdf" "white"              )) 
   (red        '("#b22222"       "red"       "red"        ))
   (orange     '("#bdb76b" "brightred" "brightred"        )); ~org~~
   (green      '("#ffa54f"       "green"       "green"    )); =org=, org-todo
   (teal       '("#9acd32" "brightgreen" "brightgreen"    ))
   (yellow     '("#daa520" "yellow" "yellow"              ))
   (blue       '("#ff7256" "brightblue" "brightblue"      ))
   (dark-blue  '("#ffd700"       "blue"       "blue"      )); org-h1
   (magenta    '("#ff7f24" "brightmagenta" "brightmagenta")); org-h2
   (violet     '("#ff4500"       "magenta"       "magenta")); org-h3
   (cyan       '("#8b4513" "brightcyan" "brightcyan"      ))
   (dark-cyan  '("#8b3626"       "cyan"       "cyan"      ))

   ;; These are mostly applied in prog modes.  And it's ok if some of these are
   ;; indistinguishable.  Up to you; if you absolutely want to distinguish
   ;; strings, then pay consideration to that.  To start off, I suggest that
   ;; comments, strings and errors should be distinguishable while the rest may
   ;; as well be grey or your preferred default face.
   (highlight    '("#ffa500" t256 t8))
   (vertical-bar '("#8b864e" t256 t8))
   (selection    '("#8b864e" t256 t8))
   (builtin      '("#ffa500" t256 t8))
   (comments     '("#8b864e" t256 t8))
   (doc-comments '("#bb9915" t256 t8)); prism-3
   (constants    '("#d2b48c" t256 t8))
   (functions    '("#ff7f24" t256 t8)); prism-1
   (keywords     '("#ffb90f" t256 t8)); prism-2
   (methods      '("#ff8247" t256 t8))
   (operators    '("#ff8247" t256 t8))
   (type         '("#ffa500" t256 t8)); prism-0
   (strings      '("#bbff00" t256 t8))
   (variables    '("#bdb76b" t256 t8))
   (numbers      '("#7ccd7c" t256 t8))
   (region       '("#443311" t256 t8))
   (error        red                 ); FIXME
   (warning      '("#ffd700" t256 t8)); TODO
   (success      '("#eeffaa" t256 t8)); NOTE
   (vc-modified  '("#ffd700" t256 t8))
   (vc-added     '("#eeffaa" t256 t8))
   (vc-deleted   red                 )

   ;; attempt a conceptual translation...
   ;; (base9)
   ;; (base10)
   ;; (base11-0A)
   ;; (base12-0B)
   ;; (base13-0C)
   ;; (base14-0D)
   ;; (base15-0E)
   ;; (base16-0F)

   )
  (
   (form-feed-line :strike-through fg)
   (mini-modeline-mode-line :background bg :foreground strings)
   )
  (
   ;; (mini-modeline-face-attr '(:background bg :foreground strings))
   ))

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
