;;; doom-storage-tube-theme.el -*- no-byte-compile: t; -*-
(require 'doom-themes)

(def-doom-theme doom-storage-tube2
  "Inspired by old Tektronix monitors and PipBoys."

  ((bg          '("#041100"     nil nil))
   (bg-alt      '("#001500"     nil nil))
   (fg          '("lime green"  "white" "brightwhite"))
   (fg-alt      '("dark green"     "white" "white"))

   (rest '("grey" "grey"))

   (highlight     (cons "grey" rest))
   (vertical-bar   '("dark green"       "#256256" "grey"))
   (selection      '("grey"             "#256256" "grey"))
   (builtin        '("#00ff00"          "#256256" "grey"))
   (comments       '("DarkOliveGreen4"  "#256256" "brightblack"))
   (doc-comments   '("light green"      "#256256" "grey"))
   (constants      '("dark khaki"       "#256256" "grey"))
   (functions      '("SeaGreen3"        "#256256" "grey"))
   (keywords       '("dark khaki"       "#256256" "grey"))
   (methods        '("dark green"       "#256256" "grey"))
   (operators      '("forest green"     "#256256" "grey"))
   (type           '("grey"             "#256256" "grey"))
   (strings        '("DarkOliveGreen1"  "#256256" "grey"))
   (variables      '("dark khaki"       "#256256" "grey"))
   (numbers        '("PaleGreen3"       "#256256" "grey"))
   (region         '("#154411"          "#256256" "grey"))
   (error          '("firebrick"        "#256256" "red"))
   (warning        '("gold"             "#256256" "yellow"))
   (success        '("#ffeecc"          "#256256" "green"))
   (vc-modified    '("gold"             "#256256" "yellow"))
   (vc-added       '("#ffeecc"          "#256256" "green"))
   (vc-deleted     '("firebrick"        "#256256" "red"))

   (base0 fg)
   (base1 fg)
   (base2 fg)
   (base3 fg)
   (base4 fg)
   (base5 fg)
   (base6 fg)
   (base7 fg)
   (base8 fg)

   (grey      '("grey"     "grey" "grey"))
   (red       (cons (car warning) '("red" "red")))
   (orange    '("orange"   "orange" "brightred")) ;; org markup~
   (green     '("green"    "green" "green")) ;; rainbow3
   (teal      '("cyan"     "cyan" "brightgreen"))
   (yellow    '("yellow"   "yellow" "yellow"))
   (blue      '("blue"     "blue" "blue"))
   (dark-blue '("navy blue"     "blue" "blue")) ;; rainbow1 and org1
   (magenta   '("magenta"  "magenta" "brightmagenta")) ;; rainbow2 and org2
   (violet    '("dark magenta"  "magenta" "magenta")) ;; org3
   (cyan      '("cyan"     "cyan" "brightcyan"))
   (dark-cyan '("dark cyan"     "cyan" "cyan"))

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
;; Dired also uses 'orange, 'cyan, 'blue, ...

;;; doom-storage-tube-theme.el ends here
