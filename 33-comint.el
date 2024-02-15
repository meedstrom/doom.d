;; -*- lexical-binding: t; -*-
;; Copyright (C) 2020-2024 Martin Edström
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

;; Emacs 28 fixed this
;; ;; Reuse current window on M-x shell, like with eshell.
;; (add-to-list 'display-buffer-alist
;;              (cons "\\*shell\\*" display-buffer--same-window-action))

;; Limit scrollback because gcc and R can spit out enough to slow my system.
;; Good values:
;; 2^12 on Latitude E7250.
;; 2^10 on Thinkpad X200.
(setopt comint-buffer-maximum-size (^ 2 10))
(add-hook 'comint-output-filter-functions #'my-truncate-buffer-and-move-excess)

;; This shouldn't affect eshell, but does...  Maybe it's been fixed since.
;; (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)
