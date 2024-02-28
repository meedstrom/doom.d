;; -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Martin Edström
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

(defvar-local my-logseq-cookies nil)
(define-minor-mode my-logseq-mode
  "De-fontify Org headings."
  :group 'org
  (when (featurep 'org)
    (let ((headline-faces '(org-level-1
                            org-level-2
                            org-level-3
                            org-level-4
                            org-level-5
                            org-level-6
                            org-level-7
                            org-level-8)))
      (if my-logseq-mode
          (dolist (x headline-faces)
            (push (face-remap-add-relative x 'default) my-logseq-cookies))
        (while my-logseq-cookies
          (face-remap-remove-relative (pop my-logseq-cookies)))))))

