;; -*- lexical-binding: t; -*-

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

