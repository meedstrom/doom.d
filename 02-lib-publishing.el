;; -*- lexical-binding: t; -*-

(defun my-org-file-id (file)
  "Quickly get the file-level id from FILE.
For use in heavy loops; it skips activating `org-mode'.
For all other uses, see `org-id-get'."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 200)
    (when (search-forward ":id: " nil t)
      (when (= (line-number-at-pos) (line-number-at-pos (point-max)))
        (error "Whoops, amend `my-org-file-id'"))
      (delete-horizontal-space)
      (buffer-substring (point) (line-end-position)))))

(defun my-org-file-tags (file)
  "Quickly get the file-tags from FILE.
For use in heavy loops; it skips activating `org-mode'.
For all other uses, see `org-get-tags'."
  (with-temp-buffer
    (insert-file-contents file nil 0 400)
    (let ((boundary (or (save-excursion (re-search-forward "^ *?[^#:]" nil t))
                        (point-max))))
      (when (search-forward "#+filetags: " boundary t)
        (when (= (line-number-at-pos) (line-number-at-pos (point-max)))
          (error "Whoops, amend `my-org-file-tags'"))
        (thread-first (buffer-substring (point) (line-end-position))
                      (string-trim)
                      (string-split ":" t)
                      (sort #'string-lessp))))))

(defun my-uuid-to-pageid (uuid)
  (let* ((hexa (string-trim (string-replace "-" "" uuid)))
         (decimal (string-to-number hexa 16)))
    (if (or (= 0 decimal) (/= 32 (length hexa)))
        (error "String should be a valid UUID 36 chars long: %s" uuid)
      (substring (my-int-to-consonants decimal 5) -5))))

(defun my-int-to-consonants (integer &optional length)
  (let ((result "")
        (remainder integer))
    (while (> remainder 0)
      (setq result (concat (char-to-string (my-int-to-consonants-one-digit
                                            (mod remainder 21)))
                           result))
      (setq remainder (/ remainder 21)))
    (setq length (max 1 (or length 1)))
    (if (< (length result))
        (string-pad result length ?b t)
      result)))

(defun my-int-to-consonants-one-digit (integer)
  "Convert INTEGER between 0 and 20 into one non-vowel letter."
  ;; A b c d E f g h I j k l m n O p q r s t U v w x y z
  ;; bcdfghjklmnpqrstvwxyz
  ;; if you start counting from b, E would've been 4th char
  (cond
   ((< integer 3) (+ ?b integer))
   ((< integer 6) (+ ?f integer -3))
   ((< integer 11) (+ ?j integer -6))
   ((< integer 16) (+ ?p integer -11))
   ((< integer 21) (+ ?v integer -16))
   (t (error "Input was larger than 20"))))

(defun my-uuid-to-pageid-old-v2 (uuid)
  (substring (my-uuid-to-base62 uuid) -4))

;;(org-id-int-to-b36 3453453452312)
(defun my-uuid-to-base62 (uuid)
  (let ((decimal (string-to-number (string-replace "-" "" uuid) 16)))
    (if (or (= 0 decimal) (/= 36 (length uuid)))
        (error "String should only contain a valid UUID 36 chars long: %s" uuid)
      ;; The highest UUID (ffffffff-ffff-ffff-ffff-ffffffffffff) makes
      ;; a base62 string 22 chars long.  Let's always return 22 chars.
      (my-int-to-base62 decimal 22))))

(defun my-int-to-base62 (integer &optional length)
  "Convert an INTEGER to a base-62 number represented as a string.
If LENGTH is given, pad the string with leading zeroes as needed
so the result is always that long or longer."
  (let ((s "")
        (i integer))
    (while (> i 0)
      (setq s (concat (char-to-string
                       (my-int-to-base62-one-digit (mod i 62))) s)
            i (/ i 62)))
    (setq length (max 1 (or length 1)))
    (if (< (length s) length)
        (setq s (concat (make-string (- length (length s)) ?0) s)))
    s))

;; Workhorse for `my-int-to-base62'
(defun my-int-to-base62-one-digit (integer)
  "Convert INTEGER between 0 and 61 into one character 0..9, a..z, A..Z."
  ;; Uses chars ?0, ?A, ?a off the ASCII table.  Evaluate those symbols and you
  ;; see important gaps between the character sets:
  ;; 0-9 has codes 48 thru 57
  ;; A-Z has codes 65 thru 90
  ;; a-z has codes 97 thru 122
  ;; Why compose chars to construct the final base62 string?  It's either
  ;; that, or you make a lookup string "0123456789abcdefg...", so you're
  ;; looking something up anyway.  The ASCII table is faster.
  (cond
   ((< integer 10) (+ ?0 integer))
   ((< integer 36) (+ ?a integer -10))
   ((< integer 62) (+ ?A integer -36))
   (t (error "Input was larger than 61"))))

(defvar my-ids nil
  "Database for checking ID collisions.")

(defun my-check-id-collisions ()
  (interactive)
  (cl-loop
   with found = nil
   for id-uuids in my-ids
   as uuids = (-distinct (cdr id-uuids))
   when (> (length uuids) 1)
   do (progn (setq found t)
             (message "These uuids make same page-id: %s"
                      uuids))
   finally do (unless found (message "All uuids unique"))))

(defun my-add-backlinks (&rest _)
  "Add a \"What links here\" subtree at the end.
Meant to run on `org-export-before-parsing-functions', where it
will not modify the source file.

Can probably be tested in a real org buffer... it never occurred
to me to do that."
  (let ((this-node (ignore-errors (org-roam-node-at-point)))
        (linked-nodes nil))
    (when this-node
      (dolist (obj (org-roam-backlinks-get this-node :unique t))
        (let ((node (org-roam-backlink-source-node obj)))
          (unless (member node linked-nodes)
            (push node linked-nodes))))
      (dolist (obj (org-roam-reflinks-get this-node))
        (let ((node (org-roam-reflink-source-node obj)))
          (unless (equal node this-node)
            (unless (member node linked-nodes)
              (push node linked-nodes)))))
      (when linked-nodes
        (save-excursion
          (if (bobp)
              (progn
                (goto-char (point-max))
                (insert "\n* What links here  :backlinks:")
                ;; If this page is a pseudo-tag such as #emacs, make it clear
                ;; that the backlinks can be used by link aggregators.  IDK
                ;; if something like Planet Emacslife is looking for a
                ;; programmatic marker or if they just do things manually.
                (when (string-prefix-p "#" (org-roam-node-title this-node))
                  (insert "\n (Sorted by recent first)")))
            (org-insert-subheading nil)
            (insert "What links here"))
          ;; sort by creation: newest on top
          (let ((sorted-nodes
                 (--sort (string-lessp
                          (map-elt (org-roam-node-properties other) "CREATED")
                          (map-elt (org-roam-node-properties it) "CREATED"))
                         linked-nodes)))
            (dolist (node sorted-nodes)
              (newline)
              (insert
               "- [[id:"
               (org-roam-node-id node)
               "]["
               (replace-regexp-in-string "[][]" "" (org-roam-node-title node))
               "]]"))))))))

(defun my-replace-web-links-with-ref-note-links (&rest _)
  "For every URL found in this page, if there exists an Org-roam
note elsewhere with the same exact URL in its :ROAM_REFS:
property, then replace that URL in this page with an id-link to
that Org-roam note.

This visitor can still go find the original web link because I
will expose it inside that note.  This just makes the visitor see
my note first.

Meant to run on `org-export-before-parsing-functions', where it
will not modify the source file."
  (when (ignore-errors (org-roam-node-at-point))
    (save-excursion
      ;; TODO: use org-link-any-re
      (while (not (equal "No further link found" (quiet! (org-next-link))))
        (let* ((elem (org-element-context))
               (link (org-element-property :path elem))
               (ref (assoc link my-refs-cache))
               (end (org-element-property :end elem)))
          (when (and ref
                     ;; ignore if referring to same page we're on
                     (not (equal (caddr ref) (org-get-title))))
            ;; REVIEW: check if the link is already an org-link with a custom
            ;; description.  Then just modify the url part into an id link,
            ;; don't overwrite the description also.
            (if (search-forward "][" end t)
                ;; has a custom description
                (progn
                  (delete-region (match-beginning 0) (point))
                  (insert "[[id:" (cadr ref) "]["))
              (delete-region (point) end)
              (insert "[[id:" (cadr ref) "]["
                      (replace-regexp-in-string (rx (any "[]")) "" (caddr ref))
                      "]]"))))))))

(defun my-replace-datestamps-with-links (&rest _)
  (when (ignore-errors (org-roam-node-at-point))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (rx "[" (group (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)) "]") nil t)
        (let ((beg (match-beginning 0))
              (end (match-end 0))
              (datestamp (substring-no-properties (match-string 1))))
          ;; check that it is not already a link
          (unless (org-element-property :path (org-element-context))
            ;; check that it isn't a property or comment line
            (unless (save-excursion
                      (goto-char (line-beginning-position))
                      (looking-at-p "[[:space:]]*?[:#]"))
              ;; check that a daily-page exists
              (when-let ((daily-id (org-roam-db-query
                                    `[:select [id]
                                      :from nodes
                                      :where (like title ,datestamp)])))
                (delete-region beg end)
                (goto-char beg)
                (let ((fancy (format-time-string
                              (car org-timestamp-custom-formats)
                              (date-to-time datestamp))))
                  (insert (concat "[[id:" (caar daily-id) "][<" fancy ">]]")))))))))))

(defun my-strip-hashlink-if-same-as-permalink (link)
  (if-let* ((hash-pos (string-search "#" link))
            (hash-part (substring link (1+ hash-pos)))
            (base-part (substring link 0 hash-pos))
            (same (string-search hash-part base-part)))
      ;; Cut off the hash
      base-part
    ;; Keep the whole link
    link))

(defun my-generate-todo-log (src path)
  "Generate a log of completed tasks using `org-agenda-write'.
Wrap the HTML output in an Org file that has a HTML export block."
  (cl-letf ((org-agenda-files (list src))
            (org-agenda-span 'fortnight)
            (org-agenda-prefix-format
             '((agenda . " %i %?-12t") (todo . "") (tags . "") (search . "")))
            (org-agenda-show-inherited-tags nil))

    ;; (setopt org-agenda-files '("/home/kept/roam/noagenda/archive.org"))
    ;; (setopt org-agenda-span 'fortnight)
    ;; (setopt org-agenda-prefix-format '((agenda . " %i %?-12t") (todo . "") (tags . "") (search . "")))
    ;; (setopt org-agenda-show-inherited-tags nil)

    (org-agenda-list)
    (org-agenda-log-mode)
    (org-agenda-archives-mode)
    (shell-command "rm /tmp/roam/todo-log-now.html")
    (org-agenda-write "/tmp/roam/todo-log-now.html")
    (org-agenda-earlier 1)
    (shell-command "rm /tmp/roam/todo-log-last-week.html")
    (org-agenda-write "/tmp/roam/todo-log-last-week.html")
    (org-agenda-quit)
    ;; (delete-other-windows)
    ;; (view-echo-area-messages)
    (with-current-buffer (or (find-buffer-visiting path)
                             (find-file path))
      (delete-region (point-min) (point-max))
      (insert ":PROPERTIES:"
              "\n:ID: e4c5ea8b-5b06-43c4-8948-3bfe84e8d5e8"
              "\n:CREATED:  " (format-time-string "[%F]")
              "\n:END:"
              "\n#+title: Completed tasks"
              "\n#+filetags: :fren:"
              "\n#+date: "
              "\n#+begin_export html"
              "\n")
      (insert-file-contents "/tmp/roam/todo-log-last-week.html")
      (delete-region (point) (search-forward "<pre>"))
      (insert "<pre class=\"agenda\">")
      (forward-line)
      (delete-region (1- (line-beginning-position)) (line-end-position))
      (search-forward "</pre>")
      (delete-region (1- (line-beginning-position)) (point-max))
      (insert-file-contents "/tmp/roam/todo-log-now.html")
      (delete-region (point) (search-forward "<pre>"))
      (forward-line)
      (delete-region (1- (line-beginning-position)) (line-end-position))
      (delete-region (search-forward "</pre>") (point-max))
      (insert "\n#+end_export")
      ;; remove special face for today (css class "org-agenda-date-weekend-today")
      (goto-char (point-min))
      (search-forward "-today")
      (replace-match "")
      (save-buffer))))

(defun my-make-atom-feed (path entries-dir)
  (when (file-exists-p path)
    (move-file-to-trash path))
  (with-temp-file path
    (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
<title>Martin Edström</title>
<link href=\"https://edstrom.dev\"/>
<updated>" (format-time-string "%FT%TZ") "</updated>
<author><name>Martin Edström</name></author>
<rights> © 2023-" (format-time-string "%Y") " Martin Edström </rights>
<id>https://edstrom.dev</id>")
    (dolist (entry (directory-files entries-dir t "[[:alpha:]]"))
      (insert-file-contents entry))
    (goto-char (point-max))
    (insert "
</feed>")))

(defun my-fail? (value problem)
  "Like `cl-assert', but print PROBLEM and buffer filename."
  (unless value
    (error "%s" (concat problem (format " in %s:%d"
                                        (buffer-file-name)
                                        (line-number-at-pos))))))


(defun my-assert-no-misplaced-syntax (str)
  (not
   (or
    ;; TODO Scan thru all the body text for line that looks like ^:end:$ but is
    ;; "off by one".

    ;; Match ^:end: that has more text on the same line
    (and (goto-char (point-min))
         (re-search-forward (concat "^" str) nil t)
         (looking-at-p "."))
    ;; Match :end:$ that isn't on its own line
    (and (goto-char (point-min))
         (re-search-forward (concat str "$") nil t)
         (goto-char (match-beginning 0))
         (looking-back "[^ \n]")))))

(defun my-validate-org-buffer ()
  (interactive)
  (let ((file (buffer-file-name)))
    ;; Look for wrong amounts of brackets
    (goto-char (point-min))
    (while-let ((pos (search-forward "[[id:" nil t)))
      (when (looking-back (rx (literal "[[[id:")))
        (warn "triple brackets at %s:%d") file (line-number-at-pos))
      (unless (org-uuidgen-p (buffer-substring pos (1- (search-forward "]"))))
        (warn "not proper UUID at %s:%d" file (line-number-at-pos)))
      (goto-char pos)
      (unless (re-search-forward (rx (*? (not (any "[]"))) "]["
                                     (*? (not (any "[]"))) "]]")
                                 nil t)
        (warn "weird brackets at %s:%d" file (line-number-at-pos))))
    (goto-char (point-min))
    (while (re-search-forward org-link-any-re nil t)
      (unless (string-match-p
               "\\(?:id:\\|http://\\|https://\\|file:\\|ftp://\\|info:\\)"
               (match-string 0))
        (warn "disallowed link type at %s:%d" file (line-number-at-pos))))
    (my-fail? (my-assert-no-misplaced-syntax ":end:") "Possible mistake")
    (my-fail? (my-assert-no-misplaced-syntax "#+end_src") "Possible mistake")
    (my-fail? (my-assert-no-misplaced-syntax "#+end_quote") "Possible mistake")
    ;; check file-level metadata
    (goto-char (point-min))
    (my-validate-org-entry)
    ;; ensure the entire tags-value is correctly wrapped in colons
    (cl-assert (progn (re-search-forward (rx bol "#+filetags:"))
                      (re-search-forward " +:.+?:$" (line-end-position))))
    ;; check metadata of each subtree that is also a roam node
    (goto-char (point-min))
    (while (not (eobp))
      (org-next-visible-heading 1)
      (when (org-id-get)
        (my-validate-org-entry)))))

(defun my-validate-org-entry ()
  "Validate entry at point, or file-level metadata if point is
 not under a heading."
  (let ((file-level-entry (not (org-get-heading)))
        (id (org-id-get))
        (title (or (org-get-heading) (org-get-title)))
        (created (org-entry-get nil "CREATED"))
        (tags (org-get-tags)))
    (cl-flet ((my-fail? (value problem)
                (unless value (error (concat problem " in %s:%d")
                                     (buffer-file-name)
                                     (line-number-at-pos)))))
      (my-fail? id "No id")
      (my-fail? (org-uuidgen-p id) "Org-id is not an UUID")
      (my-fail? title "No title")
      (my-fail? created "No CREATED property")
      (my-fail? tags "No tags")
      (let ((case-fold-search nil))
        (my-fail? (not (string-match-p "[[:upper:]]" (string-join tags)))
                  "Uppercase in tag found"))
      (when (and created (not (string-blank-p created)))
        (my-fail? (my-iso-datestamp-p (substring created 1 -1))
                  "Property CREATED is not proper datestamp"))
      ;; no links (or even datestamps) in headings
      ;; (cl-fail (not (string-prefix-p "[" title)))
      ;; (cl-fail (not (string-suffix-p "]" title)))
      ;; no links in headings
      (my-fail? (not (string-search "[[" title))
                "Link in heading")
      (let ((filetag-line (save-excursion
                            (goto-char (point-min))
                            (search-forward "#+filetags")
                            (buffer-substring (line-beginning-position) (line-end-position)))))
        (when file-level-entry
          (setq title filetag-line))
        ;; try to catch broken tags like "noexport:"
        (my-fail? (not (string-match-p (rx " " alnum (+? (not " ")) ":" eol)
                                       title))
                  "Possible broken tag")
        ;; try to catch broken tags like ":noexport"
        (my-fail? (not (string-match-p (rx " :" (+? (not " ")) (not ":") (*? space) eol)
                                       title))
                  "Possible broken tag")))))

;; (defun my-validate-org-entry-tags ()
;;   (if (org-before-first-heading-p)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (cl-assert (progn (re-search-forward (rx bol "#+filetags:"))
;;                           (re-search-forward " +:.*?:$" (line-end-position)))))
;;     (save-excursion
;;       (unless (org-at-heading-p)
;;         (org-previous-visible-heading 1))
;;       (let ((pos (goto-char (line-beginning-position))))
;;         (and (re-search-forward " +:.*:$" (line-end-position)))))))
