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

(defun my-uuid-to-short (uuid)
  (let* ((hexa (string-trim (string-replace "-" "" uuid)))
         (decimal (string-to-number hexa 16)))
    (if (or (= 0 decimal) (/= 32 (length hexa)))
        (error "String should be a valid UUID 36 chars long: %s" uuid)
      (substring (my-int-to-consonants decimal 5) -5))))

(defun my-int-to-consonants (integer &optional length)
  "Convert INTEGER to a base-21 number represented as non-vowel letters."
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

(defvar my-ids (make-hash-table :size 4000 :test #'equal)
  "Database for checking ID collisions.")

(defun my-check-id-collisions ()
  (cl-loop for v being the hash-values of my-ids
           as uuids = (-distinct v)
           when (> (length uuids) 1)
           do (warn "These uuids make same page-id: %s" uuids)))

(defun my-add-backlinks (&rest _)
  "Add a \"What links here\" subtree at the end.
Designed for `org-export-before-parsing-functions', where it
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


;; FIXME The first body text could be a #+begin_quote or #+TOC, which gets
;; skipped too.  It could also be the beginning of an :aside:.
(defvar my-org-text-line-re "^[ \t]*[^#:\n]"
  "Regexp to match a line that isn't a comment, a keyword or a property drawer.
Useful for jumping past a file's front matter.")

(defun my-add-refs-as-paragraphs (&rest _)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward my-org-text-line-re nil t)
      ;; (goto-char (line-beginning-position))
      (forward-line -1)
      (unless (or (looking-at-p "#\\+toc") (looking-at-p "#\\+begin_"))
        (forward-line 1))
      (open-line 2)
      (while (progn
               (when-let ((refs (org-entry-get nil "roam_refs")))
                 (while (progn
                          (forward-line 1)
                          (or (looking-at-p "^[ \t]*:") (eobp))))
                 ;; wrap in <div class="ref">
                 ;; (insert "\n#+begin_ref\nSource " refs "\n#+end_ref\n\n")
                 ;; (insert "\n" refs "\n\n")
                 (insert "\nGoing off " refs "\n\n"))
               (org-next-visible-heading 1)
               (not (eobp)))))))

(defun my-ensure-section-containers (&rest _)
  "Like setting `org-html-container-element' to \"section\",
but apply to all subheadings, not only the top level."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward org-element-headline-re nil t)
      (while (not (eobp))
        ;; Testing to add role=

        (org-entry-put nil "HTML_CONTAINER" "section")
        (outline-next-heading)))))

(defun my-strip-inline-anki-ids (&rest _)
  (require 'inline-anki)
  (save-excursion
    (dolist (re (list inline-anki-rx:eol
                      inline-anki-rx:eol-new
                      inline-anki-rx:item-start
                      inline-anki-rx:item-start-new))
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (let ((beg (match-beginning 0))
              (end (point)))
          (if (and (search-backward "@" (line-beginning-position) t)
                   (> 18 (- end (point))))
              (delete-region (point) end)
            (delete-region beg end)))))))

(defun my-strip-hash-if-matches-base (link)
  "Remove the hash-part of the link (i.e. the bit after the #
character in domain.com/PAGE-ID/slug#HEADING-ID) if the HEADING-ID
matches PAGE-ID anyway (i.e. it's a file-level id)"
  (let* ((splits (split-string link "#"))
         (base (car splits))
         (hash (cadr splits)))
    (if (and hash (string-search hash base))
        base
      link)))

(defun my-generate-todo-log (path)
  "Generate a log of completed tasks using `org-agenda-write'.
Wrap that function's HTML output in an Org file that has an HTML
export block."
  (let ((org-agenda-span 'fortnight)
        (org-agenda-prefix-format
         '((agenda . " %i %?-12t") (todo . "") (tags . "") (search . "")))
        (org-agenda-show-inherited-tags nil))
    (org-agenda-list)
    (org-agenda-log-mode)
    (org-agenda-archives-mode)
    (shell-command "rm /tmp/roam/todo-log-now.html")
    (org-agenda-write "/tmp/roam/todo-log-now.html")
    (org-agenda-earlier 1)
    (shell-command "rm /tmp/roam/todo-log-last-week.html")
    (org-agenda-write "/tmp/roam/todo-log-last-week.html")
    (org-agenda-quit)
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
      (save-buffer)
      (kill-buffer)))
  (view-echo-area-messages))

(defun my-compile-atom-feed (path entries-dir)
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

(defun my-make-atom-entry (post uuid)
  (let-alist post
    (let ((content-for-feed
           (with-temp-buffer
             (buffer-disable-undo)
             (insert .content)
             (goto-char (point-min))
             (let* ((locked (regexp-opt (append my-tags-to-avoid-uploading
                                                my-tags-for-hiding)))
                    (re (rx "<a " (*? nonl) "class=\"" (*? nonl)
                            (regexp locked)
                            (*? nonl) ">" (group (*? anychar)) "</a>")))
               (while (re-search-forward re nil t)
                 (replace-match (match-string 1)))
               (buffer-string)))))
      (concat "\n<entry>"
              "\n<title>" .title "</title>"
              "\n<link href=\""
              "https://edstrom.dev/" .pageid "/" .slug
              "\" />"
              "\n<id>urn:uuid:" uuid "</id>"
              "\n<published>" .created "T12:00:00Z</published>"
              (if .updated
                  (concat "\n<updated>" .updated "T12:00:00Z</updated>")
                "")
              ;; This type="xhtml" lets us skip entity-escaping unicode
              ;; https://validator.w3.org/feed/docs/atom.html#text
              "\n<content type=\"xhtml\">"
              "\n<div xmlns=\"http://www.w3.org/1999/xhtml\">\n"
              ;; FYI, never pretty-print html, indentation ruins <pre>!
              content-for-feed
              "\n</div>"
              "\n</content>"
              "\n</entry>"))))

(defun my-find-misplaced-syntax (str)
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
        (looking-back "[^ \n]"))))

(defun my-locate-warn (problem)
  (warn "%s in %s:%d" problem (buffer-file-name) (line-number-at-pos)))

(defun my-locate-err (problem)
  (error "%s in %s:%d" problem (buffer-file-name) (line-number-at-pos)))

(defun my-validate-org-buffer ()
  (interactive)
  (let ((file (buffer-file-name))
        (bufdata (list `((buftags . ,(-flatten (org-get-buffer-tags)))))))
    ;; Look for wrong amounts of brackets
    (goto-char (point-min))
    (while-let ((pos (search-forward "[[id:" nil t)))
      (when (looking-back (rx (literal "[[[id:")))
        (my-locate-warn "triple brackets"))
      (unless (org-uuidgen-p (buffer-substring pos (1- (search-forward "]"))))
        (my-locate-warn "not proper UUID"))
      (goto-char pos)
      (unless (re-search-forward (rx (*? (not (any "[]"))) "]["
                                     (*? (not (any "[]"))) "]]")
                                 nil t)
        (my-locate-warn "weird brackets")))
    (goto-char (point-min))
    (while (re-search-forward org-link-any-re nil t)
      (unless (string-match-p
               "\\(?:id:\\|http://\\|https://\\|file:\\|ftp://\\|info:\\)"
               (match-string 0))
        (my-locate-warn "disallowed link type")))
    (when (my-find-misplaced-syntax ":end:") (my-locate-err "Possible mistake"))
    (when (my-find-misplaced-syntax "#\\+end_src") (my-locate-err "Possible mistake"))
    (when (my-find-misplaced-syntax "#\\+end_quote") (my-locate-err "Possible mistake"))
    ;; check file-level metadata
    (goto-char (point-min))
    (my-validate-org-entry bufdata)
    ;; check each subtree that is its own roam node
    (goto-char (point-min))
    (while (not (eobp))
      (org-next-visible-heading 1)
      (when (org-id-get)
        (my-validate-org-entry bufdata)))
    (goto-char (point-min))
    (unless (progn (re-search-forward (rx bol "#+filetags:"))
                   (re-search-forward " +:.+?:$" (line-end-position)))
      (my-locate-err "Tags may not be correctly wrapped in colons"))
    (unless (-intersection (org-get-tags) (append '("pub")
                                                  my-tags-for-hiding
                                                  my-tags-to-avoid-uploading))
      (my-locate-err "No tag that indicates publishability"))))

(defun my-validate-org-entry (bufdata)
  "Validate entry at point, or file-level metadata if point is
 not under a heading."
  (let ((file-level-entry (not (org-get-heading)))
        (id (org-id-get))
        (title (or (org-get-heading) (org-get-title)))
        (created (org-entry-get nil "created"))
        (refs (org-entry-get nil "roam_refs"))
        (tags (org-get-tags))
        (buftags (alist-get 'buftags bufdata)))
    (unless id (my-locate-err "No id"))
    (unless (org-uuidgen-p id) (my-locate-err "Org-id is not an UUID"))
    (unless title (my-locate-err "No title"))
    (unless created (my-locate-err "No CREATED property"))
    (unless tags (my-locate-err "No tags"))
    (let ((case-fold-search nil))
      (unless (not (string-match-p "[[:upper:]]" (string-join tags)))
        (my-locate-err "Uppercase in tag found")))
    (dolist (tag tags)
      (when (--any-p (and (not (= (length it) (length tag)))
                          (or (string-search it tag)
                              (string-search tag it)))
                     buftags)
        (my-locate-err "A tag is a substring of another tag (lost colon?)")))
    (when (and created (not (string-blank-p created)))
      (unless (my-iso-datestamp-p (substring created 1 -1))
        (my-locate-err "Property CREATED is not proper datestamp")))
    (when (and refs (string-search "\"" refs))
      (my-locate-err "Quote-sign in roam_refs"))
    (let ((filetag-line (save-excursion
                          (goto-char (point-min))
                          (search-forward "#+filetags")
                          (buffer-substring (line-beginning-position) (line-end-position)))))
      (when file-level-entry
        (setq title filetag-line))
      ;; try to catch broken tags like "noexport:"
      (when (string-match-p (rx " " alnum (+? (not " ")) ":" eol)
                            title)
        (my-locate-err "Possible broken tag"))
      ;; try to catch broken tags like ":noexport"
      (when (string-match-p (rx " :" (+? (not " ")) (not ":") (*? space) eol)
                            title)
        (my-locate-err "Possible broken tag")))))



;;; Patches

;; bugfix (C-s xml-escape-string)
(require 'dom)
(defun dom-print (dom &optional pretty xml)
  "Print DOM at point as HTML/XML.
If PRETTY, indent the HTML/XML logically.
If XML, generate XML instead of HTML."
  (let ((column (current-column)))
    (insert (format "<%s" (dom-tag dom)))
    (let ((attr (dom-attributes dom)))
      (dolist (elem attr)
	;; In HTML, these are boolean attributes that should not have
	;; an = value.
	(insert (if (and (memq (car elem)
			       '(async autofocus autoplay checked
			         contenteditable controls default
			         defer disabled formNoValidate frameborder
			         hidden ismap itemscope loop
			         multiple muted nomodule novalidate open
			         readonly required reversed
			         scoped selected typemustmatch))
			 (cdr elem)
			 (not xml))
		    (format " %s" (car elem))
		  (format " %s=\"%s\"" (car elem)
	                  (url-insert-entities-in-string (cdr elem)))))))
    (let* ((children (dom-children dom))
	   (non-text nil))
      (if (null children)
	  (insert " />")
	(insert ">")
        (dolist (child children)
	  (if (stringp child)
	      (insert (xml-escape-string child))
	    (setq non-text t)
	    (when pretty
              (insert "\n" (make-string (+ column 2) ?\s)))
	    (dom-print child pretty xml)))
	;; If we inserted non-text child nodes, or a text node that
	;; ends with a newline, then we indent the end tag.
        (when (and pretty
		   (or (bolp)
		       non-text))
	  (unless (bolp)
            (insert "\n"))
	  (insert (make-string column ?\s)))
        (insert (format "</%s>" (dom-tag dom)))))))
