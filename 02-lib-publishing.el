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
                (insert "\n* What links here")
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
              (insert
               "\n- [[id:"
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

;; FIXME The first "body text" could be a #+begin_quote or #+TOC, which gets
;; skipped too.  It could also be the beginning of a :drawer:.
(defvar my-org-text-line-re "^[ \t]*[^#:\n]"
  "Regexp to match a line that isn't a comment, a keyword or a property drawer.
Useful for jumping past a file's front matter.")

(defun my-add-refs-as-paragraphs (&rest _)
  "Print out the roam-ref under each heading that has one."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward my-org-text-line-re nil t)
      ;; Workaround inexact regexp
      (forward-line -1)
      (unless (or (looking-at-p "#\\+toc") (looking-at-p "#\\+begin_"))
        (forward-line 1))
      (open-line 2)
      (while (progn
               (when-let ((refs (org-entry-get nil "roam_refs")))
                 (while (progn
                          (forward-line 1)
                          (or (looking-at-p "^[ \t]*:") (eobp))))
                 (insert "\nGoing off " refs "\n\n"))
               (outline-next-heading)
               (not (eobp)))))))

(defun my-ensure-section-containers (&rest _)
  "Like setting `org-html-container-element' to \"section\",
but apply to all subheadings, not only the top level."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward org-element-headline-re nil t)
      (while (not (eobp))
        (org-entry-put nil "HTML_CONTAINER" "section")
        (outline-next-heading)))))

(defun my-strip-inline-anki-ids (&rest _)
  "Clean the little inline-anki superscript numbers."
  (save-excursion
    (while (re-search-forward (rx (? "@") "^{" (= 13 digit) "}") nil t)
      (replace-match "")))
  ;; (require 'inline-anki)
  ;; (save-excursion
  ;;   (dolist (re (list inline-anki-rx:eol
  ;;                     inline-anki-rx:eol-new
  ;;                     inline-anki-rx:item-start
  ;;                     inline-anki-rx:item-start-new))
  ;;     (goto-char (point-min))
  ;;     (while (re-search-forward re nil t)
  ;;       (let ((beg (match-beginning 0))
  ;;             (end (point)))
  ;;         (if (and (search-backward "@" (line-beginning-position) t)
  ;;                  (> 18 (- end (point))))
  ;;             (delete-region (point) end)
  ;;           (delete-region beg end))))))
  )

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
  "Generate a new Org file showcasing recent completed TODOs."
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
    (find-file path)
    ;; with-current-buffer (or (find-buffer-visiting path)
    ;;                         )
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
    ;; remove special face for today (css .org-agenda-date-weekend-today)
    (goto-char (point-min))
    (search-forward "-today")
    (replace-match "")
    (save-buffer)
    (kill-buffer))
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
    ;; (dolist (entry (directory-files entries-dir t "[[:alpha:]]"))
    (dolist (entry (directory-files entries-dir t))
      (insert-file-contents entry))
    ;; (goto-char (point-max))
    (insert "
</feed>")))

(defun my-make-atom-entry (post uuid)
  (let-alist (kvplist->alist post)
    (let ((content-for-feed
           (with-temp-buffer
             (buffer-disable-undo)
             (insert .content)
             (goto-char (point-min))
             ;; De-linkify links to non-public URLs
             (let* ((forbidden (regexp-opt (append my-tags-to-avoid-uploading
                                                   my-tags-for-hiding)))
                    (re (rx "<a " (*? nonl) "class=\"" (*? nonl)
                            (regexp forbidden)
                            (*? nonl) ">" (group (*? anychar)) "</a>")))
               (while (re-search-forward re nil t)
                 (replace-match (match-string 1)))
               (buffer-string)))))
      (concat
       "\n<entry>"
       "\n<title>" .title "</title>"
       "\n<link href=\"" "https://edstrom.dev/" .pageid "/" .slug "\" />"
       "\n<id>urn:uuid:" uuid "</id>"
       "\n<published>" .created "T12:00:00Z</published>"
       (if .updated
           (concat "\n<updated>" .updated "T12:00:00Z</updated>")
         "")
       ;; With type="xhtml", we don't have to entity-escape unicode
       ;; (https://validator.w3.org/feed/docs/atom.html#text)
       "\n<content type=\"xhtml\">"
       "\n<div xmlns=\"http://www.w3.org/1999/xhtml\">\n"
       content-for-feed
       "\n</div>"
       "\n</content>"
       "\n</entry>"))))


;;; Validator




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
