;; -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Martin Edström
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

(defvar my-outdated-tags '("partner" "friends-eyes" "therapist"))
(defvar my-tags-to-avoid-uploading (append my-outdated-tags '("noexport" "private" "censor" "drill" "fc" "anki")))

(defun my-publish (&optional prefix)
  "A single command I can use in a child emacs."
  (interactive "p")
  (require 'ox-publish)
  (switch-to-buffer "*Messages*") ;; for watching it work
  (delete-other-windows)
  (cd "/home/kept/roam") ;; for me to quick-search when an id fails to resolve
  (org-publish "my-react-blog" (>= prefix 4)))

(setopt org-publish-project-alist
        `(("my-react-blog"
           :base-directory "/tmp/roam/"
           :publishing-directory "/home/kept/pub/posts/"
           :publishing-function my-publish-to-blog
           :preparation-function my-prep-fn
           :recursive t
           :body-only t
           :with-toc nil
           :section-numbers nil
           ;; NOTE: this works only for subtrees, so we also check at file-level
           ;; in `my-publish-to-blog'.
           :exclude-tags ,my-tags-to-avoid-uploading)))

;; Give h2...h6 headings an ID from the source org-id, if it has one,
;; instead of e.g. "org953031".  That way, hash-links such as
;; #ID-e10bbdfe-1ffb-4c54-9228-2818afdfc5ba will make the web browser jump to
;; that heading.
(after! ox (require 'org-roam-export))

(add-hook 'org-export-before-parsing-functions #'my-add-backlinks)
(add-hook 'org-export-before-parsing-functions #'my-replace-web-links-with-ref-note-links)

;; TODO: In dailies, insert links to any pages created on that same day, under a "Created pages" heading

(defun my-add-backlinks (&rest _)
  "Add a \"What links here\" subtree at the end.
Meant to run on `org-export-before-parsing-functions', where it
will not modify the source file."
  (let ((this-node (ignore-errors (org-roam-node-at-point)))
        (backlinks nil)
        (reflinks nil))
    (when this-node
      (dolist (obj (org-roam-backlinks-get this-node :unique t))
        (let ((node (org-roam-backlink-source-node obj)))
          (cl-pushnew (cons (org-roam-node-id node)
                            (org-roam-node-title node))
                      backlinks)))
      (dolist (obj (org-roam-reflinks-get this-node))
        (let ((node (org-roam-reflink-source-node obj)))
          (unless (equal node this-node)
            (cl-pushnew (cons (org-roam-node-id node)
                              (org-roam-node-title node))
                        reflinks))))
      (when (or backlinks reflinks)
        (save-excursion 
          (if (bobp)
              (progn
                (goto-char (point-max))
                (insert "\n* What links here"))
            (org-insert-subheading nil)
            (insert "What links here"))
          (dolist (link (append backlinks reflinks))
            (newline)
            (insert "- [[id:" (car link) "]["
                    (replace-regexp-in-string (rx (any "[]")) "" (cdr link))
                    "]]")))))))

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
    (let ((all-refs (org-roam-db-query
                     [:select [ref id title]
                      :from refs
                      :left-join nodes
                      :on (= refs:node-id nodes:id)])))
      (save-excursion
        (while (not (equal "No further link found" (quiet! (org-next-link))))
          (let* ((elem (org-element-context))
                 (link (org-element-property :path elem))
                 (ref (assoc link all-refs)))
            (when (and ref
                       ;; ignore if on same page
                       (not (equal (caddr ref) (org-get-title))))
              (delete-region (point) (org-element-property :end elem))
              (insert "[[id:" (cadr ref) "]["
                      (replace-regexp-in-string (rx (any "[]")) "" (caddr ref))
                      "]]"))))))))

(defvar my-publish-ran-already nil)
(defun my-prep-fn (_)
  "Prepare Emacs for publishing my website.
Since I intend to run `org-publish' in a subordinate Emacs, this
function is where I can make destructive env changes that I don't
want in my main Emacs.

A biggie: I'd like the final URLs on my website to exclude the
date slug in the org-roam filenames.  So prepare to work from a
/tmp/roam/ copy of the org-roam dir, with the dates stripped from
filenames, and the `org-id-locations' table modified likewise, so
that org-id links will resolve correctly."
  (setopt org-export-with-drawers '(not "LOGBOOK" "logbook" "NOEXPORT" "noexport"))
  (setopt org-mode-hook nil) ;; speeds up publishing
  (setopt org-export-use-babel nil)
  (setopt org-export-with-broken-links nil) ;; links would disappear, error instead
  (setopt org-export-with-smart-quotes nil)
  (setopt org-html-checkbox-type 'unicode)
  (setopt org-html-html5-fancy t)
  (setopt case-fold-search t) ;; for all the searches in `my-publish-to-blog'
  (setopt org-inhibit-startup t) ;; from org-publish-org-to
  (setopt org-html-extension "")
   ;; I'd love it, but doesn't apply to every datestamp on the site; inconsistent
  (setopt org-display-custom-times nil)

  ;; Ensure that this subordinate emacs syncs nothing to disk
  (cancel-timer my-state-sync-timer)
  (setopt kill-emacs-hook nil)
  (fset 'org-id-locations-save #'ignore)
  (setopt org-roam-db-location "/tmp/org-roam.db")
  (org-roam-db-autosync-mode 0)

  ;; Duplicate the files to /tmp to work from there
  (shell-command "rm -rf /tmp/roam/")
  (copy-directory "/home/kept/roam/" "/tmp/" t)
  (shell-command "rm -rf /tmp/roam/*/logseq/") ;; no logseq backups
  (shell-command "rm -rf /tmp/roam/lesswrong-org/*") ;; no autogenerated stubs
  ;; (shell-command "rm -rf /tmp/roam/lesswrong-org/*/") ;; no autogenerated stubs
  ;; Strip YYYY-MM-DD- from filenames
  (cl-loop
   for file in (directory-files-recursively
                "/tmp/roam"
                (rx bol (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "-" (+ nonl) ".org" eol)
                nil)
   do (rename-file file
                   (concat (file-name-directory file)
                           (substring (file-name-nondirectory file) 11))))

  ;; Flatten the directory tree (no subdirs)
  (cl-loop
   for file in (directory-files-recursively "/tmp/roam" "\\.org$" nil)
   unless (equal (file-name-directory file) "/tmp/roam/")
   do (if (file-exists-p (concat "/tmp/roam/" (file-name-nondirectory file)))
          (progn
            (message "FILE ALREADY EXISTS: %s" (file-name-nondirectory file))
            (delete-file file))
        (rename-file file "/tmp/roam/")))


  ;; Tell `org-id-locations' and the org-roam DB about the duplicate directory.
  (setopt org-roam-directory "/tmp/roam/")
  (setopt org-agenda-files '("/tmp/roam/"))
  (unless my-publish-ran-already
    (org-roam-update-org-id-locations)
    (org-roam-db-sync)
    (setq my-publish-ran-already t))
  (fset 'org-id-update-id-locations #'ignore) ;; stop triggering during publish

  )

;; Struggled so long looking for a hook that would work like the old
;; before-export-hook.  Let this be a lesson: the emacs hook system exists to
;; let you subtly modify a function IN THE MIDDLE of its body. We never actually
;; need before-hooks (or after-hooks), since in such a simple case it is always
;; possible to use `add-function' or call a wrapper such as this one around
;; `org-publish-org-to'.
(defun my-publish-to-blog (plist filename pub-dir)
  (with-current-buffer (or (find-buffer-visiting filename)
                           (find-file-noselect filename))
    (goto-char (point-min))
    (let* ((title (save-excursion
                   (when (search-forward "#+title: " nil t)
                     (buffer-substring (point) (line-end-position)))))
          (created (save-excursion
                     (when (search-forward "#+date: [" nil t)
                       (buffer-substring (point) (+ 10 (point))))))
          (tags (or (sort (org-get-tags) #'string-lessp)
                    '("")))
          (output-path (org-export-output-file-name org-html-extension nil pub-dir)))

      ;; Skip exporting if we wouldn't keep the result
      (unless (or (when (not title)
                    (warn "TITLE MISSING: %s" filename)
                    t)
                  (when (not created)
                    (warn "DATE MISSING: %s" filename)
                    t)
                  (when (not (org-id-get))
                    (warn "ID MISSING: %s" filename)
                    t)
                  (when (seq-intersection tags my-outdated-tags)
                    (warn "OUTDATED TAG FOUND: %s" filename)
                    t)
                  (when (seq-intersection tags my-tags-to-avoid-uploading)
                    (message "Found exclude-tag, excluding: %s" filename)
                    t))

        ;; The original export-function.  Do your magic!
        (org-publish-org-to 'html filename org-html-extension plist pub-dir)

        ;; Customize the result and wrap it in JSON
        (let ((slug (string-replace pub-dir "" output-path))
              (updated (format-time-string "%F" (f-modification-time filename)))
              (refs (save-excursion
                      (when (search-forward ":roam_refs: " nil t)
                        ;; Only top level ref, not from a subheading
                        (unless (search-backward "\n*" nil t)
                          (buffer-substring (point) (line-end-position))))))
              (wordcount (save-excursion
                           (if (re-search-forward "^[^#:\n]" nil t)
                               (count-words (1- (point)) (point-max))
                             0)))
              (backlinks 0)
              (links 0)
              (subheading-refs (save-excursion
                                 (let (alist)
                                   (while (search-forward ":roam_refs: " nil t)
                                     (let ((here (point))
                                           (ref (buffer-substring (point) (line-end-position))))
                                       ;; not a file-level ref
                                       (when (search-backward "\n*" nil t)
                                         (search-forward " ")
                                         (push (cons (buffer-substring (point) (line-end-position))
                                                     ref)
                                               alist)
                                         (goto-char here))))
                                   alist)))
              (data-for-json nil))
          (with-temp-buffer
            (goto-char (point-min))
            (when (member "logseq" tags)
              (insert "<div class=\"logseq\">"))
            (insert "<h1 id=\"title\">" title "</h1>")

            ;; Insert the roam_refs before the post body
            (when refs
              (insert "<p>Ref: "  )
              (dolist (ref (split-string refs))
                (setq ref (string-replace "\"" "" ref)) ;; just in case I wrapped it in quotes
                (insert " <a href=\"" ref "f\">" (replace-regexp-in-string "http.?://" "" ref) "</a> "))
              (insert "</p>"))

            ;; Insert all the HTML that Org generated
            (insert-file-contents output-path)

            ;; Close the tag we may have added earlier
            (when (member "logseq" tags)
              (goto-char (point-max))
              (insert "</div>"))

            ;; Make Bulma CSS render the Table of Contents as an infobox
            (goto-char (point-min))
            (setq content-start (point))
            (when (search-forward "<div id=\"table-of-contents\" " nil t)
              (insert " class=\"box\"")
              (search-forward "</div>")
              (search-forward "</div>")
              (setq content-start (point)))

            ;; Remove in-document divs since they mess up the look of Bulma
            ;; CSS.  Except for the pages where I will actually style the
            ;; divs to help visual structure (big collections of links with
            ;; many nested headings, or pages with Logseq-style outlining).
            (when (member "logseq" tags)
              ;; TODO: Turn logseq export into ul/li.  See old revision of this
              ;; file for some prior art.
              (progn
                (goto-char content-start)
                (while (re-search-forward "<h[[:digit:]]" nil t)
                  (replace-match "<p"))
                (goto-char content-start)
                (while (re-search-forward "</h[[:digit:]]" nil t)
                  (replace-match "</p"))))
            (unless (or (member "logseq" tags)
                        (member "outline" tags))
              (goto-char content-start)
              (while (re-search-forward "</?div.*?>" nil t)
                (replace-match "")))

            ;; Bulma CSS makes too airy tables (per the modern trend of
            ;; design-porn over usability); compact them
            (goto-char (point-min))
            (while (search-forward "<table " nil t)
              (insert "class=\"is-narrow is-striped is-bordered\""))

            ;; TODO: add roam refs for each subheading that has one.  How? I guess make an alist of headings and refs, then refer to that.
            (while subheading-refs
              (let* ((cell (pop subheading-refs))
                     (heading (car cell))
                     (ref (cdr cell)))
                (goto-char content-start)
                (search-forward (concat ">" heading "</") nil)
                (search-forward ">" nil)
                (insert "<p>Ref: ")
                (dolist (ref (split-string refs))
                  (setq ref (string-replace "\"" "" ref)) ;; just in case I wrapped it in quotes
                  (insert " <a href=\"" ref "f\">" (replace-regexp-in-string "http.?://" "" ref) "</a> "))
                (insert "</p>")))

            ;; Calculate # of backlinks
            (goto-char (point-max))
            (when (search-backward ">What links here</" nil t)
              (setq backlinks (cl-loop while (search-forward "<li>" nil t)
                                       count t)))

            ;; Calculate # of total links (except links to external sites)
            (goto-char content-start)
            (setq links (cl-loop while (re-search-forward "<a .*?href=." nil t)
                                 unless (looking-at-p "http")
                                 count t))

            (setq data-for-json
                  `((slug . ,slug)
                    (title . ,title)
                    (created . ,created)
                    (updated . ,updated)
                    (wordcount . ,wordcount)
                    (backlinks . ,backlinks)
                    (links . ,links)
                    (refs . ,refs)
                    (tags . ,tags)
                    (content . ,(buffer-string)))))

          (with-temp-file output-path
            (when-let ((output-buf (find-buffer-visiting output-path)))
              (kill-buffer output-buf))
            (insert (json-encode data-for-json))))))))

;; That's all we need!  Below this line is a construction site only.


;;; WIP: Make an Atom/RSS feed
;; unused for now

;; see syntax on https://validator.w3.org/feed/docs/atom.html
(defun my-make-atom-feed ()
  (with-temp-file "/tmp/atom.xml"
    (insert"<?xml version=\"1.0\" encoding=\"utf-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <title>Martin Edström's notes</title>
  <link href=\"https://edstrom.dev\"/>
  <updated>" (format-time-string "%FT%TZ") "</updated>
  <author>
    <name>Martin Edström</name>
  </author>
  <rights> © 2023 Martin Edström </rights>
  <id>https://edstrom.dev</id>")

    ;; TODO: now do something like this for every month generated by my news
    ;; generator `my-make-changelog'...
    (let ((title "Changelog March 2023")
          (updated "2023-04-01")
          (month-post-link "https://edstrom.dev/posts/changelog-2023-03")
          (month-post-org-id "61a76c80-d399-11d9-b93C-0003939e0af6")
          (content "this string should be the entire content of the news post"))
      (insert "
  <entry>
    <title>" title "</title>
    <link href=\"" month-post-link "\" />
    <id>urn:uuid:" month-post-org-id "</id>
    <updated>" updated "</updated>
    <content type=\"html\">" content "</content>
  </entry>"))

    ;; TODO: down the line, maybe also insert standalone entries for new pages I
    ;; deem particularly "bloggable".  These would also be linked in the monthly
    ;; news, so whoever doesn't use the feed can still see them pointed out there.

    (insert "
</feed>")))


;; WIP
;;
;; how should i add arbitrary text for the month?  I guess it would be easier if
;; I have a preexisting roam node named Changelog, and I write into it headings
;; for each month, following the format "Summary March 2022" etc.  That way,
;; this function would just plug in during org-publish to annotate a copy of
;; that one file to expand each heading with extra info.  Also creating the
;; heading if it doesn't exist yet (no need to create ID for those).
;;
;; Could discuss whether the summaries should just be titled "Changes March
;; 2022" to limit the scope, or if I'm to retitle the Changelog as a broader
;; "News", which fits better with broad-topic Summaries?
(defun my-make-changelog ()
  (my-coalesce-git-log-by-month)
  ;; etc
  )

;; WIP
(defun my-renames-table (dir)
  (require 'ts)
  (let ((default-directory dir)
        (table nil)
        (trails nil))
    (cl-loop
     for extant-file in (directory-files dir)
     as git-output = (my-process-output-to-string
                      "git" "log" "-M" "--diff-filter=R" "--summary"
                      "--follow" "--format='commit %h%n%ai'" "--"
                      extant-file)
     do
     (let (trail)
       (with-temp-buffer
         (insert git-output)
         (goto-char (point-min))
         (while (re-search-forward "^commit" nil t)
           (let ((date (ts-parse (buffer-substring (point) (line-end-position)))))
             ;; (forward-line 3) ;; just in case
             (re-search-forward "^ rename " nil)

             ;; NOTE: Some log lines print curly braces, as in these examples:
             ;;  rename {People => attachments}/laplace.jpg (100%)
             ;;  rename lisp/{00-general.el => 10-general.el} (100%)
             ;; Treat them specially
             (if (save-excursion
                   (search-forward "{" (line-end-position) t))
                 (let (fragments-old
                       fragments-new
                       (beg (point)))
                   ;; I don't know if git-log can print two or more sets of
                   ;; curlybraces, but this while-loop should handle it.
                   (while (search-forward "{" (line-end-position) t)
                     (push (buffer-substring beg (1- (point))) fragments-old)
                     (push (buffer-substring beg (1- (point))) fragments-new)
                     (let ((curly-braced-old
                            (buffer-substring (point)
                                              (- (search-forward " => ") 4)))
                           (curly-braced-new
                            (buffer-substring (point)
                                              (- (search-forward "}") 1))))
                       (push curly-braced-old fragments-old)
                       (push curly-braced-new fragments-new))
                     (setq beg (point)))

                   (let ((final-fragment (buffer-substring
                                          beg (- (search-forward " (") 2))))
                     (push final-fragment fragments-old)
                     (push final-fragment fragments-new))

                   ;; Add a row to the table.  DATE OLDNAME NEWNAME
                   (push (list (ts-format "%F" date)
                               (string-join fragments-old)
                               (string-join fragments-new)) trail))

               ;; Plain log lines look like this:
               ;;  rename index.org => 2022-10-19-index.org (56%)
               (let ((old (buffer-substring (point) (1- (search-forward " "))))
                     (new (buffer-substring (search-forward "=> ") (1- (search-forward " ")))))
                 ;; Add a row to the table
                 (push (list (ts-format "%F" date) old new) trail))))))
       (push trail trails))
     )
    )
  trails)

;; REVIEW: Maybe just construct the coalesced git log from the --follow output,
;;         instead of cross-referencing two different command outputs


;; TODO: Now that we have a list of renames, how do we get the
;; current name of any given file from a given month?  What if we
;; restructure the table so the key is month+filename, and the value
;; is the current filename?
;; (setq foo (my-renames-table org-roam-directory))
;; (my-coalesce-git-log-by-month)
;; WIP
(defun my-coalesce-git-log-by-month ()
  (require 'ts)
  (let ((default-directory org-roam-directory)
        (files (directory-files-recursively org-roam-directory "\\.org$" t))
        (renames nil))

    ;; (setq renames (my-renames-table org-roam-directory))

    (with-temp-file "/tmp/test"
      (cl-loop
       ;; Some relevant dates:
       ;; - Git-init on 2021-Aug-31, but little real use until 2022
       ;; - 2023 is when I started with auto-commits
       for year from 2023 to (decoded-time-year (decode-time))
       do (cl-loop
           for month from 1 to 12
           as this-month = (concat (int-to-string year)
                                   "-"
                                   (string-pad (int-to-string month) 2 ?0 t)
                                   "-01")
           as next-month = (if (= month 12)
                               (concat (int-to-string (1+ year)) "-01-01")
                             (concat (int-to-string year)
                                     "-"
                                     (string-pad (int-to-string (1+ month)) 2 ?0 t)
                                     "-01"))
           do (let ((files-changed-this-month
                     (--filter (member (file-name-extension it) '(nil "org" "txt" "md"))
                               (-uniq (split-string (my-process-output-to-string
                                                     "git" "log" "--format=" "--name-only"
                                                     (concat "--since=" this-month)
                                                     (concat "--until=" next-month)))))))
                ;; while we're at it, find dailies created this month
                ;; (cl-loop for file in (directory-files "/home/kept/roam/daily" t)
                ;;          when (and (string-search (substring this-month 0 8) file)
                ;;                    (with-temp-buffer
                ;;                      (insert-file-contents file)
                ;;                      ;; file is completely untagged? then it's public
                ;;                      (not (org-get-tags))))
                ;;          do (insert "\n- " file))
                (when files-changed-this-month
                  (insert "\n\n* Changelog " (ts-format "%B %Y" (ts-parse this-month)))
                  (cl-loop
                   for file in files-changed-this-month
                   as changes = (split-string
                                 (my-process-output-to-string
                                  "git" "log" "--format=" "--numstat"
                                  (concat "--since=" this-month)
                                  (concat "--until=" next-month)
                                  "--follow" "--" (shell-quote-argument file))
                                 "\n")
                   do
                   (cl-loop
                    for line in changes
                    with total-diff = 0
                    when (/= 0 (length line))
                    do (seq-let (adds dels &rest _) (split-string line)
                         (if (and adds dels
                                  (string-match-p "^[[:digit:]]+$" adds)
                                  (string-match-p "^[[:digit:]]+$" dels))
                             (cl-incf total-diff (- (string-to-number adds)
                                                    (string-to-number dels)))
                           (message "Non-number in log line %s" line)))
                    finally do
                    (when (> total-diff 20)
                      (insert "\n" (int-to-string total-diff) " new lines in " file)))
                   ))))))))
;; (my-coalesce-git-log-by-month)
;;
