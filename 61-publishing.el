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

(setq my-tags-to-avoid-uploading '("noexport" "private" "censor" "drill" "fc" "anki"))

(setopt org-publish-project-alist
        `(("react-blog"
           :base-directory "/tmp/roam/"
           :publishing-directory "/home/kept/blog/posts/"
           :publishing-function my-publish-to-blog
           :preparation-function my-prep-fn
           :recursive t
           :body-only t
           :with-toc nil
           :section-numbers nil
           :exclude "daily/\\|logseq/"
           ;; NOTE: this does not seem to work for filetags, only subtrees, but
           ;; we fix that in `my-publish-to-blog'.
           :exclude-tags ,my-tags-to-avoid-uploading)))

;; REVIEW: Does this do anything?
(after! ox
  (setq org-export-exclude-tags
        (seq-union org-export-exclude-tags my-tags-to-avoid-uploading)))

;; TODO
;; Give h1...h6 headings an ID from the source org-ids instead of "org953031",
;; so that hash-links such as #ID-e10bbdfe-1ffb-4c54-9228-2818afdfc5ba will make
;; the web browser jump to that heading.
;; (after! ox (require 'org-roam-export))

(add-hook 'org-export-before-parsing-functions #'my-add-backlinks-if-roam)
(add-hook 'org-export-before-parsing-functions #'my-replace-web-links-with-note-links-if-ref-exists)

(defun my-add-backlinks-if-roam (&rest _)
  "Add a \"What links here\" subtree at the end.
Meant to run on `org-export-before-parsing-functions', and if it
does, it will not modify the source file."
  (let (this-node
        backlinks
        reflinks)
    (when (ignore-errors (setq this-node (org-roam-node-at-point)))
      (dolist (obj (org-roam-backlinks-get this-node :unique t))
        (let ((node (org-roam-backlink-source-node obj)))
          (cl-pushnew (cons (org-roam-node-id node)
                            (org-roam-node-title node))
                      backlinks)))
      (dolist (obj (org-roam-reflinks-get this-node))
        (let* ((ref-node (org-roam-reflink-source-node obj)))
          (unless (equal ref-node this-node)
            (cl-pushnew (cons (org-roam-node-id ref-node)
                              (org-roam-node-title ref-node))
                        reflinks))))
      (when (or backlinks reflinks)
        (save-excursion 
          (if (bobp)
              (progn
                (goto-char (point-max))
                (insert "* What links here"))
            (org-insert-subheading nil)
            (insert "What links here"))
          (dolist (backlink backlinks)
            (newline)
            (insert "- [[id:" (car backlink) "][" (cdr backlink) "]]"))
          (dolist (reflink reflinks)
            (newline)
            (insert "- [[id:" (car reflink) "][" (cdr reflink) "]]")))))))

(defun my-replace-web-links-with-note-links-if-ref-exists (&rest _)
  "Anywhere there's a link to an URL, if there exists an Org note
with the same link in its :ROAM_REFS: property, then replace that
web-link with a link to the roam note.

Meant to run on `org-export-before-parsing-functions', and if it
does, it will not modify the source file."
  (when (ignore-errors (org-roam-node-at-point))
    (let ((all-refs (org-roam-db-query
                     [:select [ref id title]
                      :from refs
                      :left-join nodes
                      :on (= refs:node-id nodes:id)])))
      (save-excursion
        (while (not (equal "No further link found" (org-next-link)))
          (let* ((elem (org-element-context))
                 (link (org-element-property :path elem))
                 (ref (assoc link all-refs)))
            (when (and ref
                       ;; ignore if same page
                       (not (equal (caddr ref) (org-get-title))))
              (delete-region (point) (org-element-property :end elem))
              (insert "[[id:" (cadr ref) "][" (caddr ref) "]]"))))))))

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
  (org-id-update-id-locations)
  (org-roam-db-sync)
  (org-roam-update-org-id-locations)
  (fset 'org-id-update-id-locations #'ignore) ;; stop it autotriggering
  ;; Don't save the changes to `org-id-locations'
  (cancel-timer my-state-sync-timer)
  (remove-hook 'kill-emacs-hook #'org-id-locations-save)
  (remove-hook 'kill-emacs-hook #'save-place-kill-emacs-hook)

  (setopt org-export-use-babel nil)
  ;; (setopt org-export-with-broken-links t)

  ;; Hack the `org-id-locations' table

  (setq new (org-id-hash-to-alist org-id-locations))
  ;; Strip dates from filename references (my files have YYYY-MM-DD)
  (setq new (cl-loop
             for pair in new
             if (string-match-p (rx bol (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "-")
                                (file-name-nondirectory (car pair)))
             collect (cons (concat (file-name-directory (car pair))
                                   (substring (file-name-nondirectory (car pair)) 11))
                           (cdr pair))
             else collect pair))
  (setq new (cl-loop
             for pair in new
             collect (cons (->> (car pair)
                                ;; Pretend roam directory is /tmp/roam since we'll work from there
                                (s-replace "^/home/kept/roam/"  
                                           "/tmp/roam/")
                                ;; Flatten the directory tree (no subdirs)
                                (s-replace (rx (group bol "/tmp/roam/") (* nonl) "/" )
                                           "\\1"))
                           (cdr pair))))
  (setq org-id-locations (org-id-alist-to-hash new))

  ;; Do changes on-disk mirroring what we did to the `org-id-locations' table

  ;; Duplicate the files to /tmp so we can work from there
  (shell-command "rm -rf /tmp/roam")
  (copy-directory "/home/kept/roam/" "/tmp/" t)
  (shell-command "rm -rf /tmp/roam/daily") ;; TODO: Include it
  (shell-command "rm -rf /tmp/roam/{martin,grismartin}/logseq")

  ;; Strip dates from filenames (my files have YYYY-MM-DD)
  (cl-loop
   for file in (directory-files-recursively
                "/tmp/roam"
                (rx bol (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "-" (+ nonl) ".org" eol)
                nil
                (lambda (dir)
                  (unless (string-search "daily" dir)
                    t)))
   do (rename-file file
                   (concat (file-name-directory file)
                           (substring (file-name-nondirectory file) 11))))

  ;; Flatten the directory tree, no more subdirs except for daily/
  (cl-loop
   for file in (directory-files-recursively "/tmp/roam" "\\.org$" nil
                                            (lambda (dir)
                                              (if (string-search "daily" dir)
                                                  nil
                                                t)))
   unless (equal (file-name-directory file) "/tmp/roam/")
   do (rename-file file "/tmp/roam/")))

;; Struggled so long looking for a hook that would work like the old
;; before-export-hook.  Let this be a lesson: the emacs hook system exists to
;; let you subtly modify a function IN THE MIDDLE of its body. We never actually
;; need before-hooks (or after-hooks), since in such a simple case it is always
;; possible to use `add-function' or call a wrapper such as this.
(defun my-publish-to-blog (plist filename pub-dir)
  (let* ((org-inhibit-startup t)
         (visiting (find-buffer-visiting filename))
         (work-buffer (or visiting (find-file-noselect filename)))
         (org-html-extension ""))

    ;; Save time by not even calling `org-publish-org-to'.
    ;; Now I expect to never trigger the excluded-tags clause in the cond further below.
    (unless (with-current-buffer work-buffer
              (save-excursion
                (goto-char (point-min))
                (seq-intersection (org-get-tags) my-tags-to-avoid-uploading)))

      ;; The original publish function
      (org-publish-org-to 'html filename org-html-extension plist pub-dir)

      (unwind-protect
          (with-current-buffer work-buffer
            (goto-char (point-min))
            (let* ((output-path (org-export-output-file-name org-html-extension nil pub-dir))
                   (output-buf (find-buffer-visiting output-path))
                   (was-opened nil)
                   (case-fold-search t)
                   (slug (string-replace pub-dir "" output-path))
                   (title (save-excursion
                            (when (search-forward "#+title: " nil t)
                              (buffer-substring (point) (line-end-position)))))
                   (created (save-excursion
                              (when (search-forward "#+date: " nil t)
                                (buffer-substring (1+ (point)) (+ 11 (point))))))
                   (updated (format-time-string "%F" (f-modification-time filename)))
                   (tags (save-excursion
                           (or (sort (org-get-tags) #'string-lessp) '(""))))
                   (refs (save-excursion
                           (when (search-forward ":roam_refs: " nil t)
                             (unless (search-backward "\n*" nil t)
                               (buffer-substring (point) (line-end-position))))))
                   (wordcount (save-excursion
                                (re-search-forward "^[^#:\n]" nil t)
                                (count-words (point) (point-max))))
                   (backlinks (save-excursion
                                (goto-char (point-max))
                                (when (search-backward "* What links here" nil t)
                                  (cl-loop while (re-search-forward "^- " nil t)
                                           count t))))
                   (data `((slug . ,slug)
                           (title . ,title)
                           (created . ,created)
                           (updated . ,updated)
                           (wordcount . ,wordcount)
                           (backlinks . ,backlinks)
                           (refs . ,refs)
                           (tags . ,tags)
                           (content . nil))))
              (cond
               ((not (and title created))
                (delete-file output-path)
                (warn "FILE DELETED: LACKING TITLE OR LACKING DATE: %s" output-path))
               ;; This file is empty because it met :exclude-tags, seems
               ;; org-publish is not smart enough to just skip the export.
               ;; Though I don't understand why the next clause sometimes
               ;; succeeds?
               ((= 0 (doom-file-size output-path))
                (delete-file output-path)
                (message "File deleted because empty: %s" output-path))
               ((seq-intersection tags my-tags-to-avoid-uploading)
                (delete-file output-path)
                (message "File deleted because found excluded-tag: %s" output-path))
               ;; ((not (org-id-get))
               ;;  (delete-file output-path)
               ;;  (message "FILE DELETED BECAUSE NO ID: %s" output-path))
               (t
                (when output-buf
                  (setq was-opened t)
                  (unless (buffer-modified-p output-buf)
                    (kill-buffer output-buf)))
                (with-temp-buffer
                  (goto-char (point-min))
                  (insert "<h1 id='title'>" title "</h1>")
                  (when refs
                    (insert "Reference(s): "  )
                    (dolist (ref (split-string refs))
                      (setq ref (string-replace "\"" "" ref))
                      (insert " <a href=\"" ref "\">" (replace-regexp-in-string "http.?://" "" ref) "</a> "))
                    (insert "<br />"))
                  (insert-file-contents output-path)

                  ;; Remove divs since they mess up the look of Bulma CSS.
                  ;; As an alternative, if I want to emulate the look of
                  ;; org-indent-mode. I could probably keep the divs classed
                  ;; .outline-[123456] and manually fix the slight
                  ;; misalignment of headlines that results (or give the divs
                  ;; the Bulma class .section).  Or the alignment might
                  ;; work normally if they were spans, not divs.
                  (goto-char (point-min))
                  (while (re-search-forward "</?div.*?>" nil t)
                    (replace-match ""))

                  (setf (alist-get 'content data) (buffer-string)))
                (with-temp-file output-path
                  (insert (json-encode data))
                  (when was-opened
                    (find-file-noselect output-path)))))))
        (unless visiting (kill-buffer work-buffer))))))

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

    ;; now do something like this for every month generated by my (as yet nonexistent) news generator
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

    (insert "
</feed>")))

;; WIP
(defun my-coalesce-git-log-by-month ()
  (require 'ts)
  (let ((default-directory org-roam-directory)
        (files (directory-files-recursively org-roam-directory "\\.org$" t))
        ;; WIP: make a renames table I can look up to know a file's current name
        (renames nil))
    (with-temp-buffer
      (insert (my-process-output-to-string
               "git" "log" "-M" "--diff-filter=R" "--summary"
               "--format=commit%ai%n%B" "--date" "default"))
      (goto-char (point-min))
      (while (re-search-forward "^commit" nil t)
        (let ((date (ts-parse (buffer-substring (point) (line-end-position))))
              (end (save-excursion (or (re-search-forward "^commit" nil t)
                                       (point-max)))))
          (forward-line 3) ;; just in case
          (while (re-search-forward "^ rename " end t)
            (if (looking-at-p "{")
                ;; Some log lines print curly braces, as in this line:
                ;;  rename {People => attachments}/laplace.jpg (100%)
                (progn ;; Deal with them specially.
                  (re-search-forward (rx (group (+ nonl)) " => " (group (+ nonl)) "}") (line-end-position))
                  (let ((fname-remainder
                         (buffer-substring (point)
                                           (- (search-forward " (") 2))))
                    (push (list (concat (match-string 1) fname-remainder)
                                (concat (match-string 2) fname-remainder)
                                (ts-format "%F" date))
                          renames)))
              ;; Plain log lines look like this:
              ;;  rename index.org => 2022-10-19-index.org (56%)
              (let ((old (buffer-substring (point) (1- (search-forward " "))))
                    (new (buffer-substring (search-forward "=> ") (1- (search-forward " ")))))
                (push (list old new (ts-format "%F" date)) renames)
                )
              )
            )
          )
        )
      )
    ;; TODO: Now that we have a list of renames, how do we get the
    ;; current name of any given file from a given month?  What if we
    ;; restructure the table so the key is month+filename, and the value
    ;; is the current filename?
    (setq foo renames)
    (message "Renames: %s" renames)
    (with-temp-file "/tmp/test"
      (cl-loop
       ;; Relevant dates:
       ;; - Git-init on 2021-Aug-31, but little real use until 2022
       ;; - The first half of 2022 was dense with refactoring
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
                (when files-changed-this-month
                  (insert "\n\n Changelog " (ts-format "%B %Y" (ts-parse this-month))))
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
                    (insert "\n" (int-to-string total-diff) " lines changed in " file)))
                 )))))))
;; (my-coalesce-git-log-by-month)

(setq foo nil)

;; WIP
;; flag unhidden dailies (i.e. lacking :personal: tag) and then auto-link them
;;
;; how should i add arbitrary text for the month?  i guess it would be easier if
;; I have a preexisting roam node named Changelog, and I write into it headings
;; for each month, following the format "Summary March 2022" etc.  That way,
;; this function would just plug in during org-publish for that one file to
;; expand each heading with lots of info for that month.  Also creating the
;; heading if it doesn't exist yet (no need to create ID then).
;;
;; Could discuss whether the summaries should just be titled "Changes March
;; 2022" to limit the scope, or if I'm to retitle the Changelog as a broader
;; "News", which fits better with broad-topic summaries?
(defun my-make-changelog ()
  (my-coalesce-git-log-by-month)
  ;; etc
  )
