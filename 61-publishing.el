;; -*- lexical-binding: t; -*-

(add-hook 'org-export-before-parsing-functions #'my-add-backlinks-if-roam)
(add-hook 'org-export-before-parsing-functions #'my-replace-web-links-with-note-links-if-ref-exists)

(defun my-add-backlinks-if-roam (&rest _)
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
function is where I can make destructive changes that I don't
want in my main Emacs.

A biggie: I'd like the final URLs on my website to exclude the
date slug in the org-roam filenames.  So prepare to work from a
/tmp/roam/ copy of the org-roam dir, with the dates stripped from
filenames, and the `org-id-locations' table modified likewise, so
that org-id links will resolve correctly."
  (fset 'org-id-update-id-locations #'ignore) ;; stop it autotriggering
  (setopt org-export-use-babel nil)
  (setopt org-export-with-broken-links t)
  ;; Don't save the changes to `org-id-locations'
  (cancel-timer my-write-data-timer)
  (remove-hook 'kill-emacs-hook #'org-id-locations-save)

  ;;; Hack the `org-id-locations' table

  (setq new (org-id-hash-to-alist org-id-locations))
  ;; Strip dates from filename references
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
                                ;; Bonus: Include my Beorg files
                                (s-replace "^/home/sync-phone/beorg/"
                                           "/home/kept/roam/beorg/")
                                ;; Pretend roam directory is /tmp/roam since we'll work from there
                                (s-replace "^/home/kept/roam/"  
                                           "/tmp/roam/")
                                ;; Flatten the directory tree (no subdirs)
                                (s-replace (rx (group bol "/tmp/roam/") (* nonl) "/" )
                                           "\\1"))
                           (cdr pair))))
  (setq org-id-locations (org-id-alist-to-hash new))

  ;;; Do changes on-disk mirroring what we did to `org-id-locations'

  ;; Duplicate the files to /tmp so we can work from there
  (shell-command "rm -rf /tmp/roam")
  (copy-directory "/home/kept/roam/" "/tmp/" t)
  ;; Bonus: include my Beorg files.
  (shell-command "rm /tmp/roam/beorg") ;; rm the symlink that came along for the ride
  (copy-directory "/home/sync-phone/beorg/" "/tmp/roam/" t)

  ;; Strip dates from filenames
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
  ;; Flatten the directory tree (no subdirs)
  (cl-loop
   for file in (directory-files-recursively
                "/tmp/roam" "\\.org$"
                nil
                (lambda (dir)
                  (if (or (string-search "daily" dir)
                          (string-search "version-files" dir)
                          (string-search "bak" dir))
                      nil
                    t)))
   unless (equal (file-name-directory file) "/tmp/roam/")
   do (rename-file file "/tmp/roam/")))

;; Struggled so long looking for a hook that would work like the old
;; before-export-hook.  Let this be a lesson:  the hook system exists to let you
;; subtly modify a function IN THE MIDDLE of its body. We never actually need
;; there to exist a before-hook, since in such a simple case it is always
;; possible to use add-function or replace the function with a wrapper like this.
(defun my-publish-to-blog (plist filename pub-dir)
  (let* ((org-inhibit-startup t)
         (visiting (find-buffer-visiting filename))
         (work-buffer (or visiting (find-file-noselect filename)))
         (org-html-extension ""))

    ;; The original publish function
    (org-publish-org-to 'html filename org-html-extension plist pub-dir)

    (unwind-protect
        (with-current-buffer work-buffer
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
                 (tags (or (sort (org-get-tags) #'string-lessp) '("")))
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
                ;; the Bulma class .section).  Or maybe the alignment would
                ;; work normally if they were spans, not divs.
                (goto-char (point-min))
                (while (re-search-forward "</?div.*?>" nil t)
                  (replace-match ""))

                (setf (alist-get 'content data) (buffer-string)))
              (with-temp-file output-path
                (insert (json-encode data))
                (when was-opened
                  (find-file-noselect output-path)))))))
      (unless visiting (kill-buffer work-buffer)))))


(setq my-tags-to-avoid-uploading '("noexport" "private" "censor" "drill" "fc" "anki"))

(after! ox
  (setq org-export-exclude-tags
        (seq-union org-export-exclude-tags my-tags-to-avoid-uploading)))

;; Identify h1...h6 headings by their org-ids instead of "org953031", so that hash-links such as
;; #ID-e10bbdfe-1ffb-4c54-9228-2818afdfc5ba will make the browser jump to that heading.
;; (after! ox (require 'org-roam-export))

;; idk about the merits
;; (setopt org-html-checkbox-type 'html)

(setopt org-publish-project-alist
        `(("react-blog"
           :base-directory "/tmp/roam/"
           :publishing-directory "/home/kept/blog/posts/"
           :publishing-function my-publish-to-blog
           :recursive t
           :preparation-function my-prep-fn
           :with-toc nil
           :section-numbers nil
           :body-only t
           :exclude "daily/\\|logseq/"
           ;; this does not seem to work for filetags, only subtrees!
           :exclude-tags ,my-tags-to-avoid-uploading)))


;; WIP
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
          (month-post-org-id "60a76c80-d399-11d9-b93C-0003939e0af6")
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
;; coalesce git logs for the month
;;
;; how should i flag dailies worth sharing?  the :star: tag i guess
;;
;; how should i add arbitrary text for the month?  i guess it would be easier if
;; I have a preexisting roam node named Changelog, and I write into it headings
;; for each month.  That way, this function would just plug in during
;; org-publish for this one file to expand each (preexisting) heading with lots
;; of info for that month.  the heading must be titled precisely YYYY-MM.
;;
;; TODO: takes very long to run; better do an initial git log run to show which
;; files have in fact changed for each month and then do the --follow command on
;; only those files.
(defun my-coalesce-git-log-by-month ()
  (require 'ts)
  (let ((default-directory org-roam-directory)
        (files (directory-files-recursively org-roam-directory "\\.org$" t))
        (renames ;; wip
         (let (renames)
           (with-temp-buffer
             (insert (my-process-output-to-string
                      "git" "log" "-M" "--diff-filter=R" "" "--summary" "--format=commit%ai%n%B" "--date" "default"))
             (goto-char (point-min))
             (while (re-search-forward "^commit" nil t)
               (let ((date (ts-parse (buffer-substring (point) (line-end-position))))
                     (end (save-excursion (re-search-forward "^commit" nil t))))
                 (forward-line 3) ;; just in case
                 (while (re-search-forward "^ rename " end t)
                   (if (search-forward "{" (line-end-position) t)
                       ;; Some log lines print curly braces, as in this line:
                       ;;  rename {People => attachments}/laplace.jpg (100%)
                       (progn ;; Deal with them specially.
                         )
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
           renames)))
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
           ;; NOTE: We follow the histories of only the files that exist now.
           ;; That means if I delete a file, it's GONE even from changelogs of
           ;; the past.  Since changelogs are fundamentally a timeful sort of
           ;; text, that shouldn't really matter.  On the off chance someone
           ;; wants to read all my feed items, I expect they're more interested
           ;; in the summaries I may have manually written than in the "pages
           ;; edited" list.
           ;; TODO: Ok, now it's different.  Now... urgh
           ;; I should have an alist mapping current filenames to all the
           ;; filenames they've had.  Then, after generating the changelog, do
           ;; search-replaces across the whole changelog to update old filenames
           ;; to new.
           ;;
           ;; Small possibility of conflict, because of the date slug inside the
           ;; Org-Roam filenames.  I suspect they'll amount to 1-2 files, and I
           ;; can just let those misreports be.
           do (let ((files-changed-this-month
                     (-uniq (split-string (my-process-output-to-string
                                           "git" "log" "--format=" "--name-only"
                                           (concat "--since=" this-month)
                                           (concat "--until=" next-month))))))
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
                    (insert "\n" (int-to-string total-diff) " new lines in " file)))
                 )))))))
;; (my-make-changelogs)
