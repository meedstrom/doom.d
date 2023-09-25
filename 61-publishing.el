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

(require 'dash)
(require 'f)

;; Keep in mind that case-fold-search t doesn't affect `equal' or `-intersection'
(defvar my-extinct-tags '("drill"  "fc" "anki" "partner" "friends-eyes" "therapist" "eyes-partner" "eyes-therapist" "eyes-diana" "eyes-friend"))
(defvar my-tags-to-avoid-uploading (append my-extinct-tags '("noexport" "archive" "private" "censor")))


(defun my-org-file-id (file)
  (with-temp-buffer
    ;; faster than `org-get-id'
    (insert-file-contents-literally file)
    (and (search-forward "#+title" nil t)
         (search-backward ":id: " nil t)
         (buffer-substring (+ 5 (point)) (line-end-position)))))

(defun my-publish (&optional prefix)
  "A single command I can use in a child emacs."
  (interactive "p")
  (require 'ox-publish)
  (switch-to-buffer "*Messages*") ;; for watching it work
  (delete-other-windows)
  (cd "/home/kept/roam") ;; for me to quick-search when an id fails to resolve
  (org-publish "my-slipbox-blog" (>= prefix 4)))

(setopt org-publish-project-alist
        `(("my-slipbox-blog"
           :base-directory "/tmp/roam/"
           :publishing-directory "/home/kept/pub/posts/"
           :publishing-function my-publish-to-blog
           :preparation-function my-prep-fn
           :recursive t
           :body-only t
           :with-toc nil
           :section-numbers nil
           ;; NOTE: this works only for subtrees, so we also check file-level
           ;; tag in `my-publish-to-blog'.
           :exclude-tags ,my-tags-to-avoid-uploading)))

;; Give h2...h6 headings an ID from the source org-id, if it has one,
;; instead of e.g. "org953031".  That way, hash-links such as
;; #ID-e10bbdfe-1ffb-4c54-9228-2818afdfc5ba will make the web browser jump to
;; that heading.
;; The kind of tools you get thanks to org-roam!
(after! ox (require 'org-roam-export))

(add-hook 'org-export-before-parsing-functions #'my-add-backlinks)
(add-hook 'org-export-before-parsing-functions #'my-replace-web-links-with-ref-note-links)

;; TODO: In dailies, insert links to any pages created on that same day, under a
;; "Created pages" heading.  I guess this has to happen after exporting, so I
;; can query the final JSON of all posts.  Therefore it has to happen in the
;; frontend code.  Or, I suppose there could be an org-roam-db-query that looks
;; up the #+DATE property.
;; Something like this
;; (org-roam-db-query [:select *
;;                     :from nodes
;;                     :where (equal "2020-03-15" (plist-get :date properties))])
;; Unfortunately, #+DATE is not a property.  So I may have to reformat all my
;; files so it becomes :DATE:, or more likely :CREATED:.
;; (org-roam-db-query [:select [title] :from nodes :where (like title '"%How%")])
;; (org-roam-db-query [:select * :from nodes
                    ;; :where (in properties (= CREATED "2023-02-15"))])
                    ;; :where (= properties:id "be3674fa-870b-4198-9688-a351eba83270")])
                    ;; :where (in properties (= ID "be3674fa-870b-4198-9688-a351eba83270"))])
                    ;; :where (in "be3674fa-870b-4198-9688-a351eba83270" properties)])
                    ;; :where (= (in properties [:select "ID"]) "be3674fa-870b-4198-9688-a351eba83270")])
                    ;; :where (= (in properties [:select ID]) "be3674fa-870b-4198-9688-a351eba83270")])
                    ;; :where (like (in properties [:select ["CREATED"]]) '"2023-02-15")])
                    ;; :where (like [:select ["CREATED"] from properties] '"2023-02-15")])
;; (org-roam-db-query (concat
                    ;; "SELECT * FROM nodes n, table(n.properties) p"
                    ;; ;; " WHERE 'be3674fa-870b-4198-9688-a351eba83270' IN (SELECT id FROM properties)"
                           ;; ))

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
          (dolist (link (--sort (string-lessp (cdr it) (cdr other))
                                (append backlinks reflinks)))
            (newline)
            (insert "- [[id:" (car link) "]["
                    (replace-regexp-in-string (rx (any "[]")) "" (cdr link))
                    "]]")))))))

;; cache-variable
(defvar my-all-refs nil)

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
    ;; let ((all-refs (org-roam-db-query
    ;;                 [:select [ref id title]
    ;;                  :from refs
    ;;                  :left-join nodes
    ;;                  :on (= refs:node-id nodes:id)])))
    (save-excursion
      (while (not (equal "No further link found" (quiet! (org-next-link))))
        (let* ((elem (org-element-context))
               (link (org-element-property :path elem))
               (ref (assoc link my-all-refs))
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

;; (defconst my-date-regexp (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))
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
  (setopt org-mode-hook nil) ;; speeds up publishing
  (setopt org-export-use-babel nil)
  (setopt org-export-with-drawers '(not "logbook" "noexport")) ;; case-insensitive
  (setopt org-export-with-broken-links nil) ;; links would disappear quietly
  (setopt org-export-with-smart-quotes nil)
  (setopt org-html-checkbox-type 'unicode)
  (setopt org-html-html5-fancy t)
  (setopt case-fold-search t) ;; for all the searches in `my-publish-to-blog'
  (setopt org-inhibit-startup t) ;; from org-publish-org-to
  ;; (setopt org-html-extension "")
  ;; Love it, but won't apply to every datestamp on the site
  (setopt org-display-custom-times nil)
  (toggle-debug-on-error)
  (toggle-debug-on-quit)

  ;; Switch theme for 2 reasons
  ;; 1. Show me that this is not a normal Emacs
  ;; 2. Syntax-highlight source blocks in a way that looks OK on the web
  ;; (load-theme 'doom-rouge)
  (load-theme 'doom-monokai-machine)
  (fset 'rainbow-delimiters-mode #'prism-mode)

  ;; Ensure that this subordinate emacs syncs nothing to disk
  (cancel-timer my-state-sync-timer)
  (setopt kill-emacs-hook nil)
  (fset 'org-id-locations-save #'ignore)
  (setopt org-roam-db-location "/tmp/org-roam.db")
  (org-roam-db-autosync-mode 0)

  ;; Copy the files to /tmp to work from there
  (shell-command "rm -rf /tmp/roam/")
  (copy-directory "/home/kept/roam/" "/tmp/" t)
  (shell-command "rm -rf /tmp/roam/daily/") ;; will put them back in later
  (shell-command "rm -rf /tmp/roam/*/logseq/") ;; no logseq backups
  (shell-command "rm -rf /tmp/roam/lesswrong-org/")
  ;; (shell-command "rm -rf /tmp/roam/lesswrong-org/*/") ;; no autogenerated stubs
  (shell-command "shopt -s globstar && rm -rf /tmp/roam/**/*.gpg") ;; no crypts

  ;; Flatten the directory tree (no more subdirs)
  (cl-loop
   for file in (directory-files-recursively "/tmp/roam" "\\.org$" nil)
   unless (equal "/tmp/roam/" (file-name-directory file))
   do (rename-file file "/tmp/roam/"))

  ;; Put the dailies back in
  (copy-directory "/home/kept/roam/daily/" "/tmp/roam/" t)
  (shell-command "mv -t /tmp/roam/  /tmp/roam/daily/*/*")

  (cl-loop
   with default-directory = "/tmp/roam"
   for path in (directory-files "/tmp/roam" t "\\.org$")
   as uuid = (my-org-file-id path)
   when uuid
   do (let ((permalink (substring (my-uuid-to-base62 uuid 22) -7)))
        (mkdir permalink)
        (rename-file path (concat permalink "/"))))

  ;; Tell `org-id-locations' and the org-roam DB about the new directory.
  (setopt org-roam-directory "/tmp/roam/")
  (setopt org-agenda-files '("/tmp/roam/"))
  (unless my-publish-ran-already
    (org-roam-update-org-id-locations)
    (org-roam-db-sync)
    (setq my-publish-ran-already t))
  (fset 'org-id-update-id-locations #'ignore) ;; stop triggering during publish

  (setq my-all-refs (org-roam-db-query
                     [:select [ref id title]
                      :from refs
                      :left-join nodes
                      :on (= refs:node-id nodes:id)]))
  )

;; test: (my-strip-id-hash-from-link-if-file-level "0vwRV27mRVLFd6yoyUM0PI/some-slug#ID-10b59a2a-bf95-4f20-9b4f-f27e23e51f46")
;; test: (my-strip-id-hash-from-link-if-file-level "../0vwRV27mRVLFd6yoyUM0PI/some-slug#ID-10b59a2a-bf95-4f20-9b4f-f27e23e51f46")
(defvar my-id-regexp (rx (? "/") (group (= 22 alnum)) (? "/")))
(defun my-strip-id-hash-from-link-if-file-level (link)
  (cl-assert (stringp link))
  (if-let* ((hash-pos (string-search "#ID-" link))
            (hash-as-permalink (my-uuid-to-base62 (substring link (+ 4 hash-pos)) 22))
            (actual-permalink (string-match my-id-regexp link))
            (same (equal hash-as-permalink (match-string 1 link))))
      ;; Cut off the hash
      (substring link 0 hash-pos)
    ;; Keep the hash
    link))

;; Struggled so long looking for a hook that would work like the old
;; before-export-hook.  Let this be a lesson: the Emacs hook system exists to
;; let you subtly modify a function IN THE MIDDLE of its body. We never actually
;; need before-hooks (or after-hooks), since in such a simple case it is always
;; possible to use `add-function' or call a wrapper such as this wrapper around
;; `org-publish-org-to'.

(defun my-publish-to-blog (plist filename pub-dir)
  ;; Fast early check that avoids loading org-mode
  (let (title id created tags)
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (forward-line 7)
      (delete-region (point) (point-max)) ;; should be enough to grab the metadata

      (goto-char (point-min))
      (setq title (when (search-forward "#+title: " nil t)
                    (delete-horizontal-space)
                    (buffer-substring (point) (line-end-position))))

      (goto-char (point-min))
      (setq id (when (search-forward ":id:")
                 (delete-horizontal-space)
                 (buffer-substring (point) (line-end-position))))

      (goto-char (point-min))
      (setq created (when (search-forward "#+date: [" nil t)
                      (buffer-substring (point) (+ 10 (point)))))

      (goto-char (point-min))
      (setq tags (if (search-forward "#+filetags: ")
                     (thread-first (buffer-substring (point) (line-end-position))
                                   (string-trim)
                                   (string-split ":")
                                   (sort #'string-lessp))
                   '(""))))
    
    (cond
     ;; Skip exporting if we won't use the result

     ((not title)
      (warn "TITLE MISSING: %s" filename))
     ((not created)
      (warn "DATE MISSING: %s" filename))
     ((not id)
      (warn "ID MISSING: %s" filename))
     ((cl-intersection tags my-extinct-tags
                       :test #'string-equal-ignore-case)
      (warn "OUTDATED TAG FOUND: %s" filename))
     ((cl-intersection tags my-tags-to-avoid-uploading
                       :test #'string-equal-ignore-case)
      (message "Found exclude-tag, excluding: %s" filename))

     ;; OK, export
     (t
      (with-current-buffer (or (find-buffer-visiting filename)
                               (find-file-noselect filename))
        ;; The original export-function.  Do thy magic!
        (org-publish-org-to 'html filename "" plist pub-dir)

        ;; Customize the resulting HTML file and wrap it in a JSON object
        (let* ((output-path (org-export-output-file-name "" nil pub-dir))
               ;; (relative-path (string-replace pub-dir "" output-path))
               ;; (slug (-last-item (split-string relative-path "/")))
               (slug (string-replace pub-dir "" output-path))
               (permalink
                (if (string-search "daily" pub-dir)
                    ;; For dailies, the whole path is necessary
                    (concat "daily/" slug)
                  ;; For other posts, the subdir is its unique id
                  (-last-item (split-string pub-dir "/" t))))
               (updated (format-time-string "%F" (f-modification-time filename)))
               (refs (save-excursion
                       (when (search-forward ":roam_refs: " nil t)
                         ;; Only top level ref, not those from a subheading
                         (unless (search-backward "\n*" nil t)
                           (buffer-substring (point) (line-end-position))))))
               (wordcount (save-excursion
                            (if (re-search-forward "^[^#:\n]" nil t)
                                (count-words (1- (point)) (point-max))
                              0)))
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

            ;; This special tag means wrap in special div for special styling
            ;; (only applies to a few posts)
            (when (member "logseq" tags)
              (insert "<div class=\"logseq\">"))

            (insert "<h1 id=\"title\">" title "</h1>")

            ;; Insert roam_refs before the post body
            (when refs
              (insert "<p>Ref: "  )
              (dolist (ref (split-string refs))
                (setq ref (string-replace "\"" "" ref)) ;; in case I wrapped it in quotes
                (insert " <a href=\"" ref "f\">" (replace-regexp-in-string "http.?://" "" ref) "</a> "))
              (insert "</p>"))

            ;; Insert the post body: the HTML produced by Org-export
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

            ;; Manipulate links
            (goto-char content-start)
            (while (re-search-forward "<a .*?href=\"" nil t)
              ;; From React Router's perspective, the visitor is not in a
              ;; subdir, so get the hrefs to agree with that idea
              ;; NOTE: Breaks Svelte's router
              (when (looking-at (rx (literal "../")))
                (replace-match ""))
              ;; REVIEW: Should remove the lengthy hash-part of the link
              ;;         (i.e. the bit after the # character in LINK#ORG-ID) if
              ;;         the org-id points to a file-level id anyway
              (re-search-forward (rx (* (not "\""))))
              (replace-match (my-strip-id-hash-from-link-if-file-level (match-string 0))))

            ;; TODO: Implement collapsible sections.  We'll probably have to
            ;; undo the subsequent removal of divs.  The method I extrapolate
            ;; from
            ;; https://www.digitalocean.com/community/tutorials/css-collapsible
            ;; as follows: Add an input type='checkbox' element before the
            ;; heading.  Org already wraps the following content in a div, so
            ;; CSS can just find the div that follows a checkbox and heading.
            ;; NOTE: Interweave.js and such safety HTML filterers will remove
            ;; the <input> element!
            ;; NOTE: Actually there is a pure HTML method: <details> and
            ;; <summary>.
            (goto-char content-start)
            (insert "<details>")
            (while (re-search-forward "<h[123456]" nil t)
              (search-backward "<")
              (insert "</details><details open><summary>")
              (re-search-forward "</h[123456]>")
              (insert "</summary>"))
            (goto-char (point-max))
            (insert "</details>")

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
            ;; design-porn over ease-of-use); compact them
            (goto-char (point-min))
            (while (search-forward "<table " nil t)
              (insert "class=\"is-narrow is-striped is-bordered\""))

            ;; REVIEW: This should add roam refs for each subheading that has one.
            ;; How? We made an alist of headings and refs, then refer to that.
            ;; Why? Mainly because of the lw-concept-graph post.
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

            ;; Count # of total links (except links to external sites)
            (goto-char content-start)
            (setq links (cl-loop while (re-search-forward "<a .*?href=." nil t)
                                 unless (looking-at-p "http")
                                 count t))

            ;; REVIEW: this should add 🔗links to headings that have an id
            (goto-char content-start)
            (while (re-search-forward (rx "<h" (group digit)) nil t)
              (when-let ((id (save-excursion
                               (save-match-data
                                 (and (search-forward "id=\"ID-" nil t)
                                      (re-search-forward (rx (not "\"")))
                                      (match-string 0)))))
                         (digit (match-string 1)))
                (search-forward (concat "</h" digit ">"))
                (goto-char (match-beginning 0))
                (insert (concat "<a class=\"easylink\" href=\"#ID-" id "\"> 🔗</a>"))))

            (setq data-for-json
                  `((slug . ,slug)
                    (permalink . ,permalink)
                    (title . ,title)
                    (created . ,created)
                    (updated . ,updated)
                    (wordcount . ,wordcount)
                    (links . ,links)
                    (refs . ,refs)
                    (tags . ,tags)
                    (content . ,(buffer-string)))))

          (when-let ((output-buf (find-buffer-visiting output-path)))
            (kill-buffer output-buf))
          (with-temp-file output-path
            (insert (json-encode data-for-json))))))
     (kill-buffer (current-buffer)))))
