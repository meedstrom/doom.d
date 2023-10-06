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

;; Keep in mind that case-fold-search doesn't affect `equal', and therefore
;; doesn't affect list-comparisons such as `cl-intersection'!
(defvar my-extinct-tags '("drill"  "fc" "anki" "partner" "friends-eyes" "therapist" "eyes-partner" "eyes-therapist" "eyes-diana" "eyes-friend"))
(defvar my-tags-to-avoid-uploading (append my-extinct-tags '("noexport" "archive" "private" "censor")))

;; cache-variable
(defvar my-all-refs nil)

(defun my-publish ()
  "A single command I can use in a child emacs."
  (interactive)
  (require 'ox-publish)
  (switch-to-buffer "*Messages*") ;; for watching it work
  ;; (split-window) ;; in case the Warnings buffer appears
  (cd "/home/kept/roam") ;; for me to quick-search when an id fails to resolve
  (org-publish "my-slipbox-blog" t)
  ;; will probably have to rewrite all attachment/ links to $lib/images or something ...
  ;; or static/, and place them in svelte's static folder
  ;; (org-publish "my-slipbox-blog-images" t)
  )

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
           :exclude-tags ,my-tags-to-avoid-uploading)

          ("my-slipbox-blog-images"
           :base-directory "/tmp/roam/static/"
           :base-extension "png\\|jpg"
           :publishing-directory "/home/kept/pub/static/"
           :publishing-function org-publish-attachment)))

;; Override this thing so info: links don't turn into meaningless <a href>.  I
;; may need more overrides like this for each "special" link type I use, see
;; `org-link-parameters'.
(require 'ol-info)
(defun org-info-export (path desc _format)
  (or desc path))

;; (defconst my-date-regexp (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))
(defvar my-publish-ran-already nil)
(defun my-prep-fn (_)
  "Prepare Emacs and temp files for publishing my website.
Since I intend to run `org-publish' in a subordinate Emacs, this
function is where I can make destructive env changes that I don't
want in my main Emacs."
  (setopt org-mode-hook nil) ;; speeds up publishing
  (setopt org-export-use-babel nil)
  (setopt org-export-with-drawers '(not "logbook" "noexport")) ;; case-insensitive
  (setopt org-export-exclude-tags my-tags-to-avoid-uploading)
  (setopt org-export-with-broken-links nil) ;; links would disappear quietly
  ;; (setopt org-html-self-link-headlines t)
  (setopt org-export-with-smart-quotes nil)
  ;; If we don't set this to "", there will be .html inside some links even
  ;; though I also set "" in the `org-publish-org-to' call.
  (setopt org-html-extension "")
  (setopt org-html-checkbox-type 'unicode) ;; how will it look in eww? test it.
  (setopt org-html-html5-fancy t)
  (setopt case-fold-search t) ;; for all the searches in `my-publish-to-blog'
  (setopt org-inhibit-startup t) ;; from org-publish-org-to
  ;; Love it, but won't apply to every datestamp on the site
  ;; (setopt org-display-custom-times nil)
  (toggle-debug-on-error)
  (toggle-debug-on-quit)

  ;; Switch theme for 2 reasons
  ;; 1. Show me that this is not a normal Emacs
  ;; 2. Syntax-highlight source blocks in a way that looks OK on the web
  (fset 'rainbow-delimiters-mode #'prism-mode)
  (add-hook 'doom-load-theme-hook #'prism-set-colors)
  ;; (load-theme 'doom-rouge)
  (load-theme 'doom-zenburn)
  ;; (load-theme 'doom-monokai-machine)

  ;; For hygiene, ensure that this subordinate emacs syncs nothing to disk
  (cancel-timer my-state-sync-timer)
  (setopt kill-emacs-hook nil)
  (fset 'org-id-locations-save #'ignore)
  (setopt org-roam-db-location "/tmp/org-roam.db")
  (org-roam-db-autosync-mode 0)

  ;; Copy the files to /tmp to work from there
  (shell-command "rm -r /tmp/roam/")
  (shell-command "cp -a /home/kept/roam /tmp/")
  (shell-command "rm -r /tmp/roam/*/logseq/") ;; no logseq backups
  (shell-command "rm -r /tmp/roam/lesswrong-org/")
  (shell-command "shopt -s globstar && rm /tmp/roam/**/*.gpg") ;; no crypts

  ;; Flatten the directory tree (no more subdirs)
  (cl-loop
   for file in (directory-files-recursively "/tmp/roam" "\\.org$" nil)
   unless (equal "/tmp/roam/" (file-name-directory file))
   do (rename-file file "/tmp/roam/"))

  ;; ;; Generate a log of completed tasks my partner can peruse <3
  ;; (setopt org-agenda-files '("/tmp/roam/archive.org"))
  ;; (org-agenda-list)
  ;; (org-agenda-log-mode)
  ;; (org-agenda-archives-mode)
  ;; ;; TODO: use org-agenda-write
  ;; (org-agenda-write "/tmp/roam/todo-log.html")
  ;; (let ((agenda-log (buffer-string)))
  ;;   (with-temp-file "/tmp/roam/todo-log.org"
  ;;     (insert (string-join
  ;;              `(":PROPERTIES:"
  ;;                ":ID: e4c5ea8b-5b06-43c4-8948-3bfe84e8d5e8"
  ;;                ":END:"
  ;;                "#+title: Completed tasks"
  ;;                "#+filetags: :eyes_partner:"
  ;;                "#+date: [2023-10-06]"
  ;;                "#+begin_src"
  ;;                ,agenda-log
  ;;                "#+end_src")
  ;;              "\n"))))

  ;; Generate a log of completed tasks my partner can peruse <3
  (setopt org-agenda-files '("/tmp/roam/archive.org"))
  (org-agenda-list)
  (org-agenda-log-mode)
  (org-agenda-archives-mode)
  (shell-command "rm /tmp/todo-log.html")
  (org-agenda-write "/tmp/todo-log.html")
  (with-temp-file "/tmp/roam/todo-log.org"
    (insert ":PROPERTIES:"
            "\n:ID: e4c5ea8b-5b06-43c4-8948-3bfe84e8d5e8"
            "\n:END:"
            "\n#+title: Completed tasks"
            "\n#+filetags: :eyes_friend:"
            "\n#+date: " (format-time-string "[%F]")
            "\n#+begin_export html"
            "\n")
    (insert-file-contents "/tmp/todo-log.html")
    (delete-region (point) (search-forward "<pre>"))
    (insert "<pre class=\"agenda\"")
    (delete-region (search-forward "</body>") (point-max))
    (insert "</pre>")
    (insert "\n#+end_export"))

  ;; Ensure each post will get a unique ID in the URL
  (cl-loop
   with default-directory = "/tmp/roam"
   for path in (directory-files "/tmp/roam" t "\\.org$")
   as uuid = (my-org-file-id path)
   when uuid do
   (let ((permalink (substring (my-uuid-to-base62 uuid) -7)))
     (when (file-exists-p permalink)
       ;; This has not happened yet
       (error "Probable page ID collision, suggest renewing UUID %s" uuid))
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

  ;; Lookup table used by `my-replace-web-links-with-ref-note-links'
  (setq my-all-refs (org-roam-db-query
                     [:select [ref id title]
                      :from refs
                      :left-join nodes
                      :on (= refs:node-id nodes:id)]))
  )

;; Give each h2...h6 heading an ID attribute that matches its source org-id, if
;; it has one, instead of e.g. "org953031".  That way, hash-links such as
;; #ID-e10bbdfe-1ffb-4c54-9228-2818afdfc5ba will make the web browser jump to
;; that heading.  Thank org-roam for this convenience!  Note that I convert
;; these IDs to base62 later in this file.
(after! ox (require 'org-roam-export))

;; Change some things about the Org files, before org-export does its thing.
(add-hook 'org-export-before-parsing-functions #'my-add-backlinks)
(add-hook 'org-export-before-parsing-functions #'my-replace-web-links-with-ref-note-links)

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
          ;; reverse alphabetic sort (z-a) so that newest daily-pages on top
          (dolist (link (--sort (string-lessp (cdr other) (cdr it))
                                (append backlinks reflinks)))
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
    (save-excursion
      ;; TODO: use org-link-any-re
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

(defun my-generate-completed-todo (&rest _)
  )


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

;; test: (my-strip-hashlink-if-same-as-permalink "0vwRV27mRVLFd6yoyUM0PI/some-slug#ID-10b59a2a-bf95-4f20-9b4f-f27e23e51f46")
;; test: (my-strip-hashlink-if-same-as-permalink "../0vwRV27mRVLFd6yoyUM0PI/some-slug#ID-10b59a2a-bf95-4f20-9b4f-f27e23e51f46")
(defun my-strip-hashlink-if-same-as-permalink (link)
  (if-let* ((hash-pos (string-search "#" link))
            (hash-part (substring link (1+ hash-pos)))
            (base-part (substring link 0 hash-pos))
            (same (string-search hash-part base-part)))
      ;; Cut off the hash
      base-part
    ;; Keep the whole link
    link))

;; Struggled so long looking for a hook that would work like the old
;; before-export-hook.  Let this be a lesson: the Emacs hook system exists to
;; let you subtly modify a function IN THE MIDDLE of its body.  We never
;; actually need simple before-hooks nor after-hooks, since in such a simple
;; case it is always possible to use `add-function' or write a wrapper such as
;; the following wrapper around `org-publish-org-to'.

(defun my-publish-to-blog (plist filename pub-dir)
  (redisplay) ;; I like watching programs work    
  ;; Fast early check that avoids loading org-mode
  (let (title id created tags)
    (with-temp-buffer
      (insert-file-contents filename)
      (forward-line 10)  ;; should be enough to grab the metadata
      (delete-region (point) (point-max))

      (goto-char (point-min))
      (setq title (when (search-forward "#+title: " nil t)
                    (delete-horizontal-space)
                    (buffer-substring (point) (line-end-position))))

      (goto-char (point-min))
      (setq id (when (search-forward ":id:" nil t)
                 (delete-horizontal-space)
                 (buffer-substring (point) (line-end-position))))

      (goto-char (point-min))
      (setq created (when (search-forward "#+date: [" nil t)
                      (buffer-substring (point) (+ 10 (point)))))

      (goto-char (point-min))
      (setq tags (if (search-forward "#+filetags: " nil t)
                     (thread-first (buffer-substring (point) (line-end-position))
                                   (string-trim)
                                   (string-split ":" t)
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
        ;; The original export-function.  Thy magic ist magnifique!
        (org-publish-org-to 'html filename "" plist pub-dir)

        ;; Customize the resulting HTML file and wrap it in a JSON object
        (let* ((output-path (org-export-output-file-name "" nil pub-dir))
               (slug (string-replace pub-dir "" output-path))
               ;; NOTE: All pages get a permalink, even daily-pages despite the
               ;; also deterministic slug in their case.  The org-id is
               ;; everything: it underpins how my site will resolve hash-links
               ;; that are no longer on the page where the user bookmarked them,
               ;; for example.  Don't be tempted to think it's ugly.  The
               ;; daily-pages are rarely meant for consumption by the public
               ;; anyway.
               (permalink (-last-item (split-string pub-dir "/" t)))
               (updated (format-time-string "%F" (f-modification-time filename)))
               (wordcount (save-excursion
                            (if (re-search-forward "^[^#:\n]" nil t)
                                (count-words (point) (point-max))
                              0)))

               (refs (save-excursion
                       (when (search-forward ":roam_refs: " nil t)
                         ;; Only top-level refs; not refs from a subheading
                         (unless (search-backward "\n*" nil t)
                           (buffer-substring (point) (line-end-position))))))
               (subheadings-refs
                (save-excursion
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
               (hidden (car (-intersection '("eyes_therapist"
                                             "eyes_partner"
                                             "eyes_friend") tags)))
               (created-fancy (format-time-string (car org-timestamp-custom-formats)
                                                  (date-to-time created)))
               (updated-fancy (format-time-string (car org-timestamp-custom-formats)
                                                  (date-to-time updated)))
               (links 0)
               (m1 (make-marker))
               (data-for-json nil))
          (with-temp-buffer

            ;; 03 Insert roam_refs before the post body
            (when refs
              (insert "<p>Ref: "  )
              (dolist (ref (split-string refs))
                (setq ref (string-replace "\"" "" ref)) ;; in case I wrapped it in quotes
                (insert " <a href=\"" ref "f\">" (replace-regexp-in-string "http.?://" "" ref) "</a> "))
              (insert "</p>"))

            ;; 05 Insert the post body: the HTML produced by Org-export
            (insert-file-contents output-path)

            ;; 08 Set `content-start' after ToC, if there is one
            (setq content-start (goto-char (point-min)))
            (when (search-forward "<div id=\"table-of-contents\" " nil t)
              (search-forward "</div>")
              (search-forward "</div>")
              (setq content-start (point)))

            ;; 09
            ;; MUST BEFORE 10
            ;; Give links a CSS class depending on target note's tags
            (goto-char content-start)
            (while (re-search-forward "<a [^>]*?href=.[^\"]*?#ID-" nil t)
              (set-marker m1 (point))
              (when-let* ((uuid (buffer-substring (point)
                                                  (1- (search-forward "\""))))
                          (target-tags (-flatten
                                        (org-roam-db-query
                                         `[:select [tag]
                                           :from tags
                                           :where (= node-id ,uuid)])))
                          (classes
                           (-non-nil
                            (cons (when (-intersection target-tags
                                                       my-tags-to-avoid-uploading)
                                    "private")
                                  (-intersection target-tags
                                                 '("eyes_therapist"
                                                   "eyes_partner"
                                                   "eyes_friend"
                                                   "stub"))))))
                (search-backward "<a ")
                (forward-char 3)
                (insert "class=\"" (string-join classes " ") "\" "))
              (goto-char (marker-position m1)))

            ;; 10
            ;; Replace all UUIDv4 with truncated base62 translations.
            (goto-char (point-min)) ;; gotcha! include the ToC
            ;; (while (re-search-forward (rx (regexp my-id-re) "ID-") nil t)
            (while (re-search-forward "[\"#]ID-" nil t)
              (let* ((beg (point))
                     (end (1- (save-excursion (search-forward "\""))))
                     (uuid (buffer-substring beg end)))
                (delete-region (- beg 3) end)
                (insert (substring (my-uuid-to-base62 uuid) -7))))

            ;; 14
            ;; DEPENDS ON 10
            ;; For all links, remove the lengthy hash-part of the link (i.e. the
            ;; bit after the # character in http://.../PAGE-ID/LINK#ORG-ID) if the
            ;; ORG-ID matches PAGE-ID anyway (i.e. it's a file-level id)
            ;; (goto-char content-start)
            ;; (while (re-search-forward "<a .*?href=\"" nil t)
            ;;   (let* ((beg (point))
            ;;          (end (1- (save-excursion (search-forward "\""))))
            ;;          (link (buffer-substring beg end)))
            ;;     (delete-region beg end)
            ;;     (insert (my-strip-hashlink-if-same-as-permalink link))))

            ;; 16
            ;; Implement collapsible sections
            (unless (member "logseq" tags)
              (goto-char (point-min))
              ;; Preserve the ToC div, but remove its pointless inner div
              (when (search-forward "<div id=\"table-of-contents\"" nil t)
                (re-search-forward "<div id=\"text-table-of-contents\".*?>")
                (replace-match "")
                (search-forward "</div>\n</div>")
                (replace-match "</div>"))
              ;; First strip all non-"outline" div tags and their
              ;; hard-to-identify anonymous closing tags
              (while (search-forward "<div" nil t)
                (let ((beg (match-beginning 0)))
                  (unless (re-search-forward " id=\".*?\" class=\"outline-[123456]\"" (line-end-position) t)
                    (delete-region beg (search-forward ">"))
                    (search-forward "</div>")
                    (replace-match "")
                    (goto-char (1+ beg)))))
              (goto-char (point-min))
              ;; Now turn all remaining <div> into <details>
              (while (re-search-forward "<div .*?>" nil t)
                (replace-match "<details open><summary>")
                (re-search-forward "</h[123456]>")
                (insert "</summary>"))
              (goto-char (point-min))
              (while (search-forward "</div>" nil t)
                (replace-match "</details>")))

            ;; 19 Enable a very different stylesheet for pages tagged "logseq"
            (when (member "logseq" tags)
              (goto-char content-start)
              ;; no divs
              (while (re-search-forward "</?div.*?>" nil t)
                (replace-match ""))
              (goto-char content-start)
              (insert "<div class=\"logseq\">")
              (goto-char (point-max))
              (insert "</div>"))

            ;; 26
            ;; MUST BEFORE 30
            ;; Count # of total links (except links to external sites)
            (goto-char content-start)
            (setq links (cl-loop while (re-search-forward "<a .*?href=." nil t)
                                 unless (looking-at-p "http")
                                 count t))

            ;; 30
            ;; Add 🔗links to headings that have a permanent id
            (goto-char content-start)
            (while (re-search-forward (rx "<h" (group (any "23456"))) nil t)
              (when-let ((digit (match-string 1))
                         (beg (match-beginning 0))
                         (bound (save-excursion
                                  (save-match-data
                                    (search-forward ">"))))
                         (id (save-excursion
                               (save-match-data
                                 (and (search-forward "id=\"" bound t)
                                      (re-search-forward (rx (* (not "\""))))
                                      (match-string 0)))))
                         (is-deterministic (not (string-prefix-p "org" id))))
                (search-forward (concat "</h" digit ">"))
                (goto-char (match-beginning 0))
                (insert (concat "<a class=\"easylink\" href=\"#" id "\"> 🔗</a>"))))

            ;; 44
            ;; Org-export doesn't replace triple-dash in all situations (like in
            ;; a heading or when it butts up against a link on a newline), so
            ;; force it.  I'm pretty sure it won't break anything...
            (goto-char content-start)
            (while (search-forward "---" nil t)
              (replace-match "&mdash;"))
            ;; A little more risky but I'm hoping it's fine.  Situations where we
            ;; might not want to transform a double-dash:
            ;; - css variables in code blocks (i have none)
            ;; - a code block showing this very code (i have none)
            (goto-char content-start)
            (while (search-forward "--" nil t)
              (replace-match "&ndash;"))

            ;; 50
            ;; Remove all local images.  Temporary solution until I fix image
            ;; upload.  Broken image-references break svelte prerendering.
            (goto-char content-start)
            (while (re-search-forward "<img src=\"[^h]" nil t)
              (delete-region (match-beginning 0) (search-forward " />")))
            
            ;; 55
            ;; Wrap all tables for horizontal scrollability
            (goto-char content-start)
            (while (search-forward "<table" nil t)
              (goto-char (match-beginning 0))
              (insert "<div class=\"table-container\">")
              (search-forward "</table>")
              (insert "</div>"))

            ;; 61
            ;; Declutter the HTML a bit
            (goto-char content-start)
            (while (search-forward " class=\"org-ul\"" nil t)
              (replace-match ""))

            (setq data-for-json
                  `((slug . ,slug)
                    (permalink . ,permalink)
                    (title . ,title)
                    (created . ,created)
                    (updated . ,updated)
                    (created_fancy . ,created-fancy)
                    (updated_fancy . ,updated-fancy)
                    (wordcount . ,wordcount)
                    (links . ,links)
                    (tags . ,tags)
                    (hidden . ,hidden)
                    (content . ,(buffer-string)))))

          (when-let ((output-buf (find-buffer-visiting output-path)))
            (kill-buffer output-buf))
          (with-temp-file output-path
            (insert (json-encode data-for-json))))))
     (kill-buffer (current-buffer)))))
