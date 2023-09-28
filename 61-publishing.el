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
;; neither list-comparisons such as `-intersection'!
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
  (org-publish "my-slipbox-blog" t))

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
  (setopt org-export-with-broken-links nil) ;; links would disappear quietly
  (setopt org-export-with-smart-quotes nil)
  (setopt org-html-checkbox-type 'ascii)
  (setopt org-html-extension "") ;; maybe now?
  ;; (setopt org-html-checkbox-type 'unicode)
  (setopt org-html-html5-fancy t)
  (setopt case-fold-search t) ;; for all the searches in `my-publish-to-blog'
  (setopt org-inhibit-startup t) ;; from org-publish-org-to
  ;; Love it, but won't apply to every datestamp on the site
  (setopt org-display-custom-times nil)
  (toggle-debug-on-error)
  (toggle-debug-on-quit)

  ;; Switch theme for 2 reasons
  ;; 1. Show me that this is not a normal Emacs
  ;; 2. Syntax-highlight source blocks in a way that looks OK on the web
  (load-theme 'doom-rouge)
  ;; (load-theme 'doom-monokai-machine)
  ;; Problem: the colors get super weird unlike in actual Emacs
  ;; (fset 'rainbow-delimiters-mode #'prism-mode)
  (fset 'rainbow-delimiters-mode #'ignore)

  ;; For hygiene, ensure that this subordinate emacs syncs nothing to disk
  (cancel-timer my-state-sync-timer)
  (setopt kill-emacs-hook nil)
  (fset 'org-id-locations-save #'ignore)
  (setopt org-roam-db-location "/tmp/org-roam.db")
  (org-roam-db-autosync-mode 0)

  ;; Copy the files to /tmp to work from there
  (shell-command "rm -rf /tmp/roam/")
  (shell-command "cp -a /home/kept/roam /tmp/")
  (shell-command "rm -rf /tmp/roam/*/logseq/") ;; no logseq backups
  (shell-command "rm -rf /tmp/roam/lesswrong-org/")
  (shell-command "shopt -s globstar && rm -rf /tmp/roam/**/*.gpg") ;; no crypts

  ;; Flatten the directory tree (no more subdirs)
  (cl-loop
   for file in (directory-files-recursively "/tmp/roam" "\\.org$" nil)
   unless (equal "/tmp/roam/" (file-name-directory file))
   do (rename-file file "/tmp/roam/"))

  ;; Ensure each post will get an unique ID in the URL
  (cl-loop
   with default-directory = "/tmp/roam"
   for path in (directory-files "/tmp/roam" t "\\.org$")
   as uuid = (my-org-file-id path)
   when uuid
   do (let ((permalink (substring (my-uuid-to-base62 uuid) -7)))
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

  ;; Lookup table of refs, used by `my-replace-web-links-with-ref-note-links'
  (setq my-all-refs (org-roam-db-query
                     [:select [ref id title]
                      :from refs
                      :left-join nodes
                      :on (= refs:node-id nodes:id)])))

;; Give each h2...h6 heading an ID attribute that matches its source org-id, if
;; it has one, instead of e.g. "org953031".  That way, hash-links such as
;; #ID-e10bbdfe-1ffb-4c54-9228-2818afdfc5ba will make the web browser jump to
;; that heading.  Thanks org-roam for this convenience!  Note that I convert
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
          (dolist (link (--sort (string-lessp (cdr it) (cdr other))
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
        ;; The original export-function.  Do thy magic!
        (org-publish-org-to 'html filename "" plist pub-dir)

        ;; Customize the resulting HTML file and wrap it in a JSON object
        (let* ((output-path (org-export-output-file-name "" nil pub-dir))
               (slug (string-replace pub-dir "" output-path))
               (m1 (make-marker))
               (permalink
                ;; if (string-search "daily" pub-dir)
                ;; For dailies, reuse the slug
                ;; (concat "daily/" slug)
                ;; For other posts, the subdir is its unique id
                (-last-item (split-string pub-dir "/" t)))
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
               (links 0)
               (data-for-json nil))
          (with-temp-buffer
            (goto-char (point-min))

            (insert "<h1 id=\"title\">" title "</h1>")

            ;; 03 Insert roam_refs before the post body
            (when refs
              (insert "<p>Ref: "  )
              (dolist (ref (split-string refs))
                (setq ref (string-replace "\"" "" ref)) ;; in case I wrapped it in quotes
                (insert " <a href=\"" ref "f\">" (replace-regexp-in-string "http.?://" "" ref) "</a> "))
              (insert "</p>"))

            ;; 05 Insert the post body: the HTML produced by Org-export
            (insert-file-contents output-path)

            ;; 08 Set content-start after ToC, if there is one
            (goto-char (point-min))
            (setq content-start (point))
            (when (search-forward "<div id=\"table-of-contents\" " nil t)
              ;; ;; TODO: Remove when I migrate to Svelte
              ;;   (insert " class=\"box\"") ;; bulma CSS
              (search-forward "</div>")
              (search-forward "</div>")
              (setq content-start (point)))

            ;; 09
            ;; MUST BEFORE 10,14
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
                                           :where (= node-id ,uuid)]))))
                (let ((private (when (-intersection target-tags
                                                    my-tags-to-avoid-uploading)
                                 "private")))
                  (search-backward "<a ")
                  (forward-char 3)
                  (insert " class=\""
                          (or private
                              (car (member "eyes_therapist" target-tags))
                              (car (member "eyes_partner" target-tags))
                              (car (member "eyes_friend" target-tags))
                              "public")
                          (if (member "stub" target-tags)
                              " stub"
                            "")
                          "\" ")))
              (goto-char (marker-position m1)))

            ;; 10
            ;; Replace all UUIDv4 with truncated base62 translations.
            (goto-char content-start)
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
            ;; bit after the # character in LINK#ORG-ID) if the org-id points to
            ;; a file-level id anyway
            (goto-char content-start)
            ;; (while (re-search-forward "<a +?href=\"" nil t)
            (while (re-search-forward "<a .*?href=\"" nil t)
              (let* ((beg (point))
                     (end (1- (save-excursion (search-forward "\""))))
                     (link (buffer-substring beg end)))
                (delete-region beg end)
                (insert (my-strip-hashlink-if-same-as-permalink link))))

            ;; 16 Implement collapsible sections
            ;; Preserve the ToC div, but remove its pointless inner div
            (unless (member "logseq" tags)
              (goto-char (point-min))
              (when (search-forward "<div id=\"table-of-contents\"" nil t)
                (re-search-forward "<div id=\"text-table-of-contents\".*?>")
                (replace-match "")
                (search-forward "</div>\n</div>")
                (replace-match "</div>"))
              ;; First strip all non-outline div tags and their pesky anonymous
              ;; closing tags
              (while (search-forward "<div" nil t)
                (let ((beg (match-beginning 0)))
                  (unless (re-search-forward " id=\".*?\" class=\"outline-[123456]\"" (line-end-position) t)
                    (delete-region beg (search-forward ">"))
                    (search-forward "</div>")
                    (replace-match "")
                    (goto-char (1+ beg)))))
              (goto-char (point-min))
              ;; Now change all remaining <div> to <details>
              (while (re-search-forward "<div .*?>" nil t)
                ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
                ;; Screen readers cannot find headings wrapped in <summary>, since
                ;; the aria role changes to "button", so revert the role.  It's
                ;; more important to know that they are headings, than that they
                ;; are clickable.
                (replace-match "<details open><summary role=\"heading\"")
                (unless (looking-at "\n<h\\([123456]\\)")
                  (error "Expected to be at a heading tag"))
                (insert " aria-level=\"" (match-string 1) "\">")
                (re-search-forward "</h[123456]>")
                (insert "</summary>"))
              (goto-char (point-min))
              (while (search-forward "</div>" nil t)
                (replace-match "</details>")))

            ;; 19 Enable a very different stylesheet for pages tagged "logseq"
            (when (member "logseq" tags)
              (goto-char content-start)
              (while (re-search-forward "</?div.*?>" nil t)
                (replace-match ""))
              (goto-char content-start)
              (insert "<div class=\"logseq\">")
              (goto-char (point-max))
              (insert "</div>"))

            ;; 26
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

            (setq data-for-json
                  `((slug . ,slug)
                    (permalink . ,permalink)
                    (title . ,title)
                    (created . ,created)
                    (updated . ,updated)
                    (wordcount . ,wordcount)
                    (links . ,links)
                    (tags . ,tags)
                    (content . ,(buffer-string)))))

          (when-let ((output-buf (find-buffer-visiting output-path)))
            (kill-buffer output-buf))
          (with-temp-file output-path
            (insert (json-encode data-for-json))))))
     (kill-buffer (current-buffer)))))
