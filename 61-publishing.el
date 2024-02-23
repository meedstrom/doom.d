;; -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Martin Edström
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
(defvar my-deprecated-tags '("drill"  "fc" "anki" "partner" "friends-eyes" "therapist" "eyes-partner" "eyes-therapist" "eyes-diana" "eyes-friend" "eyes_therapist" "eyes_partner" "eyes_friend"))
(defvar my-tags-to-avoid-uploading (append my-deprecated-tags '("noexport" "archive" "private" "censor")))
(defvar my-tags-for-hiding '("gri" "shrink" "privy" "lover" "fren"))

(defvar my-refs-cache nil)

(defun my-publish ()
  "A single command I can use in a child emacs."
  (interactive)
  (require 'ox-publish)
  (switch-to-buffer "*Messages*") ;; for watching it work
  ;; (split-window) ;; in case the Warnings buffer appears
  (cd "/home/kept/roam") ;; for me to quick-search when an id fails to resolve
  ;; (shell-command "rm -r /tmp/roam/")
  (shell-command "rm -r /tmp/entries/")
  ;; (mkdir "/tmp/roam")
  (mkdir "/tmp/entries" t)
  (org-publish "my-slipbox-blog" t)
  (org-publish "my-slipbox-blog-attachments" t)
  ;; ensure it's gone from recentf so I don't accidentally edit these instead
  ;; of the originals
  (shell-command "rm -r /tmp/roam")
  (my-check-id-collisions)
  (my-make-atom-feed "/home/kept/pub/posts.atom" "/tmp/entries/")
  (f-write (json-encode-alist my-id-old-new-alist)
           'utf-8 "/home/kept/pub/idMappings.json")
  (f-write (json-encode-alist my-heading-locations)
           'utf-8 "/home/kept/pub/idsInPages.json"))

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

          ("my-slipbox-blog-attachments"
           :base-directory "/tmp/roam/attachments/"
           :base-extension "png\\|jpg\\|gif"
           :publishing-directory "/home/kept/pub/attachments/"
           :publishing-function org-publish-attachment)))

;; Override so the special link type info: won't get exported to an empty
;; <a href>.  I may need more overrides like this for each special link type,
;; see `org-link-parameters'.
(require 'ol-info)
(el-patch-defun org-info-export (path desc _format)
  (or desc path))

(defvar my-heading-locations nil)

(defvar my-ids nil
  "Database for checking ID collisions.
This is not as important as it sounds.  I am using 4-char IDs,
which haven't had a collision yet.  But maybe in the future I
move to 3-char IDs, and that's what I like to check.  By
futureproofing in this way, I won't have to set up redirects
beyond a general one that cuts one char off the ID.")

(defun my-check-id-collisions ()
  (interactive)
  (cl-loop for id-uuids in my-ids
           as uuids = (-distinct (cdr id-uuids))
           when (> (length uuids) 1)
           do (message "These uuids make same page-id: %s"
                       uuids)))

;; (defconst my-date-regexp (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))
(defvar my-publish-ran-already nil)
(defun my-prep-fn (_)
  "Prepare Emacs and temp files for publishing my website.
Since I intend to run `org-publish' in a subordinate Emacs, this
function is where I can make destructive env changes that I don't
want in my main Emacs."
  ;; Speed up publishing
  (setopt org-mode-hook nil)
  (gcmh-mode 0)
  (setopt gc-cons-threshold most-positive-fixnum)
  (fset 'org-publish-write-cache-file #'ignore)
  (advice-remove 'after-find-file #'doom--shut-up-autosave-a)
  ;; (undefadvice! '+org--fix-async-export-a :around '(org-export-to-file org-export-as))
  ;; (undefadvice! +org-babel-disable-async-maybe-a :around #'ob-async-org-babel-execute-src-block)

  ;; Not sure it helps speed at all
  (global-emojify-mode 0)
  (apheleia-global-mode 0)
  ;; (solaire-global-mode 0)
  (ws-butler-global-mode 0)
  (smartparens-global-mode 0)
  (setq whitespace-global-modes nil)
  (projectile-global-mode 0)
  ;; (remove-hook  'git-gutter-mode)

  (setopt org-export-use-babel nil)
  (setopt org-export-with-drawers '(not "logbook" "noexport")) ;; case-insensitive
  (setopt org-export-exclude-tags my-tags-to-avoid-uploading)
  (setopt org-export-with-broken-links nil) ;; links would disappear quietly
  (setopt org-export-with-smart-quotes nil)
  (setopt org-export-with-todo-keywords nil)
  (setopt org-export-headline-levels 5) ;; go all the way to <h6> before making <li>
  ;; If we don't set this to "", there will be .html inside some links even
  ;; though I also set "" in the `org-publish-org-to' call.
  (setopt org-html-extension "")
  (setopt org-html-checkbox-type 'unicode) ;; how will it look in eww? test it.
  (setopt org-html-self-link-headlines t)
  (setopt org-html-html5-fancy t)
  ;; why does it skip environments like \begin{align}?
  ;; (setopt org-html-with-latex 'verbatim)
  (setopt org-html-with-latex 'html) ;; use `org-latex-to-html-convert-command'
  (setopt org-latex-to-html-convert-command "node /home/kept/pub/texToMathML.js '%i'")
  (setopt case-fold-search t) ;; for all the searches in `my-publish-to-blog'
  (setopt org-inhibit-startup t) ;; from org-publish-org-to
  (toggle-debug-on-error)
  (toggle-debug-on-quit)

  ;; Switch theme for 2 reasons
  ;; 1. Show me that this is not a normal Emacs
  ;; 2. Syntax-highlight source blocks in a way that looks OK on the web
  ;; (load-theme 'doom-rouge)
  ;; (load-theme 'doom-zenburn)
  (load-theme 'doom-monokai-machine)

  (use-package! prism
    :config
    (setopt prism-comments nil)
    (setopt prism-desaturations nil)
    (fset 'rainbow-delimiters-mode #'prism-mode)
    (add-hook 'typescript-mode-hook #'prism-mode)
    (add-hook 'typescript-tsx-mode-hook #'prism-mode)
    (add-hook 'js-base-mode-hook #'prism-mode))

  ;; For hygiene, ensure that this subordinate emacs syncs nothing to disk
  (cancel-timer my-state-sync-timer)
  (setopt kill-emacs-hook nil)
  (fset 'org-id-locations-save #'ignore)
  (setopt org-roam-db-location "/tmp/org-roam.db")
  (org-roam-db-autosync-mode 0)

  ;; Copy the files to /tmp to work from there
  (shell-command "rm -r /tmp/roam/")
  (shell-command "cp -a /home/kept/roam /tmp/")
  (shell-command "cp -a /home/sync-phone/beorg/* /tmp/roam/")
  (shell-command "rm -r /tmp/roam/*/logseq/") ;; logseq auto-backups
  (shell-command "rm -r /tmp/roam/lesswrong-org/")
  (shell-command "rm /tmp/roam/*sync-conflict*") ;; syncthing
  (shell-command "shopt -s globstar && rm /tmp/roam/**/*.gpg") ;; no crypts

  ;; Flatten the directory tree (no more subdirs)
  (cl-loop
   for file in (directory-files-recursively "/tmp/roam" "\\.org$" nil)
   unless (equal "/tmp/roam/" (file-name-directory file))
   do (rename-file file "/tmp/roam/"))

  ;; ;; Generate a log of completed tasks my partner can peruse <3
  (my-generate-todo-log "/tmp/roam/todo-log.org")

  ;; Ensure each post will get a unique ID in the URL
  (cl-loop
   with default-directory = "/tmp/roam"
   for path in (directory-files "/tmp/roam" t "\\.org$")
   as uuid = (my-org-file-id path)
   when uuid do
   (let ((pageid (my-uuid-to-pageid uuid))
         (old-id (my-uuid-to-pageid-old2 uuid)))
     (push uuid (alist-get pageid my-ids nil nil #'equal))
     ;; record old ID
     (unless (assoc old-id my-id-old-new-alist)
       (push (cons old-id pageid) my-id-old-new-alist))
     (when (file-exists-p pageid)
       (error "Probable page ID collision, suggest renewing UUID %s" uuid))
     (mkdir pageid t)
     (rename-file path (concat pageid "/"))))

  ;; Tell `org-id-locations' and the org-roam DB about the new directory.
  (setopt org-roam-directory "/tmp/roam/")
  (setopt org-agenda-files '("/tmp/roam/"))
  (unless my-publish-ran-already
    (org-roam-update-org-id-locations)
    (org-roam-db-sync)
    (setq my-publish-ran-already t))
  (fset 'org-id-update-id-locations #'ignore) ;; stop triggering during publish

  ;; Lookup table used by `my-replace-web-links-with-ref-note-links'
  (setq my-refs-cache (org-roam-db-query
                       [:select [ref id title]
                        :from refs
                        :left-join nodes
                        :on (= refs:node-id nodes:id)])))

;; Give each h2...h6 heading an ID attribute that matches its source org-id, if
;; it has one, instead of e.g. "org953031".  That way, hash-links such as
;; #ID-e10bbdfe-1ffb-4c54-9228-2818afdfc5ba will make the web browser jump to
;; that heading.  Thank org-roam for this convenience!  Note that I convert
;; these IDs to base62 later in this file.
(after! ox (require 'org-roam-export))

(defvar my-id-old-new-alist nil)

;; Change some things about the Org files, before org-export does its thing.
(add-hook 'org-export-before-parsing-functions #'my-add-backlinks)
(add-hook 'org-export-before-parsing-functions #'my-replace-web-links-with-ref-note-links)
(add-hook 'org-export-before-parsing-functions #'my-replace-datestamps-with-links)

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

;; Struggled so long looking for a hook that would work like the old
;; before-export-hook.  Let this be a lesson: the Emacs hook system exists to
;; let you subtly modify a function IN THE MIDDLE of its body.  We never
;; actually need simple before-hooks nor after-hooks, since in such a simple
;; case it is always possible to use `add-function' or write a wrapper such as
;; the following wrapper around `org-publish-org-to'.

(defun my-publish-to-blog (plist filename pub-dir)
  (redisplay) ;; I like watching programs work    
  ;; Fast early check that avoids loading org-mode
  (let (title id created tags link-in-heading)
    (with-temp-buffer
      (insert-file-contents filename)
      (forward-line 10)  ;; should be enough to grab the metadata
      ;; protect ourselves against mentions of #+title etc in article text
      (delete-region (point) (point-max))

      (goto-char (point-min))
      (setq title (when (search-forward "\n#+title: " nil t)
                    (delete-horizontal-space)
                    (buffer-substring (point) (line-end-position))))

      (goto-char (point-min))
      (setq id (when (search-forward "\n:id: " nil t)
                 (delete-horizontal-space)
                 (buffer-substring (point) (line-end-position))))

      (goto-char (point-min))
      (setq created (when (search-forward "\n:created: " nil t)
                      (delete-horizontal-space)
                      (cl-assert (not (looking-at-p "\n")))
                      (buffer-substring (+ 1 (point)) (+ 11 (point)))))

      (goto-char (point-min))
      (setq tags (if (search-forward "\n#+filetags: " nil t)
                     (thread-first (buffer-substring (point) (line-end-position))
                                   (string-trim)
                                   (string-split ":" t)
                                   (sort #'string-lessp))
                   '("")))

      ;; Bc of `org-html-self-link-headlines', disallow links in headings.
      (goto-char (point-min))
      (while (re-search-forward "^[[:space:]]*?\\*" nil t)
        (when (search-forward "[[" (line-end-position) t)
          ;; Some exceptions where it's OK
          (unless (or (member "logseq" tags)
                      (re-search-backward "TODO\\|DONE" (line-beginning-position) t)
                      (warn "LINK INSIDE A HEADING: %s" filename))))))

    (cond
     ;; Skip exporting if we won't use the result
     ((not title)
      (warn "TITLE MISSING: %s" filename))
     ((not created)
      (warn "CREATION-DATE MISSING: %s" filename))
     ((not id)
      (warn "ID MISSING: %s" filename))
     ((-intersection tags my-deprecated-tags)
      (warn "OUTDATED TAG FOUND: %s" filename))
     ((-intersection tags my-tags-to-avoid-uploading)
      (message "Found exclude-tag, excluding: %s" filename))
     ((not (-intersection tags (cons "pub" my-tags-for-hiding)))
      (message "Not selected for publishing"))
     ;; ensure lowercase everywhere so `-intersection' works
     ((let ((case-fold-search nil))
        (string-match-p "[[:upper:]]" (string-join tags)))
      (warn "UPPERCASE IN TAG FOUND: %s" filename))

     ;; OK, export
     (t
      (with-current-buffer (or (find-buffer-visiting filename)
                               (find-file-noselect filename))
        (let ((org-html-self-link-headlines (not (member "logseq" tags))))
          ;; The original export-function.  By thy might, Bastien/Carsten/&c.
          (org-publish-org-to 'html filename "" plist pub-dir))

        ;; Customize the resulting HTML file and pack it into a JSON object
        (let* ((output-path (org-export-output-file-name "" nil pub-dir))
               (slug (string-replace pub-dir "" output-path))
               ;; NOTE: All pages get a unique permalink, even daily-pages
               ;; despite the also deterministic slug in their case.  The org-id
               ;; is everything: it underpins how my site will resolve
               ;; hash-links that are no longer on the page where the user
               ;; bookmarked them, for example.  Don't be tempted to think it's
               ;; ugly.  The daily-pages are rarely meant for consumption by the
               ;; public anyway.
               (permalink (-last-item (split-string pub-dir "/" t)))
               (updated (save-excursion
                          (goto-char (point-min))
                          (when (search-forward "\n#+date: [" nil t)
                            (buffer-substring (point) (1- (line-end-position))))))
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
               (hidden (car (-intersection my-tags-for-hiding tags)))
               (description (save-excursion
                              (goto-char (point-min))
                              (when (search-forward "\n#+subtitle: " nil t)
                                (buffer-substring (point) (line-end-position)))))
               (created-fancy
                (format-time-string (car org-timestamp-custom-formats) (date-to-time created)))
               (updated-fancy
                (when updated
                  (format-time-string (car org-timestamp-custom-formats) (date-to-time updated))))
               (links 0)
               (m1 (make-marker))
               (content-start (make-marker))
               (content nil)
               (data-for-json nil))
          (with-temp-buffer

            ;; 04
            ;; Insert title
            ;; Would have given the href a normal hash-link, but the Note
            ;; component is sometimes rendered elsewhere (the /recent page),
            ;; where it's useful to get a full link.
            ;; (insert "\n<h1 id=\"" permalink "\">"
            ;; "<a href=\"/" permalink "/" slug "\">" title
            ;; "</a></h1>")

            ;; 05 Insert the post body: the HTML produced by Org-export
            ;; (insert "\n<div class=\"e-content "
            ;;         (if (member "logseq" tags)
            ;;             "logseq"
            ;;           "notlogseq")
            ;;         "\">")
            (insert-file-contents output-path)
            ;; (goto-char (point-max))
            ;; (insert "\n</div>")

            ;; 06
            ;; Set `content-start' after ToC, if there is one
            (goto-char (point-min))
            (when (search-forward "<div id=\"table-of-contents\" " nil t)
              (search-forward "</div>\n</div>"))
            (set-marker content-start (point))

            ;; 09
            ;; MUST BEFORE 10
            ;; Give links a CSS class depending on target note's tags
            (goto-char (marker-position content-start))
            (while (re-search-forward "<a [^>]*?href=.[^\"]*?#ID-" nil t)
              (set-marker m1 (point))
              (let ((is-heading (save-excursion
                                  (search-backward "<a ")
                                  (search-backward "<")
                                  (looking-at-p "h")))
                    (uuid (buffer-substring (point)
                                            (1- (search-forward "\"")))))
                ;; do not give heading self-links a class since I don't want to
                ;; style them
                (unless is-heading
                  (when-let ((target-tags (-flatten
                                           (org-roam-db-query
                                            `[:select [tag]
                                              :from tags
                                              :where (= node-id ,uuid)]))))
                    (search-backward "<a ")
                    (forward-char 3)
                    (insert "class=\"" (string-join target-tags " ") "\" "))))
              (goto-char (marker-position m1)))

            ;; 10
            ;; Replace all UUID with my shortened form.
            (goto-char (point-min)) ;; gotcha! include the ToC
            (while (re-search-forward "[\"#]ID-" nil t)
              (let* ((beg (point))
                     (end (1- (save-excursion (search-forward "\""))))
                     (uuid (buffer-substring beg end))
                     (old-id (my-uuid-to-pageid-old2 uuid))
                     (new-id (my-uuid-to-pageid uuid)))
                (delete-region (- beg 3) end)
                ;; If point is on a heading, record that this heading ID is to
                ;; be found on this page, for automagic redirects.
                (when (looking-back "\"" (1- (point)))
                  (unless (assoc new-id my-heading-locations)
                    (push (cons new-id permalink) my-heading-locations)))
                (insert new-id)
                ;; Record old ID for redirects on the website
                (unless (assoc old-id my-id-old-new-alist)
                  (push (cons old-id new-id) my-id-old-new-alist))
                ;; (setf (alist-get (substring (my-uuid-to-base62 uuid) -4)
                ;;                  my-id-old-new-alist nil nil #'equal)
                ;;       new-id)
                ;; Record ID to check for collisions
                (push uuid (alist-get new-id my-ids nil nil #'equal))))

            ;; 14
            ;; DEPENDS ON 10
            ;; For all links, remove the lengthy hash-part of the link (i.e. the
            ;; bit after the # character in http://.../PAGE-ID/LINK#ORG-ID) if the
            ;; ORG-ID matches PAGE-ID anyway (i.e. it's a file-level id)
            (goto-char content-start)
            (while (re-search-forward "<a .*?href=\"" nil t)
              (let* ((beg (point))
                     (end (1- (save-excursion (search-forward "\""))))
                     (link (buffer-substring beg end)))
                (delete-region beg end)
                (insert (my-strip-hashlink-if-same-as-permalink link))))

            ;; 16
            ;; Give the ToC div a class, and remove its pointless inner div
            (goto-char (point-min))
            (when (re-search-forward "<div id=\"table-of-contents\".*?>" nil t)
              (replace-match "<nav class=\"toc\">")
              (re-search-forward "<div id=\"text-table-of-contents\".*?>")
              (replace-match "")
              (search-forward "</div>\n</div>")
              (replace-match "</nav>"))

            ;; 20
            ;; Edit the .outline-2, .outline-text-2... divs that Org generated
            ;;
            ;; First strip all divs that aren't .outline-N.  That way we'll
            ;; know the only closing tags that remain will be the correct ones
            ;; to turn into </section> tags.
            (goto-char (marker-position content-start))
            (while (search-forward "<div" nil t)
              (let ((beg (match-beginning 0)))
                (unless (re-search-forward " id=\".*?\" class=\"outline-[123456]\"" (line-end-position) t)
                  (delete-region beg (search-forward ">"))
                  ;; Note that this may not be the corresponding closing tag.
                  ;; That's why we goto-char (1+ beg) below, to make sure the
                  ;; loop catches them all.
                  (search-forward "</div>")
                  (replace-match "")
                  (goto-char (1+ beg)))))
            ;; Now turn all remaining <div> into <section>
            (goto-char (marker-position content-start))
            (while (re-search-forward "<div .*?class=\"outline-\\([123456]\\).*?>" nil t)
              (if (evenp (string-to-number (match-string 1)))
                  (replace-match "<section class=\"even\">")
                (replace-match "<section class=\"odd\">")))
            (goto-char (marker-position content-start))
            (while (search-forward "</div>" nil t)
              (replace-match "</section>"))
            ;; Now add something like the .outline-text-N divs again.  By
            ;; default, Org didn't always generate them, if there was no body
            ;; text under a heading, but I need it always for my CSS padding
            ;; rules.  Also unlike Org, I'll let headings sit inside so they
            ;; line up with the body text.
            (goto-char (marker-position content-start))
            (while (re-search-forward "<section .*?>" nil t)
              (insert "<div class=\"text-in-section\">")
              (re-search-forward "</?section")
              (goto-char (match-beginning 0))
              (insert "</div>"))

            ;; 07
            ;; MUST AFTER 06
            ;; Since Org didn't add an "outline-text-1" div in the beginning
            (goto-char (marker-position content-start))
            (insert "\n<div class=\"text-in-section\">")
            (if (search-forward "<section" nil t)
                (goto-char (match-beginning 0))
              ;; No subsections in this file
              (goto-char (point-max)))
            (insert "\n</div>")

            ;; 08
            ;; MUST AFTER 07
            ;; Insert roam_refs before the post body
            (when refs
              (goto-char (marker-position content-start))
              (re-search-forward "<div.*?>")
              (insert "\n<p>Referring to ")
              (dolist (ref (split-string refs))
                (setq ref (string-replace "\"" "" ref)) ;; in case I wrapped it in quotes
                (insert " <a href=\"" ref "\">" (replace-regexp-in-string "http.?://" "" ref) "</a>, "))
              (delete-char -2)
              (insert "</p>"))

            ;; (goto-char (marker-position content-start))
            ;; (while (re-search-forward "id=\".*?\" class=\"outline-\\([123456]\\)\".*?>" nil t)
            ;;   (if (evenp (string-to-number (match-string 1)))
            ;;       (replace-match "class=\"section-even\">")
            ;;     (replace-match "class=\"section-odd\">")))
            ;; (goto-char (marker-position content-start))
            ;; (while (re-search-forward "<div .*?class=\"outline-text-[123456]\".*?>" nil t)
            ;;   (replace-match "<div class=\"text-in-section\">")
            ;;   ;; Move heading inside of the div so my CSS can get it to line up
            ;;   ;; FIXME: actually org wont even generate such a div if there is
            ;;   ;; just a heading without body text on the same level.
            ;;   ;; So two options
            ;;   ;; 1. Create these divs entirely manually
            ;;   ;; 2. Make the headings align via css, using rem units.
            ;;   ;; need to do it manually bc need the div's padding so it must always be there
            ;;   ;; or...
            ;;   (goto-char (line-end-position))
            ;;   (set-marker m1 (point))
            ;;   (search-backward "<h")
            ;;   (kill-region (point) (re-search-forward "</h.>"))
            ;;   (goto-char (marker-position m1))
            ;;   (newline)
            ;;   (yank))

            ;; 26
            ;; Count # of total links
            (goto-char (marker-position content-start))
            (setq links (cl-loop while (re-search-forward "<a .*?href=." nil t)
                                 ;; Don't count external links
                                 unless (looking-at-p "http")
                                 ;; Don't count headings' self-links
                                 unless (progn
                                          (search-forward "</a>")
                                          (looking-at-p "</h"))
                                 count t))
            ;; Don't count the headings' self-links
            ;; (goto-char (marker-position content-start))
            ;; (setq links
            ;; (- links (cl-loop while (re-search-forward "<h[23456]" nil t)
            ;; count t)))

            ;; 44
            ;; Org-export doesn't replace triple-dash in all situations (like in
            ;; a heading or when it butts up against a link on a newline), so
            ;; force it.  I'm pretty sure it won't break anything...
            (goto-char (point-min))
            (while (search-forward "---" nil t)
              (unless (looking-at-p "-")
                (replace-match "—")))
            ;; A little more risky but I'm hoping it's fine.  Situations where we
            ;; might not want to transform a double-dash:
            ;; - css variables in code blocks (i have none)
            ;; - a code block showing this very code (i have none)
            ;; - FIXME a code block of elisp with private--vars
            (goto-char (point-min))
            (while (search-forward "--" nil t)
              (unless (looking-at-p "-")
                ;; NOTE can't use &ndash; for the atom feed since it is not
                ;; defined in xml, so use unicode...
                (replace-match "–")))

            ;; 45 While we're at it, the title needs the same fix
            (setq title (->> title
                             (replace-regexp-in-string "---" "—")
                             (replace-regexp-in-string "--" "–")))

            ;; 50
            ;; Correct image paths
            (goto-char (marker-position content-start))
            (while (re-search-forward "<img src=\"[^h]" nil t)
              (forward-char -1)
              (insert "/"))

            ;; 55
            ;; Wrap all tables for horizontal scrollability
            ;; I sure hope I don't have HTML code snippets
            (goto-char (marker-position content-start))
            (while (search-forward "<table" nil t)
              (goto-char (match-beginning 0))
              (insert "<div class=\"table-container\">")
              (search-forward "</table>")
              (insert "</div>"))

            ;; 61
            ;; Declutter the HTML a bit
            (goto-char (point-min))
            (while (search-forward " class=\"org-ul\"" nil t)
              (replace-match ""))

            ;; 65
            ;; Remove org-tags from headlines (each one re-appears in ToC
            ;; also).  It was a bad default to put the nbsp entities outside
            ;; the span so they couldn't be hidden with css...
            (goto-char (point-min))
            (while (search-forward "&#xa0;&#xa0;&#xa0;<span class=\"tag\">" nil t)
              (let ((beg (match-beginning 0)))
                (search-forward "</span></span>" (line-end-position))
                (delete-region beg (point))))

            ;; 68
            ;; give dailies titles a fancy date format too
            (when (member "daily" tags)
              (setq title created-fancy))

            (setq content (buffer-string))

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
                    (description . ,description)
                    (content . ,content))))

          (when-let ((output-buf (find-buffer-visiting output-path)))
            (kill-buffer output-buf))
          (with-temp-file output-path
            (insert (json-encode data-for-json)))
          (when (and (not hidden)
                     (not (-intersection tags '("tag" "daily" "stub")))
                     (string-lessp "2023" (or updated created)))
            (with-temp-file (concat
                             "/tmp/entries/" (or updated created) permalink)
              (insert "\n<entry>"
                      "\n<title>" title "</title>"
                      "\n<link href=\""
                      (concat "https://edstrom.dev/" permalink "/" slug)
                      "\" />"
                      "\n<id>urn:uuid:" id "</id>"
                      "\n<published>" created "T12:00:00Z</published>"
                      (if updated
                          (concat "\n<updated>" updated "T12:00:00Z</updated>")
                        "")
                      ;; Thru type="xhtml", we skip entity-escaping everything
                      ;; https://validator.w3.org/feed/docs/atom.html#text
                      "\n<content type=\"xhtml\">"
                      "\n<div xmlns=\"http://www.w3.org/1999/xhtml\">\n"
                      (string-replace "\n" "\n\t" content)
                      "\n</div>"
                      "\n</content>"
                      "\n</entry>"))))
        (kill-buffer (current-buffer)))))))
