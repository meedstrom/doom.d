;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'f)

;; Keep in mind that `case-fold-search' doesn't affect `equal', and therefore
;; doesn't affect list-comparisons such as `cl-intersection'!
(defvar my-deprecated-tags '("drill"  "fc" "anki" "partner" "friends-eyes" "therapist" "eyes-partner" "eyes-therapist" "eyes-diana" "eyes-friend" "eyes_therapist" "eyes_partner" "eyes_friend"))
(defvar my-tags-to-avoid-uploading (append my-deprecated-tags '("noexport" "archive" "private" "censor")))
(defvar my-tags-for-hiding '("gri" "shrink" "privy" "lover" "fren"))
(defvar my-refs-cache nil)
(defvar my-heading-locations nil)
(defvar my-id-old-new-alist nil)

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

(defun my-publish (&optional rescan)
  "A single command I can use in a child emacs."
  (interactive "P")
  ;; (require 'ox-publish)
  (cd "/home/kept/roam") ;; for me to quick-search when an id fails to resolve
  (shell-command "rm -r /tmp/feed-entries/")
  (mkdir "/tmp/feed-entries/" t)
  (switch-to-buffer "*Messages*") ;; for watching it work
  ;; (split-window) ;; in case the Warnings buffer appears
  (when rescan
    (shell-command "rm /tmp/org-roam.db"))
  (sleep-for .05)
  (org-publish "my-slipbox-blog" t)
  (org-publish "my-slipbox-blog-attachments" t)
  ;; ensure it's gone from recentf so I don't accidentally edit these instead
  ;; of the originals
  (shell-command "rm -r /tmp/roam")
  (my-check-id-collisions)
  (my-make-atom-feed "/home/kept/pub/posts.atom" "/tmp/feed-entries/")
  (f-write (json-encode-alist my-id-old-new-alist)
           'utf-8 "/home/kept/pub/idMappings.json")
  (f-write (json-encode-alist my-heading-locations)
           'utf-8 "/home/kept/pub/idsInPages.json"))

;; (defconst my-date-regexp (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))
(defvar my-publish-scanned-already nil)
(defun my-prep-fn (_)
  "Prepare Emacs and temp files for publishing my website.
Since I intend to run `org-publish' in a subordinate Emacs, this
function is where I can make destructive env changes that I don't
want in my main Emacs."
  (require 'org-roam)

  ;; Speed up publishing
  (setq org-mode-hook nil)
  (gcmh-mode 0)
  (setq gc-cons-threshold most-positive-fixnum)
  (fset 'org-publish-write-cache-file #'ignore) ;; huge effect!
  (advice-remove 'after-find-file #'doom--shut-up-autosave-a)
  ;; (undefadvice! '+org--fix-async-export-a :around '(org-export-to-file org-export-as))
  ;; (undefadvice! +org-babel-disable-async-maybe-a :around #'ob-async-org-babel-execute-src-block)

  ;; Not sure it helps speed at all
  (my-disable-modes-if-present
   '(global-emojify-mode
     apheleia-global-mode
     solaire-global-mode
     ws-butler-global-mode
     smartparens-global-mode
     projectile-mode
     global-diff-hl-mode
     beginend-global-mode
     better-jumper-mode
     global-prettify-symbols-mode
     global-ligature-mode
     global-eldoc-mode))
  (setq whitespace-global-modes nil)

  (setq debug-on-error t)
  (setq debug-on-quit t)

  ;; Switch theme for 2 reasons
  ;; 1. Show me that this is not my normal Emacs
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

  ;; Copy the files to /tmp to work from there
  (shell-command "rm -r /tmp/roam/")
  (shell-command "cp -a /home/kept/roam /tmp/")
  (shell-command "cp -a /home/sync-phone/beorg/* /tmp/roam/")
  (shell-command "rm -r /tmp/roam/*/logseq/") ;; logseq auto-backups
  (shell-command "shopt -s globstar && rm /tmp/roam/**/*.gpg") ;; no crypts

  ;; Flatten the directory tree (no more subdirs)
  (cl-loop
   for file in (directory-files-recursively "/tmp/roam" "\\.org$" nil)
   unless (equal "/tmp/roam/" (file-name-directory file))
   do (rename-file file "/tmp/roam/"))

  (shell-command "rm /tmp/roam/*sync-conflict*") ;; syncthing

  ;; Generate a log of completed tasks
  ;; (my-generate-todo-log "/tmp/roam/todo-log.org")

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

  (my-remove-all-advice 'org-roam-db-update-file) ;; remove vulpea, no need here

  ;; Tell `org-id-locations' and the org-roam DB about the new directory.
  (setopt org-roam-directory "/tmp/roam/")
  (setopt org-roam-db-location "/tmp/org-roam.db")
  (setopt org-agenda-files '("/tmp/roam/"))
  (setopt org-id-locations-file "/tmp/org-id-locations")
  (unless (file-exists-p org-roam-db-location)
    (org-id-update-id-locations) ;; find files with ROAM_EXCLUDE too
    (org-roam-update-org-id-locations)
    (org-roam-db-sync 'force))
  ;; (fset 'org-id-locations-save #'ignore)
  ;; (org-roam-db-autosync-mode 0)
  (fset 'org-id-update-id-locations #'ignore) ;; stop triggering during publish

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
  (setopt org-inhibit-startup t) ;; from org-publish-org-to

  ;; Lookup table used by `my-replace-web-links-with-ref-note-links'
  (setq my-refs-cache (org-roam-db-query
                       [:select [ref id title]
                        :from refs
                        :left-join nodes
                        :on (= refs:node-id nodes:id)])))

;; Change some things about the Org files, before org-export does its thing.
(add-hook 'org-export-before-parsing-functions #'my-add-backlinks)
(add-hook 'org-export-before-parsing-functions #'my-replace-web-links-with-ref-note-links)
(add-hook 'org-export-before-parsing-functions #'my-replace-datestamps-with-links)

;; Struggled so long looking for a hook that would work like the old
;; before-export-hook.  Let this be a lesson: the Emacs hook system exists to
;; let you subtly modify a function IN THE MIDDLE of its body.  We never
;; actually need simple before-hooks nor after-hooks, since in such a simple
;; case it is always possible to use `add-function' or write a wrapper such as
;; the following wrapper around `org-publish-org-to'.

(defun my-publish-to-blog (plist filename pub-dir)
  (redisplay) ;; I like watching programs work    
  ;; (sleep-for .01)
  ;; Fast early check that avoids loading org-mode
  (let ((case-fold-search t)
        title id created tags link-in-heading)
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
     ;; ensure all tags are lowercase so `-intersection' works
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

        ;; Customize the resulting HTML file and pack it into a JSON object.
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
          ;; TODO: Use dom.el to parse the html properly, the regexp approach
          ;;       is getting too complicated.
          (with-temp-buffer

            ;; 05 Insert the post body: the HTML produced by Org-export
            (insert-file-contents output-path)

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
              (let* ((is-heading (save-excursion
                                   (search-backward "<a ")
                                   (search-backward "<")
                                   (looking-at-p "h")))
                     (uuid (buffer-substring (point)
                                             (1- (search-forward "\""))))
                     ;; HACK now links have #ID-id:f4oirm34ofkmr3o23pm
                     ;; which is a bug, or just a change that org-roam-export
                     ;; hasn't caught up with
                     (uuid (if (string-prefix-p "id:" uuid)
                               (substring uuid 3)
                             uuid)))
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
                     ;; HACK same as the other hack
                     (uuid (if (string-prefix-p "id:" uuid)
                               (substring uuid 3)
                             uuid))
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
              (if (cl-evenp (string-to-number (match-string 1)))
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

            ;; 44
            ;; Org-export doesn't replace triple-dash in all situations (like
            ;; in a heading or when it butts up against a link on a newline),
            ;; so force it.  DO NOT do this inside code blocks tho.
            (goto-char (point-min))
            (while
                (let* ((beg (point))
                       (end (or (re-search-forward
                                 "<\\(?:code\\|kbd\\|pre\\|samp\\)" nil t)
                                (point-max)))
                       (elem-type (if (equal end (point-max))
                                      nil
                                    (word-at-point t))))
                  (my-multi-hyphens-to-en-em-dashes beg end)
                  (when elem-type
                    (search-forward (concat "</" elem-type ">") nil t))))

            ;; 45 While we're at it, fix multi-hyphens in the title too
            (setq title
                  (string-replace "--" "–" (string-replace "---" "—" title)))

            ;; 50
            ;; Correct image paths
            (goto-char (marker-position content-start))
            (while (re-search-forward "<img src=\"[^h]" nil t)
              (forward-char -1)
              (insert "/")
              ;; (when (search-backward "<a " (line-beginning-position) t)
              ;;   (forward-char 3)
              ;;   (insert "rel=\"external\""))
              )

            ;; 52
            ;;
            ;; Correct anchor paths to assets for same reason as 50.  Usually
            ;; it's an <a href="localfile"><img src="localfile"></img></a>
            ;; thing, but the principle generalizes: any anchor tag linking to
            ;; a resource that is NOT on another domain (i.e. doesn't start
            ;; with "http") and is NOT another roam note (i.e. hasn't been
            ;; given a class at 09), needs the fix.
            ;;
            ;; While we're at it, such links also need rel="external" in order
            ;; to prevent SvelteKit from interpreting the URL as a route and
            ;; executing [first]/[[second]]/+page.ts.
            (goto-char (marker-position content-start))
            (while (re-search-forward "<a href=\"[^h]" nil t)
              (backward-char 1)
              (insert "/")
              (search-backward " ")
              (insert " rel=\"external\""))

            ;; 55
            ;; Wrap all tables for horizontal scrollability.  I sure hope I
            ;; don't have code snippets displaying HTML.  TODO merge with 44
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
                             "/tmp/feed-entries/" (or updated created) permalink)
              (insert "\n\t<entry>"
                      "\n\t<title>" title "</title>"
                      "\n\t<link href=\""
                      (concat "https://edstrom.dev/" permalink "/" slug)
                      "\" />"
                      "\n\t<id>urn:uuid:" id "</id>"
                      "\n\t<published>" created "T12:00:00Z</published>"
                      (if updated
                          (concat "\n\t<updated>" updated "T12:00:00Z</updated>")
                        "")
                      ;; Thru type="xhtml", we skip entity-escaping everything
                      ;; https://validator.w3.org/feed/docs/atom.html#text
                      "\n\t<content type=\"xhtml\">"
                      "\n\t\t<div xmlns=\"http://www.w3.org/1999/xhtml\">\n"
                      ;; NOTE: don't try to indent content.  it
                      ;; messes with <pre> tags inside!
                      content
                      "\n\t\t</div>"
                      "\n\t</content>"
                      "\n\t</entry>"))))
        (kill-buffer (current-buffer)))))))


;; Override the link type info: so it won't get exported into an empty
;; <a href>.  May need more overrides like this for each link type,
;; see `org-link-parameters'.
(require 'ol-info)
(defun org-info-export (path desc _format)
  (or desc path))

;; Give each h2...h6 heading an ID attribute that matches its source org-id, if
;; it has one, instead of e.g. "org953031".  That way, hash-links such as
;; #ID-e10bbdfe-1ffb-4c54-9228-2818afdfc5ba will make the web browser jump to
;; that heading.  Thank org-roam for this convenience!  Note that I convert
;; these IDs later via `my-uuid-to-pageid'.
(after! ox (require 'org-roam-export))
