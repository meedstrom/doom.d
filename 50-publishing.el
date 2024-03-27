;; -*- lexical-binding: t; -*-

(require 'dom)
(require 'dash)
(require 'f)

(defvar my-tags-to-avoid-uploading '("noexport" "archive" "private" "censor"))
(defvar my-tags-for-hiding '("gri" "shrink" "privy" "lover" "fren"))
(defvar my-refs-cache nil)
(defvar my-id-old-new-alist nil)

;; TODO: Upstream
;; Override the info: link type so it won't get exported into an empty <a
;; href>, not even a link description.  What nasty default behavior!  May need
;; more overrides like this for each link type, see `org-link-parameters'.
(require 'ol-info)
(defun org-info-export (path desc _format)
  (or desc path))

;; TODO: Upstream
;; Give each h2...h6 heading an ID attribute that matches its source org-id, if
;; it has one, instead of e.g. "org953031".  That way, hash-links such as
;; #ID-e10bbdfe-1ffb-4c54-9228-2818afdfc5ba will make the web browser jump to
;; that heading.  Thank org-roam for this convenience!  Note that I convert
;; these IDs later via `my-uuid-to-pageid'.
(after! ox (require 'org-roam-export))

(setopt org-publish-project-alist
        `(("my-slipbox-blog"
           :base-directory "/tmp/roam/org/"
           :publishing-directory "/tmp/roam/html/"
           :publishing-function my-publish-to-blog
           :recursive t
           :body-only t
           :with-toc nil
           :section-numbers nil
           ;; NOTE: this works only for subtrees, so we also check file-level
           ;; tag in `my-publish-to-blog'.
           :exclude-tags ,my-tags-to-avoid-uploading)

          ("my-slipbox-blog-attachments"
           :base-extension "png\\|jpg\\|gif"
           :base-directory "/home/kept/roam/attachments/"
           :publishing-directory "/tmp/roam/attachments/"
           :publishing-function org-publish-attachment)))

;; This function is where I can make destructive env changes that I
;; don't want in my main Emacs.
(defun my-publish ()
  "All-in-one command for use in a child emacs.

With C-u, also rebuild the org-roam-db that's in /tmp, else reuse
it from some past run (makes sense if you only changed the export
code, but not the notes).

With C-u C-u, also run `my-validate-org-buffer' on each file
scanned."
  (interactive)
  (require 'org-roam)
  (require 'org-agenda)
  (switch-to-buffer "*Messages*") ;; for watching it work
  (goto-char (point-max))
  (setopt org-export-use-babel nil)
  (setopt org-export-with-drawers '(not "logbook" "noexport")) ;; case-insensitive
  (setopt org-export-exclude-tags my-tags-to-avoid-uploading)
  (setopt org-export-with-broken-links nil) ;; links would disappear quietly!
  (setopt org-export-with-smart-quotes nil)
  (setopt org-export-with-tags nil)
  (setopt org-export-with-todo-keywords nil)
  (setopt org-export-headline-levels 5) ;; go all the way to <h6> before making <li>
  ;; If we don't set this to "", there will be .html inside some links even
  ;; though I also set "" in the `org-publish-org-to' call.
  (setopt org-html-extension "")
  (setopt org-html-checkbox-type 'unicode) ;; how will it look in eww? test it.
  (setopt org-html-html5-fancy t)
  ;; why does it skip envs like \begin{align}?
  ;; (setopt org-html-with-latex 'verbatim)
  (setopt org-html-with-latex 'html) ;; use `org-latex-to-html-convert-command'
  (setopt org-latex-to-html-convert-command "node /home/kept/pub/texToMathML.js '%i'")
  (setopt org-inhibit-startup t) ;; from org-publish-org-to
  (setq debug-on-error t)
  ;; (setq debug-on-quit t)

  ;; Speed up publishing
  (setq org-mode-hook nil)
  (gcmh-mode 0)
  (setq gc-cons-threshold (* 4 1000 1000 1000))
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
  (remove-hook 'org-export-before-parsing-functions #'org-attach-expand-links)

  (my-remove-all-advice 'org-roam-db-update-file) ;; disable vulpea

  ;; Switch theme for 2 reasons
  ;; 1. Show me that this is not my normal Emacs
  ;; 2. Syntax-highlight code blocks in a way that looks OK on the web in
  ;;    both light and dark mode
  (let ((theme 'doom-monokai-machine))
    ;; (setq theme 'doom-rouge)
    ;; (setq theme 'doom-zenburn)
    (unless (member theme custom-enabled-themes)
      (load-theme theme)))

  (use-package! prism
    :config
    (setq prism-comments nil)
    (setq prism-desaturations nil)
    (fset 'rainbow-delimiters-mode #'prism-mode)
    (add-hook 'typescript-mode-hook #'prism-mode)
    (add-hook 'typescript-tsx-mode-hook #'prism-mode)
    (add-hook 'js-base-mode-hook #'prism-mode))

  ;; For hygiene, ensure that this subordinate emacs syncs nothing to disk
  (my-state-sync-mode 0)
  ;; (eager-state-preempt-kill-emacs-hook-mode 0)
  (setq kill-emacs-hook nil)

  ;; Copy the files to /tmp to work from there
  (shell-command "rm -rfv /tmp/roam/org/")
  (shell-command "cp -a /home/kept/roam /tmp/roam/org")

  ;; Exclude backups and duplicates
  (shell-command "rm -r /tmp/roam/org/*/logseq/")
  (shell-command "shopt -s globstar && rm -f /tmp/roam/org/**/*sync-conflict*")

  (my-generate-todo-log "/tmp/roam/org/noagenda/archive.org"
                        "/tmp/roam/org/todo-log.org")

  ;; Ensure each post URL will have a unique ID by now placing them in
  ;; subdirectories named by ID, so the org-id resolver will translate all ID
  ;; links into these filesystem paths.
  ;; NOTE: we check for ID collisions later
  (cl-loop
   for path in (directory-files-recursively "/tmp/roam/org/" "\\.org$")
   as uuid = (my-org-file-id path)
   if uuid do
   (let ((new (concat "/tmp/roam/org/" (my-uuid-to-pageid uuid) "/")))
     (mkdir new t)
     (rename-file path new))
   else do (delete-file path))

  (when (>= (car current-prefix-arg) 4)
    (shell-command "rm /tmp/roam/org-roam.db"))
  (when (>= (car current-prefix-arg) 16)
    (add-hook 'my-org-roam-pre-scan-hook #'my-validate-org-buffer))

  ;; Tell `org-id-locations' and the org-roam DB about the temporary directory.
  (setopt org-roam-directory "/tmp/roam/org/")
  (setopt org-roam-db-location "/tmp/roam/org-roam.db")
  (setopt org-agenda-files '("/tmp/roam/org/"))
  (setopt org-id-locations-file "/tmp/roam/org-id-locations")
  (unless (file-exists-p org-roam-db-location)
    (org-id-update-id-locations) ;; find files with ROAM_EXCLUDE too
    (org-roam-update-org-id-locations)
    (org-roam-db-sync 'force))
  ;; (fset 'org-id-update-id-locations #'ignore) ;; stop triggering during publish

  (shell-command "rm -rf /tmp/roam/{html,json}/")
  (shell-command "mkdir -p /tmp/roam/{html,json}")
  (shell-command "rm -r /tmp/roam/feed-entries/")
  (shell-command "mkdir -p /tmp/roam/feed-entries/")

  ;; Reset
  (setq my-id-old-new-alist nil)
  ;; A lookup-table used by `my-replace-web-links-with-ref-note-links'
  (setq my-refs-cache (org-roam-db-query
                       [:select [ref id title]
                        :from refs
                        :left-join nodes
                        :on (= refs:node-id nodes:id)]))

  ;; Change some things about the Org files, before org-export does its thing.
  (add-hook 'org-export-before-parsing-functions #'my-add-backlinks)
  (add-hook 'org-export-before-parsing-functions #'my-replace-datestamps-with-links)
  (add-hook 'org-export-before-parsing-functions #'org-transclusion-mode)
  ;; (add-hook 'org-export-before-parsing-functions #'my-replace-web-links-with-ref-note-links)
  (add-hook 'org-export-before-parsing-functions #'my-add-refs-as-paragraphs)

  (org-publish "my-slipbox-blog-attachments" t)
  (org-publish "my-slipbox-blog" t)

  ;; Ensure it's gone from recentf so I don't accidentally edit these instead
  ;; of the originals
  (shell-command "rm -r /tmp/roam/org/")
  (my-check-id-collisions)
  (my-make-atom-feed "/tmp/roam/posts.atom" "/tmp/roam/feed-entries/")
  (f-write (json-encode my-id-old-new-alist) 'utf-8 "/tmp/roam/idMappings.json")
  (find-file "/home/kept/pub/")
  (message "Prepping website")
  (async-shell-command "./encrypt-rebuild.sh")
  (start-process "chromium" nil "chromium-browser" "--app=http://localhost:5173"))

(defun my-publish-to-blog (plist filename pub-dir)
  "Take org file FILENAME and make html file in PUB-DIR.
Also wrap that same html in a json file and an atom entry.

All arguments pass through to `org-publish-org-to'."
  (redisplay) ;; I like watching it work
  (if (-intersection (my-org-file-tags filename) my-tags-to-avoid-uploading)
      ;; If we already know we won't publish it, don't export the file at all.
      ;; Saves so much time.  Some other issues can also disqualify the file,
      ;; but I take care of them in `my-validate-org-buffer'.
      (message "Found exclude-tag, excluding: %s" filename)
    (org-publish-org-to 'html filename "" plist pub-dir)
    ;; Now pack a JSON object holding the exported HTML plus metadata
    (when-let ((already-open (find-buffer-visiting filename)))
      (cl-assert (not (buffer-modified-p already-open)))
      (kill-buffer already-open))
    (with-current-buffer (find-file-noselect filename)
      (buffer-disable-undo)
      (goto-char (point-min))
      (let* ((output-path (org-export-output-file-name "" nil pub-dir))
             (permalink (-last-item (split-string pub-dir "/" t)))
             (slug (string-replace pub-dir "" output-path))
             (id (org-id-get))
             (tags (org-get-tags))
             (refs (org-entry-get nil "roam_refs"))
             (hidden (car (-intersection my-tags-for-hiding tags)))
             (keywords (org-collect-keywords '("date" "subtitle")))
             (description (car (map-elt keywords "SUBTITLE")))
             (created (substring (org-entry-get nil "CREATED") 1 -1))
             (updated (let ((value (map-elt keywords "DATE")))
                        (when (and value (not (string-blank-p (car value))))
                          (substring (car value) 1 -1))))
             (created-fancy
              (format-time-string (car org-timestamp-custom-formats)
                                  (date-to-time created)))
             (updated-fancy
              (when updated
                (format-time-string (car org-timestamp-custom-formats)
                                    (date-to-time updated))))
             (title (if (member "daily" tags)
                        created-fancy
                      (string-replace "--" "–"
                                      (string-replace "---" "—"
                                                      (org-get-title)))))
             (wordcount
              (save-excursion
                (cl-loop while (re-search-forward my-org-text-line-re nil t)
                         if (and (eq (preceding-char) ?*)
                                 (member "noexport" (org-get-tags)))
                         ;; Don't count words under hidden subtrees
                         do (org-next-visible-heading 1)
                         else sum (count-words (point) (line-end-position)))))
             (links-count
              (save-excursion
                (cl-loop while (re-search-forward org-link-bracket-re nil t)
                         if (member "noexport" (org-get-tags))
                         do (org-next-visible-heading 1)
                         else count t)))
             (content (my-customize-the-html output-path refs permalink slug))
             (content-for-feed
              (with-temp-buffer
                (buffer-disable-undo)
                (insert content)
                (goto-char (point-min))
                (let* ((locked (regexp-opt (append my-tags-to-avoid-uploading
                                                   my-tags-for-hiding)))
                       (re (rx "<a " (*? nonl) "class=\"" (*? nonl)
                               (regexp locked)
                               (*? nonl) ">" (group (*? anychar)) "</a>")))
                  (while (re-search-forward re nil t)
                    (replace-match (match-string 1)))
                  (buffer-string)))))
        ;; Write JSON object
        (let ((dir (concat "/tmp/roam/json/" permalink "/")))
          (mkdir dir)
          (with-temp-file (concat dir slug)
            (buffer-disable-undo)
            (insert (json-encode `((slug . ,slug)
                                   (permalink . ,permalink)
                                   (title . ,title)
                                   (created . ,created)
                                   (updated . ,updated)
                                   (created_fancy . ,created-fancy)
                                   (updated_fancy . ,updated-fancy)
                                   (wordcount . ,wordcount)
                                   (links . ,links-count)
                                   (tags . ,tags)
                                   (hidden . ,hidden)
                                   (description . ,description)
                                   (content . ,content))))))
        ;; Write Atom entry
        (when (and (not hidden)
                   (not (-intersection tags '("tag" "daily" "stub")))
                   (string-lessp "2023" (or updated created)))
          (with-temp-file (concat "/tmp/roam/feed-entries/" slug)
            (buffer-disable-undo)
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
                    ;; NOTE: don't try to indent content.  it
                    ;; messes with <pre> tags inside!
                    content-for-feed
                    "\n</div>"
                    "\n</content>"
                    "\n</entry>"))))
      (cl-assert (not (buffer-modified-p)))
      (kill-buffer (current-buffer)))))


(defun my-customize-the-html (path refs permalink slug)
  (let ((dom nil)
        (after-toc (make-marker)))
    (with-temp-buffer
      (buffer-disable-undo)
      (insert-file-contents path)

      ;; Give the ToC div a class and remove its pointless inner div
      (goto-char (point-min))
      (when (re-search-forward "^<div id=\"table-of-contents\".*?>" nil t)
        (replace-match "<nav class=\"toc\">")
        (re-search-forward "<div id=\"text-table-of-contents\".*?>")
        (replace-match "")
        (search-forward "</div>\n</div>")
        (replace-match "</nav>"))
      (set-marker after-toc (point))

      ;; Edit the .outline-2, .outline-3... divs that Org generated.
      ;; First, strip all divs that aren't .outline-N.  That way we'll
      ;; know the only closing tags that remain will be the correct ones
      ;; to turn into </section> tags.
      (while (search-forward "\n<div" nil t)
        (let ((beg (match-beginning 0)))
          (unless (re-search-forward
                   " id=\".*?\" class=\"outline-[123456]\"" (line-end-position) t)
            (delete-region beg (search-forward ">"))
            ;; Note that this may not be the corresponding closing tag.
            ;; That's why we goto-char (1+ beg) below, to make sure the
            ;; loop catches them all.
            (search-forward "\n</div>")
            (replace-match "")
            (goto-char (1+ beg)))))
      ;; Now turn all remaining <div> into <section>
      (goto-char (marker-position after-toc))
      (while (re-search-forward "^<div .*?class=\"outline-\\([123456]\\).*?>" nil t)
        (if (cl-evenp (string-to-number (match-string 1)))
            (replace-match "<section class=\"even\">")
          (replace-match "<section class=\"odd\">")))
      (goto-char (marker-position after-toc))
      (while (search-forward "\n</div>" nil t)
        (replace-match "\n</section>"))
      ;; Now add something like the .outline-text-N divs again.  By
      ;; default, Org didn't always generate them, if there was no body
      ;; text under a heading, but I need it always for my CSS padding
      ;; rules.  Also unlike Org, I'll let headings sit inside so they
      ;; line up with the body text.
      (goto-char (marker-position after-toc))
      (while (re-search-forward "^<section .*?>" nil t)
        (insert "\n<div class=\"text-in-section\">")
        (re-search-forward "^</?section")
        (goto-char (match-beginning 0))
        (insert "</div>\n"))

      ;; Since Org didn't add any wrapper div before first headline (and I want
      ;; the wrapper to swallow the table of contents too), do it manually.
      (goto-char (point-min))
      (insert "\n<div class=\"text-in-section\">\n")
      (goto-char (point-min))
      (if (search-forward "\n<section" nil t)
          (goto-char (match-beginning 0))
        ;; No subsections in this file
        (goto-char (point-max)))
      (insert "\n</div>\n")

      ;; Now parse HTML because some things are easier to do to a parsed tree
      ;; than to a text buffer
      (setq dom (libxml-parse-html-region)))

    ;; Mess with internal links
    (cl-loop
     for anchor in (dom-by-tag dom 'a)
     as href = (dom-attr anchor 'href)
     as hash = (when href (cadr (split-string href "#")))
     as uuid? = (when hash (->> hash
                                (string-replace "ID-" "")
                                (string-replace "id:" "")))
     when (and uuid? (org-uuidgen-p uuid?))
     do (let ((shortid-old-v2 (my-uuid-to-pageid-old-v2 uuid?))
              (shortid (my-uuid-to-pageid uuid?))
              (target-tags (-flatten (org-roam-db-query
                                      `[:select [tag]
                                        :from tags
                                        :where (= node-id ,uuid?)]))))
          ;; Replace all UUID with my shortened form.
          ;; Then remove the hash-part of the link (i.e. the bit after the #
          ;; character in domain.com/PAGE-ID/slug#HEADING-ID) if the HEADING-ID
          ;; matches PAGE-ID anyway (i.e. it's a file-level id)
          (dom-set-attribute anchor 'href
                             (my-strip-hashlink-if-same-as-permalink
                              (string-replace hash shortid href)))

          ;; Record old ID for redirects on the website
          (unless (assoc shortid-old-v2 my-id-old-new-alist)
            (push (cons shortid-old-v2 shortid) my-id-old-new-alist))

          ;; Associate ID with original UUID to check for collisions
          (push uuid? (gethash shortid my-ids*))

          ;; Style the link based on target tag
          (when target-tags
            (dom-set-attribute anchor 'class (string-join target-tags " ")))))

    ;; Format undescribed links more nicely
    (cl-loop for anchor in (dom-by-tag dom 'a)
             as children = (dom-children anchor)
             as desc = (car children)
             as fixed-desc = (->> desc
                                  (replace-regexp-in-string "^http.?://" "")
                                  (string-replace "%20" " "))
             unless (equal fixed-desc desc)
             do (progn
                  (dom-add-child-before children fixed-desc desc)
                  (dom-remove-node children desc)))

    ;; Mess with headings
    (cl-loop
     for heading in (--mapcat (dom-by-tag dom it) '(h2 h3 h4 h5 h6))
     as id = (dom-attr heading 'id)
     when id do
     (let ((uuid? (string-replace "ID-" "" id)))
       (when (org-uuidgen-p uuid?)
         (let* ((hashid (my-uuid-to-pageid uuid?))
                ;; Since my blog's Note component may appear on other routes
                ;; than the canonical one (like /recent/400 or
                ;; /unlocked/...), hardcode the full URL
                (selflink
                 (dom-node 'a
                           `((href . ,(concat "/" permalink "/" slug "#" hashid))
                             (class . "heading-permalink")
                             (rel . "bookmark"))
                           "🔗")))
           (dom-set-attribute heading 'id hashid)
           (dom-append-child heading selflink)))))

    (funcall
     (defun recurse-avoiding-code-blocks (dom)
       (cl-loop for child in (dom-children dom)
                ;; Org-export doesn't replace double/triple-dash in all
                ;; situations (like in a heading or when it butts up against a
                ;; link on a newline), so force it
                if (stringp child)
                do (let ((fixed-child (->> child
                                           (string-replace "---" "—")
                                           (string-replace "--" "–"))))
                     (unless (equal fixed-child child)
                       (dom-add-child-before dom fixed-child child)
                       (dom-remove-node dom child)))
                ;; Wrap tables in divs that can be scrolled left-right
                else if (eq 'table (dom-tag child))
                do (let ((wrapped-table (dom-node 'div
                                                  '((class . "table-container"))
                                                  child)))
                     (dom-add-child-before dom wrapped-table child)
                     (dom-remove-node dom child))
                else if (not (member (dom-tag child) '(code pre kbd samp)))
                do (recurse-avoiding-code-blocks child)))
     dom)

    ;; Mess with img elements
    (cl-loop for img in (dom-by-tag dom 'img)
             as path = (dom-attr img 'src)
             as alt = (dom-attr img 'alt)
             when (string-prefix-p "attach" path)
             ;; Fix paths
             do (dom-set-attribute img 'src (concat "/" path))
             ;; Org exports an image alt-text that is just the image
             ;; basename. So now I try to rename my images so they all have
             ;; descriptive names. Still have to strip .jpg/.png extension.
             ;; REVIEW Maybe use underscores in the filename and replace them
             ;; here for spaces?
             and when (string-search alt path)
             do (dom-set-attribute img 'alt (file-name-sans-extension alt)))

    ;; Correct anchor paths to assets for <a href="localfile"><img
    ;; src="localfile"></img></a> type of things. Such links also need
    ;; rel="external" in order to prevent SvelteKit from interpreting the
    ;; URL as a route and executing [first]/[[second]]/+page.ts.
    (cl-loop for a in (dom-by-tag dom 'a)
             as path = (dom-attr a 'href)
             when (string-prefix-p "attach" path)
             do (progn
                  (dom-set-attribute a 'href (concat "/" path))
                  (dom-set-attribute a 'rel "external")))
    
    ;; Declutter
    (cl-loop for node in (dom-by-class dom "^org-ul$")
             do (dom-remove-attribute node 'class))

    ;; Return final HTML.  Phew!
    (with-temp-buffer
      (dom-print dom)
      (search-backward "</body></html>")
      (replace-match "")
      (goto-char (point-min))
      (search-forward "<html><body>")
      (replace-match "")
      (buffer-string))))
