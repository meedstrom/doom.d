;; -*- lexical-binding: t; -*-

(require 'dom)
(require 'dash)
(require 'f)

;; Keep in mind that `case-fold-search' doesn't affect `equal', and therefore
;; doesn't affect list-comparisons such as `cl-intersection'!
;; So use (cl-intersection LIST1 LIST2 :test #'string-equal-ignore-case).
(defvar my-deprecated-tags '())
(defvar my-tags-to-avoid-uploading (append my-deprecated-tags '("noexport" "archive" "private" "censor")))
(defvar my-tags-for-hiding '("gri" "shrink" "privy" "lover" "fren"))
(defvar my-refs-cache nil)
;; (defvar my-heading-locations nil)
(defvar my-id-old-new-alist nil)

;; TODO: Upstream
;; Override the info: link type so it won't get exported into an empty <a
;; href>.  What nasty default behavior!  May need more overrides like this for
;; each link type, see `org-link-parameters'.
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
           :publishing-directory "/home/kept/pub/posts/"
           ;; :publishing-directory "/tmp/roam/html/"
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
           :base-extension "png\\|jpg\\|gif"
           :base-directory "/tmp/roam/org/attachments/"
           :publishing-directory "/home/kept/pub/attachments/"
           :publishing-function org-publish-attachment)))

(defun my-publish (&optional rescan)
  "A single command I can use in a child emacs.

The command started out as a nonessential convenience wrapper,
but turns out it's essential for wrapup after all files are
exported: compile and move the feed entries and that other json.

With prefix argument RESCAN, rebuild the org-roam-db that's in
/tmp, else reuse it from some past run (makes sense if you only
changed the export code, but not the notes)."
  (interactive "P")
  ;; (require 'ox-publish)
  (cd "/home/kept/roam/") ;; for me to quick-search when an id fails to resolve
  (shell-command "rm -r /tmp/roam/feed-entries/")
  (mkdir "/tmp/roam/feed-entries/" t)
  (switch-to-buffer "*Messages*") ;; for watching it work
  (goto-char (point-max))
  ;; (split-window) ;; in case the Warnings buffer appears
  (when rescan
    (shell-command "rm /tmp/roam/org-roam.db"))
  (sleep-for .05)
  (org-publish "my-slipbox-blog" t)
  (org-publish "my-slipbox-blog-attachments" t)
  ;; (org-publish "my-slipbox-blog-feed" t)
  ;; Ensure it's gone from recentf so I don't accidentally edit these instead
  ;; of the originals
  (shell-command "rm -r /tmp/roam/org/")
  (my-check-id-collisions)
  (my-make-atom-feed "/home/kept/pub/posts.atom" "/tmp/roam/feed-entries/")
  ;; (f-write (json-encode-alist my-id-old-new-alist)
  ;;      'utf-8 "/home/kept/pub/idMappings.json")
  (f-write (json-encode my-id-old-new-alist)
           'utf-8 "/home/kept/pub/idMappings.json")
  ;; (f-write (json-encode-alist my-heading-locations)
  ;;          'utf-8 "/home/kept/pub/idsInPages.json")
  (find-file "/home/kept/pub/")
  (message "Prepping website")
  (cd "/home/kept/pub/")
  (async-shell-command "./encrypt-rebuild.sh")
  (async-shell-command "chromium --app=http://localhost:5173"))

(defvar my-publish-scanned-already nil)
(defun my-prep-fn (_)
  "Prepare Emacs and temp files for publishing my website.
Since I intend to run `org-publish' in a subordinate Emacs, this
function is where I can make destructive env changes that I don't
want in my main Emacs."
  (require 'org-roam)
  (require 'org-agenda)
  (my-remove-all-advice 'org-roam-db-update-file) ;; disable vulpea

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

  (when (equal '(16) current-prefix-arg)
    (add-hook 'my-org-roam-pre-scan-hook #'my-validate-org-buffer))
  (setq debug-on-error t)
  (setq debug-on-quit t)

  ;; Switch theme for 2 reasons
  ;; 1. Show me that this is not my normal Emacs
  ;; 2. Syntax-highlight code blocks in a way that looks OK on the web in
  ;;    both light and dark mode
  (setq theme 'doom-monokai-machine)
  ;; (setq theme 'doom-rouge)
  ;; (setq theme 'doom-zenburn)
  (unless (member theme custom-enabled-themes)
    (load-theme theme))

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
  (setq kill-emacs-hook nil)

  ;; Copy the files to /tmp to work from there
  (shell-command "rm -r /tmp/roam/org/")
  (shell-command "cp -a /home/kept/roam /tmp/roam/org")
  ;; (shell-command "cp -a /home/sync-phone/beorg/* /tmp/roam/org/")
  (shell-command "rm -r /tmp/roam/org/*/logseq/") ;; logseq auto-backups
  ;; (shell-command "shopt -s globstar && rm /tmp/roam/org/**/*.gpg") ;; no crypts

  ;; Flatten the directory tree (no more subdirs)
  (cl-loop
   for file in (directory-files-recursively "/tmp/roam/org/" "\\.org$" nil)
   unless (equal "/tmp/roam/org/" (file-name-directory file))
   do (rename-file file "/tmp/roam/org/"))

  (shell-command "rm /tmp/roam/org/*sync-conflict*") ;; syncthing

  ;; Generate a pretty log of completed TODOs
  (my-generate-todo-log "/tmp/roam/org/todo-log.org")

  ;; Ensure each post will get a unique ID in the URL
  ;; NOTE: we check for page id collision later, not now
  (cl-loop
   with default-directory = "/tmp/roam/org/"
   for path in (directory-files "/tmp/roam/org/" t "\\.org$")
   as uuid = (my-org-file-id path)
   when uuid do
   (let ((pageid (my-uuid-to-pageid uuid)))
     (mkdir pageid t)
     (rename-file path (concat pageid "/"))))

  ;; Tell `org-id-locations' and the org-roam DB about the new directory.
  (setopt org-roam-directory "/tmp/roam/org/")
  (setopt org-roam-db-location "/tmp/roam/org-roam.db")
  (setopt org-agenda-files '("/tmp/roam/org/"))
  (setopt org-id-locations-file "/tmp/roam/org-id-locations")
  (unless (file-exists-p org-roam-db-location)
    (org-id-update-id-locations) ;; find files with ROAM_EXCLUDE too
    (org-roam-update-org-id-locations)
    (org-roam-db-sync 'force))
  (fset 'org-id-update-id-locations #'ignore) ;; stop triggering during publish

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
  ;; why does it skip environments like \begin{align}?
  ;; (setopt org-html-with-latex 'verbatim)
  (setopt org-html-with-latex 'html) ;; use `org-latex-to-html-convert-command'
  (setopt org-latex-to-html-convert-command "node /home/kept/pub/texToMathML.js '%i'")
  (setopt org-inhibit-startup t) ;; from org-publish-org-to

  ;; Reset
  ;; (setq my-heading-locations nil)
  (setq my-id-old-new-alist nil)
  ;; A lookup-table used by `my-replace-web-links-with-ref-note-links'
  (setq my-refs-cache (org-roam-db-query
                       [:select [ref id title]
                        :from refs
                        :left-join nodes
                        :on (= refs:node-id nodes:id)])))

;; Change some things about the Org files, before org-export does its thing.
(add-hook 'org-export-before-parsing-functions #'my-add-backlinks)
(add-hook 'org-export-before-parsing-functions #'my-replace-datestamps-with-links)
(add-hook 'org-export-before-parsing-functions #'org-transclusion-mode)
;; (add-hook 'org-export-before-parsing-functions #'my-replace-web-links-with-ref-note-links)

(defun my-postprocess (source-org-file html-file)
  )

;; Struggled so long looking for a hook that would work like the old
;; before-export-hook.  Let this be a lesson: the Emacs hook system exists to
;; let you subtly modify a function IN THE MIDDLE of its body.  We never
;; actually need simple before-hooks nor after-hooks, since in such a simple
;; case it is always possible to use `add-function' or write a wrapper such as
;; the following wrapper around `org-publish-org-to'.

(defun my-publish-to-blog (plist filename pub-dir)
  (redisplay) ;; I like watching programs work
  ;; Skip exporting if we won't use the result
  (cond
   ((-intersection (my-org-file-tags filename)
                   my-tags-to-avoid-uploading)
    (message "Found exclude-tag, excluding: %s" filename))
   ;; if neither pub or a hiding-tag, there could be an outdated tag, so skip
   ;; to be safe
   ((not (-intersection (my-org-file-tags filename)
                        (cons "pub" my-tags-for-hiding)))
    (warn "Not selected for publishing: %s" filename))
   ;; OK, export
   (t
    ;; The original export-function.  By thy might, Bastien/Carsten/&c.
    (org-publish-org-to 'html filename "" plist pub-dir)
    ;; (my-postprocess filename (org-export-output-file-name "" nil pub-dir))
    (with-current-buffer (or (find-buffer-visiting filename)
                             (find-file-noselect filename))
      (goto-char (point-min))
      ;; Customize the resulting HTML file and pack it into a JSON object.
      (let* ((title (org-get-title))
             (created (substring (org-entry-get nil "CREATED") 1 -1))
             (id (org-id-get))
             (tags (org-get-tags))
             (output-path (org-export-output-file-name "" nil pub-dir))
             (slug (string-replace pub-dir "" output-path))
             (permalink (-last-item (split-string pub-dir "/" t)))
             (updated (save-excursion
                        (goto-char (point-min))
                        (when (search-forward "\n#+date: [" nil t)
                          (buffer-substring (point) (1- (line-end-position))))))
             ;; (wordcount (save-excursion
             ;;              (if (re-search-forward "^[^#:\n]" nil t)
             ;;                  (count-words (point) (point-max))
             ;;                0)))
             (wordcount
              (save-excursion
                (let ((sum 0))
                  (while (re-search-forward "^[ \t]*[^#:\n]" nil t)
                    (cl-incf sum (count-words (point) (line-end-position))))
                  sum)))
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
             ;; (links-count 0)
             (links-count
              (save-excursion
                (cl-loop while (re-search-forward org-link-bracket-re nil t)
                         count t)))
             (m1 (make-marker))
             (after-toc (make-marker))
             (content (my-customize-the-html output-path refs permalink))
             (data-for-json nil)
             (title (if (member "daily" tags)
                        created-fancy
                      (string-replace "--" "–"
                                      (string-replace "---" "—" title)))))
        (setq content-for-feed
              (with-temp-buffer
                (insert content)
                (goto-char (point-min))
                (let* ((locked (regexp-opt (append my-tags-to-avoid-uploading
                                                   my-tags-for-hiding)))
                       (re (rx "<a " (*? nonl) "class=\"" (*? nonl) (regexp locked) (*? nonl) ">"
                               (group (*? anychar))
                               "</a>")))
                  (while (re-search-forward re nil t)
                    (replace-match (match-string 1))))
                (buffer-string)))
        (setq data-for-json
              `((slug . ,slug)
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
                (content . ,content)))

        (when-let ((output-buf (find-buffer-visiting output-path)))
          (kill-buffer output-buf))
        (with-temp-file output-path
          (insert (json-encode data-for-json)))
        (when (and (not hidden)
                   (not (-intersection tags '("tag" "daily" "stub")))
                   (string-lessp "2023" (or updated created)))
          ;; uniq filename that filesystem will sort chrono (idk if
          ;; important)
          (with-temp-file (concat "/tmp/roam/feed-entries/"
                                  (or updated created)
                                  permalink)
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
      (kill-buffer (current-buffer))))))

(defun my-customize-the-html (path refs permalink)
  (setq dom nil)
  (with-temp-buffer
    (insert-file-contents path)

    ;; Give the ToC div a class and remove its pointless inner div
    (goto-char (point-min))
    (setq after-toc (make-marker))
    (set-marker after-toc (point))
    (when (re-search-forward "<div id=\"table-of-contents\".*?>" nil t)
      (replace-match "<nav class=\"toc\">")
      (re-search-forward "<div id=\"text-table-of-contents\".*?>")
      (replace-match "")
      (search-forward "</div>\n</div>")
      (replace-match "</nav>")
      (set-marker after-toc (point)))

    ;; Edit the .outline-2, .outline-text-2... divs that Org generated
    ;;
    ;; First strip all divs that aren't .outline-N.  That way we'll
    ;; know the only closing tags that remain will be the correct ones
    ;; to turn into </section> tags.
    (goto-char (marker-position after-toc))
    (while (search-forward "<div" nil t)
      (let ((beg (match-beginning 0)))
        (unless (re-search-forward
                 " id=\".*?\" class=\"outline-[123456]\"" (line-end-position) t)
          (delete-region beg (search-forward ">"))
          ;; Note that this may not be the corresponding closing tag.
          ;; That's why we goto-char (1+ beg) below, to make sure the
          ;; loop catches them all.
          (search-forward "</div>")
          (replace-match "")
          (goto-char (1+ beg)))))
    ;; Now turn all remaining <div> into <section>
    (goto-char (marker-position after-toc))
    (while (re-search-forward "<div .*?class=\"outline-\\([123456]\\).*?>" nil t)
      (if (cl-evenp (string-to-number (match-string 1)))
          (replace-match "<section class=\"even\">")
        (replace-match "<section class=\"odd\">")))
    (goto-char (marker-position after-toc))
    (while (search-forward "</div>" nil t)
      (replace-match "</section>"))
    ;; Now add something like the .outline-text-N divs again.  By
    ;; default, Org didn't always generate them, if there was no body
    ;; text under a heading, but I need it always for my CSS padding
    ;; rules.  Also unlike Org, I'll let headings sit inside so they
    ;; line up with the body text.
    (goto-char (marker-position after-toc))
    (while (re-search-forward "<section .*?>" nil t)
      (insert "<div class=\"text-in-section\">")
      (re-search-forward "</?section")
      (goto-char (match-beginning 0))
      (insert "</div>"))

    ;; 07
    ;; Since Org didn't add an "outline-text-1" div in the beginning
    (goto-char (point-min))
    ;; (goto-char (marker-position after-toc))
    (insert "\n<div class=\"text-in-section\">")
    (if (search-forward "<section" nil t)
        (goto-char (match-beginning 0))
      ;; No subsections in this file
      (goto-char (point-max)))
    (insert "\n</div>")

    ;; 08
    ;; MUST AFTER 07
    ;; Insert roam_refs before the post body
    ;; TODO: consider doing this in the org markup before export?
    (when refs
      (goto-char (point-min))
      (search-forward "<div class=\"text-in-section\">")
      (insert "\n<p>Source ")
      (dolist (ref (split-string refs))
        (setq ref (string-replace "\"" "" ref)) ;; in case I wrapped it in quotes
        (insert " <a href=\"" ref "\">"
                (replace-regexp-in-string "http.?://" "" ref)
                "</a>, "))
      (delete-char -2)
      (insert "</p>"))

    ;; ;; Remove org-tags from headlines (each one re-appears in ToC
    ;; ;; also).  It was a bad default to put the nbsp entities outside
    ;; ;; the span so they couldn't be hidden with css...
    ;; (goto-char (point-min))
    ;; (while (search-forward "&#xa0;&#xa0;&#xa0;<span class=\"tag\">" nil t)
    ;;   (let ((beg (match-beginning 0)))
    ;;     (search-forward "</span></span>" (line-end-position))
    ;;     (delete-region beg (point))))

    ;; Now parse XML because some things are easier to do to a parsed tree than
    ;; to a text buffer
    (setq dom (libxml-parse-html-region)))

  ;; Internal link stuff
  (cl-loop
   for anchor in (dom-by-tag dom 'a)
   as href = (dom-attr anchor 'href)
   as hash = (when href (cadr (split-string href "#")))
   ;; HACK now links have #ID-id:f4oirm34ofkmr3o23pm
   ;; which is a bug, or just a change that org-roam-export
   ;; hasn't caught up with
   as uuid? = (when hash (->> hash
                              (string-replace "ID-" "")
                              (string-replace "id:" "")))
   when (and uuid? (org-uuidgen-p uuid?))
   do (let ((id-old-v2 (my-uuid-to-pageid-old-v2 uuid?))
            (id (my-uuid-to-pageid uuid?))
            (target-tags (-flatten (org-roam-db-query
                                    `[:select [tag]
                                      :from tags
                                      :where (= node-id ,uuid?)]))))

        ;; Replace all UUID with my shortened form.
        ;; Then remove the hash-part of the link (i.e. the bit after the #
        ;; character in domain.com/PAGE-ID/slug#HEADING-ID) if the HEADING-ID
        ;; matches PAGE-ID anyway (i.e. it's a file-level id)
        (dom-set-attribute anchor 'href (my-strip-hashlink-if-same-as-permalink
                                         (string-replace hash id href)))

        ;; Record old ID for redirects on the website
        (unless (assoc id-old-v2 my-id-old-new-alist)
          (push (cons id-old-v2 id) my-id-old-new-alist))

        ;; Associate ID with original UUID to check for collisions
        (push uuid? (alist-get id my-ids nil nil #'equal))
        ;; (map-put! my-ids id (cons uuid? (map-elt my-ids id)))

        ;; NOTE: This attempt to avoid heading-self-links also means no links
        ;; inside headings will be treated.  I mandate a rule against links in
        ;; headings anyway, so it just affects my Logseq pages.
        (unless (member (dom-tag (dom-parent dom anchor)) '(h2 h3 h4 h5 h6))
          ;; Style the link based on target tag
          (when target-tags
            (dom-set-attribute anchor 'class (string-join target-tags " "))))))

  ;; Translate heading IDs too
  (cl-loop for heading in (--mapcat (dom-by-tag dom it) '(h2 h3 h4 h5 h6))
           as id = (dom-attr heading 'id)
           when id do
           (let ((uuid? (string-replace "ID-" "" id)))
             (when (org-uuidgen-p uuid?)
               (let ((pageid (my-uuid-to-pageid uuid?)))
                 (dom-set-attribute heading 'id pageid)
                 (dom-append-child heading (dom-node
                                            'a
                                            `((href . ,(concat "#" pageid)))
                                            " 🔗"))))))

  (funcall
   (defun recurse-avoiding-code-blocks (dom)
     (cl-loop for child in (dom-children dom)
              ;; Org-export doesn't replace double/triple-dash in all
              ;; situations (like in a heading or when it butts up against a
              ;; link on a newline), so force it
              if (stringp child)
              do (let ((fixed-dashes (->> child
                                          (string-replace "---" "—")
                                          (string-replace "--" "–"))))
                   (unless (equal fixed-dashes child)
                     (dom-add-child-before dom fixed-dashes child)
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

  ;; Correct img paths to assets
  (cl-loop for img in (dom-by-tag dom 'img)
           as path = (dom-attr img 'src)
           when (string-prefix-p "attach" path)
           do (dom-set-attribute img 'src (concat "/" path))
           ;; Org exports an image alt-text that is just the image
           ;; basename. Bad default, better no alt-text then.
           and when (string-search (dom-attr img 'alt) path)
           do (dom-remove-attribute img 'alt))

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
    ;; Remove <body>/<html>
    (goto-char (point-min))
    (delete-line)
    (delete-line)
    (goto-char (point-max))
    (delete-line)
    (delete-line)
    ;; (while (re-search-forward "^</?body>\\|^</?html>" nil t)
    ;;   (replace-match "")
    ;;   (delete-blank-lines))
    (buffer-string)))
