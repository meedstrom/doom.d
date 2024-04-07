;; -*- lexical-binding: t; -*-
;; See also 02-lib-publishing.el

(defvar my-tags-to-avoid-uploading '("noexport" "archive" "private" "censor"))
(defvar my-tags-for-hiding '("gri" "shrink" "privy" "lover" "fren"))

;; TODO: Upstream
;; Override the info: link type so it won't get exported into an empty <a href>
;; with not even a link description.  What nasty default behavior!  See
;; `org-link-parameters' if I need the same treatment for more link types.
(after! ol-info
  (defun org-info-export (path desc _format)
    (or desc path)))

;; TODO: Upstream
;; Give each h2...h6 heading (and its container div) an ID attribute that
;; matches the source Org heading ID property, if it has one, instead of
;; e.g. "org953031".  That way, hash-links such as
;; #ID-e10bbdfe-1ffb-4c54-9228-2818afdfc5ba will make the web browser jump to
;; that heading.  Thank org-roam for including this code!  Note that I convert
;; these IDs later using `my-uuid-to-short'.
(after! ox
  (require 'org-roam-export))

(setopt org-publish-project-alist
        `(("my-slipbox-blog"
           :base-directory "/tmp/roam/org/"
           :publishing-directory "/tmp/roam/html/"
           :publishing-function my-publish-to-blog
           :recursive t
           :body-only t
           :section-numbers nil
           :headline-levels 5  ;; Go all the way to <h6> before making <li>
           :with-toc nil
           :with-tags nil
           :with-todo-keywords nil
           :with-smart-quotes nil
           :with-latex 'html ;; Use `org-latex-to-html-convert-command'
           :with-drawers '(not "logbook" "noexport") ;; (case-insensitive FYI)
           ;; NOTE: This only excludes subtrees, so we also check file-level
           ;; tag in `my-publish-to-blog'.  Maybe upstream a patch?
           :exclude-tags ,my-tags-to-avoid-uploading)))

;; This function is where I can make destructive env changes that I
;; don't want in my main Emacs.
(defun my-publish ()
  "All-in-one command for use in a child emacs.

With C-u, first rebuild the org-roam-db that's in /tmp, else reuse
it from some past run (makes sense if you only changed the export
code, but not the notes).

With C-u C-u, also run `my-validate-org-buffer' on each file
scanned."
  (interactive)
  (require 'org-roam)
  (require 'org-agenda)
  (require 'ox-publish)
  (require 'f)
  (require 'prism)
  (require 'dash)
  (view-echo-area-messages) ;; for watching it work
  (setq org-export-use-babel nil)
  (setq org-export-with-broken-links nil) ;; links would disappear quietly!
  (setq org-html-extension "")
  (setq org-html-checkbox-type 'unicode)
  (setq org-html-html5-fancy t)
  ;; TODO: upstream as an option for `org-preview-latex-process-alist'
  (setq org-latex-to-html-convert-command "node /home/kept/pub/texToMathML.js %i")
  (setq save-silently t)
  (setq org-inhibit-startup t) ;; from org-publish-org-to
  (setq debug-on-error t)
  (my-remove-all-advice 'org-roam-db-update-file) ;; disable my hacks

  ;; Speed up publishing
  (gcmh-mode 0)
  (setq gc-cons-threshold (* 4 1000 1000 1000))
  (setq org-mode-hook nil)
  (fset 'org-publish-write-cache-file #'ignore) ;; mega speedup!

  ;; Attempt to speed up publishing, not sure these help much
  (advice-remove 'after-find-file #'doom--shut-up-autosave-a)
  (remove-hook 'org-export-before-parsing-functions #'org-attach-expand-links)
  (setq whitespace-global-modes nil)
  (my-disable-modes-if-present '(apheleia-global-mode
                                 auto-encryption-mode
                                 auto-save-mode
                                 auto-save-visited-mode
                                 awesome-tray-mode
                                 beginend-global-mode
                                 better-jumper-mode
                                 context-menu-mode
                                 diff-hl-flydiff-mode
                                 dired-hist-mode
                                 editorconfig-mode
                                 electric-indent-mode
                                 global-diff-hl-mode
                                 global-eldoc-mode
                                 global-emojify-mode
                                 global-form-feed-mode
                                 global-ligature-mode
                                 global-prettify-symbols-mode
                                 my-auto-commit-mode
                                 nerd-icons-completion-mode
                                 pixel-scroll-precision-mode
                                 projectile-mode
                                 recentf-mode
                                 repeat-mode
                                 save-place-mode
                                 savehist-mode
                                 show-paren-mode
                                 smartparens-global-mode
                                 solaire-global-mode
                                 window-divider-mode
                                 winner-mode
                                 ws-butler-global-mode))

  ;; For hygiene, ensure that this subordinate emacs syncs nothing to disk
  (eager-state-preempt-kill-emacs-hook-mode 0)
  (setq kill-emacs-hook nil)

  ;; Switch theme for 2 reasons
  ;; 1. Remind me that this is not my normal Emacs
  ;; 2. The theme carries over to code blocks, so ensure it's a theme that
  ;;    looks okay in both light and dark mode
  (let ((theme 'doom-monokai-machine))
    ;; (let ((theme 'ef-rosa))
    ;; (let ((theme 'doom-zenburn))
    ;; (let ((theme 'doom-rouge))
    (unless (member theme custom-enabled-themes)
      (load-theme theme)))

  ;; Copy the files to /tmp to work from there
  (mkdir "/tmp/roam" t)
  (shell-command "rm -rfv /tmp/roam/org/")
  (shell-command "cp -a /home/kept/roam /tmp/roam/org")

  ;; Pretty-print a post of recent completed todos
  ;; (cl-letf ((org-agenda-files '("/tmp/roam/org/noagenda/archive.org")))
  (let ((org-agenda-files '("/tmp/roam/org/noagenda/archive.org")))
    (my-generate-todo-log "/tmp/roam/org/todo-log.org"))

  ;; Ensure that each post URL will contain a unique ID by now placing them in
  ;; subdirectories named by that ID, so org-export will translate all
  ;; org-id links into these relative filesystem paths.
  (cl-loop for path in (directory-files-recursively "/tmp/roam/org/" "\\.org$")
           unless (and (not (string-search ".sync-conflict-" path))
                       (not (string-search "/logseq/" path))
                       (let* ((uuid (my-org-file-id path))
                              (newdir (concat "/tmp/roam/org/"
                                              (my-uuid-to-short uuid)
                                              "/")))
                         (mkdir newdir t)
                         (rename-file path newdir)
                         t))
           do (delete-file path))

  (when (equal current-prefix-arg '(4))
    (shell-command "rm /tmp/roam/org-roam.db"))
  (when (equal current-prefix-arg '(16))
    (shell-command "rm /tmp/roam/org-roam.db")
    (add-hook 'my-org-roam-pre-scan-hook #'my-validate-org-buffer))

  ;; Tell `org-id-locations' and the org-roam DB about the new work directory
  (setq org-roam-directory "/tmp/roam/org/")
  (setq org-roam-db-location "/tmp/roam/org-roam.db")
  (setq org-agenda-files '("/tmp/roam/org/"))
  (setq org-id-locations-file "/tmp/roam/org-id-locations")
  (unless (file-exists-p org-roam-db-location)
    (org-id-update-id-locations) ;; find files with ROAM_EXCLUDE too
    (org-roam-update-org-id-locations)
    (org-roam-db-sync 'force))

  ;; Reset
  (shell-command "rm -rf /tmp/roam/{html,json,atom}/")
  (shell-command "mkdir -p /tmp/roam/{html,json,atom}")
  (setq my-ids (clrhash my-ids))

  ;; Change some things about the Org files, before org-export does its thing.
  (add-hook 'org-export-before-parsing-functions #'my-add-backlinks 10)
  (add-hook 'org-export-before-parsing-functions #'my-ensure-section-containers 20)
  (add-hook 'org-export-before-parsing-functions #'my-add-refs-as-paragraphs)
  (add-hook 'org-export-before-parsing-functions #'my-replace-datestamps-with-links)
  (add-hook 'org-export-before-parsing-functions #'my-strip-inline-anki-ids)
  (add-hook 'org-export-before-parsing-functions #'org-transclusion-mode)

  (org-publish "my-slipbox-blog" t)
  (my-check-id-collisions)
  (my-compile-atom-feed "/tmp/roam/posts.atom" "/tmp/roam/atom/")
  (find-file "/home/kept/pub/")
  (async-shell-command "./encrypt-rebuild.sh")
  (start-process "firefox" nil "firefox" "http://localhost:5173"))

(defun my-publish-to-blog (plist filename pub-dir)
  "Take org file FILENAME and make html file in PUB-DIR.
Then postprocess that same html into a json file and maybe an
atom entry.

Designed to be called by `org-publish', all arguments pass
through to `org-html-publish-to-html'."
  (redisplay) ;; Let me watch it work
  (if (-intersection (my-org-file-tags filename) my-tags-to-avoid-uploading)
      ;; If we already know we won't publish it, don't export the file at all.
      ;; Saves so much time.  Some other issues can also disqualify the file,
      ;; but I take care of them in `my-validate-org-buffer'.
      (message "Found exclude-tag, excluding: %s" filename)
    (when-let ((open (find-buffer-visiting filename)))
      (cl-assert (not (buffer-modified-p open)))
      (cl-assert (memq (buffer-local-value 'buffer-undo-list open) '(t nil)))
      (kill-buffer open))
    (with-current-buffer (find-file-noselect filename)
      (goto-char (point-min))
      (let* (;; Black box by the grace of Bastien, Carsten &c
             (html-path (org-html-publish-to-html plist filename pub-dir))
             ;; (html-path (org-export-output-file-name org-html-extension nil pub-dir))
             ;; Collect metadata from Org source
             (keywords (org-collect-keywords '("DATE" "SUBTITLE")))
             (tags (mapcar #'downcase (org-get-tags)))
             
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
             (pageid (-last-item (split-string pub-dir "/" t)))
             (hidden (not (null (-intersection my-tags-for-hiding tags))))
             (metadata
              (list
               (cons 'pageid pageid)
               (cons 'slug (string-replace pub-dir "" html-path))
               (cons 'created created)
               (cons 'createdFancy created-fancy) ;; JS camelCase
               (cons 'updated updated)
               (cons 'updatedFancy updated-fancy)
               (cons 'title (if (member "daily" tags)
                                created-fancy
                              (->> (org-get-title)
                                   (string-replace "---" "—")
                                   (string-replace "--" "–"))))
               (cons 'description (car (map-elt keywords "SUBTITLE")))
               (cons 'wordcount
                     (save-excursion
                       (cl-loop
                        while (re-search-forward my-org-text-line-re nil t)
                        if (and (eq (preceding-char) ?*)
                                (member "noexport" (org-get-tags)))
                        ;; Don't count words under hidden subtrees
                        do (org-next-visible-heading 1)
                        else sum (count-words (point) (line-end-position)))))
               (cons 'linkcount
                     (save-excursion
                       (cl-loop
                        while (re-search-forward org-link-bracket-re nil t)
                        if (member "noexport" (org-get-tags))
                        do (org-next-visible-heading 1)
                        else count t)))
               (cons 'tags tags)
               (cons 'hidden hidden)))
             ;; `((pageid . ,pageid)
             ;;   (slug . ,(string-replace pub-dir "" html-path))
             ;;   (created . ,created)
             ;;   (createdFancy . ,created-fancy) ;; JS camelCase
             ;;   (updated . ,updated)
             ;;   (updatedFancy . ,updated-fancy)
             ;;   (title .  ,(if (member "daily" tags)
             ;;                  created-fancy
             ;;                (->> (org-get-title)
             ;;                     (string-replace "---" "—")
             ;;                     (string-replace "--" "–"))))
             ;;   (description . ,(car (map-elt keywords "SUBTITLE")))
             ;;   (wordcount . ,(save-excursion
             ;;                   (cl-loop
             ;;                    while (re-search-forward my-org-text-line-re nil t)
             ;;                    if (and (eq (preceding-char) ?*)
             ;;                            (member "noexport" (org-get-tags)))
             ;;                    ;; Don't count words under hidden subtrees
             ;;                    do (org-next-visible-heading 1)
             ;;                    else sum (count-words (point) (line-end-position)))))
             ;;   (linkcount . ,(save-excursion
             ;;                   (cl-loop
             ;;                    while (re-search-forward org-link-bracket-re nil t)
             ;;                    if (member "noexport" (org-get-tags))
             ;;                    do (org-next-visible-heading 1)
             ;;                    else count t)))
             ;;  (tags . ,tags)
             ;;  (hidden . ,hidden)))
             ;; Final "post object" for use by blog
             (post
              (-snoc metadata
                     `(content . ,(my-customize-the-html html-path metadata))))
             (uuid (org-id-get)))
        ;; Write JSON object
        (with-temp-file (concat "/tmp/roam/json/" pageid)
          (insert (json-encode post)))
        ;; Write Atom entry if it's an okay post for the feed
        (when (and (not hidden)
                   (not (-intersection tags '("tag" "daily" "stub")))
                   (string-lessp "2023" (or updated created)))
          (with-temp-file (concat "/tmp/roam/atom/" pageid)
            (insert (my-make-atom-entry post uuid)))))
      (kill-buffer (current-buffer)))))

(defun my-customize-the-html (html-path metadata)
  "Take contents of HTML-PATH and return customized content."
  (if (f-empty-p html-path)
      ""
    (let ((dom (with-temp-buffer
                 (buffer-disable-undo)
                 (insert-file-contents html-path)
                 ;; Give the ToC div a class and remove its pointless inner div
                 (when (re-search-forward "^<div id=\"table-of-contents\".*?>" nil t)
                   (replace-match "<nav class=\"toc\" role=\"doc-toc\">")
                   (re-search-forward "^<div id=\"text-table-of-contents\".*?>")
                   (replace-match "")
                   (search-forward "</div>\n</div>")
                   (replace-match "</nav>"))
                 ;; Add role="doc-endnotes"
                 ;; (goto-char (point-min))
                 ;; (when (re-search-forward "^<h2 id.*>What links here" nil t)
                 ;;   (forward-line -1)
                 ;;   (search-forward " class=\"outline-2\"" (line-end-position))
                 ;;   (insert " role=\"doc-endnotes\""))

                 (libxml-parse-html-region))))

      ;; Declutter unused classes
      (cl-loop for node in (--mapcat (dom-by-class dom it) '("org-ul" "org-ol"))
               do (dom-remove-attribute node 'class))

      ;; Edit the .outline-2, .outline-3... divs that Org generated to enable
      ;; the CSS selectors "section.even" and "section.odd".
      ;; (Already converted div->section in `my-ensure-section-containers').
      (cl-loop
       for section in (dom-by-tag dom 'section)
       as class = (dom-attr section 'class)
       as parity = (if (string-match-p "[246]" class) "even" "odd")
       do (dom-set-attribute section 'class (concat parity " " class)))

      ;; Mess with internal links
      (cl-loop
       for anchor in (dom-by-tag dom 'a)
       as href = (dom-attr anchor 'href)
       as hash = (when href (cadr (split-string href "#")))
       as uuid? = (when hash (->> hash
                                  (string-replace "ID-" "")
                                  (string-replace "id:" "")))
       when (and uuid? (org-uuidgen-p uuid?))
       do (let ((shortid (my-uuid-to-short uuid?))
                (target-tags (-flatten (org-roam-db-query
                                        `[:select [tag]
                                          :from tags
                                          :where (= node-id ,uuid?)]))))
            ;; Replace all UUID with my shortened form, and strip the #HEADING-ID
            ;; if it matches /PAGE-ID.
            (dom-set-attribute anchor 'href (my-strip-hash-if-matches-base
                                             (string-replace hash shortid href)))
            ;; https://www.w3.org/TR/dpub-aria-1.0/#doc-backlink
            (let-alist metadata
              (when (equal shortid .slug)
                (dom-set-attribute anchor 'role "doc-backlink")))
            ;; Associate short-ID with original UUID to check for collisions later
            (push uuid? (gethash shortid my-ids))
            ;; Style the link based on tags of target document
            (when target-tags
              (dom-set-attribute anchor 'class (string-join target-tags " ")))))

      ;; Format undescribed links more nicely
      (cl-loop for anchor in (dom-by-tag dom 'a)
               as children = (dom-children anchor)
               as desc = (car children)
               as fixed-desc = (when (and desc (stringp desc))
                                 (->> desc
                                      (replace-regexp-in-string "^http.?://" "")
                                      ;; (replace-regexp-in-string "\?.*" "")
                                      (string-replace "%20" " ")))
               when fixed-desc
               unless (equal fixed-desc desc)
               do (progn
                    (dom-add-child-before anchor fixed-desc desc)
                    (dom-remove-node anchor desc)))

      ;; Fix IDs for sections and add self-links next to headings
      (let-alist metadata
        (cl-loop
         for section in (dom-by-tag dom 'section)
         as id = (string-replace "outline-container-" "" (dom-attr section 'id))
         as heading = (car (dom-non-text-children section))
         as uuid? = (string-replace "ID-" "" id)
         do (if (org-uuidgen-p uuid?)
                ;; The id= looked like:
                ;; "outline-container-ID-e9eaf8ff-b2ea-4891-8e93-bbb426f277c1"
                (let ((hashid (my-uuid-to-short uuid?)))
                  (dom-set-attribute section 'id hashid)
                  ;; Add a self-link.  Hardcode the canonical URL, because my
                  ;; blog may display the same post on several paths.
                  (dom-append-child heading
                                    (dom-node 'a
                                              `((href . ,(concat "/" .pageid
                                                                 "/" .slug
                                                                 "#" hashid))
                                                (class . "heading-permalink")
                                                (rel . "bookmark"))
                                              "🔗")))
              ;; The id= looked like:
              ;; "outline-container-org1234567"
              (dom-set-attribute section 'id id))))

      ;; Org-export doesn't replace double/triple-dash in all situations (like
      ;; in a heading or when it butts up against a link on a newline), so
      ;; force it
      (cl-labels ((fix-non-code-nodes (dom)
                    (cl-loop
                     for child in (dom-children dom)
                     if (stringp child)
                     do (let ((fixed-child (->> child
                                                (string-replace "---" "—")
                                                (string-replace "--" "–"))))
                          (unless (equal fixed-child child)
                            (dom-add-child-before dom fixed-child child)
                            (dom-remove-node dom child)))
                     else if (not (member (dom-tag child) '(code pre kbd samp)))
                     do (progn
                          ;; Bonus: strip unnecessary ID attributes
                          (unless (eq (dom-tag child) 'section)
                            (dom-remove-attribute child 'id))
                          (fix-non-code-nodes child)))))
        (fix-non-code-nodes dom))

      ;; Wrap tables in divs that can be scrolled left-right
      (cl-loop for tbl in (dom-by-tag dom 'table)
               as parent = (dom-parent dom tbl)
               as wrapped-tbl = (dom-node 'div
                                          '((class . "table-container"))
                                          (copy-sequence tbl))
               do (progn
                    (dom-add-child-before parent wrapped-tbl tbl)
                    (dom-remove-node parent tbl)))

      ;; Fix img attributes
      (cl-loop for img in (dom-by-tag dom 'img)
               as path = (dom-attr img 'src)
               as alt = (dom-attr img 'alt)
               when (string-prefix-p "attach" path)
               ;; Fix paths
               do (dom-set-attribute img 'src (concat "/" path))
               ;; Org exports an image alt-text that is just the image
               ;; basename.  Interesting idea, since the alt-text becomes
               ;; portable if that's where you put the alt-text!  Go with it
               ;; and just strip .jpg/.png extension.
               and when (string-search alt path)
               do (dom-set-attribute img 'alt (->> alt
                                                   (file-name-sans-extension)
                                                   (string-replace "_" " "))))

      ;; Let images that aren't links become self-links.  Such links, that
      ;; point to resources on the same domain, also need rel="external" in
      ;; order to prevent SvelteKit from interpreting the URL as a SPA route.
      (cl-loop for img in (dom-by-tag dom 'img)
               as parent = (dom-parent dom img)
               unless (eq 'a (dom-tag parent))
               do (let ((linkified-img
                         (dom-node 'a `((href . ,(dom-attr img 'src))
                                        (rel . "external"))
                                   (copy-sequence img))))
                    (dom-add-child-before parent linkified-img img)
                    (dom-remove-node parent img)))

      ;; Return final HTML.  Phew!
      (with-temp-buffer
        (dom-print dom)
        (search-backward "</body></html>")
        (replace-match "")
        (goto-char (point-min))
        (search-forward "<html><body>")
        (replace-match "")
        (buffer-string)))))
