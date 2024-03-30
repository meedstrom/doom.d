;; -*- lexical-binding: t; -*-
;; See also 02-lib-publishing.el
(require 'dash)

(defvar my-tags-to-avoid-uploading '("noexport" "archive" "private" "censor"))
(defvar my-tags-for-hiding '("gri" "shrink" "privy" "lover" "fren"))
(defvar my-refs-cache nil)

;; TODO: Upstream
;; Override the info: link type so it won't get exported into an empty <a
;; href>, not even a link description.  What nasty default behavior!  May need
;; more overrides like this for each link type, see `org-link-parameters'.
(after! ol-info
  (defun org-info-export (path desc _format)
    (or desc path)))

;; TODO: Upstream
;; Give each h2...h6 heading an ID attribute that matches its source org-id, if
;; it has one, instead of e.g. "org953031".  That way, hash-links such as
;; #ID-e10bbdfe-1ffb-4c54-9228-2818afdfc5ba will make the web browser jump to
;; that heading.  Thank org-roam for including this code!  Note that I
;; convert these IDs later using `my-uuid-to-short'.
(after! ox
  (require 'org-roam-export))

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
  (require 'prism)
  (view-echo-area-messages) ;; for watching it work
  (setopt org-export-use-babel nil)
  (setopt org-export-with-drawers '(not "logbook" "noexport")) ;; case-insensitive
  (setopt org-export-exclude-tags my-tags-to-avoid-uploading)
  (setopt org-export-with-broken-links nil) ;; links would disappear quietly!
  (setopt org-export-with-smart-quotes nil)
  (setopt org-export-with-tags nil)
  (setopt org-export-with-todo-keywords nil)
  (setopt org-export-headline-levels 5) ;; go all the way to <h6> before making <li>
  ;; BUG If we don't set this to "", there will be .html inside some links even
  ;; though I set "" in the `org-publish-org-to' call.
  (setopt org-html-extension "")
  (setopt org-html-checkbox-type 'unicode)
  (setopt org-html-html5-fancy t)
  ;; why does it skip envs like \begin{align}?
  ;; (setopt org-html-with-latex 'verbatim)
  (setopt org-html-with-latex 'html) ;; use `org-latex-to-html-convert-command'
  (setopt org-latex-to-html-convert-command "node /home/kept/pub/texToMathML.js '%i'")
  (setopt org-inhibit-startup t) ;; from org-publish-org-to
  (setopt save-silently t)
  (setq debug-on-error t)
  ;; (setq debug-on-quit t)
  (my-remove-all-advice 'org-roam-db-update-file) ;; disable vulpea

  ;; Speed up publishing
  (setq org-mode-hook nil)
  (gcmh-mode 0)
  (setq gc-cons-threshold (* 4 1000 1000 1000))
  (fset 'org-publish-write-cache-file #'ignore) ;; huge effect!

  ;; Attempt to speed up publishing, not sure these help much
  (advice-remove 'after-find-file #'doom--shut-up-autosave-a)
  (remove-hook 'org-export-before-parsing-functions #'org-attach-expand-links)
  (setq whitespace-global-modes nil)
  (my-disable-modes-if-present
   '(apheleia-global-mode
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
  ;; TODO: Review if I ever need these
  (when (modulep! org)
    (undefadvice! '+org--fix-async-export-a :around '(org-export-to-file org-export-as))
    (undefadvice! '+org-babel-disable-async-maybe-a :around #'ob-async-org-babel-execute-src-block))

  ;; For hygiene, ensure that this subordinate emacs syncs nothing to disk
  (my-state-sync-mode 0)
  ;; (preempt-kill-emacs-hook-mode 0)
  (setq kill-emacs-hook nil)

  ;; Switch theme for 2 reasons
  ;; 1. Show me that this is not my normal Emacs
  ;; 2. The theme carries over to code blocks, so ensure it's a theme that
  ;;    suits both light and dark mode
  (let ((theme 'doom-monokai-machine))
    ;; (setq theme 'doom-rouge)
    ;; (setq theme 'ef-rosa)
    ;; (setq theme 'doom-zenburn)
    (unless (member theme custom-enabled-themes)
      (load-theme theme)))

  ;; Copy the files to /tmp to work from there
  (shell-command "rm -rfv /tmp/roam/org/")
  (shell-command "cp -a /home/kept/roam /tmp/roam/org")

  ;; Make a pretty post of recent completed todos
  (my-generate-todo-log-from "/tmp/roam/org/noagenda/archive.org")

  ;; Ensure that each post URL will contain a unique ID by now placing them in
  ;; subdirectories named by that ID, so org-export will translate all
  ;; org-id links into these filesystem paths.
  (cl-loop for path in (directory-files-recursively "/tmp/roam/org/" "\\.org$")
           unless (and (not (string-search ".sync-conflict-" path))
                       (not (string-search "/logseq/" path))
                       (let* ((uuid (my-org-file-id path))
                              (new (concat "/tmp/roam/org/"
                                           (my-uuid-to-short uuid)
                                           "/")))
                         (mkdir new t)
                         (rename-file path new)
                         (make-symbolic-link "../attachments"
                                             (concat new "attachments"))
                         t))
           do (delete-file path))

  (when (equal current-prefix-arg '(4))
    (shell-command "rm /tmp/roam/org-roam.db"))
  (when (equal current-prefix-arg '(16))
    (shell-command "rm /tmp/roam/org-roam.db")
    (add-hook 'my-org-roam-pre-scan-hook #'my-validate-org-buffer))

  ;; Tell `org-id-locations' and the org-roam DB about the new work directory.
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
  ;; Used by `my-replace-web-links-with-ref-note-links'
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

  ;; (org-publish "my-slipbox-blog-attachments" t)
  (org-publish "my-slipbox-blog" t)

  ;; (shell-command "rm -r /tmp/roam/org/") ;; so recentf won't take me there
  (my-check-id-collisions)
  (my-make-atom-feed "/tmp/roam/posts.atom" "/tmp/roam/atom/")
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
    ;; Begin postprocess
    (when-let ((already-open (find-buffer-visiting filename)))
      (cl-assert (not (buffer-modified-p already-open)))
      (cl-assert (memq (buffer-local-value 'buffer-undo-list already-open)
                       '(t nil)))
      (kill-buffer already-open))
    (with-current-buffer (find-file-noselect filename)
      (goto-char (point-min))
      (let* ((html-path (org-export-output-file-name "" nil pub-dir))
             (keywords (org-collect-keywords '("date" "subtitle")))
             (uuid (org-id-get))
             (tags (-flatten (org-get-tags)))
             (created (substring (org-entry-get nil "CREATED") 1 -1))
             (updated (let ((value (map-elt keywords "DATE")))
                        (when (and value (not (string-blank-p (car value))))
                          (substring (car value) 1 -1))))
             ;; underscore bc of js
             (created_fancy
              (format-time-string (car org-timestamp-custom-formats)
                                  (date-to-time created)))
             (updated_fancy
              (when updated
                (format-time-string (car org-timestamp-custom-formats)
                                    (date-to-time updated))))
             (pageid (-last-item (split-string pub-dir "/" t)))
             (hidden (car (-intersection my-tags-for-hiding tags)))
             (metadata
              `((pageid . ,pageid)
                (slug . ,(string-replace pub-dir "" html-path))
                (hidden . ,hidden)
                (created . ,created)
                (created_fancy . ,created_fancy)
                (updated . ,updated)
                (updated_fancy . ,updated_fancy)
                (title .  ,(if (member "daily" tags)
                               created_fancy
                             (->> (org-get-title)
                                  (string-replace "---" "—")
                                  (string-replace "--" "–"))))
                (tags . ,tags)
                (description . ,(car (map-elt keywords "SUBTITLE")))))
             (post (-snoc metadata
                          `(content . ,(my-customize-the-html metadata html-path)))))
        ;; Write JSON object
        (with-temp-file (concat "/tmp/roam/json/" pageid)
          (insert (json-encode post)))
        ;; Write Atom entry if it's an ok one for the feed
        (when (and (not hidden)
                   (not (-intersection tags '("tag" "daily" "stub")))
                   (string-lessp "2023" (or updated created)))
          (with-temp-file (concat "/tmp/roam/atom/" pageid)
            (insert (my-make-atom-entry post uuid)))))
      (kill-buffer (current-buffer)))))

(defun my-customize-the-html (metadata html-path)
  (let-alist metadata
    (let ((dom nil)
          (after-toc (make-marker)))
      (with-temp-buffer
        (buffer-disable-undo)
        (insert-file-contents html-path)

        ;; Give the ToC div a class and remove its pointless inner div
        ;; (This part seems easy to do with dom.el, but first have to figure out
        ;; how do the rest in dom.el)
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

        ;; Now parse HTML because most things are easier & safer to do to a
        ;; parsed tree than to a text buffer
        (setq dom (libxml-parse-html-region)))

      ;; New version
      ;; (when-let ((toc (dom-by-id dom "table-of-contents")))
      ;;   (let ((new-toc (dom-node 'nav ((class . "toc")) (dom-children toc))))
      ;;     (dom-add-child-before dom new-toc toc)
      ;;     (dom-remove-node dom toc)))

      ;; (cl-loop
      ;;  for div in (dom-by-class dom "^outline-.$")
      ;;  as parity = (if (string-match-p "[246]" (dom-attr div 'class))
      ;;                  "even"
      ;;                "odd")
      ;;  as new = (dom-node 'section `((class . ,parity)) (dom-children div))
      ;;  )

      ;; (cl-loop for section in (dom-by-tag dom 'section)
      ;;          as children = (dom-children section)
      ;;          do (--find-index (eq 'section (dom-tag it)) children)
      ;;          )

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
            ;; Associate short-ID with original UUID to check for collisions
            (push uuid? (gethash shortid my-ids))
            ;; Style the link based on target tag
            (when target-tags
              (dom-set-attribute anchor 'class (string-join target-tags " ")))))

      ;; Format undescribed links more nicely
      (cl-loop for anchor in (dom-by-tag dom 'a)
               as children = (dom-children anchor)
               as desc = (car children)
               as fixed-desc = (when (and desc (stringp desc))
                                 (->> desc
                                      (replace-regexp-in-string "^http.?://" "")
                                      (string-replace "%20" " ")))
               when fixed-desc
               unless (equal fixed-desc desc)
               do (progn
                    (dom-add-child-before anchor fixed-desc desc)
                    (dom-remove-node anchor desc)))

      ;; Mess with headings
      (cl-loop for heading in (--mapcat (dom-by-tag dom it) '(h2 h3 h4 h5 h6))
               as id = (dom-attr heading 'id)
               when (and id (stringp id)) do
               (let ((uuid? (string-replace "ID-" "" id)))
                 (when (org-uuidgen-p uuid?)
                   (let* ((hashid (my-uuid-to-short uuid?))
                          ;; Since my blog's Note component may appear on other
                          ;; routes than the canonical one (like /recent/400 or
                          ;; /unlocked/...), hardcode the full URL
                          (selflink (dom-node 'a
                                              `((href . ,(concat "/" .pageid "/" .slug "#" hashid))
                                                (class . "heading-permalink")
                                                (rel . "bookmark"))
                                              "🔗")))
                     (dom-set-attribute heading 'id hashid)
                     (dom-append-child heading selflink)))))

      ;; Org-export doesn't replace double/triple-dash in all situations (like
      ;; in a heading or when it butts up against a link on a newline), so
      ;; force it
      (cl-labels ((fix-text-nodes-except-code-blocks (dom)
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
                     do (fix-text-nodes-except-code-blocks child))))
        (fix-text-nodes-except-code-blocks dom))

      ;; Wrap tables in divs that can be scrolled left-right
      (cl-loop for tbl in (dom-by-tag dom 'table)
               as parent = (dom-parent dom tbl)
               as wrapped-tbl = (dom-node 'div
                                          '((class . "table-container"))
                                          (copy-sequence tbl))
               do (progn
                    (dom-add-child-before parent wrapped-tbl tbl)
                    (dom-remove-node parent tbl)))

      ;; Mess with img elements
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

      ;; FIXME: I think it actually replaces all image links
      ;; Let images that aren't links become self-links.  Such links also need
      ;; rel="external" in order to prevent SvelteKit from interpreting the URL
      ;; as a route and executing the route.
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
