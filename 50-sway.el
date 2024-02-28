;; -*- lexical-binding: t; -*-

;; NOTE: `sway-list-windows' returns a list of tables, not a table of tables.
;;        Also it doesn't catch floating windows.  To see what keys are in one
;;        of those tables, do (map-keys (car (sway-list-windows))).

;; unused untested
(defun my-sway-frame-by-id (con-id)
  "Return the Emacs frame corresponding to a Sway window CON-ID.
Does this by matching frame names, so you may want to ensure
they are always uniquified."
  (when-let* ((sway-window (--find (eq con-id (map-elt it "id"))
                                   (sway-list-windows)))
              (swayname (map-elt sway-window "name")))
    (--find (equal (frame-parameter it 'name) swayname)
            (frame-list))))

(defun sway-find-wayland-window-frame (window)
  "Return the Emacs frame corresponding to a Sway window WINDOW.
Does this by matching frame names, so you may want to ensure
they are always uniquified.

WINDOW is a hash table, typically one of the members of
`sway-list-windows'."
  (let ((name (map-elt window "name")))
    (--find (equal (frame-parameter it 'name) name)
            (frame-list))))

(defun my-swaymsg2 (cmd)
  "Handle CMD if possible, otherwise pass to the real swaymsg."
  (condition-case nil
      (cond
       ((equal cmd "focus next")
        (call-interactively #'other-window)
        (when (eq (selected-window) (frame-first-window))
          (signal "completed loop, bye emacs frame")))
       ;; windmove commands will error-out at an edge, as we want.
       ((equal cmd "focus left") (windmove-left))
       ((equal cmd "focus right") (windmove-right))
       ((equal cmd "focus up") (windmove-up))
       ((equal cmd "focus down") (windmove-down))
       ((equal cmd "move left") (windmove-swap-states-left))
       ((equal cmd "move right") (windmove-swap-states-right))
       ((equal cmd "move up") (windmove-swap-states-up))
       ((equal cmd "move down") (windmove-swap-states-down))
       (t (signal "unrecognized command")))
    (error
     ;; we couldn't handle it
     (start-process-shell-command cmd nil (concat "swaymsg " cmd)))))

(use-package! sway :disabled
              :config
              (el-patch-defun sway-list-frames (&optional tree visible-only focused-only)
                "List all Emacs frames in TREE.

VISIBLE-ONLY and FOCUSED-ONLY select only frames that are,
respectively, visible and focused.

Return value is a list of (FRAME-OBJECT . SWAY-ID)"
                (unless tree (setq tree (sway-tree)))
                (let* ((wins (sway-list-windows tree visible-only focused-only)))
                  (seq-filter (lambda (x) (car x))
                              (-zip
                               (mapcar (if (eq window-system 'pgtk)
                                           #'sway-find-wayland-window-frame
                                         #'sway-find-x-window-frame)
                                       wins)
                               (mapcar #'sway-get-id wins)))))
              (setq Man-notify-method 'pushy)
              ;; (setq shackle-display-buffer-frame-function #'sway-shackle-display-buffer-frame)
              ;; (setq shackle-default-rule '(:frame t :other t))
              ;; (setq shackle-inhibit-window-quit-on-same-windows t)
              (sway-undertaker-mode)
              (sway-socket-tracker-mode)
              (sway-x-focus-through-sway-mode)
              )

(defun my-sway-bury-oldest-window ()
  "Among currently visible windows, bury the oldest unfocused one.
If it's an Emacs frame, kill it."
  (let* ((oldest (car (s-split "\n"
                               (shell-command-to-string
                                (concat "swaymsg -t get_tree "
                                        " | jq 'recurse(.nodes[]) "
                                        " | select(.visible) "
                                        " | select(.focused == false) "
                                        " | .id"
                                        "'"))))))
    (unless (s-blank? oldest)
      (start-process-shell-command
       "swaymsg" nil
       (concat "swaymsg [con_id=" oldest "] focus"
               ", move to scratchpad"))
      (start-process-shell-command
       "swaymsg" nil
       (concat "swaymsg [con_id=" oldest " app_id=\".*[Ee]macs.*\"] focus"
               ", kill")))))

;; (defun my-sway-place-minibuf ()
;;   (let* ((id-name-pairs (shell-command-to-string
;;                          (concat "swaymsg -t get_tree "
;;                                  " | jq 'recurse(.nodes[]) "
;;                                  " | recurse(.floating_nodes[]) "
;;                                  " | \"\\(.id) \\(.name)\""
;;                                  "'")))
;;          (minibuf-id-str (->> id-name-pairs
;;                               (s-split "\n")
;;                               (--find (s-contains? "Minibuf-0" it))))
;;          (minibuf-id (->> (string-trim minibuf-id-str "\"" "\"")
;;                           (s-match (rx (* digit)))
;;                           (car))))
;;     (shell-command-to-string
;;      (concat "swaymsg [con_id=" minibuf-id "] "
;;              ""))))

;; (--find (eq 43 (map-elt it "id")) (sway-list-windows))

(defvar my-tmpfs (if guix
                     "/run/user/1000/"
                   "/tmp/"))

(defun my-write-message-to-tmpfile ()
  "Sync current echo area message to a file for use by swaybar."
  (require 'eva)
  (require 'tramp)
  (if (current-message)
      (eva-write-safely (current-message) (concat my-tmpfs "message"))
    (let ((elapsed-secs
           (tramp-time-diff (current-time)
                            (file-attribute-modification-time
                             (file-attributes (concat my-tmpfs "message"))))))
      (when (< 7 elapsed-secs)
        (eva-write-safely " " (concat my-tmpfs "message"))))))

(defvar my-minibuf-sway-id nil
  "Sway con_id corresponding to my minibuffer frame.
This is good to save b/c `sway-list-frames' will not catch
floating windows.")

(if (getenv "SWAYSOCK")
    (progn
      ;; (named-timer-run :my-msg 1 1 #'my-write-message-to-tmpfile)  ;; for swaybar

      (add-to-list 'default-frame-alist '(internal-border-width . 30))
      ;; (remove-hook 'doom-init-ui-hook #'window-divider-mode)

      ;; WIP: Stuff to make a Rofi-like minibuf frame
      ;; (use-package! mini-frame)
      ;; (use-package! maple-minibuffer)
      ;; (use-package! posframe)
      ;; (setq minibuffer-frame-alist `((width . ,(/ 1226 (frame-char-width)))
      ;;                                (height . 10)
      ;;                                (internal-border-width . 2)
      ;;                                (minibuffer-auto-raise . t)
      ;;                                ))
      ;; (set-frame-parameter default-minibuffer-frame 'minibuffer-auto-raise t)
      ;; (add-to-list 'default-frame-alist '(minibuffer . nil))
      ;; (general-after-init
      ;;   (run-after 10
      ;;     ;; place the dedicated minibuffer frame
      ;;     (unless my-minibuf-sway-id
      ;;       (setq my-minibuf-sway-id
      ;;             (cdr (assoc default-minibuffer-frame (sway-list-frames)))))
      ;;     (if my-minibuf-sway-id
      ;;         (sway-msg nil (concat "[con_id="
      ;;                               (number-to-string my-minibuf-sway-id) "] focus"
      ;;                               ", floating enable, sticky enable"
      ;;                               ;; ", resize set 1286 120, move position 0 590"
      ;;                               ;; ", resize set 1286 10, move position 0 -40"
      ;;                               ", resize set 800 200, move position center"
      ;;                               ;; ", opacity .5"
      ;;                               ", border none"
      ;;                               ", focus tiling"
      ;;                               ))
      ;;       (warn "Failed to find and place minibuffer frame"))))
      )
  ;; (scroll-bar-mode)
  )


;; (setq pop-up-frames t)

(setq display-buffer-base-action '((display-buffer-use-some-window
                                    display-buffer-use-some-frame
                                    ;; display-buffer-pop-up-frame
                                    )
                                   ((reusable-frames . t))))

(setq frame-auto-hide-function #'ignore)

;; Ensure that -base-action and -fallback-action use "reusable-frames" when no
;; other rule matched.
(add-to-list 'display-buffer-alist
             '("." nil (reusable-frames . t))
             'append)

(add-to-list 'display-buffer-alist
             '(("*org-roam*")
               (display-buffer-reuse-mode-window
                display-buffer-at-bottom)
               ((window-height . shrink-window-if-larger-than-buffer))))

(add-to-list 'display-buffer-alist
             '((" *Org todo*")
               (display-buffer-at-bottom)))

;; (use-package! popper
;;   :config
;;   (setq popper-group-function #'popper-group-by-directory)
;;   ;; (setq popper-display-control 'user) ;; pushes a rule onto display-buffer-alist
;;   ;; If I want popups to reuse other frames just like my other buffers do, I
;;   ;; either have to set this to nil, or write an alternative for popper-display-function.
;;   (setq popper-display-control nil)
;;   (setq popper-mode-line nil)
;;   (setq popper-reference-buffers
;;         (list
;;          'Custom-mode
;;          'org-roam-mode
;;          'compilation-mode
;;          'messages-mode
;;          'help-mode
;;          'helpful-mode
;;          'occur-mode
;;          'ess-help-mode
;;          ;; 'inferior-ess-r-mode ;; make ess-eval always pop to it and it's not a problem declaring this as a popup
;;          (rx bos "*Messages*" eos)
;;          (rx bos "*Warnings" eos)
;;          (rx bos " *Org todo*" eos)
;;          (rx bos "*Compile-Log*" eos)
;;          (rx bos "*Backtrace*" eos)
;;          (rx bos "*Completions*" eos)
;;          (rx bos "*Async Shell Command*" eos)
;;          (rx bos "*Shell Command Output*" eos)
;;          (rx bos "*TeX Help*" eos)
;;          (rx bos "*evil-registers*" eos)
;;          (rx bos "*Apropos")
;;          (rx bos "*ielm")
;;          (rx bos "Calc:")
;;          (rx "Output*" eos)
;;          (rx "output*" eos)))
;;   (popper-mode)
;;   (general-def "M-7" #'popper-cycle)
;;   (general-def "M-<f7>" #'popper-toggle-latest))
