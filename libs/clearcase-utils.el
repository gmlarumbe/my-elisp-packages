;;; clearcase-utils.el --- ClearCase Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;; Wrappers + Hydra

(require 'hydra)
(require 'clearcase)


(defvar larumbe/clearcase-edcs-views nil
  "List of available Config Spec views")


;;;###autoload
(defun larumbe/clearcase-checkout ()
  "Clearcase checkout based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-checkout-dired-files)
    (call-interactively #'clearcase-checkout-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-uncheckout ()
  "Clearcase uncheckout based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-uncheckout-dired-files)
    (call-interactively #'clearcase-uncheckout-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-checkin ()
  "Clearcase checkin based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-checkin-dired-files)
    (call-interactively #'clearcase-checkin-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-ediff-pred ()
  "Clearcase ediff-pred based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-ediff-pred-dired-file)
    (call-interactively #'clearcase-ediff-pred-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-ediff-named-version ()
  "Clearcase ediff-named-version based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-ediff-named-version-dired-file)
    (call-interactively #'clearcase-ediff-named-version-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-ediff-branch-base ()
  "Clearcase ediff-branch-base based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-ediff-branch-base-dired-file)
    (call-interactively #'clearcase-ediff-branch-base-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-diff-pred ()
  "Clearcase diff-pred based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-diff-pred-dired-file)
    (call-interactively #'clearcase-diff-pred-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-diff-branch-base ()
  "Clearcase diff-branch-base based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-diff-branch-base-dired-file)
    (call-interactively #'clearcase-diff-branch-base-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-diff-named-version ()
  "Clearcase diff-named-version based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-diff-named-version-dired-file)
    (call-interactively #'clearcase-diff-named-version-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-list-history ()
  "Clearcase list-history based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-list-history-dired-file)
    (call-interactively #'clearcase-list-history-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-browse-vtree ()
  "Clearcase browse-vtree based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-browse-vtree-dired-file)
    (call-interactively #'clearcase-browse-vtree-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-what-rule ()
  "Clearcase what-rule based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-what-rule-dired-file)
    (call-interactively #'clearcase-what-rule-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-annotate ()
  "Clearcase annotate based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-annotate-dired-file)
    (call-interactively #'clearcase-annotate-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-edit-checkout-comment ()
  "Clearcase edit-checkout-comment based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-edit-checkout-comment-dired-file)
    (call-interactively #'clearcase-edit-checkout-comment-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-mkelem ()
  "Clearcase mkelem based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-mkelem-dired-files)
    (call-interactively #'clearcase-mkelem-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-next-action ()
  "Clearcase next-action based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-next-action-dired-files)
    (call-interactively #'clearcase-next-action-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-describe ()
  "Clearcase describe based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-describe-dired-file)
    (call-interactively #'clearcase-describe-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-gui-diff-pred ()
  "Clearcase describe based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-gui-diff-pred-dired-file)
    (call-interactively #'clearcase-gui-diff-pred-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-gui-diff-branch-base ()
  "Clearcase describe based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-gui-diff-branch-base-dired-file)
    (call-interactively #'clearcase-gui-diff-branch-base-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-gui-diff-named-version ()
  "Clearcase describe based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-gui-diff-named-version-dired-file)
    (call-interactively #'clearcase-gui-diff-named-version-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-gui-vtree-browser ()
  "Clearcase describe based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-gui-vtree-browser-dired-file)
    (call-interactively #'clearcase-gui-vtree-browser-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-hijack ()
  "Clearcase hijack based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-hijack-dired-files)
    (call-interactively #'clearcase-hijack-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-unhijack ()
  "Clearcase unhijack based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-unhijack-dired-files)
    (call-interactively #'clearcase-unhijack-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-update ()
  "Clearcase update based on `current-buffer'/dired' context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-update-dired-files)
    (call-interactively #'clearcase-update-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-dired-checkout-current-dir ()
  "Clearcase checkout current dir based on `default-directory' or dired context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-dired-checkout-current-dir)
    ;; `current-buffer' Check current
    (if (y-or-n-p (format "Check out dir %s ?" default-directory))
        (clearcase-commented-checkout default-directory)
      (message "Aborting checkout dir..."))))


;;;###autoload
(defun larumbe/clearcase-dired-uncheckout-current-dir ()
  "Clearcase uncheckout current dir based on `default-directory' or dired context."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-dired-uncheckout-current-dir)
    ;; `current-buffer' Check current
    (if (y-or-n-p (format "Uncheck out dir %s ?" default-directory))
        (clearcase-uncheckout default-directory)
      (message "Aborting uncheckout dir..."))))


;;;###autoload
(defun larumbe/clearcase-version-other-window ()
  "Clearcase version other window based on  `current-buffer' or dired context."
  (interactive)
  (let (version filename)
    (if (not (string= major-mode "dired-mode"))
        (call-interactively #'clearcase-version-other-window)
      ;; `dired-mode' pointed fileCheck current
      (setq filename (dired-get-filename))
      (setq version (clearcase-read-version-name (format "Version of %s to visit: " filename) filename))
      (find-file-other-window (clearcase-vxpath-cons-vxpath
                               (clearcase-vxpath-element-part filename)
                               version)))))


;;;###autoload
;; DANGER: Seems it's not very useful since the original function already creates a cache of existin views?
(defun larumbe/clearcase-edcs-edit ()
  "Clearcase config spec editing from a list of possible views.
If called with prefix arg, call original command to fetch list of available views."
  (interactive)
  (let (view)
    (if current-prefix-arg
        (call-interactively #'clearcase-edcs-edit)
      (setq view (completing-read "Select view: " larumbe/clearcase-edcs-views nil t))
      (clearcase-edcs-edit view))))


;;;###autoload
(defun larumbe/clearcase-lsprivate ()
  "List all view private files."
  (interactive)
  (clearcase-utl-populate-and-view-buffer
   "*clearcase*"
   nil
   (lambda ()
     (clearcase-ct-do-cleartool-command "lsprivate"
                                        nil
                                        'unused
                                        nil))))


;;;###autoload
(defun larumbe/clearcase-find-checkouts-current-dir-recursively ()
  "Find the checkouts on current-directory.
INFO: Tried to do a copy/paste/modified version from `clearcase-find-checkouts-in-current-view'
by using `clearcase-ct-blocking-call' but there were some errors with the directory not being
updated sometimes (could have to do with `clearcase-ct-wdir' or other stuff...)."
  (interactive)
  (clearcase-utl-populate-and-view-buffer
   "*clearcase*"
   nil
   (lambda ()
     (clearcase-ct-do-cleartool-command "lsco"
                                        nil
                                        'unused
                                        '("-recurse"))))
  (with-current-buffer "*clearcase-lsco*"
    (setq buffer-read-only nil)
    (insert (format "List of checkouts for dir: %s\n\n" default-directory))
    (setq buffer-read-only nil)))


;;;###autoload
(defun larumbe/clearcase-ediff-two-versions ()
  "Ediff two versions of `current-buffer' or dired file at point."
  (interactive)
  (let (file old-version new-version old-file new-file)
    (if (string= major-mode "dired-mode")
        (setq file (dired-get-filename))
      (setq file buffer-file-name))
    ;; Some basic check
    (unless (clearcase-file-is-in-mvfs-p file)
      (error "File not in MVFS!"))
    (setq old-version (clearcase-read-version-name "Old version: " file))
    (setq new-version (clearcase-read-version-name "New version: " file))
    (setq old-file (clearcase-vxpath-cons-vxpath (clearcase-vxpath-element-part file) old-version))
    (setq new-file (clearcase-vxpath-cons-vxpath (clearcase-vxpath-element-part file) new-version))
    ;; Run `ediff-swap-buffers' to show new version on the right window
    (ediff-files old-file new-file #'ediff-swap-buffers)))




(defhydra hydra-clearcase (:color blue
                           :hint  nil)
  ("f"  larumbe/clearcase-checkout "Check-out file(s)" :column "Check")     ; "Fetch"
  ("u"  larumbe/clearcase-uncheckout "Uncheck-out file(s)")                 ; "Unstage"
  ("p"  larumbe/clearcase-checkin "Check-in file(s)")                       ; "Push"
  ("s"  clearcase-find-checkouts-in-current-view "List CO of Current View") ; "Status"
  ("SS" larumbe/clearcase-find-checkouts-current-dir-recursively "List CO of current dir")
  ("FF" larumbe/clearcase-dired-checkout-current-dir "Check-out dir")
  ("FU" larumbe/clearcase-dired-uncheckout-current-dir "Uncheck-out dir")
  ("CE" larumbe/clearcase-edit-checkout-comment "Edit CO comment")
  ("PP" larumbe/clearcase-edcs-edit "Edit Config-Spec")

  ("e"  larumbe/clearcase-ediff-pred "Ediff predecesor" :column "Diff")
  ("E"  larumbe/clearcase-ediff-named-version "Ediff named version")
  ("be" larumbe/clearcase-ediff-branch-base "Ediff branch base")
  ("ve" larumbe/clearcase-ediff-two-versions "Ediff two specific versions")
  ("d"  larumbe/clearcase-diff-pred "Diff predecesor")
  ("D"  larumbe/clearcase-diff-named-version "Diff named version")
  ("bd" larumbe/clearcase-diff-branch-base "Diff branch base")

  ("l"  larumbe/clearcase-list-history "Element history" :column "History")
  ("L"  larumbe/clearcase-browse-vtree "Browse Vtree")
  ("~"  larumbe/clearcase-version-other-window "Version other window")
  ("w"  larumbe/clearcase-what-rule "Config Spec Rule")
  ("a"  larumbe/clearcase-annotate "Annotate")
  ("?"  larumbe/clearcase-describe "Describe")
  ("#"  larumbe/clearcase-lsprivate "List private files")

  ("Gd"  larumbe/clearcase-gui-diff-pred "Diff predecesor" :column "GUI")
  ("GD"  larumbe/clearcase-gui-diff-named-version "Diff named version")
  ("Gbd" larumbe/clearcase-gui-diff-branch-base "Diff branch base")
  ("GL"  larumbe/clearcase-gui-vtree-browser "Browse Vtree")
  ("Gc"  clearcase-gui-clearexplorer "ClearExplorer")
  ("Gr"  clearcase-gui-rebase "Rebase")
  ("Gl"  clearcase-gui-deliver "Deliver")
  ("Gm"  clearcase-gui-merge-manager "Merge Manager")
  ("Gp"  clearcase-gui-project-explorer "Project Explorer")
  ("Gu"  clearcase-gui-snapshot-view-updater "View Updater")

  ("On"  larumbe/clearcase-next-action "Next Action" :column "Others")
  ("Om"  larumbe/clearcase-mkelem "Make Element")
  ("Oh"  larumbe/clearcase-hijack "Hijack")
  ("Ou"  larumbe/clearcase-unhijack "Unhijack")
  ("Ot"  clearcase-mkbrtype "Make Branch Type")
  ("Of"  larumbe/clearcase-update "Update file(s)")
  ("Od"  clearcase-update-default-directory "Update current dir")
  ("Ov"  clearcase-update-view "Update view")

  ("Zd" clearcase-dump "Dump files/views props" :column "Debug")
  ("Ze" clearcase-enable-tracing "Enable tracing")
  ("Zq" clearcase-disable-tracing "Disable tracing")
  ("Zf" clearcase-flush-caches "Flush caches")
  ("Zp" clearcase-fprop-display-properties "File properties")
  ("Zs" clearcase-ct-start-cleartool "Cleartool start")
  ("Zk" clearcase-ct-kill-cleartool "Cleartool kill")

  ;; Others
  ;; INFO:`clearcase-dired-mode' already started with `clearcase-integrate' in `clearcase-hook-dired-mode-hook'
  ;;      `clearcase-mode' already started with `clearcase-integrate' in `clearcase-hook-find-file-hook'
  ;;      UCM (Unified Change Management) related commands not included yet

  ;; quitting and stopping
  ("q"   top-level :color blue)
  ("Q"   top-level :color blue)
  ("C-g" nil       :color blue))


;;;###autoload
(defun larumbe/hydra-clearcase ()
  "Wrapper for hydra-clearcase that prevents use of this tool if not inside a ClearCase object/directory."
  (interactive)
  (let ((file (if (string= major-mode "dired-mode")
                  dired-directory
                buffer-file-name)))
    (unless (clearcase-file-is-in-mvfs-p file)
      (error "Not in a ClearCase object!"))
    (call-interactively #'hydra-clearcase/body)))


;;;; Compilation-log
(defvar larumbe/font-lock-cc-log-date-time-face 'larumbe/font-lock-cc-log-date-time-face)
(defface larumbe/font-lock-cc-log-date-time-face
  '((t (:foreground "dark gray"))) ""
  :group 'cc-log-font-lock-faces)

(defvar larumbe/font-lock-cc-log-separator-face 'larumbe/font-lock-cc-log-separator-face)
(defface larumbe/font-lock-cc-log-separator-face
  '((t (:foreground "dim gray"))) ""
  :group 'cc-log-font-lock-faces)

(defvar larumbe/font-lock-cc-log-author-face 'larumbe/font-lock-cc-log-author-face)
(defface larumbe/font-lock-cc-log-author-face
  '((t (:foreground "yellow green"))) ""
  :group 'cc-log-font-lock-faces)

(defvar larumbe/font-lock-cc-log-action-face 'larumbe/font-lock-cc-log-action-face)
(defface larumbe/font-lock-cc-log-action-face
  '((t (:foreground "tan"))) ""
  :group 'cc-log-font-lock-faces)

(defvar larumbe/font-lock-cc-log-file-face 'larumbe/font-lock-cc-log-file-face)
(defface larumbe/font-lock-cc-log-file-face
  '((t (:foreground "light sky blue"))) ""
  :group 'cc-log-font-lock-faces)

(defvar larumbe/font-lock-cc-log-labels-notes-face 'larumbe/font-lock-cc-log-labels-notes-face)
(defface larumbe/font-lock-cc-log-labels-notes-face
  '((t (:foreground "dark gray"))) ""
  :group 'cc-log-font-lock-faces)


(defvar larumbe/clearcase-log-regexp "--\\(?1:[0-9-]+\\)\\\(?2:T\\)\\(?3:[0-9:]+\\)\s+\\(?4:[a-z]+\\)\s+\\(?5:[a-z ]+\\)\"\\(?6:.*\\)\"\\(?7:[ /_a-zA-Z0-9(),-.]+\\)?")
(defvar larumbe/clearcase-log-regexp-alist-alist
  `((log ,larumbe/clearcase-log-regexp 6 nil nil 2 nil
         (1 larumbe/font-lock-cc-log-date-time-face)
         (2 larumbe/font-lock-cc-log-separator-face)
         (3 larumbe/font-lock-cc-log-date-time-face)
         (4 larumbe/font-lock-cc-log-author-face)
         (5 larumbe/font-lock-cc-log-action-face)
         (6 larumbe/font-lock-cc-log-file-face)
         (7 larumbe/font-lock-cc-log-labels-notes-face))))


(defun larumbe/clearcase-log-clean-buffer ()
  "Clean (kill/quit) clearcase-log buffer."
  (interactive)
  (let (kill-buffer-query-functions)
    (quit-window t)))


(defun larumbe/clearcase-log-ediff-version ()
  "Ediff version of file at current error with another selected version via `completing-read'."
  (interactive)
  (let* ((current-version (larumbe/clearcase-log-mode-find-version))
         (current-file (clearcase-vxpath-element-part current-version))
         (version (clearcase-read-version-name "Version for comparison: " current-file)))
    (clearcase-ediff-file-with-version current-version version)))


(defun larumbe/clearcase-log-mode-find-version ()
  "Find file@@/version of current errr."
  (let (file)
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward larumbe/clearcase-log-regexp (point-at-eol) t)
        (setq file (match-string-no-properties 6)))
      (if (file-name-absolute-p file)
          file ; lshistory provides absolute path
        (expand-file-name file))))) ; while lsco -recurse provides relatives paths


(defun larumbe/clearcase-log-ediff-pred ()
  "Use Ediff to compare the selected version against its predecessor in log mode."
  (interactive)
  (let* ((filename (larumbe/clearcase-log-mode-find-version))
         (predecessor (clearcase-fprop-predecessor-version filename)))
    (if (string= predecessor "")
        (error "%s has no predecessor!" filename)
      (clearcase-ediff-file-with-version filename predecessor))))

(defun larumbe/clearcase-log-diff-pred ()
  "Use diff to compare the selected version against its predecessor in log mode."
  (interactive)
  (let* ((filename (larumbe/clearcase-log-mode-find-version))
         (predecessor (clearcase-fprop-predecessor-version filename)))
    (if (string= predecessor "")
        (error "%s has no predecessor!" filename)
      (clearcase-diff-file-with-version filename predecessor))))


;;;###autoload
(define-compilation-mode clearcase-log-mode "CC-log"
  "A mode for showing log from clearcase lshistory."
  (setq-local compilation-error-regexp-alist '(log))
  (setq-local compilation-error-regexp-alist-alist larumbe/clearcase-log-regexp-alist-alist)
  (setq-local compilation-scroll-output nil)
  ;; See `compilation-move-to-column' for details.
  (setq-local compilation-first-column 0)
  (setq-local compilation-error-screen-columns nil)
  (setq-local compilation-disable-input t)
  (setq-local compilation-always-kill t)
  (setq-local find-file-suppress-same-file-warnings t) ; Fetched from ggtags
  (setq-local truncate-lines t))

(define-key clearcase-log-mode-map (kbd "C-o") #'compile-goto-error)
(define-key clearcase-log-mode-map (kbd "C-j") #'compile-goto-error)
(define-key clearcase-log-mode-map (kbd "o") #'compile-goto-error)
(define-key clearcase-log-mode-map (kbd "p") #'compilation-previous-error)
(define-key clearcase-log-mode-map (kbd "n") #'compilation-next-error)
(define-key clearcase-log-mode-map (kbd "k") #'larumbe/clearcase-log-clean-buffer)
(define-key clearcase-log-mode-map (kbd "q") #'larumbe/clearcase-log-clean-buffer)
(define-key clearcase-log-mode-map (kbd "e") #'larumbe/clearcase-log-ediff-pred)
(define-key clearcase-log-mode-map (kbd "E") #'larumbe/clearcase-log-ediff-version)
(define-key clearcase-log-mode-map (kbd "d") #'larumbe/clearcase-log-diff-pred)


(provide 'clearcase-utils)

;;; clearcase-utils.el ends here
