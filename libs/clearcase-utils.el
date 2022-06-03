;;; clearcase-utils.el --- ClearCase Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;; Wrappers + Hydra

(require 'hydra)
(require 'clearcase)
(require 'compile)


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
  "Clearcase checkin based on `current-buffer'/dired' context.
If in dired-mode but not pointing on a file, check-in current directory."
  (interactive)
  (if (string= major-mode "dired-mode")
      ;; Dired-mode
      (if (dired-get-filename nil t)
          (call-interactively #'clearcase-checkin-dired-files)
        (call-interactively #'clearcase-dired-checkin-current-dir))
    ;; Current buffer
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
  "Clearcase hijack based on `current-buffer'/dired' context.
INFO: Only works on snapshot view.
Hijacking takes a file outside direct Rational ClearCase control."
  (interactive)
  (if (string= major-mode "dired-mode")
      (call-interactively #'clearcase-hijack-dired-files)
    (call-interactively #'clearcase-hijack-current-buffer)))


;;;###autoload
(defun larumbe/clearcase-unhijack ()
  "Clearcase unhijack based on `current-buffer'/dired' context.
INFO: Only works on snapshot view.
Hijacking takes a file outside direct Rational ClearCase control."
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
   "*clearcase*" ; DANGER: For some strange reason, if set to "*clearcase-lsprivate*" the result goes to *clearcase* instead
   nil
   (lambda ()
     (clearcase-ct-do-cleartool-command "lsprivate"
                                        nil
                                        'unused
                                        nil)))
  (clearcase-lsprivate-mode))


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


;;;###autoload
(defun larumbe/clearcase-merge-with-latest ()
  "Merge file with LATEST version.
If there are conflicts, use GUI Merge Manager.
Intended to be used if it is not possible to check-in."
  (interactive)
  (let (file latest)
    (if (string= major-mode "dired-mode")
        (setq file (dired-get-filename))
      (setq file buffer-file-name))
    (setq latest (concat file "@@/main/LATEST"))
    ;; Some basic check
    (unless (clearcase-file-is-in-mvfs-p file)
      (error "File not in MVFS!"))
    (unless (file-exists-p latest)
      (error "Error trying to find LATEST version!"))
    ;; Merge command
    (clearcase-utl-populate-and-view-buffer
     "*clearcase-merge*"
     nil
     (lambda ()
       (clearcase-ct-do-cleartool-command "merge"
                                          nil
                                          'unused
                                          `("-to" ,file ,latest))))
    (with-current-buffer "*clearcase-merge*"
      (read-only-mode 1))))


;;;###autoload
(defun larumbe/clearcase-ediff-with-latest ()
  "Ediff with latest version.
Useful to be performed before running a merge with LATEST to predict merge conflicts."
  (interactive)
  (let ((file (if (string= major-mode "dired-mode")
                  (dired-get-filename)
                buffer-file-name)))
    (clearcase-ediff-file-with-version file "/main/LATEST")))



(defun larumbe/clearcase-issues-to-checkin (file)
  "Similar to `clearcase-assert-file-ok-to-checkin' but making more checks:
- Throw an error if file is not checkedout and user is not the owner
- Throw an error if file is checkedout in reserved mode by someone else
- Return warning message in case the file needs to be merged with LATEST or checkedout by someone else
- Return nil if everything's fine
"
  (let (co-reserved
        existing-versions-string
        existing-versions-int
        latest-version
        pred-version
        merge-needed
        co-others
        warning-string)
    (unless (executable-find "cleartool")
      (error "Cleartool not found in $PATH!"))
    ;; clearcase.el API: checks if file is checked-out and I am the owner, otherwise throws an error
    (clearcase-assert-file-ok-to-checkin file)
    ;; Check if other people have checked out the same file and if it was a reserved/unreserved checkout
    (setq co-reserved (shell-command-to-string (concat "cleartool lsco " file " | grep -v " (user-login-name))))
    (when (not (string= "" co-reserved))
      (with-temp-buffer
        (insert co-reserved)
        (goto-char (point-min))
        (save-excursion
          (when (re-search-forward "\(reserved\)" nil t)
            (error "Someone else has a RESERVED checkout on %s" file)))
        (save-excursion
          (when (re-search-forward "\\(?1:[a-z]+\\)\s+checkout version.*\(unreserved\)" nil t)
            (setq co-others t)
            (setq warning-string (concat warning-string (format "File %s checkedout by %s\n" file (match-string-no-properties 1))))))))
    ;; Check if a merge is needed before checking in (compare LATEST with the one in the buffer)
    (setq existing-versions-string (directory-files (concat file "@@/main/") nil "^[0-9]+$"))
    (setq existing-versions-int (mapcar #'string-to-number existing-versions-string))
    (setq latest-version (reduce #'max existing-versions-int))
    (setq pred-version (string-to-number (string-remove-prefix "/main/" (clearcase-fprop-predecessor-version file))))
    (when (> latest-version pred-version)
      (setq merge-needed t))
    (when merge-needed
      (setq warning-string (concat warning-string (format "File %s requires merge\n" file))))
    ;; Return value
    (if (or merge-needed
            co-others)
        warning-string
      nil)))


(defun larumbe/clearcase-checkin-file-list (files)
  "Check-in list of strings FILES at once."
  (let (cmd-args merge-warnings)
    (unless (string= "" (setq merge-warnings (mapconcat #'larumbe/clearcase-issues-to-checkin files "\n")))
      (unless (yes-or-no-p (concat "Warnings: \n\n" merge-warnings "\nContinue?\n"))
        (user-error "Aborting!")))
    (if (yes-or-no-p (concat "Checking in following files:\n\n" (mapconcat #'identity files "\n") "\n\nContinue?\n"))
        (progn
          (mapcar (lambda (elm) (push elm cmd-args)) files)
          (push (concat "\"" (read-string "Check-in comment: ") "\"") cmd-args)
          (push "-c" cmd-args)
          (clearcase-utl-populate-and-view-buffer
           "*clearcase*"
           nil
           (lambda ()
             (clearcase-ct-do-cleartool-command "ci"
                                                nil
                                                'unused
                                                cmd-args))))
      ;; Else, abort
      (message "Aborting!"))))


;;;###autoload
(defun larumbe/clearcase-checkin-multiple ()
  "Clearcase checkin of many files at once.

Based on `completing-read' for current checked-out files."
  (interactive)
  (let (checked-out-files check-in-files file)
    ;; Fetch list of checked-out files
    (save-window-excursion
      (clearcase-find-checkouts-in-current-view)
      (with-temp-buffer
        (insert-buffer-substring "*clearcase*")
        (setq checked-out-files (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
        (push "Done" checked-out-files)))
    ;; Choose files to be checked-in
    (while (not (string= (setq file (completing-read "Check in files: " checked-out-files)) "Done"))
      (push file check-in-files)
      (delete file checked-out-files))
    ;; Check-in or abort
    (larumbe/clearcase-checkin-file-list check-in-files)))


;;;###autoload
(defun larumbe/clearcase-find-checkouts-in-current-view ()
  "Wrapper around `clearcase-find-checkouts-in-current-view' that enables `clearcase-checkout-mode'."
  (interactive)
  (clearcase-find-checkouts-in-current-view)
  (clearcase-checkout-mode))


;;;###autoload
(defun larumbe/clearcase-clean-buffers ()
  "Kill all Ediff sessions spawned by Clearcase.
Also kill @@/main/ versions."
  (interactive)
  (let ((ediff-bufs-re1 "*Ediff ")
        (ediff-bufs-re2 "*ediff-")
        (version-re     "@@/main/[0-9]+")
        (buf-num 0))
    (mapc (lambda (buf)
            (when (or (string-prefix-p ediff-bufs-re1 (buffer-name buf))
                      (string-prefix-p ediff-bufs-re2 (buffer-name buf))
                      (string-match version-re (buffer-name buf)))
              (kill-buffer buf)
              (setq buf-num (1+ buf-num))))
          (buffer-list))
    (message "Killed %0d Ediff/Version buffers" buf-num)))


(defhydra hydra-clearcase (:color blue
                           :hint  nil)
  ("f"  larumbe/clearcase-checkout "Check-out file(s)" :column "Check")     ; "Fetch"
  ("u"  larumbe/clearcase-uncheckout "Uncheck-out file(s)")                 ; "Unstage"
  ("p"  larumbe/clearcase-checkin "Check-in file(s)/dir")                   ; "Push"
  ("P"  larumbe/clearcase-checkin-multiple "Check-in many files at once")   ; "Push multiple"
  ("s"  larumbe/clearcase-find-checkouts-in-current-view "List CO of Current View") ; "Status"
  ("S"  larumbe/clearcase-find-checkouts-current-dir-recursively "List CO of current dir")
  ("F"  larumbe/clearcase-dired-checkout-current-dir "Check-out dir")
  ("U"  larumbe/clearcase-dired-uncheckout-current-dir "Uncheck-out dir")
  ("CE" larumbe/clearcase-edit-checkout-comment "Edit CO comment")
  ("CS" larumbe/clearcase-edcs-edit "Edit Config-Spec")

  ("e"  larumbe/clearcase-ediff-pred "Ediff predecesor" :column "Diff")
  ("E"  larumbe/clearcase-ediff-named-version "Ediff named version")
  ("be" larumbe/clearcase-ediff-branch-base "Ediff branch base")
  ("ve" larumbe/clearcase-ediff-two-versions "Ediff two specific versions")
  ("d"  larumbe/clearcase-diff-pred "Diff predecesor")
  ("D"  larumbe/clearcase-diff-named-version "Diff named version")
  ("bd" larumbe/clearcase-diff-branch-base "Diff branch base")
  ("k"  larumbe/clearcase-clean-buffers "Kill Ediff/Version buffers")

  ("l"  larumbe/clearcase-list-history "Element history" :column "History")
  ("a"  larumbe/clearcase-annotate "Annotate")
  ("?"  larumbe/clearcase-describe "Describe")
  ("L"  larumbe/clearcase-browse-vtree "Browse Vtree")
  ("#"  larumbe/clearcase-lsprivate "List private files")
  ("w"  larumbe/clearcase-what-rule "Config Spec Rule")
  ("~"  larumbe/clearcase-version-other-window "Version other window")

  ("me" larumbe/clearcase-ediff-with-latest "Ediff with LATEST" :column "Merge")
  ("mm" larumbe/clearcase-merge-with-latest "Merge with LATEST")

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
  ("Ot"  clearcase-mkbrtype "Make Branch Type")

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
                (if buffer-file-name
                    buffer-file-name
                  (error "Not visiting a file!")))))
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


(defvar larumbe/clearcase-log-regexp "\\(--\\)?\\(?1:[0-9-:]+\\)\\\(?2:T\\)?\\(?3:[0-9-:]*\\)\s+\\(?4:[a-z]+\\)\s+\\(?5:[a-z ]+\\)\"\\(?6:.*\\)\"\\(?7:[ /_a-zA-Z0-9(),-.]+\\)?")
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
  "Find file@@/version of current error."
  (let (file)
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward larumbe/clearcase-log-regexp (point-at-eol) t)
        (setq file (match-string-no-properties 6)))
      (if (file-name-absolute-p file)
          file ; lshistory provides absolute path
        (expand-file-name file))))) ; while lsco -recurse provides relatives paths


(defun larumbe/clearcase-log-mode-find-author ()
  "Find author of current error."
  (let (author)
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward larumbe/clearcase-log-regexp (point-at-eol) t)
        (setq author (match-string-no-properties 4))))))


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


(defun larumbe/clearcase-log-ediff-with-checkout ()
  "Ediff with checked-out version.
INFO: Requires that view of person who checked out the file is present in /view
INFO: That was done at some point by executing graphical lsvtree and diff of another person
checkedout file."
  (interactive)
  (let* ((filename (larumbe/clearcase-log-mode-find-version)) ; INFO: If over a checkedout file, only filename will be returned, no revision
         (view-dir-list (delete "" (split-string filename "/")))
         (cur-view (if (string= (car view-dir-list) "view")
                       (nth 1 view-dir-list) ; We are in other person's view already!
                     (shell-command-to-string "cleartool pwv -short | tr -d '\\n'"))) ; We are in our own view
         (other-views (delete "" (split-string (shell-command-to-string (concat "cleartool lsvtree " filename ; ct command
                                                                                " | grep CHECKEDOUT"          ; filter checked out version line
                                                                                " | awk '{ print $3 }'"       ; view name
                                                                                " | tr -d '\"'"))             ; remove surrounding quotes
                                               "\n")))
         (other-view (if (cdr other-views) ; If more than one view available ask which revision to compare with
                         (completing-read "View to compare with: " other-views nil t)
                       (car other-views)))
         (other-filename (concat "/view/" other-view filename)))
    ;; Some error checking!
    (when (string= cur-view other-view)
      (error "Trying to compare files on the same view!"))
    (unless (file-exists-p (concat "/view/" other-view))
      (error "View: %s not present in current machine! Maybe try first with GUI Lsvtree to automatically create it?" other-view))
    (unless (file-exists-p other-filename)
      (error "File: %s not present in view %s! Are you sure it's a checked-out file?" other-filename))
    (when (string-prefix-p "/view/" filename)
      (unless (y-or-n-p (concat "Comparing file from views: " cur-view " / " other-view ". Proceed?"))
        (error "Aborting")))
    ;; Actual command
    (ediff-files filename other-filename)))



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
(define-key clearcase-log-mode-map (kbd "c") #'larumbe/clearcase-log-ediff-with-checkout)



;;;; Dired-like
(defun larumbe/clearcase-next-line (&optional arg)
  (interactive "P")
  (goto-char (point-at-bol))
  (if arg
      (forward-line -1)
    (forward-line))
  (looking-at larumbe/clearcase-checkout-filepath-regexp)
  (goto-char (match-beginning 2)))

(defun larumbe/clearcase-prev-line ()
  (interactive)
  (larumbe/clearcase-next-line -1))

(defun larumbe/clearcase-mark-file ()
  (interactive)
  (overlay-put (make-overlay (point-at-bol) (point-at-eol)) 'face 'highlight)
  (add-to-list 'larumbe/clearcase-marked-files (thing-at-point 'filename t))
  (larumbe/clearcase-next-line))

(defun larumbe/clearcase-unmark-file ()
  (interactive)
  (if (overlays-in (point-at-bol) (point-at-eol))
      (progn
        (remove-overlays (point-at-bol) (point-at-eol))
        (setq larumbe/clearcase-marked-files (delete (thing-at-point 'filename t) larumbe/clearcase-marked-files))
        (larumbe/clearcase-next-line))
    ;; File was not marked
    (message "File was not marked")))

(defun larumbe/clearcase-mark-deletion-file ()
  (interactive)
  (if (save-excursion
        (goto-char (point-at-bol))
        (looking-at larumbe/clearcase-lsprivate-filepath-cc-checkedout-re))
      (message "Trying to delete a checked-out file!")
    ;; Else
    (overlay-put (make-overlay (point-at-bol) (point-at-eol)) 'face '(:foreground "red"))
    (add-to-list 'larumbe/clearcase-marked-files (thing-at-point 'filename t))
    (larumbe/clearcase-next-line)))

(defun larumbe/clearcase-delete-marked-files ()
  (interactive)
  (unless larumbe/clearcase-marked-files
    (user-error "No files marked for deletion"))
  (when (y-or-n-p (format "About to delete %0d files, continue? " (length larumbe/clearcase-marked-files)))
    (mapc (lambda (file)
            (if (file-directory-p file)
                (delete-directory file t)
              (delete-file file)))
          larumbe/clearcase-marked-files)
    (setq larumbe/clearcase-marked-files nil)
    (larumbe/clearcase-lsprivate-revert-buffer)))


;;;;; checkout
(defvar larumbe/clearcase-checkout-filepath-verilog-regexp "\\(?1:[/_\.a-zA-Z0-9]+/\\)\\(?2:[_a-zA-Z0-9]+\\)\\(?3:\.\\)\\(?4:s?vh?+\\)$")
(defvar larumbe/clearcase-checkout-filepath-regexp         "\\(?1:[/_\.a-zA-Z0-9]+/\\)\\(?2:[_a-zA-Z0-9]+\\)\\(?3:\.\\)\\(?4:[a-z]+\\)")

;; INFO: Verilog regexp must have precedence over generic regexp since it's a subgroup
(defvar larumbe/clearcase-checkout-font-lock-defaults ; Reuse clearcase-log faces
  `(((,larumbe/clearcase-checkout-filepath-verilog-regexp . ((1 'larumbe/font-lock-cc-log-date-time-face) ; path
                                                             (2 'larumbe/font-lock-cc-log-file-face)      ; filename
                                                             (3 'larumbe/font-lock-cc-log-separator-face) ; .
                                                             (4 'larumbe/font-lock-cc-log-action-face)))  ; ext
     (,larumbe/clearcase-checkout-filepath-regexp . ((1 'larumbe/font-lock-cc-log-date-time-face)
                                                     (2 'larumbe/font-lock-cc-log-author-face)
                                                     (3 'larumbe/font-lock-cc-log-separator-face)
                                                     (4 'larumbe/font-lock-cc-log-action-face))))))


(defvar clearcase-checkout-mode-map (make-sparse-keymap))
(define-key clearcase-checkout-mode-map (kbd "C-o") #'(lambda () (interactive) (find-file-other-window (thing-at-point 'filename))))
(define-key clearcase-checkout-mode-map (kbd "o")   #'(lambda () (interactive) (find-file-other-window (thing-at-point 'filename))))
(define-key clearcase-checkout-mode-map (kbd "C-j") #'(lambda () (interactive) (find-file-other-window (thing-at-point 'filename))))
(define-key clearcase-checkout-mode-map (kbd "RET") #'(lambda () (interactive) (find-file-other-window (thing-at-point 'filename))))
(define-key clearcase-checkout-mode-map (kbd "k")   #'(lambda () (interactive) (quit-window 'kill)))
(define-key clearcase-checkout-mode-map (kbd "p") #'larumbe/clearcase-prev-line)
(define-key clearcase-checkout-mode-map (kbd "n") #'larumbe/clearcase-next-line)
(define-key clearcase-checkout-mode-map (kbd "m") #'larumbe/clearcase-mark-file)
(define-key clearcase-checkout-mode-map (kbd "u") #'larumbe/clearcase-unmark-file)
(define-key clearcase-checkout-mode-map (kbd "P") #'larumbe/clearcase-checkout-checkin-marked)
(define-key clearcase-checkout-mode-map (kbd "l") #'recenter-top-bottom)


(defun larumbe/clearcase-checkout-revert-buffer (&optional ignore-auto noconfirm preserve-modes)
  (interactive)
  (let ((pos (point)))
    (when (and larumbe/clearcase-marked-files
               (not (y-or-n-p "Warning: Marked files in buffer! Reverting will reset them before checkout. Continue? ")))
      (user-error "Aborting revert..."))
    (save-window-excursion
      (larumbe/clearcase-find-checkouts-in-current-view))
    (if (> pos (point-max))
        (goto-char (point-min))
      (goto-char pos))))

(defun larumbe/clearcase-checkout-checkin-marked ()
  (interactive)
  (larumbe/clearcase-checkin-file-list larumbe/clearcase-marked-files)
  (remove-overlays (point-min) (point-max))
  (setq larumbe/clearcase-marked-files nil))



;;;###autoload
(define-derived-mode clearcase-checkout-mode special-mode
  (defvar-local larumbe/clearcase-marked-files nil)
  (setq font-lock-defaults larumbe/clearcase-checkout-font-lock-defaults)
  (setq-local revert-buffer-function #'larumbe/clearcase-checkout-revert-buffer)
  (setq mode-name "CC-Checkout"))


;;;;; lsprivate
(defvar larumbe/clearcase-lsprivate-filepath-temp-re          "\\(?1:[/_\.a-zA-Z0-9]+/\\)\\(?2:[_a-zA-Z0-9]+\\)\\(?3:\.\\)\\(?4:[a-z]+~\\)")
(defvar larumbe/clearcase-lsprivate-filepath-cc-keep-re       "\\(?1:[/_\.a-zA-Z0-9]+/\\)\\(?2:[_a-zA-Z0-9\.]+\\)\\(?3:\.\\(keep[\.0-9]*\\)\\|\\(contrib[\.0-9]*\\)$\\)")
(defvar larumbe/clearcase-lsprivate-filepath-cc-checkedout-re "\\(?1:[/_\.a-zA-Z0-9]+/\\)\\(?2:[\._a-zA-Z0-9]+\\)\s+\\(?3:\\[checkedout\\]\\)")
(defvar larumbe/clearcase-lsprivate-filepath-regexp           "\\(?1:[/_\.a-zA-Z0-9]+/\\)\\(?2:[_a-zA-Z0-9]+\\)\\.\\(?3:[a-zA-Z0-9]+\\)")
(defvar larumbe/clearcase-lsprivate-filepath-regexp-no-ext    "\\(?1:[/_\.a-zA-Z0-9]+/\\)\\(?2:[_a-zA-Z0-9]+\\)")

(defvar larumbe/clearcase-lsprivate-font-lock-defaults ; Reuse clearcase-log faces
  `(((,larumbe/clearcase-lsprivate-filepath-temp-re          . ((1 'larumbe/font-lock-cc-log-date-time-face) ; path
                                                                (2 'larumbe/font-lock-cc-log-author-face)    ; filename
                                                                (3 'larumbe/font-lock-cc-log-separator-face) ; .
                                                                (4 'larumbe/font-lock-cc-log-action-face)))  ; ext
     (,larumbe/clearcase-lsprivate-filepath-cc-keep-re       . ((1 'larumbe/font-lock-cc-log-date-time-face) ; path
                                                                (2 'font-lock-function-name-face)            ; filename
                                                                (3 'larumbe/font-lock-cc-log-action-face)))  ; rest
     (,larumbe/clearcase-lsprivate-filepath-cc-checkedout-re . ((1 'larumbe/font-lock-cc-log-date-time-face) ; path
                                                                (2 'font-lock-builtin-face)                  ; filename
                                                                (3 'larumbe/font-lock-cc-log-action-face)))
     (,larumbe/clearcase-lsprivate-filepath-regexp           . ((1 'larumbe/font-lock-cc-log-date-time-face) ; path
                                                                (2 'larumbe/font-lock-cc-log-file-face)      ; filename
                                                                (3 'larumbe/font-lock-cc-log-action-face)))  ; ext
     (,larumbe/clearcase-lsprivate-filepath-regexp-no-ext    . ((1 'larumbe/font-lock-cc-log-date-time-face) ; path
                                                                (2 'larumbe/font-lock-cc-log-file-face)))    ; filename
     )))


(defvar clearcase-lsprivate-mode-map (make-sparse-keymap))
(define-key clearcase-lsprivate-mode-map (kbd "C-o") #'(lambda () (interactive) (find-file-other-window (thing-at-point 'filename))))
(define-key clearcase-lsprivate-mode-map (kbd "o")   #'(lambda () (interactive) (find-file-other-window (thing-at-point 'filename))))
(define-key clearcase-lsprivate-mode-map (kbd "C-j") #'(lambda () (interactive) (find-file-other-window (thing-at-point 'filename))))
(define-key clearcase-lsprivate-mode-map (kbd "RET") #'(lambda () (interactive) (find-file-other-window (thing-at-point 'filename))))
(define-key clearcase-lsprivate-mode-map (kbd "k")   #'(lambda () (interactive) (quit-window 'kill)))
(define-key clearcase-lsprivate-mode-map (kbd "p") #'larumbe/clearcase-prev-line)
(define-key clearcase-lsprivate-mode-map (kbd "n") #'larumbe/clearcase-next-line)
(define-key clearcase-lsprivate-mode-map (kbd "d") #'larumbe/clearcase-mark-deletion-file)
(define-key clearcase-lsprivate-mode-map (kbd "u") #'larumbe/clearcase-unmark-file)
(define-key clearcase-lsprivate-mode-map (kbd "x") #'larumbe/clearcase-delete-marked-files)
(define-key clearcase-lsprivate-mode-map (kbd "l") #'recenter-top-bottom)


(defun larumbe/clearcase-lsprivate-revert-buffer (&optional ignore-auto noconfirm preserve-modes)
  (interactive)
  (let ((pos (point)))
    (save-window-excursion
      (larumbe/clearcase-lsprivate))
    (if (> pos (point-max))
        (goto-char (point-min))
      (goto-char pos))))


;;;###autoload
(define-derived-mode clearcase-lsprivate-mode special-mode
  (setq font-lock-defaults larumbe/clearcase-lsprivate-font-lock-defaults)
  (setq-local revert-buffer-function #'larumbe/clearcase-lsprivate-revert-buffer)
  (setq mode-name "CC-PrivateFiles"))



(provide 'clearcase-utils)

;;; clearcase-utils.el ends here
