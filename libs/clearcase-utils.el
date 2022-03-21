;;; clearcase-utils.el --- ClearCase Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'hydra)
(require 'clearcase)


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




(defhydra hydra-clearcase (:color blue
                           :hint  nil)
  ("f"  larumbe/clearcase-checkout "Check-out file(s)" :column "Check")     ; "Fetch"
  ("u"  larumbe/clearcase-uncheckout "Uncheck-out file(s)")                 ; "Unstage"
  ("p"  larumbe/clearcase-checkin "Check-in file(s)")                       ; "Push"
  ("s"  clearcase-find-checkouts-in-current-view "List CO of Current View") ; "Status"
  ("FF" larumbe/clearcase-dired-checkout-current-dir "Check-out dir")
  ("FU" larumbe/clearcase-dired-uncheckout-current-dir "Uncheck-out dir")
  ("CE" larumbe/clearcase-edit-checkout-comment "Edit CO comment")
  ("SS" clearcase-edcs-edit "Edit Config-Spec")

  ("e"  larumbe/clearcase-ediff-pred "Ediff predecesor" :column "Diff")
  ("E"  larumbe/clearcase-ediff-named-version "Ediff named version")
  ("be" larumbe/clearcase-ediff-branch-base "Ediff branch base")
  ("d"  larumbe/clearcase-diff-pred "Diff predecesor")
  ("D"  larumbe/clearcase-diff-named-version "Diff named version")
  ("bd" larumbe/clearcase-diff-branch-base "Diff branch base")

  ("l"  larumbe/clearcase-list-history "Element history" :column "History")
  ("L"  larumbe/clearcase-browse-vtree "Browse Vtree")
  ("~"  larumbe/clearcase-version-other-window "Version other window")
  ("w"  larumbe/clearcase-what-rule "Config Spec Rule")
  ("a"  larumbe/clearcase-annotate "Annotate")
  ("?"  larumbe/clearcase-describe "Describe")

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



(provide 'clearcase-utils)

;;; clearcase-utils.el ends here
