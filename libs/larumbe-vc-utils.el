;;; larumbe-vc-utils.el --- Version Control Utilities  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'magit)


;;;; Repohome
(defvar larumbe/gite-work-tree (expand-file-name "~"))
(defvar larumbe/gite-repo-path (expand-file-name "~/.repohome"))

;;;###autoload
(defun larumbe/repohome-magit-status ()
  "Perform `magit-status' with `git-dir' and `work-tree' changed accordingly.
INFO: Is not possible to use `magit-git-global-arguments' as a local variable,
since it needs to be set for the whole magit session, not only for the command."
  (interactive)
  (let ((gite-args (concat "--git-dir=" larumbe/gite-repo-path " --work-tree=" larumbe/gite-work-tree)))
    (larumbe/repohome-magit-reset-args)
    (setq magit-git-global-arguments (append magit-git-global-arguments (split-string gite-args))) ; Append `gite' args
    (magit-status-setup-buffer larumbe/gite-work-tree)
    (message "Gite arguments set...")))


;; https://emacs.stackexchange.com/questions/3022/reset-custom-variable-to-default-value-programmatically
;;;###autoload
(defun larumbe/repohome-magit-reset-args ()
  "Reset git global arguments to switch between `gite' workspace and the rest."
  (interactive)
  (setq magit-git-global-arguments (eval (car (get 'magit-git-global-arguments 'standard-value))))
  (message "Git arguments reset!"))



;;;; Git
;;;;; Branch merging and checkout
(defvar larumbe/git-branch-files-to-exclude-from-merge
      '(".bashrc"
        ".gitconfig"
        ".xinitrc"
        ".globalrc"))

(defun larumbe/git-find-changed-files-between-branches (rev-a rev-b)
  "Return a list of strings with changed files between REV-A and REV-B."
  (let ((str)
        (buffer-name "*shell-git-diff-output*"))
    (save-window-excursion
      (shell-command
       (concat "git diff --name-status " rev-a ".." rev-b)
       buffer-name)
      (switch-to-buffer buffer-name)
      (goto-char (point-min))
      (while (re-search-forward "[MAD]\t" nil t)  ; Modified/Added/Deleted = MAD
        (replace-match ""))
      (setq str (split-string (buffer-string)))
      str)))


;; https://stackoverflow.com/questions/25009453/how-to-delete-a-list-of-elements-from-another-list-in-elisp
;; It is the same as solution 1 but using delete instead of delq, and printing the value of temp variable at the end
(defun larumbe/git-exclude-files-before-ediff (merge-files exclude-files)
  "Remove EXCLUDE-FILES from MERGE-FILES parameter list."
  (let (temp)
    (setq temp merge-files)
    (mapc
     (lambda (x)
       (setq temp (delete x temp)))
     exclude-files)
    (setq temp temp)))  ; Return only last value after all the iterations


(defun larumbe/git-merge-all-files-between-branches (reva revb changed-files)
  "Ediff/Merge every file from CHANGED-FILES.
Compares same file of revisions REVA and REVB using `magit-ediff-compare'"
  (mapcar
   (lambda (file)
     (magit-ediff-compare reva revb file file))
   changed-files))


;;;###autoload
(defun larumbe/git-checkout-file-from-another-branch ()
  "Used when an override needs to be performed."
  (interactive)
  (let (fetch-file-from-branch
        fetch-file-to-branch
        filename
        files-changed)
    (save-window-excursion
      ;; Prepare variables according to initial setup and prompts
      (setq fetch-file-from-branch (completing-read "Checkout file from branch: " (magit-list-branch-names)))
      (setq fetch-file-to-branch   (completing-read "Checkout file to branch: "   (magit-list-branch-names) nil nil (magit-get-current-branch)))
      (setq files-changed (larumbe/git-find-changed-files-between-branches fetch-file-from-branch fetch-file-to-branch))
      (setq files-changed (larumbe/git-exclude-files-before-ediff files-changed larumbe/git-branch-files-to-exclude-from-merge))
      (setq filename (completing-read "Choose file to checkout: " files-changed))
      ;; And finally choose between the possible files and execute the shell-command to checkout the file
      (shell-command
       (concat
        magit-git-executable " checkout " fetch-file-to-branch " && "
        magit-git-executable " checkout " fetch-file-from-branch " " filename)))))


;;;###autoload
(defun larumbe/git-manual-branch-ediff-merge ()
  "Ediff manual merging of every changed file between chosen revisions."
  (interactive)
  (let (changed-files rev-a rev-b)
    (setq rev-a (completing-read "RevA: " (magit-list-branch-names)))
    (setq rev-b (completing-read "RevB: " (magit-list-branch-names) nil nil (magit-get-current-branch)))
    ;; First part is to find which files have changed between both branches
    (setq changed-files (larumbe/git-find-changed-files-between-branches rev-a rev-b))
    ;; An (optional) step would be to determine which of previous files require manual merging
    (setq changed-files (larumbe/git-exclude-files-before-ediff changed-files larumbe/git-branch-files-to-exclude-from-merge))
    ;; Last step would be to merge manually these files
    (larumbe/git-merge-all-files-between-branches rev-a rev-b changed-files)))




;;;; Misc
;;;###autoload
(defun larumbe/git-pull-all-at-dir (dir)
  "Update all the Git repos in DIR.
Assumes DIR is formed only by directories and all of them are Git repos.
Pulls on master branch."
  (let ((git-repo-list (f-directories dir)))
    (dolist (git-repo git-repo-list)
      (async-shell-command (concat "cd " git-repo " && git checkout master && git pull")))))


;;;###autoload
(defun larumbe/update-repo-all-at-dir (dir)
  (let ((repo-sandbox-list (f-directories dir)))
    (dolist (repo-sandbox repo-sandbox-list)
      (async-shell-command (concat "cd " repo-sandbox " && update_repo")))))


;;;###autoload
(defun larumbe/git-check-dirty-repos (repos &optional buf)
  "Show dirty repos/files of every repo in REPOS in *git-dirty* buffer.
REPOS is assumed to be a list of strings containing the path of each repo.

If optional variable BUF is set show output in BUF, otherwise in *git-dirty* buffer."
  (interactive)
  (unless (executable-find "git")
    (error "Git is not installed!"))
  (unless buf
    (setq buf "*git-dirty*"))
  (let ((shell-command-dont-erase-buffer t) ; Append output to buffer
        (cmd))
    ;; Clean buffer at the beginning
    (get-buffer-create buf)
    (with-current-buffer buf
      (view-mode -1)
      (erase-buffer))
    ;; Check dirty repos
    (dolist (repo repos)
      (message "Checking %s..." repo)
      (setq cmd (concat "git -C " repo " status --short"))
      (unless (string= "" (shell-command-to-string cmd))
        (message "Repo %s has uncommitted changes!" repo)
        (with-current-buffer buf
          (goto-char (point-max))
          (insert "Repo " repo ":\n"))
        (shell-command cmd buf buf)
        (with-current-buffer buf
          (goto-char (point-max))
          (insert "\n"))))
    (pop-to-buffer buf)
    (view-mode)))


;;;###autoload
(defun larumbe/git-check-forked-repos (repos)
  "Check REPOS to know which of them are forked.
REPOS is assumed to be a list of strings containing the path of each repo.

Return list of strings with forked repos.
A repo is considered forked if remote is not tracking origin."
  (interactive)
  (unless (executable-find "git")
    (error "Git is not installed!"))
  (let (forked-repos cmd cmd-out remote-branch)
    ;; Check repos
    (dolist (repo repos)
      (message "Checking if %s is forked..." repo)
      ;; (setq cmd (concat "git -C " repo " rev-parse --abbrev-ref --symbolic-full-name @{u}"))
      (setq cmd (concat "git -C " repo " status -sb"))
      ;; (setq cmd (concat "git -C " repo " for-each-ref --format='%(upstream:short)' \"$(git symbolic-ref -q HEAD)\""))
      (setq cmd-out (shell-command-to-string cmd))
      (setq remote-branch (car (cdr (split-string cmd-out "\\.\\.\\."))))
      (when (string-prefix-p "fork/" remote-branch)
        (message "Forked repo: %s" repo)
        (push repo forked-repos)))
    forked-repos))


;;;###autoload
(defun larumbe/emacs-check-dirty-repos (&optional all)
  "Show dirty emacs-conf files in *emacs-dirty* buffer.
If prefix-arg or ALL argument is passed, check all my emacs conf repos."
  (interactive "P")
  (let (repos)
    (if current-prefix-arg
        (setq repos (append larumbe/emacs-conf-repos-core (larumbe/straight-packages)))
      (setq repos larumbe/emacs-conf-repos-devel))
    (larumbe/git-check-dirty-repos repos "*emacs-dirty*")))



;;;###autoload
(defun larumbe/set-vc-follow-symlinks ()
  "Set interactively the value of `vc-follow-symlinks'."
  (interactive)
  (let ((value (completing-read "Set `vc-follow-symlinks' to: " '(yes no ask))))
    (setq vc-follow-symlinks value)
    (message "`vc-follow-symlinks' set to: %s" vc-follow-symlinks)))



(provide 'larumbe-vc-utils)

;;; larumbe-vc-utils.el ends here
