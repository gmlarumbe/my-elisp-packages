;;; git-dirty.el --- Navigate in Git-dirty buffer  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Inspired by `repo-status', this mode allows navigation in buffers
;; created by `git-dirty-check-repos'.
;;
;; Move between dirty repos with p, n, C-c C-n, C-c C-p
;; Start magit with RET, C-m or C-j
;;
;;; Code:

(require 'magit)
(require 'straight-utils)

;;;; Vars/Customizable
(defvar git-dirty-repo-list nil "List of repos to look for `git-dirty-check-repos'.")
(defvar git-dirty-repo-list-emacs nil "List of Emacs repos to look for `git-dirty-check-repos-emacs'.")
(defvar git-dirty-repo-list-straight nil "List of straight repos to look for `git-dirty-check-repos-straight'.")

;;;; Internal vars
(defconst git-dirty-project-regexp "^\\(?1:Repo\\) \\(?2:[^ ]+\\):")
(defconst git-dirty-font-lock-defaults
  `(((,git-dirty-project-regexp . ((1 font-lock-function-name-face) (2 font-lock-variable-name-face))))))

(defvar git-dirty-current-repo-list nil)
(defvar git-dirty-current-repo-buf nil)

;;;; Defuns
(defun git-dirty-next-project ()
  "Move to next project in Git-Dirty status buffer."
  (interactive)
  (let ((pos (point)))
    (save-excursion
      (forward-line)
      (when (re-search-forward git-dirty-project-regexp nil t)
        (setq pos (point))))
    (when (> pos (point))
      (goto-char pos)
      (beginning-of-line))))

(defun git-dirty-previous-project ()
  "Move to previous project in Git-Dirty status buffer."
  (interactive)
  (re-search-backward git-dirty-project-regexp nil t))

(defun git-dirty-clean-buffer ()
  "Kill/clean the current buffer."
  (interactive)
  (quit-window t))

(defun git-dirty-revert-buffer ()
  "Revert buffer."
  (interactive)
  (git-dirty-check-repos git-dirty-current-repo-list git-dirty-current-repo-buf))

(defun git-dirty-magit ()
  "Run `magit' on repo where the point is at."
  (interactive)
  (let (repo-name)
    (save-excursion
      (goto-char (line-beginning-position))
      (when (re-search-forward "^Repo " (line-end-position) t)
        (setq repo-name (thing-at-point 'filename t))
        (setq repo-name (string-remove-suffix ":" repo-name))))
    (if repo-name
        (magit-status-setup-buffer repo-name)
      (error "Point must be at 'Repo' line, not at a file!"))))

;;;###autoload
(defun git-dirty-check-repos (repos &optional buf)
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
      (read-only-mode -1)
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
    (setq git-dirty-current-repo-list repos)
    (setq git-dirty-current-repo-buf buf)
    (pop-to-buffer buf)
    (goto-char (point-min))
    (git-dirty-mode)))

;;;###autoload
(defun git-dirty-check-repos-emacs (&optional all)
  "Show dirty emacs-conf files in *emacs-dirty* buffer.
If prefix-arg or ALL argument is passed, check all my emacs conf repos."
  (interactive "P")
  (let (repos)
    (if all
        (setq repos (append git-dirty-repo-list-emacs git-dirty-repo-list-straight))
      (setq repos git-dirty-repo-list-emacs))
    (git-dirty-check-repos repos "*emacs-dirty*")))

;;;###autoload
(defun git-dirty-check-repos-straight ()
  "Show dirty straight repos/files in *straight-dirty* buffer."
  (interactive)
  (git-dirty-check-repos git-dirty-repo-list-straight "*straight-dirty*"))


(defvar git-dirty-mode-map nil "Keymap for git-dirty-mode.")
(when (not git-dirty-mode-map)
  (setq git-dirty-mode-map (make-sparse-keymap))
  (define-key git-dirty-mode-map (kbd "RET")     #'git-dirty-magit)
  (define-key git-dirty-mode-map (kbd "C-j")     #'git-dirty-magit)
  (define-key git-dirty-mode-map (kbd "q")       #'git-dirty-clean-buffer)
  (define-key git-dirty-mode-map (kbd "k")       #'git-dirty-clean-buffer)
  (define-key git-dirty-mode-map (kbd "p")       #'git-dirty-previous-project)
  (define-key git-dirty-mode-map (kbd "n")       #'git-dirty-next-project)
  (define-key git-dirty-mode-map (kbd "g")       #'git-dirty-revert-buffer)
  (define-key git-dirty-mode-map (kbd "C-c C-p") #'git-dirty-previous-project)
  (define-key git-dirty-mode-map (kbd "C-c C-n") #'git-dirty-next-project))

;;;###autoload
(define-derived-mode git-dirty-mode fundamental-mode "Git-Dirty"
  "A mode for Git-dirty status buffer."
  (setq-local font-lock-defaults git-dirty-font-lock-defaults)
  (read-only-mode)
  (view-mode -1))



(provide 'git-dirty)

;;; git-dirty.el ends here
