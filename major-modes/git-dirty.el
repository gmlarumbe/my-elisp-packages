;;; git-dirty.el --- Navigate in Git-dirty buffer  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Inspired by `repo-status', this mode allows navigation in buffers
;; created by `larumbe/git-check-dirty-repos'.
;;
;; Move between dirty repos with p, n, C-c C-n, C-c C-p
;; Start magit with RET, C-m or C-j
;;
;;; Code:



(defvar git-dirty-project-regexp "^\\(?1:Repo\\) \\(?2:[^ ]+\\):")

(defvar git-dirty-font-lock-defaults
  `(((,git-dirty-project-regexp . ((1 font-lock-function-name-face) (2 font-lock-variable-name-face))))))


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


(defun git-dirty-magit ()
  "Run `magit' on repo where the point is at."
  (interactive)
  (let (repo-name)
    (save-excursion
      (when (not (eq 0 (skip-chars-forward "Repo ")))
        (setq repo-name (thing-at-point 'filename t))
        (setq repo-name (string-remove-suffix ":" repo-name))))
    (if repo-name
        (magit-status-setup-buffer repo-name)
      (error "Point must be at 'Repo' line, not at a file!"))))


(defvar git-dirty-mode-map nil "Keymap for git-dirty-mode.")
(when (not git-dirty-mode-map)
  (setq git-dirty-mode-map (make-sparse-keymap))
  (define-key git-dirty-mode-map (kbd "RET")     #'git-dirty-magit)
  (define-key git-dirty-mode-map (kbd "C-j")     #'git-dirty-magit)
  (define-key git-dirty-mode-map (kbd "q")       #'git-dirty-clean-buffer)
  (define-key git-dirty-mode-map (kbd "k")       #'git-dirty-clean-buffer)
  (define-key git-dirty-mode-map (kbd "p")       #'git-dirty-previous-project)
  (define-key git-dirty-mode-map (kbd "n")       #'git-dirty-next-project)
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
