;;; dired-utils.el --- Dired Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dired)
(require 'dired-async)

(defun larumbe/dired-jump (arg)
  "Execute `dired-jump'.
With universal ARG, delete every dired-mode buffer to have only 1 dired buffer.
Provides a more convenient solution to cluttering dired buffers than `dired-single'."
  (interactive "P")
  (when arg
    (dolist ($buf (buffer-list (current-buffer)))
      (with-current-buffer $buf
        (when (string= major-mode "dired-mode")
          (kill-buffer $buf)))))
  (cond ((string= major-mode "vterm-mode")
         ;; INFO: Updates `default-directory' from current shell through file write/read
         (let* ((pwd-file "/tmp/vterm-last-dir")
                (pwd-cmd (concat "echo -n $(pwd) > " pwd-file "\n"))
                (default-directory default-directory)) ; Save global status of `default-directory'
           (larumbe/sh-send-string-vterm pwd-cmd)
           (sleep-for 0.15) ; Without this line point moved far above in *vterm*
           (setq default-directory (shell-command-to-string (concat "cat " pwd-file)))
           (dired-jump)))
        (t
         (dired-jump))))

(defun larumbe/dired-toggle-deletion-confirmer ()
  "Toggles deletion confirmer for dired from (y-or-n) to nil and viceversa."
  (interactive)
  (if (equal dired-deletion-confirmer 'yes-or-no-p)
      (progn
        (setq dired-deletion-confirmer '(lambda (x) t))
        (message "Dired deletion confirmation: FALSE"))
    (progn
      (setq dired-deletion-confirmer 'yes-or-no-p)
      (message "Dired deletion confirmation: TRUE"))))

(defun larumbe/dired-do-async-shell-command-or-okular ()
  "Same as `dired-do-async-shell-command' but if on a PDF will open Okular directly."
  (interactive)
  (when (not (string-equal major-mode "dired-mode"))
    (error "Needs to be executed in dired...! "))
  (let ((program "okular")
        (filename (thing-at-point 'filename t)))
    (if (string-equal (file-name-extension filename) "pdf")
        (progn
          (dired-do-async-shell-command program filename (list filename))
          (delete-window (get-buffer-window "*Async Shell Command*")))
      (call-interactively #'dired-do-async-shell-command))))

;; `dired-async-mode' is an autoload from `async' package in straight repo "emacs-async"
;; Seems more reasonable to reference it here than in async's use-package call
(defun larumbe/dired-async-toggle ()
  "Run asynchronously dired commands for copying, renaming and symlinking, through async library.
Useful since sometimes it takes longer renaming/copying for small files due to async processing overhead.
To cancel a copy call `dired-async-kill-process'. "
  (interactive)
  (if dired-async-mode
      (progn
        (dired-async-mode -1)
        (message "dired-async disabled"))
    (dired-async-mode 1)
    (message "dired-async enabled")))


(provide 'dired-utils)

;;; dired-utils.el ends here
