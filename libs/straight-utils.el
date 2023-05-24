;;; straight-utils.el --- Straight Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;###autoload
(defun larumbe/straight-not-repo-p (repo)
  "Return true if REPO is not a straight repo."
  (not (string-prefix-p (expand-file-name "~/.emacs.d/straight/") repo)))

;;;###autoload
(defun larumbe/straight-packages ()
  "Return list of strings with the paths of every straight repo."
  (let* ((straight-repos-dir (expand-file-name (file-name-concat straight-base-dir "straight/repos")))
         (straight-packages (directory-files straight-repos-dir t)))
    (setq straight-packages (delete (file-name-concat straight-repos-dir ".")  straight-packages))
    (setq straight-packages (delete (file-name-concat straight-repos-dir "..") straight-packages))
    straight-packages))

;;;###autoload
(defun larumbe/straight-check-dirty-repos ()
  "Show dirty straight repos/files in *straight-dirty* buffer."
  (interactive)
  (unless straight-base-dir
    (error "Variable `straight-base-dir' not set!"))
  (larumbe/git-check-dirty-repos (larumbe/straight-packages) "*straight-dirty*"))

;;;###autoload
(defun larumbe/straight-check-forked-repos ()
  "Show straight forked repos/files in *straight-forked* buffer."
  (interactive)
  (larumbe/git-check-forked-repos (larumbe/straight-packages)))


(provide 'straight-utils)

;;; straight-utils.el ends here
