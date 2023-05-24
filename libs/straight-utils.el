;;; straight-utils.el --- Straight Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'straight)

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


(provide 'straight-utils)

;;; straight-utils.el ends here
