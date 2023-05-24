;;; untabify-trailing-ws.el --- Untabify/Trailing WS  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Basic Larumbe's minor mode to untabify and delete trailing whitespaces in buffers (except for Makefiles).
;; This seems to be an obsolete method and there are very good alternatives out there, but it still works, so that's why I keep it.
;; For more info on alternatives, check the followings related modes:
;;
;;;; Related modes
;;
;; ethan-wspace: cleans only if file is not full of whitespaces to avoid `diff' issues in large projects
;;   https://github.com/glasserc/ethan-wspace
;;
;; ws-trimm
;;
;; ws-butler
;;
;; etc...
;;
;;; Code:

(defvar untabify-trailing-delete-whitespace t) ; Default initial value
(defvar untabify-trailing-disable-on-files nil
  "List of files where Untabify/Delete trailing whitespace should not be executed.")

(defun untabify-trailing-whitespace ()
  "Untabify and delete trailing whitespace depending on MAJOR-MODE of current buffer.
Meant to be used as a wrapper for write-file-functions hook."
  (interactive)
  (unless (or (string-match "makefile-" (format "%s" major-mode)) ; Do not untabify `makefile-mode'
              (member buffer-file-name (mapcar #'expand-file-name untabify-trailing-disable-on-files)))
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace (point-min) (point-max))))

;;;###autoload
(define-minor-mode untabify-trailing-ws-mode
  "Basic minor mode to untabify and delete trailing whitespaces by using write-file-functions hooks."
  :global t
  (if untabify-trailing-ws-mode
      (progn   ;; Enable
        (setq untabify-trailing-delete-whitespace t)
        (add-hook 'write-file-functions #'untabify-trailing-whitespace)
        (message "Untabify set to: %s" untabify-trailing-delete-whitespace))
    ;; Disable
    (setq untabify-trailing-delete-whitespace nil)
    (remove-hook 'write-file-functions #'untabify-trailing-whitespace)
    (message "Untabify set to: %s" untabify-trailing-delete-whitespace)))



;;; Provide
(provide 'untabify-trailing-ws)

;;; untabify-trailing-ws.el ends here
