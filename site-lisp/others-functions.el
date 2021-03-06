;;; others-functions.el --- Third party custom functions  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This file hosts some general purpose functions that I found over the Internet
;;
;; These are meant to be used all along the configuration file.
;;
;;; Code:

(require 'with-editor)

;;;; Restart code
;; https://emacs.stackexchange.com/questions/5428/restart-emacs-from-within-emacs
(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))


(defun launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))


;;;###autoload
(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                           #'launch-separate-emacs-under-x
                                                         #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))


;;;; Buffer management
;;;###autoload
(defun close-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc #'kill-buffer (buffer-list)))


;;;###autoload
(defun only-current-buffer ()
  "Kill all buffers except active one."
  (interactive)
  (mapc #'kill-buffer (cdr (buffer-list (current-buffer)))))


;;;###autoload
(defun buffer-mode (&optional buffer)
  "Return the major mode associated with BUFFER."
  (let (buf)
    (if buffer
        (setq buf buffer)
      (setq buf (current-buffer)))
    (with-current-buffer buf
      major-mode)))


;;;###autoload
(defun file-title ()
  "Return file title; e.g. for '/opt/asdf.txt' eval 'asdf'."
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))



;;;; Editing
;; http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
;;;###autoload
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.

If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))


;; https://stackoverflow.com/questions/730751/hiding-m-in-emacs
;;;###autoload
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


;; https://emacsredux.com/blog/2013/04/21/edit-files-as-root/
;;;###autoload
(defun er-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


;;;; Lists/regexp/strings/files/directories
;; http://ergoemacs.org/emacs/elisp_read_file_content.html
;;;###autoload
(defun read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))


;; http://ergoemacs.org/emacs/elisp_read_file_content.html
;;;###autoload
(defun get-string-from-file (filePath)
  "Return FILEPATH file content as a string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))


;; https://stackoverflow.com/questions/17325713/looking-for-a-replace-in-string-function-in-elisp
;;;###autoload
(defun replace-in-string (what with in)
  "Replace WHAT to WITH in string IN."
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))



;;;; More complex/less frequently used
;;;###autoload
(defun move-same-indent (&optional backward)
  "Move forward to next line with same indent.

Copied from `vhdl-mode' and generalized.

Return new point position if found same indent, otherwise return nil.

If BACKWARD is non-nil, search for same indent backward."
  (let ((start (point))
        (indent (current-indentation))
        (arg (if backward
                 -0
               2)))
    (beginning-of-line arg)
    (while (and (not (eobp))
                (or (looking-at (concat "^\\s-*" "\\(" comment-start ".*" "\\)?$"))
                    (> (current-indentation) indent)))
      (beginning-of-line arg))
    (if (= (current-indentation) indent)
        (progn
          (back-to-indentation)
          (point))
      (goto-char start)
      nil)))

;;;###autoload
(defun forward-same-indent ()
  "Move forward to next line with same indent.
Return new point position if found same indent, otherwise return nil."
  (interactive)
  (move-same-indent))

;;;###autoload
(defun backward-same-indent ()
  "Move backward to previous line with same indent.
Return new point position if found same indent, otherwise return nil."
  (interactive)
  (move-same-indent :backward))


;; https://emacs.stackexchange.com/questions/5441/function-to-delete-all-comments-from-a-buffer-without-moving-them-to-kill-ring
;;;###autoload
(defun delete-comments-from-buffer ()
  "Delete comments from buffer without moving them to the kill ring."
  (interactive)
  (goto-char (point-min))
  (let (kill-ring)
    (comment-kill (count-lines (point-min) (point-max)))))


;; https://stackoverflow.com/questions/31767779/is-there-an-apply-command-to-each-line-in-region-in-emacs
;; INFO: Do not use functions that alter the length of the buffer
;; (e.g. #'kill-line) as the start/end parameters will change during execution.
;;;###autoload
(defun do-lines (fun &optional start end)
  "Invoke function FUN on the text of each line from START to END."
  (interactive
   (let ((fn (intern (completing-read "Function: " obarray 'functionp t))))
     (if (use-region-p)
         (list fn (region-beginning) (region-end))
       (list fn (point-min) (point-max)))))
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (funcall fun (buffer-substring (line-beginning-position) (line-end-position)))
      (forward-line 1))))



;; https://gist.github.com/ffevotte/9345586#file-gistfile1-el
;;;###autoload
(defun source (filename)
  "Update environment variables from FILENAME source file."
  (interactive "fSource file: ")
  (message "Sourcing environment from `%s'..." filename)
  (with-temp-buffer
    (shell-command (format "diff -u <(true; export) <(source %s; export)" filename) '(4))
    (let ((envvar-re "declare -x \\([^=]+\\)=\\(.*\\)$"))
      ;; Remove environment variables
      (while (search-forward-regexp (concat "^-" envvar-re) nil t)
        (let ((var (match-string 1)))
          (message "%s" (prin1-to-string `(setenv ,var nil)))
          (setenv var nil)))
      ;; Update environment variables
      (goto-char (point-min))
      (while (search-forward-regexp (concat "^+" envvar-re) nil t)
        (let ((var (match-string 1))
              (value (read (match-string 2))))
          (message "%s" (prin1-to-string `(setenv ,var ,value)))
          (setenv var value)))))
  (message "Sourcing environment from `%s'... done." filename))


;; https://emacs.stackexchange.com/questions/10077/how-to-edit-crontab-directly-within-emacs-when-i-already-have-emacs-open
;;;###autoload
(defun crontab-e ()
  "Run `crontab -e' in an Emacs buffer."
  (interactive)
  (with-editor-async-shell-command "crontab -e"))



(provide 'others-functions)

;;; others-functions.el ends here
