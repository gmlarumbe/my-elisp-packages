;;; gtags-utils.el --- Gtags Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Global/ctags utils
;;
;;
;;; Code:


;;;; Dependencies
(require 'ggtags)


;;;; Auxiliar functions
;;;###autoload
(defun larumbe/gtags-backend-switch ()
  "Switch between diferent backends for Global and ggtags.
The function `ggtags-create-tags' used by all the wrappers relies on the
environment variable GTAGSLABEL, which will select between backends
available at GTAGSCONF globalrc file."
  (interactive)
  (let ((active-backend)
        (backends '("pygments" "ctags" "default")))
    (setq active-backend (completing-read "Select backend: " backends))
    (setenv "GTAGSLABEL" active-backend)
    (message "Set env GTAGSLABEL=%s" active-backend)))


;;;; Gtags creation async
;; List of available regexps for different languages gtags extraction
;; If cdr of an element is a string use it as the regexp of the file extension
;; If cdr of an element is a cons cell, use first element as the regexp and second as the exclude-re
(defvar larumbe/gtags-create-tags-lang-regexps
  '(("(System)Verilog" . ("\\.[s]?v[h]?$" . "[^/]+_targets")) ; Exclude re
    ("Python"          . "\\.py$")
    ("Elisp"           . "\\.el$")
    ("c"               . "\\.[ch]\\\(pp\\)?$")
    ("VHDL"            . "\\.vhd[l]?$")
    ("other"           . nil)))


;; INFO: Global does not allow to find external definitions outside project root directory (probably due to security reasons).
;; In order to do so, there are 2 methods:
;;   - Use symbolic links to external directories.
;;   - Make use of GTAGSLIBPATH environment variable.
;; Associated thread: https://emacs.stackexchange.com/questions/13254/find-external-definition-with-gtags-or-ggtags
(defun larumbe/gtags-create-tags-filelist-create (regex &optional exclude-re dir append)
  "Generate gtags.files on current directory for tags in directory DIR if it is set.
Include files that match REGEX.
If EXCLUDE-RE is set, delete paths with that regexp from generated file.
If DIR is not specified, use current-directory.
If APPEND is set, append directory files to already existing tags file."
  (let (tags-dir)
    (if dir
        (setq tags-dir dir)
      (setq tags-dir default-directory))
    (message "Creating gtags.files ...")
    (larumbe/directory-files-recursively-to-file tags-dir "gtags.files" regex append exclude-re)))


(defun larumbe/gtags-create-tags-async-sentinel (process signal)
  "Sentinel for asynchronous gtags creation."
  (let* ((buf (process-buffer process)))
    (cond
     ((equal signal "finished\n")
      (pop-to-buffer buf)
      (message "GTAGS generated in %s" buf))
     ;; Error handling
     ('t
      (message "#'larumbe/gtags-create-tags-async-sentinel: %s failed with error code %s" buf signal)
      (display-buffer buf)))))


(defun larumbe/gtags-create-tags-async-kill-buf-sentinel (process signal)
  "Sentinel for asynchronous gtags creation.
Kills gtags buffer after finishing the process if created sucessfully."
  (let* ((buf (process-buffer process)))
    (cond
     ((equal signal "finished\n")
      (message "GTAGS successfully generated: %s" buf)
      (switch-to-buffer buf)
      (kill-buffer))
     ;; Error handling
     ('t
      (message "#'larumbe/gtags-create-tags-async-kill-buf-sentinel: %s failed with error code %s" buf signal)
      (display-buffer buf)))))


(defun larumbe/gtags-create-tags-async-process (dir &optional bufname kill-buf)
  "Spawn shell and execute gtags asynchronously at directory DIR.
Use buffer BUFNAME if optional arg is provided. Otherwise, default will be *gtags-<dirname>*.
If optional KILL-BUF is non-nil, create a sentinel that kills the process buffer after creating tags."
  (let ((gtags-cmd "gtags -v")
        (output-buffer (if bufname
                           bufname
                         (concat "*gtags-" (file-name-nondirectory (directory-file-name dir)) "*")))
        (sentinel (if kill-buf
                      #'larumbe/gtags-create-tags-async-kill-buf-sentinel
                    #'larumbe/gtags-create-tags-async-sentinel)))
    (save-window-excursion
      (async-shell-command (concat "cd " dir " && " gtags-cmd) output-buffer))
    (message "Started gtags at buffer %s" output-buffer)
    (set-process-sentinel (get-buffer-process output-buffer) sentinel)))



;;;###autoload
(defun larumbe/gtags-create-tags-async (&optional dir lang bufname kill-buf)
  "Similar to `ggtags-create-tags' but asynchronously and adapted to Global+Ctags+Pygments workflow.

Create tags at DIR for language LANG.
If DIR is nil create tags at `default-directory'.
If LANG is nil default to first language in `larumbe/gtags-create-tags-lang-regexps'.
Use buffer BUFNAME if optional arg is provided. Otherwise, default will be *gtags-<dirname>*.
If KILL-BUF is non-nil kill the *gtags-<path>* buffer after finishing the process.

If called interactively, default to create tags for first language in `larumbe/gtags-create-tags-lang-regexps'
at `default-directory'. If called interactively with prefix, prompt for DIR and LANG."
  (interactive "P")
  (let ((lang-regexps larumbe/gtags-create-tags-lang-regexps)
        (regex)
        (regex-lang-cdr) ; cdr of element of `larumbe/gtags-create-tags-lang-regexps' (listp or stringp)
        (exclude-re))
    ;; Default dir and lang
    (unless dir
      (setq dir default-directory))
    (unless lang
      (setq lang (car (car lang-regexps))))
    ;; Prompt for dir and lang if called interactively with prefix
    (when (and current-prefix-arg
               (called-interactively-p))
      (setq dir (read-directory-name "Directory: "))
      (setq lang (completing-read "Lang: " (mapcar #'car lang-regexps))))
    ;; Set variable values
    (setq dir (expand-file-name dir)) ; Expand in case tags are created at ~ or something similar
    (setq regex-lang-cdr (cdr (assoc lang lang-regexps)))
    ;; Set proper regexp
    (cond
     ;; If cdr is a string, use it as the regex for file creation
     ((stringp regex-lang-cdr)
      (setq regex regex-lang-cdr))
     ;; If cdr is a list (non-nil, e.g. "other"), use first elm as the regex and second as the exclude-re
     ((and (listp regex-lang-cdr)
           regex-lang-cdr)
      (setq regex      (car regex-lang-cdr))
      (setq exclude-re (cdr regex-lang-cdr)))
     ;; Other language: cdr should be nil, prompt for regex for file creation
     ((string= lang "other")
      (when regex-lang-cdr
        (error "If selecting other languaage, regexp must be nil!"))
      (setq regex (read-string "Lang file extension regexp: " "\\.{ext}$")))
     ;; Default (throw error)
     (t (error "Should have not arrived here!")))
    ;; Create filelist
    (larumbe/gtags-create-tags-filelist-create regex exclude-re dir) ; Do not append created tags to existing file
    ;; Execute gtags
    (larumbe/gtags-create-tags-async-process dir bufname kill-buf)))


;;;###autoload
(defun larumbe/gtags-create-tags-async-dirs (dirs)
  "Create gtags for list of strings directories DIRS.
Skip the ones where there is no write permissions. "
  (dolist (dir dirs)
    (if (file-writable-p dir)
        (larumbe/gtags-create-tags-async dir
                                         "Elisp"
                                         (concat "*gtags-" (expand-file-name dir) "*")
                                         :kill-buf)
      (message "Skipping %s due to write permissions..." dir))))


;;;; Gtags update async
(defvar larumbe/gtags-update-buf-name  "*gtags-update*")
(defvar larumbe/gtags-update-proc-name "gtags-update")
(defvar larumbe/gtags-update-verbose nil)

(defun larumbe/gtags-update-start-process ()
  "Start gtags-update process.
Spawn a /bin/bash shell that will run the update commands for ggtags project associated
to current buffer, if existing."
  (let* ((buf larumbe/gtags-update-buf-name)
         (proc larumbe/gtags-update-proc-name))
    (unless (get-process proc)
      (start-process proc buf "/bin/bash")
      (set-process-filter (get-process proc) #'larumbe/gtags-update-filter)
      (set-process-sentinel (get-process proc) #'larumbe/gtags-update-sentinel)
      (set-process-query-on-exit-flag (get-process proc) nil)
      (message "Started %s" buf))))


(defun larumbe/gtags-update-kill-process ()
  "Kill gtags-update process.
Clean-up of buffer/window done by corresponding sentinel."
  (let ((proc larumbe/gtags-update-proc-name))
    (if (get-process proc)
        (progn
          (delete-process proc)
          (message "Finished gtags-update"))
      (message "gtags-update was not running!"))))


(defun larumbe/gtags-update-send-string (str)
  "Utility function to send commands to gtags-update process."
  (process-send-string larumbe/gtags-update-proc-name (concat str "\n")))


(defun larumbe/gtags-update-hook ()
  ""
  (let ((dir (ggtags-current-project-root)))
    (when dir
      (larumbe/gtags-update-send-string (concat "cd " dir))
      (larumbe/gtags-update-send-string "global -u -v")
      (when larumbe/gtags-update-verbose
        (message "Trying gtags-update of dir: %s" dir)))))


(defun larumbe/gtags-update-filter (process output)
  "Filter to report status depending on process output."
  (let* ((buf (process-buffer process))
         (dir (ggtags-current-project-root))
         (msg (concat dir ":" output "\n")))
    (with-current-buffer buf
      (cond
       ((string-match "Global databases are up to date.\n" output)
        (insert msg)
        (when larumbe/gtags-update-verbose
          (message "%s: GTAGS already up to date" dir)))
       ((string-match "Global databases have been modified.\n" output)
        (insert msg)
        (when larumbe/gtags-update-verbose
          (message "%s: GTAGS updated: successfully" dir)))
       (t
        (insert output))))))


(defun larumbe/gtags-update-sentinel (process signal)
  "Sentinel that cleans up buffer and window of spawned shell."
  (let* ((buf (process-buffer process))
         (win (get-buffer-window buf)))
    (when win
      (quit-window t win))))


;;;###autoload
(define-minor-mode gtags-update-async-minor-mode
  "Minor mode to update gtags of a project every time a file is saved.
It spawns a shell in the background that updates tags of current project."
  :global nil
  (if gtags-update-async-minor-mode
      (progn   ;; Enable
        (add-hook 'after-save-hook #'larumbe/gtags-update-hook nil t)
        (larumbe/gtags-update-start-process)
        (when larumbe/gtags-update-verbose
          (message "Enabled gtags-update-async-minor-mode [current buffer]")))
    ;; Disable
    (remove-hook 'after-save-hook #'larumbe/gtags-update-hook t)
    (larumbe/gtags-update-kill-process)
    (when larumbe/gtags-update-verbose
      (message "Disabled gtags-update-async-minor-mode [current buffer]"))))




(provide 'gtags-utils)

;;; gtags-utils.el ends here
