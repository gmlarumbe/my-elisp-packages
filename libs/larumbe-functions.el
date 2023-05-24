;;; larumbe-functions.el --- Larumbe's functions  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'xref)


;;;; Window resizing
(defvar larumbe/shrink-window-horizontally-delta   15)
(defvar larumbe/shrink-window-vertically-delta      5)


;;;###autoload
(defun larumbe/enlarge-window-horizontally ()
  "Use `shrink-window' as a wrapper."
  (interactive)
  (shrink-window larumbe/shrink-window-horizontally-delta t))


;;;###autoload
(defun larumbe/shrink-window-horizontally ()
  "Use `shrink-window' as a wrapper."
  (interactive)
  (shrink-window (- larumbe/shrink-window-horizontally-delta) t))


;;;###autoload
(defun larumbe/enlarge-window-vertically ()
  "Use `shrink-window' as a wrapper."
  (interactive)
  (shrink-window larumbe/shrink-window-vertically-delta))


;;;###autoload
(defun larumbe/shrink-window-vertically ()
  "Use `shrink-window' as a wrapper."
  (interactive)
  (shrink-window (- larumbe/shrink-window-vertically-delta)))



;;;; Buffer management
;;;###autoload
(defun larumbe/kill-current-buffer ()
  "Kill current buffer without confirmation."
  (interactive)
  (kill-buffer (buffer-name)))


(defvar larumbe/revert-buffer-confirm-p t
  "Ask for confirmation when reverting current buffer.")

;;;###autoload
(defun larumbe/revert-buffer-maybe-no-confirm (toggle)
  "Revert current buffer without prompting for confirmation.
If universal arg or TOGGLE are provided, toggle for confirmation."
  (interactive "P")
  (if toggle
      (if larumbe/revert-buffer-confirm-p
          (progn
            (setq larumbe/revert-buffer-confirm-p nil)
            (message "Revert confirmation: nil"))
        (setq larumbe/revert-buffer-confirm-p t)
        (message "Revert confirmation: t"))
    ;; If not toggling, revert
    (if larumbe/revert-buffer-confirm-p
        (revert-buffer nil nil t)
      (revert-buffer nil t nil)))) ; When skipping confirmation, reload major-mode


;;;###autoload
(defun larumbe/current-buffer-to-file (out-file)
  "Export current buffer to OUT-FILE.
Seems useful to export long compilation logs."
  (interactive "FEnter output path: ")
  (append-to-file (point-min) (point-max) out-file))


;;;###autoload
(defun larumbe/pwd-to-kill-ring (&optional line-num)
  "Copy current file path to `kill-ring'.
If optional LINE-NUM is given, copy line to `kill-ring'."
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer is not visiting a file!"))
  (let (file-name)
    (if line-num
        (setq file-name (concat (buffer-file-name) ":" (format "%s" (line-number-at-pos))))
      (setq file-name (buffer-file-name)))
    (kill-new file-name)
    (message (buffer-file-name))))



;;;; Navigation
;; Inspiration from: https://stackoverflow.com/questions/3139970/open-a-file-at-line-with-filenameline-syntax
;;;###autoload
(defun larumbe/find-file-dwim ()
  "Wrapper for `find-file-at-point'.
Avoids asking for the file input if point is over a file that exists.  If it
does not exist prompt for regular completion framework input.  Push to the xref
marker stack to be able to navigate back to original file.  Supports shell envs
in the filename and jump to line if file is of the form: file:[0-9]+"
  (interactive)
  (let* ((raw-filename (thing-at-point 'filename :noprops))
         expanded-filename filename bounds line-num)
    (cond (;; Use current completion framework to find a file (tested with ivy/counsel)
           (or (string= major-mode "dired-mode") ; Exclude dired from dwim functions
               (not raw-filename))
           (find-file-at-point))
          (;; Try to guess what's the filename if point is over a non-existing file
           (progn
             (setq expanded-filename (substitute-in-file-name raw-filename))
             (setq filename (larumbe/strip-file-line-number expanded-filename))
             (setq bounds (bounds-of-thing-at-point 'filename))
             (not (file-exists-p filename)))
           (find-file-at-point))
          (;; Find existing file and jump to corresponding line number if applicable
           bounds
           (save-excursion
             (goto-char (car bounds))
             (search-forward-regexp "[^ ]:" (cdr bounds) t)
             (if (looking-at "[0-9]+")
                 (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
           (xref-push-marker-stack)
           (find-file-at-point filename)
           (unless (equal line-num nil)
             (forward-line line-num)))
          (t
           (find-file-at-point)
           (message "Unexpected case in `larumbe/find-file-dwim'!")))))



;;;###autoload
(defun larumbe/pop-to-previous-mark ()
  "Pop to previous mark."
  (interactive)
  (set-mark-command 4))


;;;; Editing
;;;###autoload
(defun larumbe/copy-region-or-symbol-at-point ()
  "Copy symbol under cursor.  If region is active, copy it instead.
If there is no symbol at point, just skip functionality."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (if (use-region-p)
        (progn
          (call-interactively #'kill-ring-save)
          (deactivate-mark))
      ;; If there is no region case, must be a symbol in order to do something relevant
      (when symbol
        (kill-new symbol)
        (message symbol)))))


;;;###autoload
(defun larumbe/insert-time-stamp (&optional regex)
  "Insert time-stamp at header comments.
Try to add it in line before matching REGEX.
If REGEX is nil or not found, add it at the beginning."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when regex
      (re-search-forward regex nil t))
    (beginning-of-line)
    (open-line 1)
    (insert ";;\n;; Time-stamp: <>\n;; ")))


;;;###autoload
(defun larumbe/comment-tag-line-or-region (text-begin text-end)
  "Surround current line or region with commented TEXT-BEGIN and TEXT-END.
Makes use of `comment-dwim' because the approach of using the variables
`comment-start' and `comment-end' was more complicated than it seemed.
For example, `verilog-mode' or `python-mode' have no `comment-end' defined.
This approach works well for line-style as well as for block-style comments
(e.g. block-style /* Comment */ in C-mode). However, line-style are preferred."
  (interactive)
  (let (beg end)
    (if (use-region-p)
        (progn
          (setq beg (region-beginning))
          (setq end (region-end))
          (deactivate-mark)
          (goto-char end)
          (beginning-of-line)
          (open-line 1)
          (call-interactively #'comment-dwim)
          (insert text-end)
          (goto-char beg)
          (beginning-of-line)
          (open-line 1)
          (call-interactively #'comment-dwim)
          (insert text-begin)
          (forward-line 1))
      ;; If not using the region wrap around current line
      (beginning-of-line)
      (open-line 1)
      (call-interactively #'comment-dwim)
      (insert text-begin)
      (beginning-of-line)
      (forward-line 2)
      (open-line 1)
      (call-interactively #'comment-dwim)
      (insert text-end)
      (forward-line -1))))


;;;; Lists/regexp/strings/files/directories
;;;###autoload
(defun larumbe/print-elements-of-list-of-strings (list-of-strings)
  "Print each element of LIST-OF-STRINGS on a line of its own."
  (let (return-string)
    (while list-of-strings
      (setq return-string (concat return-string (message "%s\n" (car list-of-strings))))
      (setq list-of-strings (cdr list-of-strings)))
    (message "%s" return-string)))


;;;###autoload
(defun larumbe/insert-elements-of-list-of-strings (list-of-strings)
  "Insert each element of LIST-OF-STRINGS on a line of its own at current point."
  (dolist (elm list-of-strings)
    (insert (concat elm "\n"))))


;;;###autoload
(defun larumbe/replace-regexp (regexp to-string start end)
  "Wrapper function for programatic use of `replace-regexp'.
Replace REGEXP with TO-STRING from START to END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward regexp end t)
      (replace-match to-string))))


;;;###autoload
(defun larumbe/replace-regexp-whole-buffer (regexp to-string)
  "Replace REGEXP with TO-STRING on whole current-buffer."
  (larumbe/replace-regexp regexp to-string (point-min) nil))


;;;###autoload
(defun larumbe/replace-string (string to-string start end &optional fixedcase)
  "Wrapper function for programatic use of `replace-string'.
Replace STRING with TO-STRING from START to END.

If optional arg FIXEDCASE is non-nil, do not alter the case of
the replacement text (see `replace-match' for more info)."
  (save-excursion
    (goto-char start)
    (while (search-forward string end t)
      (replace-match to-string fixedcase))))


;;;###autoload
(defun larumbe/replace-string-whole-buffer (string to-string &optional fixedcase)
  "Replace STRING with TO-STRING on whole current-buffer.

If optional arg FIXEDCASE is non-nil, do not alter the case of
the replacement text (see `replace-match' for more info)."
  (larumbe/replace-string string to-string (point-min) nil fixedcase))


;;;###autoload
(defun larumbe/find-extensions-major-mode (mode)
  "Return a list of strings with extensions currently associated with MAJOR-MODE.
Make use of `auto-mode-alist' registered extensions."
  (let ((alist (copy-alist auto-mode-alist))
        (alist-elm)
        (ext)
        (ext-list))
    (while (setq alist-elm (rassoc mode alist))
      (delete alist-elm alist)
      (setq ext (car alist-elm))
      (push ext ext-list))
    ext-list))


;;;###autoload
(defun larumbe/strip-file-line-number (file)
  "Strip line number from FILE.
Assummes line number is of the form: filepath:[0-9]+"
  (replace-regexp-in-string "\\(?1:.*\\):[0-9]+" "\\1" file))

;;;###autoload
(defun larumbe/directories-in-dir (dir)
  "Find directories in DIR non-recursively"
  (seq-filter #'file-directory-p
              (remove ".." (remove "." (directory-files dir)))))


;;;; Misc
;;;###autoload
(defun larumbe/toggle-keyboard-layout ()
  "Toggle keyboard language between US and ES."
  (interactive)
  (let (cur-layout)
    (setq cur-layout (shell-command-to-string "setxkbmap -query | grep layout | awk '{print $2}'"))
    (setq cur-layout (replace-regexp-in-string "\n$" "" cur-layout))
    (if (string-equal cur-layout "us")
        (progn
          (shell-command "setxkbmap es")
          (message "Switched to ES"))
      (shell-command "setxkbmap us")
      (message "Switched to US"))))


;; Default font: Monospace Regular 11 (`F10/Options/SetDefaultFont')
(defvar larumbe/current-font-size 11)

;;;###autoload
(defun larumbe/set-font-size (size)
  "Set current font size among a list of possible values."
  (interactive
   (list (completing-read "Font size: " '("11" "12" "14"))))
  (let ((font (concat "DejaVu Sans Mono-" size)))
    (set-frame-font font t t)
    (setq larumbe/current-font-size size)))



;;;###autoload
(defun larumbe/scratch-toggle ()
  "Toggle showing scratch buffer at current buffer."
  (interactive)
  (if (string= (buffer-name) "*scratch*")
      (previous-buffer)
    (switch-to-buffer "*scratch*")))



;;;###autoload
(defun larumbe/newline-advice (&optional ARG INTERACTIVE)
  "Advice to set :before-until for newline functions of major-modes that
kill *ag* or *xref* buffers."
  (let* ((buf-list '("*xref*" "*ag search*" "*ripgrep-search*" "*Help*"))
         ;; INFO: At some point tried to add the "*Compile-Log*" buffer, but very
         ;; rare bugs appeared when byte/native compiling, removing code from
         ;; current buffer...
         buf-win)
    ;; Look for buffers sequentialy and break loop when one is found
    (catch 'found
      (dolist (buf buf-list)
        (setq buf-win (get-buffer-window buf))
        ;; Kill corresponding window and buffer
        (when buf-win
          (quit-window t buf-win)
          (throw 'found buf-win))))))


;;;; More complex/less frequently used
;;;###autoload
(defun larumbe/buffer-expand-filenames (&optional absolute exp-dir)
  "Expands filenames paths present in `current-buffer' line by line.
If ABSOLUTE is nil expand relative to `default-directory'.
If ABSOLUTE is non-nil filenames will expand to their absolute paths.
If EXP-DIR is non-nil, expand relative to this argument instead of `default-directory'."
  (let ((cur-line)
        (default-directory (if exp-dir
                               exp-dir
                             default-directory)))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (delete-horizontal-space)
        (if absolute
            (setq cur-line (expand-file-name (thing-at-point 'line) default-directory))
          (setq cur-line (file-relative-name (thing-at-point 'line) default-directory)))
        (kill-line 1)
        (insert cur-line)))))


;;;###autoload
(defun larumbe/sort-regexp-at-the-beginning-of-file (regexp)
  "Move lines containing REGEXP recursively at the beginning of the file.
Done line by line, this might be useful when managing a list of files,
one file at a line, and there is some need of sorting by regexp.
For example, in SystemVerilog, packages might need to be included before other files."
  (interactive)
  (let ((sorted-files-p nil))
    (goto-char (point-min))
    (while (not sorted-files-p)
      (save-excursion
        (unless (search-forward-regexp regexp nil 1)
          (setq sorted-files-p t))
        (beginning-of-line)
        (kill-line 1)) ; Kill trailing newline as well
      (yank))))



;;;###autoload
(defun larumbe/directory-files-recursively-to-file (base-dir filename re &optional append exclude-re)
  "Retrieve all files matching regexp RE of a specified BASE-DIR to output FILENAME.

FILENAME is just the name, not a path, and will be stored in BASE-DIR.

If optional APPEND is set to non-nil, append result to existing FILE.
Otherwise, overwrite old existing FILENAME with new results.

If optional EXCLUDE-RE is set, delete paths with that regexp from generated file."
  (let ((default-directory base-dir)
        buf)
    (save-window-excursion
      (with-temp-buffer
        (mapc
         (lambda (dir) (insert (mapconcat #'identity (directory-files-recursively dir re nil nil t) "\n")))
         (list base-dir))
        ;; Append to existing filename
        (when (and (file-exists-p (file-name-concat base-dir filename))
                   append)
          (setq buf (current-buffer))
          (find-file filename)
          (goto-char (point-max))
          (newline)
          (insert-buffer-substring buf))
        ;; Filter according to optional parameter
        (when exclude-re
          (flush-lines exclude-re (point-min) (point-max)))
        (write-file (file-name-concat base-dir filename))))))


;; https://stackoverflow.com/questions/3775377/how-do-you-diff-a-directory-for-only-files-of-a-specific-type
;;;###autoload
(defun larumbe/directory-diff-recursive (dir1 dir2 out-file)
  "Export diff between DIR1 and DIR2 to OUT-FILE.
It uses an exclude schema that leaves out of the diff
the files/expresions in exclude.list This is because there
is no include option for `diff' utils."
  (interactive "DSelect first directory: \nDSelect second directory: \nFSelect output file:")
  (let ((exclude-file)
        (exclude-patterns '("*.wdf"
                            "*.xml"
                            "*.bxml"
                            "*.wpc"
                            "*.target"
                            "*.rdl.ast"
                            "file_list.py"
                            "source_list.tcl"
                            "run_vivado.tcl")))
    (setq exclude-file (concat (file-name-directory out-file) "exclude.pats"))
    (with-temp-file exclude-file
      (insert (larumbe/print-elements-of-list-of-strings exclude-patterns)))
    ;; If return value is `1' is because differences were found
    (start-process-shell-command
     "*diff-dirs*" nil
     (concat "diff -X " exclude-file " -r " dir1 " " dir2 " > " out-file))))


;;;; Autoloads
;; https://emacs.stackexchange.com/questions/33627/how-to-generate-and-activate-autoloads-for-local-packages
;;
;; Could also be based upon `update-directory-autoloads' (non-recursive).
;; It seems it works at startup, not possible once functions have already been defined.
;;
;; INFO: Currently, autoloads are managed by `straight' by placing packages inside Git repos.
;; If for some reason, any package function needs to be autoloaded by using this function,
;; it would be necessary to first load this package in the init file.
;;
;; At some point, the package `autoload' seemed deprecated in favor of loaddefs

(defvar larumbe/autoloads-local-dir (file-name-concat user-emacs-directory "local-autoloads")
  "Emacs directory for local packages generated autoloads.")

;;;###autoload
(defun larumbe/autoloads-dir (dir)
  "Generate autoloads for files in DIR in `larumbe/autoloads-local-dir' directory and load them.

This could be used to generate autoloads from the magic comments
instead of :command keyword inside a use-package macro, e.g:

(use-package my-pkg
  :init
  (larumbe/autoloads-dir \"~/.elisp/pkg/some-pkg.el\"))

This would be a manual approach without adding each new function to the :commands keyword,
nor using a package manager (i.e. straight) that handles autoloads automatically."
  (let* ((autoloads-dir larumbe/autoloads-local-dir)
         (dirname (file-name-nondirectory (directory-file-name dir)))
         (generated-autoloads-file (file-name-concat autoloads-dir (concat dirname "-autoloads.el"))))
    (unless (file-exists-p autoloads-dir)
      (make-directory autoloads-dir))
    (loaddefs-generate dir generated-autoloads-file)
    (load-file generated-autoloads-file)))




(provide 'larumbe-functions)


;;; larumbe-functions.el ends here
