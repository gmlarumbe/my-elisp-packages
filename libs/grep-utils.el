;;; grep-utils.el --- Grep Utils: grep/ag/ripgrep  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ag)
(require 'ripgrep)
(require 'projectile)

;;;; Ag
(defun larumbe/ag-search-file-list (regex file directory)
  "Search REGEX limited to the files included in FILE in DIRECTORY.
INFO: Might block Emacs for large filelists during search as it is not
asynchronous.

FILE is expected to be something of the format of gtags.files or similar,
i.e. one absolute file path per line"
  (let ((files))
    (with-temp-buffer
      (insert-file-contents file)
      (setq files (split-string (buffer-string) "\n")))
    (ag/search regex directory :files files)))

;;;###autoload
(defun larumbe/ag-search-project-gtags ()
  "Search `symbol-at-point' based on current projectile project.
List of files provided by project's \='gtags.files\=' will filter the search."
  (interactive)
  (let* ((proj-dir   (projectile-project-root))
         (gtags-file (concat proj-dir "gtags.files")))
    (unless (file-exists-p gtags-file)
      (error "Error: gtags.files not found for current project"))
    (larumbe/ag-search-file-list (thing-at-point 'symbol) gtags-file proj-dir)))

;; Thanks to Kaushal Modi
(defun ag/jump-to-result-if-only-one-match ()
  "Jump to the first ag result if that ag search came up with just one match."
  (let (only-one-match)
    (when (member "--stats" ag-arguments)
      (save-excursion
        (goto-char (point-min))
        (setq only-one-match (re-search-forward "^1 matches\\s-*$" nil :noerror)))
      (when only-one-match
        (next-error)
        (kill-buffer (current-buffer))
        (message (concat "ag: Jumping to the only found match and "
                         "killing the *ag* buffer."))))))


;;;; Ripgrep
(defvar larumbe/ripgrep-types
  '((verilog-mode    . "verilog")
    (python-mode     . "py")
    (emacs-lisp-mode . "elisp")
    (c-mode          . "c")
    (c++-mode        . "cpp")
    (vhdl-mode       . "vhdl"))
  "Variable to determine the -t argument of rg depending on major-mode.")

(defun larumbe/ripgrep-get-lang-type-args (lang)
  "Return formatted ripgrep type arguments for major-mode LANG."
  (let ((key (assoc lang larumbe/ripgrep-types))
        type)
    (when key
      (setq type (cdr key))
      (list "-t" type))))

;;;###autoload
(defun larumbe/ripgrep-regexp-symbol-at-point ()
  "Perform ripgrep of current symbol at point in a compilation buffer.

Use current projectile directory or default-dir if not in a project.
Try to find matches for files associated with current `major-mode'.

Could be useful to find references on specific projects.

Return the type of file used to perform ripgrep."
  (interactive)
  (let ((symbol (symbol-name (symbol-at-point)))
        (rg-dir (or (project-root (project-current))
                    default-directory))
        (type-args (larumbe/ripgrep-get-lang-type-args major-mode)))
    (ripgrep-regexp symbol rg-dir type-args)
    ;; Return value
    (or (nth 1 type-args)
        "all")))

;; Variables to show in the modeline which reference is being searched for, at the
;; beginning of the ripgrep, and after search has finished.
(defvar larumbe/ripgrep-current-reference nil)
(defvar larumbe/ripgrep-current-type nil)

(defun larumbe/ripgrep-search-finish-update-vars (ref type)
  "Update REF / TYPE searched variables and use them to debug in the modeline."
  (setq larumbe/ripgrep-current-reference ref)
  (setq larumbe/ripgrep-current-type type))

(defun larumbe/ripgrep-search-finished-hook ()
  "Hook to run on ripgrep buffers once search has finished."
  (message "[ripgrep-%s] References of: %s" larumbe/ripgrep-current-type larumbe/ripgrep-current-reference))

;;;###autoload
(defun larumbe/ripgrep-xref (ref)
  "Use `ripgrep' to find xrefs."
  (let (rg-type)
    (setq rg-type (larumbe/ripgrep-regexp-symbol-at-point))
    (larumbe/ripgrep-search-finish-update-vars ref rg-type)
    (with-current-buffer "*ripgrep-search*"
      (setq-local ripgrep-search-finished-hook #'larumbe/ripgrep-search-finished-hook))
    ;; Execute hook at the beginning of ripgrep and after compilation has finished to keep seeing message on the mode-line
    (larumbe/ripgrep-search-finished-hook)))



(provide 'grep-utils)

;;; grep-utils.el ends here
