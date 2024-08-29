;;; debug-utils.el --- Debugging Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Some functions created during the debugging of different processes:
;;  - Tree-sitter
;;  - lsp/eglot for vhdl-ext/verilog-ext
;;
;;; Code:


(require 'larumbe-functions)
(require 'lsp)
(require 'eglot)
(require 'verilog-ext)
(require 'vhdl-ext)


;;;###autoload
(defun larumbe/tree-sitter-parse-current-buffer ()
  (interactive)
  (let ((cmd "cd /home/gonz/Repos/foss/tree-sitter-verilog && tree-sitter parse larumbe"))
    (setq cmd (file-name-concat cmd (file-name-nondirectory buffer-file-name)))
    (compile cmd)))

;;;###autoload
(defun larumbe/tree-sitter-parse-clean ()
  (interactive)
  (larumbe/replace-regexp-whole-buffer "\[[0-9]+, [0-9]+\] - \[[0-9]+, [0-9]+\]" "")
  (query-replace-regexp "\\(?1:\\s-+\\)\\(?2:[\)]+\\)" "\\2"))

;;;###autoload
(defun larumbe/lsp-toggle ()
  (interactive)
  (if lsp-mode
      (lsp-disconnect)
    (lsp)))

;;;###autoload
(defun larumbe/eglot-toggle ()
  (interactive)
  (if eglot--managed-mode
      (call-interactively #'eglot-shutdown)
    (call-interactively #'eglot)))

;;;###autoload
(defun larumbe/lsp-bridge-toggle ()
  "Copied from `lsp-bridge-restart-process'."
  (interactive)
  (lsp-bridge-diagnostic-hide-overlays)
  (if lsp-bridge-mode
      (progn
        (lsp-bridge-kill-process)
        (lsp-bridge-mode -1))
    (call-interactively #'lsp-bridge-mode)
    (lsp-bridge-start-process)))

;;;###autoload
(defun larumbe/lsp-verilog-set ()
  (interactive)
  (let ((server-id (intern (completing-read "Server-id: " verilog-ext-lsp-server-ids nil t))))
    (verilog-ext-eglot-set-server server-id)
    (verilog-ext-lsp-set-server server-id)))

;;;###autoload
(defun larumbe/lsp-vhdl-set ()
  (interactive)
  (let ((server-id (intern (completing-read "Server-id: " vhdl-ext-lsp-server-ids nil t))))
    (vhdl-ext-eglot-set-server server-id)
    (vhdl-ext-lsp-set-server server-id)))

;;;###autoload
(defun larumbe/tree-sitter-add-conflicts-regenerate ()
  "Updates list of conflicts.
Needs a *vterm* buffer open."
  (interactive)
  (let (conflicts)
    (pop-to-buffer "*vterm*")
    (vterm-copy-mode)
    (vterm-end-of-line)
    (save-excursion
      (re-search-backward "Add a conflict for these rules: \\(?1:.*\\)$"))
    (setq conflicts (match-string-no-properties 1))
    (setq conflicts (replace-regexp-in-string "`," ","  conflicts))
    (setq conflicts (replace-regexp-in-string "`"  "$." conflicts))
    (setq conflicts (replace-regexp-in-string "\$\.$" ""  conflicts))
    (setq conflicts (concat "[" conflicts "],"))
    (vterm-copy-mode-done 0)
    (save-excursion
      (pop-to-buffer "grammar.js")
      (goto-char (point-max))
      (re-search-backward "],")
      (split-line)
      (insert conflicts)
      (indent-region (line-beginning-position) (line-end-position))
      (save-buffer))
    (process-send-string vterm--process "tree-sitter generate\C-m")))


;;;###autoload
(defun larumbe/tree-sitter-format-buffer-for-conflicts ()
  "Convert conflict in the output of terminal to a formatted version for grammar.js.
Steps:
1 - Copy conflicts from vterm to *scratch*
2 - Run this command in the *scratch* buffer
3 - Paste the resulting text in the grammar.js file."
  (interactive)
  (unless (string= (buffer-name) "*scratch*")
    (user-error "Copy conflicts from vterm to *scratch* buffer first and execute there!"))
  (replace-regexp-in-region " `" " \$\." (point-min) (point-max))
  (replace-regexp-in-region "`," "," (point-min) (point-max))
  (replace-regexp-in-region "`" "" (point-min) (point-max))
  (replace-regexp-in-region "^\s*\\$" "[$" (point-min) (point-max))
  (replace-regexp-in-region "\n" "]\n" (point-min) (point-max)))


(defun larumbe/tree-sitter-comment-conflicts ()
  "Comment conflicts in the *scratch* buffer, previously formatted via
`larumbe/tree-sitter-format-buffer-for-conflicts'."
  (interactive)
  (unless (string= (buffer-name) "*scratch*")
    (user-error "Make sure conflicts are formatted and in *scratch* buffer!"))
  (let ((conflicts (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))
    (pop-to-buffer "grammar.js")
    (dolist (conflict conflicts)
      (goto-char (point-min))
      (if (search-forward conflict nil :noerror)
          (comment-line 1)
        (message "Skipping: %s" confict)))))



(provide 'debug-utils)

;;; debug-utils.el ends here
