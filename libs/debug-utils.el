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
  (let ((cmd "cd /path/to/tree-sitter-verilog && tree-sitter parse larumbe/"))
    (setq cmd (concat cmd (file-name-nondirectory buffer-file-name)))
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


(provide 'debug-utils)

;;; debug-utils.el ends here
