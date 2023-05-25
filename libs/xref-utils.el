;;; xref-utils.el --- Xref Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; External
(require 'elpy)
(require 'lsp-mode)
(require 'org)
;; Internal
(require 'larumbe-functions)
(require 'grep-utils)
;; Verilog/VHDL Extensions
(require 'vhdl-ext-nav)
(require 'verilog-ext-nav)


(defun larumbe/xref-report-backend (tag backend &optional ref-p)
  "Show in the minibuffer what is the current used BACKEND for TAG.

BACKEND is mandatory to make sure that responsibility of backend reporting comes
from the caller.  E.g: If it was run after finding definitions it would report
the active backend of the destination file!

Optional display references if REF-P is non-nil."
  (let (formatted-backend formatted-tag)
    ;; Add some coloring
    (setq formatted-backend (propertize (symbol-name backend) 'face '(:foreground "goldenrod" :weight bold)))
    (setq formatted-tag (propertize tag 'face '(:foreground "green")))
    ;; Report backend and tag
    (if ref-p
        (message "[%s] Refs of: %s" formatted-backend formatted-tag)
      (message "[%s] Defs of: %s" formatted-backend formatted-tag))))

(defun larumbe/xref--find-def (def)
  "Default action to find DEF in a particular mode."
  (let ((tag-xref-backend (xref-find-backend))
        skip)
    ;; Handle `lsp-mode' differently
    (if (eq tag-xref-backend 'xref-lsp)
        (let (return-code) ; Propagate error to backend report
          (setq return-code (lsp-find-definition)) ; On failure, return value will be a propertized string (nil on succes)
          (if (and (stringp return-code)
                   (string-match "LSP :: Not found for: " return-code))
              (message "%s" return-code)
            (larumbe/xref-report-backend def tag-xref-backend)))
      ;; Else, try to find definitions unless the found backend is `dumb-jump'.
      ;; This only supports definitions and does some basic processing of them
      ;; Since sometimes these could not be the desired ones, ask for confirmation
      (when (and (equal tag-xref-backend 'dumb-jump)
                 (not (y-or-n-p "No definitions found, try dumb-jump? ")))
        (setq skip t))
      (unless skip
        (xref-find-definitions def)
        (larumbe/xref-report-backend def tag-xref-backend)))))

(defun larumbe/xref--find-ref (ref)
  "Default action to find REF in a particular mode."
  (let ((tag-xref-backend (xref-find-backend)))
    (cond (;; Handle `lsp-mode' differently
           (eq tag-xref-backend 'xref-lsp)
           (let (return-code) ; Propagate error to backend report
             (setq return-code (lsp-find-references)) ; On failure, return value will be a propertized string (otherwise a buffer)
             (if (and (stringp return-code)
                      (string-match "LSP :: Not found for: " return-code))
                 (message "%s" return-code)
               (larumbe/xref-report-backend ref 'lsp :ref))))
          ;; Else, try to find definitions unless the found backend is `dumb-jump'.
          ((equal tag-xref-backend 'dumb-jump)
           ;; `dumb-jump' does not support reference lookup only supports
           ;; definitions (doesn't provide implementation for
           ;; xref-find-references).  Since references would be searched through
           ;; grep and processed by default `semantic-symref' a customized
           ;; ripgrep command is preferred.
           (when (y-or-n-p "[Skipping dumb-jump] No refs found, try ripgrep? ")
             (larumbe/ripgrep-xref ref)))
          ;; Default
          (t
           ;; Find references with corresponding backend
           (xref-find-references ref)
           (larumbe/xref-report-backend ref tag-xref-backend :ref)))))

;;;###autoload
(defun larumbe/xref-find-definitions (&optional force-backend)
  "Find definition of symbol at point.
If pointing a URL/file, visit that URL/file instead.

Selects between specific xref backends to find definitions.

Assumes that prog-modes will have `dumb-jump' as a fallback backend before
etags.  In case definitions are not found and dumb-jump is detected ask for use
it as a backend.

With optional prefix, prompt for a specific backend to be used."
  (interactive "P")
  (let ((file (thing-at-point 'filename :noprop))
        (url  (thing-at-point 'url      :noprop))
        (def  (thing-at-point 'symbol   :noprop))
        (tag-xref-backend (xref-find-backend)) ; Default found backend
        forced-backend)
    (cond (;; Force select-backend
           force-backend
           (setq forced-backend (intern (completing-read "Backend: " (remove t xref-backend-functions))))
           (let ((xref-backend-functions `(,forced-backend t)))
             (if (not def)
                 (call-interactively #'xref-find-definitions)
               (setq tag-xref-backend (xref-find-backend))
               (if (not tag-xref-backend)
                   (user-error "[xref-utils]: Could not find definitions for `%s' with [%s]" def (symbol-name forced-backend))
                 (xref-find-definitions def)
                 (larumbe/xref-report-backend def tag-xref-backend)))))
          ;; URL
          (url
           (browse-url url))
          ;; File
          ((and file (file-exists-p (larumbe/strip-file-line-number (substitute-in-file-name file))))
           (larumbe/find-file-dwim))
          ;;   - Org: `org-open-at-point'
          ((string= major-mode "org-mode")
           (call-interactively #'org-open-at-point))
          ;; If not pointing to a file choose between different navigation functions
          ;;   - Verilog: try to jump to module at point if not over a tag
          ((or (string= major-mode "verilog-mode")
               (string= major-mode "verilog-ts-mode"))
           (if def
               (larumbe/xref--find-def def)
             ;; Context based jump if no thing-at-point:
             (cond (;; Inside a module instance
                    (and (or (verilog-ext-point-inside-block 'module)
                             (verilog-ext-point-inside-block 'interface))
                         (verilog-ext-instance-at-point))
                    (setq def (match-string-no-properties 1))
                    (when (or (eq tag-xref-backend 'xref-lsp) ; eglot and lsp work with ...
                              (eq tag-xref-backend 'eglot))   ; ... symbol at point
                      (verilog-ext-find-module-instance-bwd-2)
                      (unless (string= (thing-at-point 'symbol) def)
                        (error "[xref-utils]: Error while looking for def with LSP!")))
                    (larumbe/xref--find-def def))
                   ;; Default fallback
                   (t
                    (call-interactively #'xref-find-definitions)))))
          ((or (string= major-mode "vhdl-mode")
               (string= major-mode "vhdl-ts-mode"))
           (if def
               (larumbe/xref--find-def def)
             ;; Context based jump if no thing-at-point:
             (cond (;; Inside an entity instance
                    (setq def (car (vhdl-ext-instance-at-point)))
                    (when (or (eq tag-xref-backend 'xref-lsp) ; eglot and lsp work with ...
                              (eq tag-xref-backend 'eglot))   ; ... symbol at point
                      (vhdl-ext-find-entity-instance-bwd)
                      (goto-char (match-beginning 6))
                      (unless (string= (thing-at-point 'symbol) def)
                        (error "[xref-utils]: Error while looking for def with LSP!")))
                    (larumbe/xref--find-def def))
                   ;; Default fallback
                   (t
                    (call-interactively #'xref-find-definitions)))))
          ;; - Python: elpy
          ((string-match "python-" (symbol-name major-mode))
           (if def
               (progn
                 (setq tag-xref-backend (xref-find-backend)) ; Should be elpy if enabled
                 (xref-find-definitions (elpy-xref--identifier-at-point))
                 (larumbe/xref-report-backend def tag-xref-backend))
             (call-interactively #'xref-find-definitions)))
          ;; Default to use xref
          (t
           (if def
               (larumbe/xref--find-def def)
             ;; Ask for input if there is no def at point
             (call-interactively #'xref-find-definitions))))))

;;;###autoload
(defun larumbe/xref-find-references (&optional force-backend)
  "Find references of symbol at point using xref.

Assumes that prog-modes will have `dumb-jump' as a fallback backend before etags.
In case references are not found, and dumb-jump is detected as a backend, perform a ripgrep instead.

This ripgrep will be executed at `projectile-project-root' or `default-directory'
and will be applied to only files of current `major-mode' if existing in `larumbe/ripgrep-types'.

With optional prefix, prompt for a specific backend to be used."
  (interactive "P")
  (let ((ref (thing-at-point 'symbol))
        (tag-xref-backend (xref-find-backend)) ; Default found backend
        forced-backend)
    (cond (;; Force select-backend
           force-backend
           (setq forced-backend (intern (completing-read "Backend: " (remove t xref-backend-functions))))
           (let ((xref-backend-functions `(,forced-backend t)))
             (if (not ref)
                 (call-interactively #'xref-find-references)
               (setq tag-xref-backend (xref-find-backend))
               (if (not tag-xref-backend)
                   (user-error "[xref-utils]: Could not find references for `%s' with [%s]" ref (symbol-name forced-backend))
                 (xref-find-references ref)
                 (larumbe/xref-report-backend ref tag-xref-backend :ref)))))
          ;; Verilog
          ((or (string= major-mode "verilog-mode")
               (string= major-mode "verilog-ts-mode"))
           (if ref
               (larumbe/xref--find-ref ref)
             ;; Context based jump if no thing-at-point:
             (cond (;; Inside a module instance
                    (and (verilog-ext-point-inside-block 'module)
                         (verilog-ext-instance-at-point))
                    (setq ref (match-string-no-properties 1))
                    (when (or (eq tag-xref-backend 'xref-lsp) ; eglot and lsp work with ...
                              (eq tag-xref-backend 'eglot))   ; ... symbol at point
                      (verilog-ext-find-module-instance-bwd-2)
                      (unless (string= (thing-at-point 'symbol) ref)
                        (error "[xref-utils]: Error while looking for def with LSP!")))
                    (larumbe/xref--find-ref ref))
                   ;; Default fallback
                   (t
                    (call-interactively #'xref-find-references)))))
          ;; VHDL
          ((or (string= major-mode "vhdl-mode")
               (string= major-mode "vhdl-ts-mode"))
           (if ref
               (larumbe/xref--find-ref ref)
             ;; Context based jump if no thing-at-point:
             (cond (;; Inside an entity instance
                    (setq ref (car (vhdl-ext-instance-at-point)))
                    (when (or (eq tag-xref-backend 'xref-lsp) ; eglot and lsp work with ...
                              (eq tag-xref-backend 'eglot))   ; ... symbol at point
                      (vhdl-ext-find-entity-instance-bwd)
                      (goto-char (match-beginning 6))
                      (unless (string= (thing-at-point 'symbol) ref)
                        (error "[xref-utils]: Error while looking for def with LSP!")))
                    (larumbe/xref--find-ref ref))
                   ;; Default fallback
                   (t
                    (call-interactively #'xref-find-references)))))
          (;; Python
           (string-match "python-" (symbol-name major-mode))
           (if ref
               (progn
                 (setq tag-xref-backend (xref-find-backend))
                 (xref-find-references (elpy-xref--identifier-at-point))
                 (larumbe/xref-report-backend ref tag-xref-backend :ref)) ; Should be elpy if enabled
             (call-interactively #'xref-find-references)))
          ;; Default
          (t (if ref
                 (larumbe/xref--find-ref ref)
               ;; Ask for input if there is no ref at point
               (call-interactively #'xref-find-references))))))


(provide 'xref-utils)

;;; xref-utils.el ends here
