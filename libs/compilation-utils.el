;;; compilation-utils.el --- Compilation Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; The variable `larumbe/compilation-custom-regexp-sets' holds parser names
;; and their corresponding regexp-alist-alists.
;;
;; In order to extend it, just create the proper `larumbe/compilation-error-re-<program>'
;; and add its parser at `larumbe/compilation-custom-regexp-sets'.
;;
;; The function `larumbe/compilation-error-re-set' does all the logic.
;;
;; Plus, there are some functions to do interactive compilation with regexp
;; parsing.
;;
;;; Code:


(require 'compile)


;;;; Additional faces
(defvar larumbe/compilation-gray-face 'larumbe/compilation-gray-face)
(defface larumbe/compilation-gray-face
  '((t (:foreground "gray55")))
  "Gray extra face for compilation regexp parsing"
  :group 'compilation-extra-faces)

(defvar larumbe/compilation-binary-face 'larumbe/compilation-binary-face)
(defface larumbe/compilation-binary-face
  '((t (:foreground "goldenrod")))
  "Binary extra face for instances compilation regexp parsing"
  :group 'compilation-extra-faces)



;;;; Variables
(defvar larumbe/compilation-error-re-vivado
  '((vivado-error     "^\\(?1:^ERROR:\\) \\(?2:\\[[ a-zA-Z0-9\./_-]+\\]\\)\\(?3:.*\\[\\(?4:.*\\):\\(?5:[0-9]+\\)\\]\\)"            4 5   nil 2 nil (1 compilation-error-face)   (2 larumbe/compilation-gray-face))
    (vivado-error2    "^\\(?1:^ERROR:\\) \\(?2:\\[[ a-zA-Z0-9\./_-]+\\]\\)"                                                        1 nil nil 2 nil (1 compilation-error-face)   (2 larumbe/compilation-gray-face))
    (vivado-critical  "^\\(?1:^CRITICAL WARNING:\\) \\(?2:\\[[ a-zA-Z0-9\./_-]+\\]\\)\\(?3:.*\\[\\(?4:.*\\):\\(?5:[0-9]+\\)\\]\\)" 4 5   nil 1 nil (1 compilation-error-face)   (2 larumbe/compilation-gray-face))
    (vivado-critical2 "^\\(?1:^CRITICAL WARNING:\\) \\(?2:\\[[ a-zA-Z0-9\./_-]+\\]\\)"                                             1 nil nil 1 nil (1 compilation-error-face)   (2 larumbe/compilation-gray-face))
    (vivado-warning   "^\\(?1:^WARNING:\\) \\(?2:\\[[ a-zA-Z0-9\./_-]+\\]\\)\\(?3:.*\\[\\(?4:.*\\):\\(?5:[0-9]+\\)\\]\\)"          4 5   nil 1 nil (1 compilation-warning-face) (2 larumbe/compilation-gray-face))
    (vivado-warning2  "^\\(?1:^WARNING:\\) \\(?2:\\[[ a-zA-Z0-9\./_-]+\\]\\)"                                                      1 nil nil 1 nil (1 compilation-warning-face) (2 larumbe/compilation-gray-face))
    (vivado-info      "^\\(?1:^INFO:\\) \\(?2:\\[[ a-zA-Z0-9\./_-]+\\]\\)\\(?3:.*\\[\\(?4:.*\\):\\(?5:[0-9]+\\)\\]\\)"             4 5   nil 0 nil (1 compilation-info-face)    (2 larumbe/compilation-gray-face))
    (vivado-info2     "^\\(?1:^INFO:\\) \\(?2:\\[[ a-zA-Z0-9\./_-]+\\]\\)"                                                         1 nil nil 0 nil (1 compilation-info-face)    (2 larumbe/compilation-gray-face))))

;; Leveraged from verilog-mode (verilog-IES) and extended for UVM/OVM
(defvar larumbe/compilation-error-re-xrun
  '((xrun-fatal    "\\(?1:^[a-z]+\\): \\(?2:\\*F\\),[0-9A-Z]+\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?3:[^ \t,]+\\),\\(?4:[0-9]+\\)|\\(?5:[0-9]+\\)\\)" 3 4 5 2 nil (1 larumbe/compilation-binary-face) (2 compilation-error-face))
    (xrun-fatal2   "\\(?1:^[a-z]+\\): \\(?2:\\*F\\),[0-9A-Z]+:" 1 nil nil 2 nil (1 larumbe/compilation-binary-face) (2 compilation-error-face))
    (xrun-error    "\\(?1:^[a-z]+\\): \\(?2:\\*E\\),[0-9A-Z]+\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?3:[^ \t,]+\\),\\(?4:[0-9]+\\)|\\(?5:[0-9]+\\)\\)" 3 4 5 2 nil (1 larumbe/compilation-binary-face) (2 compilation-error-face))
    (xrun-error2   "\\(?1:^[a-z]+\\): \\(?2:\\*E\\),[0-9A-Z]+:" 1 nil nil 2 nil (1 larumbe/compilation-binary-face) (2 compilation-error-face))
    (xrun-warning  "\\(?1:^[a-z]+\\): \\(?2:\\*W\\),[0-9A-Z]+\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?3:[^ \t,]+\\),\\(?4:[0-9]+\\)|\\(?5:[0-9]+\\)\\)" 3 4 5 1 nil (1 larumbe/compilation-binary-face) (2 compilation-warning-face))
    (xrun-warning2 "\\(?1:^[a-z]+\\): \\(?2:\\*W\\),[0-9A-Z]+:" 1 nil nil 1 nil (1 larumbe/compilation-binary-face) (2 compilation-warning-face))
    (xrun-note     "\\(?1:^[a-z]+\\): \\(?2:\\*N\\),[0-9A-Z]+\\(?:\\(?:\\[[0-9A-Z_,]+\\]\\)? (\\(?3:[^ \t,]+\\),\\(?4:[0-9]+\\)|\\(?5:[0-9]+\\)\\)" 3 4 5 0 nil (1 larumbe/compilation-binary-face) (2 compilation-info-face))
    (xrun-note2    "\\(?1:^[a-z]+\\): \\(?2:\\*N\\),[0-9A-Z]+:" 1 nil nil 0 nil (1 larumbe/compilation-binary-face) (2 compilation-info-face))
    ;; UVM
    (uvm-fatal    "^\\(?1:UVM_FATAL\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))"   2 3 nil 2 nil (1 compilation-error-face))
    (uvm-fatal2   "^\\(?1:UVM_FATAL\\) @"   1 nil nil 2 nil)
    (uvm-error    "^\\(?1:UVM_ERROR\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))"   2 3 nil 2 nil (1 compilation-error-face))
    (uvm-error2   "^\\(?1:UVM_ERROR\\) @"   1 nil nil 2 nil)
    (uvm-warning  "^\\(?1:UVM_WARNING\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))" 2 3 nil 1 nil (1 compilation-warning-face))
    (uvm-warning2 "^\\(?1:UVM_WARNING\\) @" 1 nil nil 1 nil)
    (uvm-info     "^\\(?1:UVM_INFO\\) \\(?2:[a-zA-Z0-9\./_-]+\\)(\\(?3:[0-9]+\\))"    2 3 nil 0 nil (1 compilation-info-face))
    (uvm-info2    "^\\(?1:UVM_INFO\\) @"    1 nil nil 0 nil)
    ;; OVM
    (ovm-fatal    "^\\(?1:OVM_FATAL\\) @ \\(?2:[0-9]+\\): "   1 nil nil 2 nil (2 compilation-line-face))
    (ovm-error    "^\\(?1:OVM_ERROR\\) @ \\(?2:[0-9]+\\): "   1 nil nil 2 nil (2 compilation-line-face))
    (ovm-warning  "^\\(?1:OVM_WARNING\\) @ \\(?2:[0-9]+\\): " 1 nil nil 1 nil (2 compilation-line-face))
    (ovm-info     "^\\(?1:OVM_INFO\\) @ \\(?2:[0-9]+\\): "    1 nil nil 0 nil (2 compilation-line-face))))

;; Fetched from verilog-mode variable: `verilog-error-regexp-emacs-alist'.
(defvar larumbe/compilation-error-re-verilator
  '((verilator-error   "%?\\(Error\\)\\(-[^:]+\\|\\):[\n ]*\\([^ \t:]+\\):\\([0-9]+\\):"    3 4 nil 2 nil (1 compilation-error-face)   (2 compilation-line-face))
    (verilator-warning "%?\\(Warning\\)\\(-[^:]+\\|\\):[\n ]*\\([^ \t:]+\\):\\([0-9]+\\):"  3 4 nil 1 nil (1 compilation-warning-face) (2 compilation-line-face))))

(defvar larumbe/compilation-error-re-iverilog
  '((iverilog-unsupported  "\\(?1:.*\\):\\(?2:[0-9]+\\):.*sorry:"            1 2 nil   0 nil (1 compilation-info-face) (2 compilation-line-face))
    (iverilog-warning      "\\(?1:.*\\):\\(?2:[0-9]+\\):.*warning:"          1 2 nil   1 nil (1 compilation-warning-face) (2 compilation-line-face))
    (iverilog-warning2     "^\\(?1:warning\\):"                              1 nil nil 1 nil)
    (iverilog-error        "\\(?1:.*\\):\\(?2:[0-9]+\\):.*error:"            1 2 nil   2 nil (1 compilation-error-face)   (2 compilation-line-face))
    (vvp-warning           "^\\(?1:WARNING\\): \\(?2:.*\\):\\(?3:[0-9]+\\):" 2 3 nil   1 nil (1 compilation-warning-face) (2 compilation-warning-face) (3 compilation-line-face))
    (vvp-error             "^\\(?1:ERROR\\): \\(?2:.*\\):\\(?3:[0-9]+\\):"   2 3 nil   2 nil (1 compilation-warning-face) (2 compilation-warning-face) (3 compilation-line-face))
    (vvp-info              "^\\(?1:LXT2 info\\):"                            1 nil nil 0 nil)))

(defvar larumbe/compilation-error-re-synplify
  '((synp-error     "^@\\(?1:E\\): \\(?2:[A-Z0-9]+\\) :\"\\(?3:[0-9a-zA-Z./_-]+\\)\":\\(?4:[0-9]+\\):\\(?5:[0-9]+\\):" 3 4 5 2 nil (1 compilation-error-face) (2 larumbe/compilation-gray-face))
    (synp-error2    "^@\\(?1:E\\): \\(?2:[A-Z0-9]+\\) [:]?|" 1 nil nil 2 nil (2 larumbe/compilation-gray-face))
    (synp-error3    "^@\\(?1:E\\):" 1 nil nil 2 nil)
    (synp-warning   "^@\\(?1:W\\): \\(?2:[A-Z0-9]+\\) :\"\\(?3:[0-9a-zA-Z./_-]+\\)\":\\(?4:[0-9]+\\):\\(?5:[0-9]+\\):" 3 4 5 1 nil (1 compilation-warning-face) (2 larumbe/compilation-gray-face))
    (synp-warning2  "^@\\(?1:W\\): \\(?2:[A-Z0-9]+\\) [:]?|" 1 nil nil 1 nil (2 larumbe/compilation-gray-face))
    (synp-warning3  "^@\\(?1:W\\):" 1 nil nil 1 nil)
    (synp-note      "^@\\(?1:N\\): \\(?2:[A-Z0-9]+\\) :\"\\(?3:[0-9a-zA-Z./_-]+\\)\":\\(?4:[0-9]+\\):\\(?5:[0-9]+\\):" 3 4 5 0 nil (1 compilation-info-face) (2 larumbe/compilation-gray-face))
    (synp-note2     "^@\\(?1:N\\): \\(?2:[A-Z0-9]+\\) [:]?|" 1 nil nil 0 nil (2 larumbe/compilation-gray-face))
    ;; Did not find what those meant online, so set as warnings
    (synp-alt-info  "^@\\(?1:A\\): \\(?2:[A-Z0-9]+\\) :\"\\(?3:[0-9a-zA-Z./_-]+\\)\":\\(?4:[0-9]+\\):\\(?5:[0-9]+\\):" 3 4 5 0 nil (1 compilation-info-face) (2 larumbe/compilation-gray-face))
    (synp-alt-info2 "^@\\(?1:A\\): \\(?2:[A-Z0-9]+\\) [:]?|" 1 nil nil 0 nil (2 larumbe/compilation-gray-face))
    (synp-alt-info3 "^@\\(?1:A\\):" 1 nil nil 0 nil)
    (synp-note3     "^@\\(?1:N\\):" 1 nil nil 0 nil)
    (synp-info      "^@\\(?1:I\\):" nil nil nil 0 nil (1 compilation-line-face))
    (synp-log       "^@\\(?1:L\\):" nil nil nil 0 nil (1 compilation-line-face))))

(defvar larumbe/compilation-error-re-synopsys-dc
  '((dc-error     "\\(?1:^Error\\):  \\(?2:[0-9a-zA-Z./_-]+\\):\\(?3:[0-9]+\\): "       2 3   nil 2 nil (1 compilation-error-face))
    (dc-error-2   "\\(?1:^Error\\): .*"                                                 1 nil nil 2 nil)
    (dc-warning   "\\(?1:^Warning\\):  \\(?2:[0-9a-zA-Z./_-]+\\):\\(?3:[0-9]+\\): "     2 3   nil 1 nil (1 compilation-warning-face))
    (dc-warning-2 "\\(?1:^Warning\\): .*"                                               1 nil nil 1 nil)
    (dc-info      "\\(?1:^Information\\):  \\(?2:[0-9a-zA-Z./_-]+\\):\\(?3:[0-9]+\\): " 2 3   nil 0 nil (1 compilation-info-face))
    (dc-info-2    "\\(?1:^Information\\): .*"                                           1 nil nil 0 nil)))

;; Adapted from compilation.el for Python tracebacks
(defvar larumbe/compilation-error-re-python
  '((python-tracebacks-and-caml "File \\(\"?\\)\\([^,\" \n\t<>]+\\)\\1, lines? \\([0-9]+\\)-?\\([0-9]+\\)?\\(?:$\\|, \\(?: characters? \\([0-9]+\\)-?\\([0-9]+\\)?:\\)?\\([ \n]Warning\\(?: [0-9]+\\)?:\\)?\\)?" 2 (3 . 4) (5 . 6) (7)) ; Some regexps where not detected on some SCons errors (original one did not have `?' at the end)
    (python-log-error   "\\(?1:[0-9-]+ [0-9:,]+\\) - \\(?2:[a-zA-Z0-9.]+\\) - \\(?3:ERROR\\) - "   3 nil nil 2 2 (1 compilation-line-face) (3 compilation-error-face))
    (python-log-warning "\\(?1:[0-9-]+ [0-9:,]+\\) - \\(?2:[a-zA-Z0-9.]+\\) - \\(?3:WARNING\\) - " 3 nil nil 1 2 (1 compilation-line-face) (3 compilation-warning-face))
    (python-log-info    "\\(?1:[0-9-]+ [0-9:,]+\\) - \\(?2:[a-zA-Z0-9.]+\\) - \\(?3:INFO\\) - "    3 nil nil 0 2 (1 compilation-line-face) (3 compilation-info-face))))

(defvar larumbe/compilation-error-re-scons
  '((scons-target-cmd    "\\(?1:^[a-zA-Z_-]+\\)(\\(?2:.*\\))$" nil nil nil 0 nil (1 compilation-line-face) (2 compilation-info-face))
    (scons-target-err    "\\(?1:NOK\\)$"                         1 nil nil 2 nil (1 compilation-error-face))
    (scons-target-cw     "\\(?1:critical warning\\)$"            1 nil nil 1 nil (1 compilation-warning-face))
    (scons-target-ok     "\\(?1:OK\\)$"                          1 nil nil 0 nil (1 compilation-info-face))))

(defvar larumbe/compilation-error-re-pax
  '((pax-assert-err  "** \\(?1:assertion failure\\) at time \\(?2:[0-9.]+\\)"   1 nil nil 2 nil (2 compilation-line-face))
    (pax-tb-note     "\\(?1:^TB_NOTE\\) @ [0-9\.]+:"                            1 nil nil 0 nil)
    (pax-tb-debug    "\\(?1:^TB_DEBUG\\) @ [0-9\.]+:"                           1 nil nil 0 nil (1 compilation-line-face))
    (pax-tb-warning  "\\(?1:^TB_WARNING\\) @ [0-9\.]+:"                         1 nil nil 1 nil)
    (pax-tb-err      "\\(?1:^TB_ERROR\\) @ [0-9\.]+:"                           1 nil nil 2 nil)
    (pax-tb-fatal    "\\(?1:^TB_FATAL\\) @ [0-9\.]+:"                           1 nil nil 2 nil)
    (pax-perl-err    "\\(?1:^ERROR\\):"                                         1 nil nil 2 nil)
    (pax-perl-err2   "\\(?1:^ERROR\\)!"                                         1 nil nil 2 nil)
    (pax-gasearch    "^\\(?1:[0-9]+\\) \\(?2:[a-zA-Z0-9\\_\\-\\.\\/]+\\)+ \"\\(?3:[a-zA-Z0-9\\_\\-\\.]+\\)+\" \\(?4:[0-9]+\\)" 2 nil nil 2 nil (1 compilation-info-face) (3 compilation-warning-face) (4 compilation-info-face))))

(defvar larumbe/compilation-error-re-gcc
  '((gcc-warning "^\\(?1:[0-9a-zA-Z\/\._-]+\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): \\(?4:warning\\):" 1 2 3 1 nil)
    (gcc-error   "^\\(?1:[0-9a-zA-Z\/\._-]+\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): \\(?4:error\\):"   1 2 3 2 nil)))


;; INFO: To be used with: `C-u M-x compile RET tail -f Log.txt'
;; Or just make a wrapper function to set up this debug config
(defvar larumbe/compilation-error-re-ableton
  '((ableton-error      "\\(?1:[0-9-]+T[0-9:.]+:\\) \\(?2:info:\\) \\(?3:RemoteScriptError:\\)"    nil nil nil 1 2 (1 compilation-line-face) (3 compilation-warning-face))
    (ableton-exception  "\\(?1:[0-9-]+T[0-9:.]+:\\) \\(?2:info:\\) \\(?3:Exception:\\)"            nil nil nil 2 2 (1 compilation-line-face) (3 compilation-error-face))
    (ableton-message    "\\(?1:[0-9-]+T[0-9:.]+:\\) \\(?2:info:\\) \\(?3:RemoteScriptMessage:\\)"  nil nil nil 0 2 (1 compilation-line-face) (3 compilation-info-face))
    (ableton-others     "\\(?1:[0-9-]+T[0-9:.]+:\\) \\(?2:info:\\)"                                nil nil nil 0 2 (1 compilation-line-face))))



(defvar larumbe/compilation-custom-regexp-sets
  '(("verilog-make" . (larumbe/compilation-error-re-iverilog larumbe/compilation-error-re-verilator larumbe/compilation-error-re-vivado))
    ("vivado"       . (larumbe/compilation-error-re-vivado))
    ("xrun"         . (larumbe/compilation-error-re-xrun))
    ("verilator"    . (larumbe/compilation-error-re-verilator))
    ("iverilog"     . (larumbe/compilation-error-re-iverilog))
    ("synplify"     . (larumbe/compilation-error-re-synplify))
    ("synopsys-dc"  . (larumbe/compilation-error-re-synopsys-dc))
    ("scons"        . (larumbe/compilation-error-re-xrun larumbe/compilation-error-re-vivado larumbe/compilation-error-re-synplify larumbe/compilation-error-re-scons larumbe/compilation-error-re-python))
    ("pax"          . (larumbe/compilation-error-re-xrun larumbe/compilation-error-re-pax larumbe/compilation-error-re-gcc))
    ("ableton"      . (larumbe/compilation-error-re-python larumbe/compilation-error-re-ableton))))

(defvar larumbe/compilation-custom-regexp-active nil)




;;;; Functions
;;;###autoload
(defun larumbe/compilation-error-re-set (parser)
  "Set variables `compilation-error-regexp-alist' and `compilation-error-regexp-alist-alist' according to PARSER."
  (interactive (list (completing-read "Select parser: " (mapcar 'car larumbe/compilation-custom-regexp-sets))))
  (let* ((regex-alist-quoted (cdr (assoc parser larumbe/compilation-custom-regexp-sets)))
         (regex-alist (apply 'append (mapcar
                                      (lambda (elm) (mapcar 'car (eval elm)))
                                      regex-alist-quoted)))
         (regex-alist-alist (apply 'append (mapcar
                                            (lambda (elm) (eval elm))
                                            regex-alist-quoted))))
    (when (boundp 'compilation-error-regexp-alist-alist)
      (setq compilation-error-regexp-alist regex-alist)
      (setq compilation-error-regexp-alist-alist regex-alist-alist))))


;;;###autoload
(defun larumbe/compile (cmd &optional buf parser)
  "Compile with command CMD in buffer BUF with parser PARSER.

If BUF is in use, ask for confirmation to re-use it."
  ;; Check BUF exists if optional argument was passed
  (when (and buf
             (get-buffer buf))
    (if (yes-or-no-p (concat "Buffer " buf " in use. Reuse it?"))
        (switch-to-buffer buf)
      (error "Aborting compilation!")))
  ;; Compile
  (compile cmd)
  ;; If BUF does not exist, set up properties from default *compilation*
  (when (and buf
             (not (get-buffer buf)))
    (rename-buffer buf))
  ;; Set parser
  (when parser
    (larumbe/compilation-error-re-set parser))
  ;; Final tweaks
  (delete-other-windows)
  (setq truncate-lines t)
  (goto-char (point-max)))


;;;###autoload
(defun larumbe/compilation-threshold ()
  (interactive)
  (let* ((choices '("error" "warning" "info"))
         (choice   (completing-read "Threshold: " choices)))
    (pcase choice
      ("error"   (setq compilation-skip-threshold 2))
      ("warning" (setq compilation-skip-threshold 1))
      ("info"    (setq compilation-skip-threshold 0)))))



;;;###autoload
(defun larumbe/recompile-set-active-regexp-alist ()
  "Set current regexp-alist for EVERY *compilation* buffer.

INFO: Tried to set `larumbe/compilation-custom-regexp-active' locally to each
buffer, but it actually was more effort.  It is assumed that most of the time
work will be done with the same tool consecutively, i.e. there won't be constant
switches between Vivado and IES.  However, if it is set locally to each buffer,
every buffer would require confirmation."
  (interactive)
  (setq larumbe/compilation-custom-regexp-active (completing-read "Select compiler: " (mapcar 'car larumbe/compilation-custom-regexp-sets)))
  (message "Compilation Error Regexp set Globally to: %s" larumbe/compilation-custom-regexp-active))



;;;###autoload
(defun larumbe/recompile-with-regexp-alist ()
  "Recompile current *compilation* buffer and set proper regexp-alist for different programs"
  (interactive)
  (when (not (bound-and-true-p larumbe/compilation-custom-regexp-active))
    (larumbe/recompile-set-active-regexp-alist))
  (recompile)
  (larumbe/compilation-error-re-set larumbe/compilation-custom-regexp-active)
  (goto-char (point-max)))



;;;###autoload
(defun larumbe/compilation-log-add-re-header (&optional parser)
  "Add elisp header to current visited log file.
Open it in compilation mode with custom regexp parsing.

If passed PARSER, set corresponding regexp to be evaluated at the header."
  (interactive)
  (unless parser
    (setq parser (completing-read "Select parser: " (mapcar 'car larumbe/compilation-custom-regexp-sets))))
  (let ((header (concat "-*- mode: compilation; default-directory: \"" default-directory "\"; eval: (" (symbol-name 'larumbe/compilation-error-re-set) " \"" parser "\") -*-")))
    (read-only-mode -1)
    (goto-char (point-min))
    (open-line 2)
    (insert header)
    (save-buffer)
    (read-only-mode 1)
    (revert-buffer nil t)))



;;;; Interactive comint library
(defvar larumbe/compilation-interactive-buildcmd nil
  "Buffer-local variable used to determine the executed build command.
It's main use is to allow for recompiling easily.")

(defun larumbe/compilation-interactive (command bufname parser)
  "Create a `compilation-mode' comint shell almost identical to *ansi-term*.
It will have the same environment and aliases without the need of setting
`shell-command-switch' to '-ic'.

Execute COMMAND in the buffer.  Buffer will be renamed to BUFNAME.
Regexp parsing of PARSER is applied.

Useful to spawn a *tcl-shell* with Vivado regexps, or to init sandbox modules."
  (when (get-buffer bufname)
    (pop-to-buffer bufname)
    (error (concat "Buffer " bufname " already in use!")))
  (compile command t)
  (select-window (get-buffer-window "*compilation*"))
  (goto-char (point-max))
  (setq truncate-lines t)
  (larumbe/compilation-error-re-set parser)
  (rename-buffer bufname))


(defun larumbe/compilation-interactive-sandbox (initcmd buildcmd bufname parser)
  "Initialize a comint Bash sandbox with INITCMD and execute BUILDCMD next.
Buffer will be renamed to BUFNAME, and regexp parsing depending on PARSER.

Acts as wrapper for `larumbe/compilation-interactive'with an additional build command.

INFO: With some minor tweaks could be extended to allow a list
of commands to be executed by sending them through `comint-send-string'"
  (let ((command initcmd)
        (proc))
    (larumbe/compilation-interactive command bufname parser)
    (setq-local larumbe/compilation-interactive-buildcmd buildcmd)
    (setq proc (get-buffer-process bufname))
    (comint-send-string proc buildcmd)
    (comint-send-string proc "\n")))


;;;###autoload
(defun larumbe/compilation-interactive-recompile ()
  "Will only work in `comint-mode' after `larumbe/compilation-interactive-sandbox' was executed.
Makes use of buffer-local variable `larumbe/compilation-interactive-buildcmd' to rebuild a target."
  (interactive)
  (unless larumbe/compilation-interactive-buildcmd
    (error "No interactive recompile command was set"))
  (let (proc)
    (when (string= major-mode "comint-mode")
      (setq proc (get-buffer-process (current-buffer)))
      (comint-send-string proc larumbe/compilation-interactive-buildcmd)
      (comint-send-string proc "\n"))))


;;;; Misc
;; https://stackoverflow.com/questions/10946219/emacs-compilation-mode-wont-see-bash-alias
(defun larumbe/compilation-shell-interactive-hook ()
  "Needed to see bash alias in Emacs subshells."
  (setq-local shell-file-name "bash")
  (setq-local shell-command-switch "-ic"))

;;;###autoload
(define-minor-mode larumbe/compilation-read-bashrc-alias-mode
  "Basic minor mode to set shell command interactive to read .bashrc alias for compilation modes."
  :global t
  (if larumbe/compilation-read-bashrc-alias-mode
      ;; Enable
      (add-hook 'compilation-mode-hook #'larumbe/compilation-shell-interactive-hook)
    ;; Disable
    (remove-hook 'compilation-mode-hook #'larumbe/compilation-shell-interactive-hook)))





(provide 'compilation-utils)

;;; compilation-utils.el ends here
