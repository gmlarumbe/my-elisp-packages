;;; fpga-utils.el --- FPGA Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defvar larumbe/hdl-source-extension-regex "\\(.sv$\\|.v$\\|.svh$\\|.vh$\\|.vhd$\\)")


;;;; Vivado tags
;; Projects list for the `larumbe/fpga-tags-vivado-list':
;; Name of the project (+plus)
;; 1) Path of the .xpr file (without name)
;; 2) Name of the .xpr
;; 3) Path where GTAGS file will be created
;; 4) Name of the file that will be read by global to generate GTAGS (e.g. verilog files)

;; Variables
(defvar larumbe/fpga-tags-vivado-list                 nil)
(defvar larumbe/fpga-tags-vivado-xpr-dir              nil)
(defvar larumbe/fpga-tags-vivado-xpr-file             nil)
(defvar larumbe/fpga-tags-vivado-gtags-dirs-directory nil)
(defvar larumbe/fpga-tags-vivado-gtags-dirs-file      nil)
(defvar larumbe/fpga-tags-vivado-gtags-file           nil)


(defun larumbe/fpga-tags-vivado-set-active-xpr ()
  "Retrieve project list and set variables accordingly."
  (let ((project)
        (files-list))
    ;; Get Project name
    (setq project (completing-read "Select project: " (mapcar 'car larumbe/fpga-tags-vivado-list))) ;; Read previous variable and get list of first element of each assoc list
    (setq files-list (cdr (assoc project larumbe/fpga-tags-vivado-list)))
    ;; Set parameters accordingly
    (setq larumbe/fpga-tags-vivado-xpr-dir              (nth 0 files-list))
    (setq larumbe/fpga-tags-vivado-xpr-file             (nth 1 files-list))
    (setq larumbe/fpga-tags-vivado-gtags-dirs-directory (nth 2 files-list))
    (setq larumbe/fpga-tags-vivado-gtags-dirs-file      (nth 3 files-list))
    (setq larumbe/fpga-tags-vivado-gtags-file           (larumbe/path-join larumbe/fpga-tags-vivado-gtags-dirs-directory larumbe/fpga-tags-vivado-gtags-dirs-file))))


(defun larumbe/fpga-tags-vivado-convert-xci-to-v-and-downcase ()
  "Convert .xci file paths present in gtags.files to .v and downcase.
Vivado generates them in this way...
Assumes it is being used in current buffer (i.e. gtags.files).

INFO: This is a Workaround for Vivado Naming Conventions at IP Wizard generation."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "\\([a-zA-Z0-9_-]*\\).xci" nil t) ; Fail silently
        (progn
          (replace-match "\\1.v")
          (re-search-backward "/")
          (downcase-region (point) (point-at-eol))))))


(defun larumbe/fpga-tags-vivado-files-from-xpr ()
  "Create `gtags.files' from Vivado XPR file."
  (with-temp-buffer
    ;; (view-buffer-other-window (current-buffer))      ; Option A: preferred (not valid if modifying the temp buffer)
    ;; (clone-indirect-buffer-other-window "*debug*" t) ; Option B: used here (however, cannot save temp buffer while debugging)
    (insert-file-contents (larumbe/path-join larumbe/fpga-tags-vivado-xpr-dir larumbe/fpga-tags-vivado-xpr-file))
    ;; Start Regexp replacement for file
    (keep-lines "<.*File Path=.*>" (point-min) (point-max))
    (larumbe/replace-regexp-whole-buffer "<.*File Path=\"" "")
    (larumbe/replace-regexp-whole-buffer "\">" "")
    (larumbe/replace-string-whole-buffer "$PPRDIR" larumbe/fpga-tags-vivado-xpr-dir t)
    (delete-whitespace-rectangle (point-min) (point-max))
    (larumbe/fpga-tags-vivado-convert-xci-to-v-and-downcase) ; Replace xci by corresponding .v files (if existing)
    (keep-lines larumbe/hdl-source-extension-regex (point-min) (point-max)) ; Remove any non verilog/vhdl file (such as waveconfig, verilog templates, etc...)
    ;; Make sure expansion is made relative to SVN sandbox path (same as gtags.file path)
    (larumbe/buffer-expand-filenames nil larumbe/fpga-tags-vivado-gtags-dirs-directory)
    (write-file larumbe/fpga-tags-vivado-gtags-file)))


;;;###autoload
(defun larumbe/fpga-tags-vivado ()
  "Create gtags from created `gtags.files' by parsing Vivado XPR files."
  (interactive)
  (larumbe/fpga-tags-vivado-set-active-xpr)
  (larumbe/fpga-tags-vivado-files-from-xpr)
  (larumbe/gtags-create-tags-async-process larumbe/fpga-tags-vivado-gtags-dirs-directory))


;;;; Quartus tags
;; Projects list for the `larumbe/fpga-tags-altera-list' variables:
;; Name of the project (+plus)
;; 1) Path of the altera dir (without name)
;; 2) Name of the tcl file used to get the file list (files_and_libraries.tcl)
;; 3) Path where GTAGS file will be created
;; 4) Name of the file that will be read by global to generate GTAGS (e.g. gtags.files)
(defvar larumbe/fpga-tags-altera-list                 nil)
(defvar larumbe/fpga-tags-altera-tcl-dir              nil)
(defvar larumbe/fpga-tags-altera-tcl-file             nil)
(defvar larumbe/fpga-tags-altera-gtags-dirs-directory nil)
(defvar larumbe/fpga-tags-altera-gtags-dirs-file      nil)

(defvar larumbe/fpga-tags-altera-tcl-file-regexp "\\(.*_FILE\\|SEARCH_PATH\\) ")
(defvar larumbe/fpga-tags-altera-tcl-file-regexp-file "\\(.*_FILE\\) ")
(defvar larumbe/fpga-tags-altera-tcl-file-regexp-dir "\\(.*SEARCH_PATH\\) ")

;; Functions and variables for directory expansion (retrieve files from a dir on each line for gtags processing)
(defvar larumbe/fpga-tags-altera-tcl-env-archons-path  nil)
(defvar larumbe/fpga-tags-altera-tcl-env-archons-regex nil)
;; Output of `echo $ARCHONS_PATH' at LFP CEE obelix environment

(defun larumbe/fpga-tags-altera-append-files-from-dir (dir)
  "Append list of files from DIR to FILE.
Used on `tempfile' from `files_and_libraries.tcl' to expand directories
Global needs the file name, hence this function"
  (save-excursion
    (mapcar
     (lambda (x)
       (goto-char (point-max))
       (insert (concat x "\n")))
     (directory-files dir t))))


(defun larumbe/fpga-tags-altera-find-repeated-included-files ()
  "Find repeated files in current buffer (meant for gtags.files).
There are duplicates in `larumbe/fpga-tags-altera-append-files-from-dir' if files and
dirs are included.  This function checks if there is a repeated file in
gtags.files for GTAGS not to have a duplicate tag.
Checks Works in current buffer."
  (let ((file-to-check))
    (goto-char (point-min))
    (while (< (point) (point-max))
      (save-excursion
        (setq file-to-check (concat (file-name-base (thing-at-point 'filename)) "." (file-name-extension (thing-at-point 'filename))))
        (move-end-of-line 1)
        (while (re-search-forward (concat file-to-check "$") nil t) ; If file is included more than once we keep only the first one
          (beginning-of-line)
          (kill-line 1)))
      (forward-line))))


(defun larumbe/fpga-tags-altera-create-file-list ()
  "Create `gtags.files' from altera project tcl file."
  (save-window-excursion
    (with-temp-buffer
      ;; INFO: Debugging with-temp-buffer:
      ;; (view-buffer-other-window (current-buffer))      ; Option A: preferred (not valid since temp buffer cannot be modified)
      ;; (clone-indirect-buffer-other-window "*debug*" t) ; Option B: used here (however, cannot save temp buffer while debugging)
      ;; End of INFO
      (insert-file-contents (larumbe/path-join larumbe/fpga-tags-altera-tcl-dir larumbe/fpga-tags-altera-tcl-file))
      ;; Start Regexp replacement for file
      (keep-lines larumbe/fpga-tags-altera-tcl-file-regexp (point-min) (point-max)) ; Get only files
      (goto-char (point-min))
      (while (re-search-forward "^#" nil t)   ; Remove comments
        (beginning-of-line)
        (kill-line 1))
      ;; Replace files
      (larumbe/replace-regexp-whole-buffer
       (concat "set_global_assignment -name " larumbe/fpga-tags-altera-tcl-file-regexp-file)
       (concat (file-name-as-directory larumbe/fpga-tags-altera-tcl-dir)))
      ;; Replace SEARCH_PATH dirs
      (goto-char (point-min))
      (while (re-search-forward larumbe/fpga-tags-altera-tcl-file-regexp-dir nil t)
        (kill-line 0) ; Kill until the beginning of line
        (insert (file-name-as-directory larumbe/fpga-tags-altera-tcl-dir))
        (larumbe/fpga-tags-altera-append-files-from-dir (thing-at-point 'filename)))
      ;; Replace $env(ARCHONS_PATH) dirs
      (goto-char (point-min))
      (while (re-search-forward larumbe/fpga-tags-altera-tcl-env-archons-regex nil t)
        (kill-line 0) ; Kill until the beginning of line
        (insert larumbe/fpga-tags-altera-tcl-env-archons-path))
      ;; Cleanup file
      (larumbe/replace-regexp-whole-buffer " +" "")  ; Delete whitespaces in PATHs
      (goto-char (point-min))
      (while (re-search-forward "\\.$" nil t) ; Remove search paths with previous or current dir
        (beginning-of-line)                  ; Equivalent to `flush-lines' but
        (kill-line 1))                       ; for non-interactive use
      (larumbe/fpga-tags-altera-find-repeated-included-files) ; Remove repeated files (due to previous directory expansion)
      ;; Make sure expansion is made relative to SVN sandbox path (same as gtags.file path)
      (larumbe/buffer-expand-filenames nil larumbe/fpga-tags-altera-gtags-dirs-directory)
      (write-file (larumbe/path-join larumbe/fpga-tags-altera-gtags-dirs-directory larumbe/fpga-tags-altera-gtags-dirs-file)))))


(defun larumbe/fpga-tags-altera-set-active-project ()
  "Retrieve project list and set variables accordingly.
Copied from `larumbe/fpga-tags-vivado-set-active-xpr' for Vivado xpr."
  (let ((project)
        (files-list))
    ;; Get Project name
    (setq project (completing-read "Select project: " (mapcar 'car larumbe/fpga-tags-altera-list))) ;; Read previous variable and get list of first element of each assoc list
    (setq files-list (cdr (assoc project larumbe/fpga-tags-altera-list)))
    ;; Set parameters accordingly
    (setq larumbe/fpga-tags-altera-tcl-dir              (nth 0 files-list))
    (setq larumbe/fpga-tags-altera-tcl-file             (nth 1 files-list))
    (setq larumbe/fpga-tags-altera-gtags-dirs-directory (nth 2 files-list))
    (setq larumbe/fpga-tags-altera-gtags-dirs-file      (nth 3 files-list))))



;;;###autoload
(defun larumbe/fpga-tags-altera ()
  "Create `gtags.files' file for a specific Altera project.
Based on a search from `files_and_libraries.tcl' file."
  (interactive)
  (larumbe/fpga-tags-altera-set-active-project)
  (larumbe/fpga-tags-altera-create-file-list)
  (larumbe/gtags-create-tags-async-process larumbe/fpga-tags-altera-gtags-dirs-directory))


;;;; source_files.tcl tags
(defun larumbe/fpga-tags-files-from-source-files-tcl-get-files (file dir)
  "Create gtags.files from FILE `source_files.tcl'.

INFO: This function assumes that the `source_files.tcl' will be placed inside its original path (syn_targets).
This is necessary to properly generate the relative paths for the file list when expanding filenames.
It will expand these according to the input DIR.
DANGER: Therefore, make sure DIR is the root project path, where `gtags.files' would be placed.

INFO: Useful function for Verilog-Perl hierarchy extraction."
  (let ((output-file (larumbe/path-join dir "gtags.files")))
    (unless (or (string= (file-name-nondirectory file) "source_list.tcl")
                (string= (file-name-nondirectory file) "source_list_script.tcl"))
      (error "Not in 'source_list.tcl file!!"))
    (with-temp-buffer
      ;; (clone-indirect-buffer-other-window "*debug*" t) ; Option B: used here (however, cannot save temp buffer while debugging)
      (insert-file-contents file)
      (keep-lines larumbe/hdl-source-extension-regex)
      (delete-duplicate-lines (point-min) (point-max)) ; for libraries setup of previous files
      ;; First expand with absolute path ...
      (larumbe/buffer-expand-filenames t (file-name-directory file))
      ;; and then get relative path with respect to current dir
      ;; INFO: Must be executed at the root of a sandbox!
      (larumbe/buffer-expand-filenames nil dir)
      (write-file output-file))))


;;;###autoload
(defun larumbe/fpga-tags-files-from-source-files-tcl ()
  "Extract GTAGS from `source_files.tcl' at current projectile root."
  (interactive)
  (let* ((dir         (projectile-project-root))
         (syn-tgt-dir (larumbe/path-join dir "syn_targets"))
         (sources-file))
    (unless (file-exists-p syn-tgt-dir)
      (error "No syn_targets for current project!"))
    (setq sources-file (read-file-name "source_files.tcl path: " syn-tgt-dir))
    (larumbe/fpga-tags-files-from-source-files-tcl-get-files sources-file dir)
    (larumbe/gtags-create-tags-async-process dir)))


;;;; Vivado Synthesis
(defvar larumbe/vivado-binary-path nil)
(defvar larumbe/vivado-compile-script-path nil)
(defvar larumbe/vivado-compile-project-list nil)


(defun larumbe/vivado-compile-command ()
  "Return compilation command for selected project of `larumbe/vivado-compile-project-list'."
  (let* ((project (completing-read "Select project: " (mapcar 'car larumbe/vivado-compile-project-list)))
         (project-path (cdr (assoc project larumbe/vivado-compile-project-list)))
         (project-dir  (file-name-directory    project-path))
         (project-name (file-name-nondirectory project-path)))
    (concat "cd " project-dir " && " ; Temp files will be stored in this path
            larumbe/vivado-binary-path " -mode tcl "
            project-name " "
            "-source " larumbe/vivado-compile-script-path)))


;;;###autoload
(defun larumbe/vivado-compile ()
  "Use TCL console to elaborate/compile a Vivado design."
  (interactive)
  (compile (larumbe/vivado-compile-command))
  (larumbe/compilation-show-buffer "vivado"))


;;;; Vivado XSim
;; INFO: It is required to create the simulation first with Vivado GUI, and then run the script
(defvar larumbe/vivado-sim-project-list nil)
(defvar larumbe/vivado-sim-compilation-command nil)

(defun larumbe/vivado-sim-compile-command ()
  "Return compilation command for simulation of selected project of `larumbe/vivado-sim-project-list'."
  (let* ((sim-project (completing-read "Select project: " (mapcar 'car larumbe/vivado-sim-project-list)))
         (sim-project-path (cdr (assoc sim-project larumbe/vivado-sim-project-list)))
         (sim-project-dir (file-name-directory sim-project-path)))
    (concat
     "cd " sim-project-dir " && " ; Temp files will be stored in this path
     "source compile.sh && "
     "source elaborate.sh")))


;;;###autoload
(defun larumbe/vivado-sim (&optional universal-arg)
  "Use TCL console to elaborate a design with Isim based on previous variables.
If UNIVERSAL-ARG is provided, then simulate as well."
  (interactive "P")
  (let ((cmd (larumbe/vivado-sim-compile-command)))
    (when universal-arg
      (setq cmd (concat cmd " && source simulate.sh")))
    (compile cmd)
    (larumbe/compilation-show-buffer "vivado")))


;;;; Xcelium
;; Vivado IP simulation variables
(defvar larumbe/xrun-vivado-installation-path     nil)
(defvar larumbe/xrun-vivado-simlibs-compiled-path nil)
(defvar larumbe/xrun-vivado-simlibs '(("unisims"  . "data/verilog/src/unisims")
                                      ("unifast"  . "data/verilog/src/unisims")
                                      ("unimacro" . "data/verilog/src/unisims")
                                      ("retarget" . "data/verilog/src/retarget")
                                      ("secureip" . "data/secureip")))

;; Internal variables
(defvar larumbe/xrun-library-name "defaultlib")
(defvar larumbe/xrun-opts '("-64bit"
                            "-v93"
                            "-relax"
                            "-access"
                            "+rwc"
                            "-namemap_mixgen"
                            "-clean"
                            "-vlog_ext"
                            "+.vh"))
(defvar larumbe/xrun-command nil) ; Command built upon previous variables

;; Variables to be set by the user
(defvar larumbe/xrun-projects           nil)
(defvar larumbe/xrun-sources-file       nil)
(defvar larumbe/xrun-top-module         nil)
(defvar larumbe/xrun-extra-args-file    nil)
(defvar larumbe/xrun-uvm-test-name      nil)
(defvar larumbe/xrun-vivado-use-reflibs nil)


(defun larumbe/xrun-vivado-libs ()
  "Return list of strings with names of Vivado simlibs."
  (mapcar #'car larumbe/xrun-vivado-simlibs))


(defun larumbe/xrun-vivado-simlib-reflib-args ()
  "Return precompiled Vivado reflib simlib args."
  (mapconcat (lambda (lib) (concat "-reflib " (larumbe/path-join larumbe/xrun-vivado-simlibs-compiled-path lib ) ":" lib))
             (larumbe/xrun-vivado-libs)
             " "))


(defun larumbe/xrun-file-list ()
  "Return string to create library for files of current project."
  (let (files)
    (with-temp-buffer
      (insert-file-contents larumbe/xrun-sources-file)
      (setq files (split-string (buffer-substring (point-min) (point-max)) "\n"))
      (setq files (mapconcat #'identity files " ")))
    (concat "-makelib " larumbe/xrun-library-name " " files "-endlib")))


(defun larumbe/xrun-vivado-glbl-path ()
  "Return path of glbl.v file."
  (concat (larumbe/path-join larumbe/xrun-vivado-installation-path "data/verilog/src/glbl.v")))


(defun larumbe/xrun-compilation-dir ()
  "Return path of current project compilation directory.
Defaults to `build' at project root directory, where files.f should be placed."
  (concat (file-name-directory larumbe/xrun-sources-file) "build"))


(defun larumbe/xrun-build-command ()
  "Xrun build command."
  (let (extra-args-file uvm-args vivado-args)
    ;; Optional/extra args
    (when larumbe/xrun-extra-args-file
      (setq extra-args-file (concat " -f " larumbe/xrun-extra-args-file)))
    (when larumbe/xrun-uvm-test-name
      (setq uvm-args (concat " -uvm +UVM_TESTNAME=" larumbe/xrun-uvm-test-name)))
    (when larumbe/xrun-vivado-use-reflibs
      (setq vivado-args (concat " " (larumbe/xrun-vivado-simlib-reflib-args))))
    ;; Build command
    (concat "xrun "
            (mapconcat #'identity larumbe/xrun-opts " ")                   " "
            (larumbe/xrun-file-list)                                       " "
            "-top " larumbe/xrun-library-name "." larumbe/xrun-top-module  " "
            "-top glbl " (larumbe/xrun-vivado-glbl-path)
            uvm-args
            extra-args-file
            vivado-args)))


(defun larumbe/xrun-set-active-project ()
  "Set active project based on `larumbe/xrun-projects'."
  (let (xrun-project files-list)
    (setq xrun-project (completing-read "Select project: " (mapcar #'car larumbe/xrun-projects)))
    (setq files-list (cdr (assoc xrun-project larumbe/xrun-projects)))
    (setq larumbe/xrun-sources-file       (nth 0 files-list))
    (setq larumbe/xrun-top-module         (nth 1 files-list))
    (setq larumbe/xrun-extra-args-file    (nth 2 files-list))
    (setq larumbe/xrun-uvm-test-name      (nth 3 files-list))
    (setq larumbe/xrun-vivado-use-reflibs (nth 4 files-list))
    (setq larumbe/xrun-command (larumbe/xrun-build-command))))


;;;###autoload
(defun larumbe/xrun-compile-vivado-simlib (lib)
  "Compile a simlib of the ones available in `larumbe/xrun-vivado-simlibs'
at `larumbe/xrun-vivado-simlibs-compiled-path'."
  (interactive
   (list (completing-read "Library: " (larumbe/xrun-vivado-libs))))
  ;; Check for wrong input
  (unless (member lib (larumbe/xrun-vivado-libs))
    (error "Library value: %s not allowed" lib))
  (make-directory larumbe/xrun-vivado-simlibs-compiled-path t)
  (let ((vivado-lib-path (cdr (assoc lib larumbe/xrun-vivado-simlibs)))
        (lib-files)
        (cmd-lib-args)
        (cmd))
    ;; Get files and libraries
    (setq vivado-lib-path (cdr (assoc lib larumbe/xrun-vivado-simlibs)))
    (if (string= lib "secureip")
        (setq lib-files (delete "." (delete ".." (directory-files-recursively (larumbe/path-join larumbe/xrun-vivado-installation-path vivado-lib-path) "\\.vp$"))))
      (setq lib-files (delete "." (delete ".." (directory-files (larumbe/path-join larumbe/xrun-vivado-installation-path vivado-lib-path) t "\\.[s]?v[h]?$")))))
    (setq lib-files (mapconcat #'identity lib-files " "))
    (setq cmd-lib-args (concat cmd-lib-args
                               "-makelib "
                               (larumbe/path-join larumbe/xrun-vivado-simlibs-compiled-path lib) " "
                               lib-files " "
                               "-endlib "))
    ;; Build command
    (setq cmd (concat "xrun -compile "
                      (mapconcat #'identity larumbe/xrun-opts " ") " "
                      cmd-lib-args))
    ;; Compile
    (set (make-local-variable 'compile-command) cmd)
    (compile (concat "cd " larumbe/xrun-vivado-simlibs-compiled-path " && " compile-command))
    (larumbe/compilation-show-buffer "xrun")))


;;;###autoload
(defun larumbe/xrun-compile-vivado-simlib-all ()
  "Compile all the simlibs.

After running it, do not kill buffer and start next compilation
until previous has finished.

INFO: Since it is based on the compile command, it is not possible
to have more than one compilation at a time with the same buffer name.

When tried to compile all the libs in one command there was a bash
error saying that argument list was too long due to high amount of files."
  (interactive)
  (dolist (lib (larumbe/xrun-vivado-libs))
    (larumbe/xrun-compile-vivado-simlib lib)))



;;;###autoload
(defun larumbe/xrun-sim-elab (&optional universal-arg)
  "Simulate a design with `xrun' after selecting project.
If UNIVERSAL-ARG is given, elaborate the design instead."
  (interactive "P")
  (let (cmd)
    (larumbe/xrun-set-active-project)
    (if universal-arg
        (setq cmd (concat larumbe/xrun-command " -elaborate"))
      (setq cmd larumbe/xrun-command))
    (set (make-local-variable 'compile-command) cmd)
    (make-directory (larumbe/xrun-compilation-dir) t)
    (compile (concat "cd " (larumbe/xrun-compilation-dir) " && " compile-command))
    (larumbe/compilation-show-buffer "xrun")))



;;;; Verilator
;; INFO: Verilator does not support SystemVerilog verification constructs.
;; Therefore, any source with constructs such as a clocking blocks or classes must be
;; deleted from `verilator.files' (copied previously from gtags.file for example)
;; If that is not possible because it is used as a source (e.g. a SystemVerilog interface
;; with a clocking block), then tweak/comment temporarily files by hand.
;;
;; INFO: This is useful while developing small IPs
(defvar larumbe/verilator-project-list nil)

(defun larumbe/verilator-lint-command ()
  "Return current verilator int command based on selected project from `larumbe/verilator-project-list'."
  (let* ((project (completing-read "Select project: " (mapcar #'car larumbe/verilator-project-list)))
         (lint-files (nth 0 (cdr (assoc project larumbe/verilator-project-list))))
         (lint-top   (nth 1 (cdr (assoc project larumbe/verilator-project-list)))))
    (concat "verilator --lint-only +1800-2012ext+sv "
            "-f " lint-files " "
            "--top-module " lint-top)))

;;;###autoload
(defun larumbe/verilator-lint ()
  "Files created with ggtags and renamed (useful for small projects).
It's faster than Vivado elaboration since it does not elaborate design"
  (interactive)
  (compile (larumbe/verilator-lint-command))
  (larumbe/compilation-show-buffer "verilator"))


;;;; Reggen
(defvar larumbe/reggen-tool-path nil)
(defvar larumbe/reggen-template-types '("c_header"
                                        "docbook"
                                        "verilog_header"
                                        "verilog_defspkg"
                                        "verilog_regcomponent_simple"
                                        "verilog_regcomponent_regbusitf"
                                        "verilog_regcomponent_axilite"))


;;;###autoload
(defun larumbe/reggen (rdl-file output-dir)
  "Generate reggen outputs for input RDL-FILE at OUTPUT-DIR."
  (interactive "FRDL file: \nDOutput path: ")
  (unless (and (file-exists-p rdl-file)
               (directory-name-p output-dir))
    (error "RDL file must be a file, and Output path must be a directory!"))
  (let ((reggen-command)
        (output-file)
        (output-filename)
        (reggen-template)
        (addrmap))
    ;; Get value of addrmap
    (with-temp-buffer
      (insert-file-contents rdl-file)
      (re-search-forward "^addrmap \\(?1:[^ ]+\\)")
      (setq addrmap (match-string-no-properties 1)))
    ;; Check which type of output has to be generated
    (setq reggen-template (completing-read "Select template: " larumbe/reggen-template-types))
    ;; Set output filename extension
    (pcase reggen-template
      ("c_header"
       (setq output-filename (concat addrmap ".h")))
      ("docbook"
       (setq output-filename (concat addrmap ".xml")))
      ("verilog_header"
       (setq output-filename (concat addrmap ".vh")))
      ("verilog_regcomponent_simple"
       (setq output-filename (concat addrmap ".sv")))
      ("verilog_regcomponent_regbusitf"
       (setq output-filename (concat addrmap ".sv")))
      ("verilog_regcomponent_axilite"
       (setq output-filename (concat addrmap ".sv")))
      ("verilog_defspkg"
       (setq output-filename (concat addrmap "_defs_pkg.sv"))))
    ;; Set output filename
    (setq output-file (larumbe/path-join output-dir output-filename))
    ;; Set compilation command
    (setq reggen-command
          (concat
           larumbe/reggen-tool-path " "
           "-i " rdl-file " "
           "-o " output-file " "
           "-t " reggen-template " "
           "-a " addrmap " "
           "-m " "full "
           "-v" ; Verbose
           ))
    ;; Compile
    (compile reggen-command)
    (larumbe/compilation-error-re-set "scons")))



(provide 'fpga-utils)

;;; fpga-utils.el ends here
