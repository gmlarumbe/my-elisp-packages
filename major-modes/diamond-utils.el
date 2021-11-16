;;; diamond-utils.el --- Lattice Diamond Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'comint)
(require 'company)
(require 'compilation-utils)


;;;; Diamond-TCL Shell
(defvar larumbe/diamond-shell-bin (executable-find "diamondc"))
(defvar larumbe/diamond-shell-cmd-switches nil)

(defvar larumbe/diamond-shell-buffer "*diamond-tcl*")

;; Lattice Diamond User Guide Tcl Scripting section converted to text via `pdftotext'
(defvar larumbe/diamond-shell-commands
  '(;; Help
    "help"
    ;; Lattice Diamond Tcl Console extended commands
    "history" "reset" "clear" "save_script" "set_prompt"
    ;; Project manager extended Tcl commands
    "prj_project" "prj_src" "prj_impl" "prj_strgy" "prj_run" "prj_syn" "prj_dev" "prj_incr"
    ;; Sys
    "sys_install"
    ;; Ncd extended Tcl commands
    "ncd_port" "ncd_inst" "ncd_net" "ncd_attr"
    ;; Ngd extended Tcl commands
    "ngd_port" "ngd_inst" "ngd_net" "ngd_attr"
    ;; Reveal Inserter extended Tcl commands
    "rvl_project" "rvl_core" "rvl_trace" "rvl_tu" "rvl_te"
    ;; Clarity Designer extended Tcl commands
    "sbp_design" "sbp_resource" "sbp_builder"
    ;; Reveal Analyzer extended Tcl commands
    "rva_trace" "rva_core" "rva_tu" "rva_te" "rva_tokenmgr" "rva_trigoptn" "rva_project" "rva_pcs"
    ;; Power Calculator extended Tcl commands
    "pwc_command" "pwc_device" "pwc_parameters" "pwc_thermal" "pwc_settings"
    "pwc_supply" "pwc_logicblocks" "pwc_clocks" "pwc_inout" "pwc_blockram"
    "pwc_dspblock" "pwc_plldll" "pwc_maco" "pwc_serdes" "pwc_mipidphy"
    "pwc_writereport" "pwc_efb" "pwc_misc" "pwc_power" "pwc_esb"
    ;; Programmer extended Tcl commands
    "pgr_project" "pgr_program"
    ;; Platform Manager II extended Tcl commands
    "psb_vmon" "psb_trim" "psb_vid" "psb_imon" "psb_tmon" "psb_fan"
    "psb_lbd_flow" "psb_lbd_sqn" "psb_lbd_svsy" "psb_lbd_timer" "psb_lbd_impt" "psb_usp_glb"
    "psb_usp_port" "psb_usp_node" "psb_usp_asc" "psb_usp_gpio" "psb_usp_hvport" "psb_global"
    "psb_summary" "psb_faultlogger"
    ;; Incremental Design Flow Database extended Tcl commands
    "icf_data" "icf_part"
    ;; Compile Lattice FPGA simulation libraries.
    "cmpl_libs"
    ;; ECO extended Tcl commands
    "eco_design" "eco_add" "eco_delete" "eco_unbind"
    "eco_clone" "eco_swap" "eco_rename" "eco_place"
    "eco_route" "eco_config"
    ))



(defun larumbe/diamond-shell-completion-at-point ()
  "Used as an element of `completion-at-point-functions'."
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
         (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point)))
         (str (buffer-substring b e))
         (allcomp (all-completions str larumbe/diamond-shell-commands)))
    (list b e allcomp)))


(defun larumbe/diamond-shell-send-exit-command ()
  "Send 'exit' command to quit Diamond console."
  (interactive)
  (let ((proc (get-buffer-process larumbe/diamond-shell-buffer)))
    (comint-send-string proc "exit")
    (comint-send-string proc "\n")))


(define-minor-mode larumbe/diamond-shell-completion-at-point-mode
  "Add extensions for Diamond TCL shell.
Autocompletion based on `diamond' package keywords."
  :keymap
  '(("\C-d" . larumbe/diamond-shell-send-exit-command)) ; Should override `comint-delchar-or-maybe-eof'
  (when (not (equal (buffer-name (current-buffer)) larumbe/diamond-shell-buffer))
    (error "Not in Diamond shell buffer!"))
  (make-local-variable 'comint-dynamic-complete-functions) ; Use this variable instead of `completion-at-point-functions' to preserve file-name expansion
  (if larumbe/diamond-shell-completion-at-point-mode
      ;; INFO: It seems that without appending, the `larumbe/diamond-shell-completion-at-point' will have precedence
      ;; over other functions present in `comint-dynamic-complete-functions'
      (add-to-list 'comint-dynamic-complete-functions #'larumbe/diamond-shell-completion-at-point)
    (delete #'larumbe/diamond-shell-completion-at-point comint-dynamic-complete-functions)))


;;;###autoload
(defun larumbe/diamond-shell ()
  "Invoke a TCL diamond shell with the proper regexps, suited for compilation."
  (interactive)
  (unless larumbe/diamond-shell-bin
    (error "Could not find diamond in $PATH.  Add it or set `larumbe/diamond-shell-bin'"))
  (let ((command (concat larumbe/diamond-shell-bin " " (mapconcat #'identity larumbe/diamond-shell-cmd-switches " ")))
        (bufname larumbe/diamond-shell-buffer)
        (parser  "diamond"))
    (larumbe/compilation-interactive command bufname parser)
    (larumbe/diamond-shell-completion-at-point-mode 1)
    (company-mode 1)))


(defun larumbe/diamond-shell-tcl-send-line-or-region-and-step ()
  "Send the current line to the inferior shell and step to the next line.
When the region is active, send the region instead."
  (interactive)
  (let (from to end (proc (get-buffer-process larumbe/diamond-shell-buffer)))
    (if (use-region-p)
        (setq from (region-beginning)
              to (region-end)
              end to)
      (setq from (line-beginning-position)
            to (line-end-position)
            end (1+ to)))
    (comint-send-string proc (buffer-substring-no-properties from to))
    (comint-send-string proc "\n")
    (goto-char end)))


(provide 'diamond-utils)

;;; diamond-utils.el ends here
