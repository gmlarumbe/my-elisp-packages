;;; fpga-larumbe-utils.el --- FPGA Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Renamed from `fpga-utils' to avoid namespace collision with utils
;; package inside `fpga'.
;;
;;; Code:

(require 'fpga)

(defun larumbe/fpga-uvm-copy-timestamp (re-alist)
  "Copy current UVM timestamp from line at point.

Extract UVM regexp from RE-ALIST."
  (unless (string= major-mode "compilation-mode")
    (error "Could only be used in `compilation-mode'!"))
  (let* ((line-text (thing-at-point 'line :no-props))
         (uvm-re-alist `(,(cadr (assoc 'uvm-error   re-alist))
                         ,(cadr (assoc 'uvm-info    re-alist))
                         ,(cadr (assoc 'uvm-warning re-alist))
                         ,(cadr (assoc 'uvm-fatal   re-alist))))
         uvm-re-time timestamp)
    (unless (catch 'found
              (dolist (uvm-re uvm-re-alist)
                (setq uvm-re-time (concat uvm-re " @ \\(?4:[0-9]+\\( ns\\)?\\)"))
                (when (string-match uvm-re-time line-text)
                  (setq timestamp (match-string-no-properties 4 line-text))
                  (kill-new timestamp)
                  (message timestamp)
                  (throw 'found t))))
      (message "Not in a UVM report line!"))))

;;;###autoload
(defun larumbe/fpga-uvm-copy-timestamp-xrun ()
  "Copy current UVM timestamp from line at point for Xcelium regexps."
  (interactive)
  (larumbe/fpga-uvm-copy-timestamp fpga-utils-compilation-uvm-re))

;;;###autoload
(defun larumbe/fpga-uvm-copy-timestamp-vsim ()
  "Copy current UVM timestamp from line at point for Modelsim/Questa regexps."
  (interactive)
  (larumbe/fpga-uvm-copy-timestamp fpga-siemens-vsim-uvm-compile-re))


(provide 'fpga-larumbe-utils)

;;; fpga-larumbe-utils.el ends here
