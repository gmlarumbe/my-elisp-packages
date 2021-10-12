;;; larumbe-macros.el --- Macros  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Own macros for EXWM Automation
;;
;; 1) Go to EXWM buffer
;; 2) F3 (start recording macro)
;; 3) record macro
;; 4) F4 (stop recording macro)
;; 5) M-x kmacro-name-last-macro
;; 6) insert-kbd-macro
;;
;; 7a)  Assign kmacro-lambda-form output vector to a fset and execute via `execute-kbd-macro'
;;        (fset 'copy-firefox-link [?\C-l ?\M-w ?\C-l])
;;        (execute-kbd-macro 'copy-firefox-link)
;;
;; https://stackoverflow.com/questions/28039958/emacs-call-a-kbd-macro-in-defun-function
;;
;;
;; 7b)  Assign whole output to a fset and execute via M-x
;;        (fset 'copy-firefox-link (kmacro-lambda-form [?\C-l ?\M-w ?\C-l] 0 "%d"))
;;        (copy-firefox-link)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; For elmacro:
;;   - Record Macro
;;   - elmacro-show-last-macro
;;   - copy/use it as desired!
;;
;; https://emacs.stackexchange.com/questions/70/how-to-save-a-keyboard-macro-as-a-lisp-function
;;
;;; Code:


;; INFO: Discouraged method of handling macros for EXWM buffers
;; Use better the `exwm-input--fake-key' function.
(fset 'open-google
   [?\C-c ?\C-q ?\C-l ?w ?w ?w ?. ?g ?o ?o ?g ?l ?e ?. ?c ?o ?m return return])

;;;###autoload
(defun larumbe/macros-exwm-firefox-open-google ()
  "Script for opening a search engine in an EXWM buffer."
  (interactive)
  (start-process-shell-command "firefox" nil "firefox")
  (sleep-for 1)
  (switch-to-buffer "Firefox")
  (execute-kbd-macro 'open-google))


;; TODO: Not working correctly yet.. Some tweaking might be necessary.
;; EXWM buffers are handled specially due to this thing of simulation keys.
;;;###autoload
(defun larumbe/macros-exwm-firefox-youtube-dl ()
  "Download MP3 from current's Firefox window link."
  (interactive)
  (let ((dir "~/youtube-dl-mp3")
        (link)
        (buf)
        (cmd))
    (save-window-excursion
      (switch-to-buffer "Firefox")
      (exwm-input--fake-key ?\C-l) ; Get URL
      (exwm-input--fake-key ?\C-c) ; Copy to clipboard
      ;; (exwm-input--fake-key 'f6)   ; Go back from URL. TODO: Still doesn't seem to fully work
      (setq link (current-kill 0))
      (larumbe/ansi-term-new)
      (setq buf (get-buffer-process (current-buffer)))
      (setq cmd (concat
                 "mkdir -p " dir " && "
                 "cd " dir " && "
                 "youtube-dl --extract-audio --audio-format mp3 " link
                 "\n"))
      (comint-send-string buf cmd))))


(provide 'larumbe-macros)

;;; larumbe-macros.el ends here
