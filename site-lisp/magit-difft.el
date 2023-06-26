;;; magit-difft.el --- Difftastic integration with Magit  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
;; https://shivjm.blog/better-magit-diffs/
;; https://www.reddit.com/r/emacs/comments/123khq4/better_magit_diffs_with_delta_and_difftastic/
;; https://www.reddit.com/r/emacs/comments/tr42hl/how_to_configure_magit_with_difftastic/
;; https://github.com/Bitnut/diffgit
;;
;;; Code:

(require 'ansi-color)
;; (require 'magit)
(require 'magit-delta)

;;;; Integration
;; https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
(defun th/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))

(defun th/magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (th/magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))

(defun th/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (th/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

(transient-define-prefix th/magit-aux-commands ()
  "My personal auxiliary magit commands."
  ["Auxiliary commands"
   ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
   ("s" "Difftastic Show" th/magit-show-with-difftastic)])

(transient-append-suffix 'magit-dispatch "!"
  '("#" "My Magit Cmds" th/magit-aux-commands))

(define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands)


;; TODO: Still check this one: https://shivjm.blog/better-magit-diffs/
(defun aankh/toggle-magit-delta ()
  (interactive)
  (magit-delta-mode
   (if magit-delta-mode
       -1
     1))
  (magit-refresh))

;; (transient-append-suffix 'magit-diff '(-1 -1 -1)
;;   '("l" "Toggle magit-delta" aankh/toggle-magit-delta))

(defconst +aankh/difftastic-colour-remapping+
  `(("red2" . "#a8353e") ;; https://oklch.com/#50,0.15,20,100
    ("green2" . "#107823")
    ("yellow2" . "#2f3b97")))

(defun aankh/recolor-difftastic ()
  (let ((ovs (overlays-in (point-min) (point-max))))
    (dolist (ov ovs)
      (let ((face (overlay-get ov 'face)))
        (when (and (not (null face)) (listp face))
          (when (plist-get face :foreground)
            (plist-put face :foreground (aankh/get-remapped-difftastic-colour (plist-get face :foreground))))
          (when-let ((existing (cl-find :foreground face :key (lambda (x) (if (consp x) (car x) nil)))))
            (setf face
                  (cl-subst `(:foreground ,(aankh/get-remapped-difftastic-colour (plist-get existing :foreground)))
                            :foreground
                            face
                            :key (lambda (x) (if (consp x) (car x) nil)))))
          (overlay-put ov 'face face))))))

(defun aankh/get-remapped-difftastic-colour (original)
  (alist-get original +aankh/difftastic-colour-remapping+ nil nil 'string=))


;; For some reason, this was being called twice without the guard.
(unless (boundp 'aankh/added-magit-diff-suffixes)
  (transient-append-suffix 'magit-diff '(-1 -1)
    [("l" "Toggle magit-delta" aankh/toggle-magit-delta)
     ("D" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
     ("S" "Difftastic Show" th/magit-show-with-difftastic)]))
(defvar aankh/added-magit-diff-suffixes t)


(provide 'magit-difft)

;;; magit-difft.el ends here
