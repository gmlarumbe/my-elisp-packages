;;; simple-utils.el --- Simple.el.gz Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; INFO: Same as `newline' but withouth the (interactive "*" form):
;;  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Interactive.html
;;  - If ‘*’ appears at the beginning of the string, then an error is signaled if the buffer is read-only.
;;  - This prevents signaling an error when pressing C-m (RET) if buffer is read-only
;;  i.e. var `buffer-read-only' was non-nil.
;;  - If this was the case, the function `larumbe/newline-advice' could not be used properly
;;  on read-only buffers to kill xref/help/ag popups since the read-only error had
;;  precedence over the call to `larumbe/newline-advice'. This seems to be implemented
;;  in C instead of Elisp and therefore is not possible to easily override it.
;;  - Plus, this function calls `barf-if-buffer-read-only' so the (interactive "*") check
;;  in C seems redundant.
;;
;; - Copied from <emacs-dir>/share/emacs/30.0.50/lisp/simple.el.gz
;; - Tried creating a `larumbe/newline' function and adding both
;; :override and :before-until advices to newline but did not seem to work,
;; (might be due to order of advice dependency).

(defun newline (&optional arg interactive)
  "Insert a newline, and move to left margin of the new line.
With prefix argument ARG, insert that many newlines.

If `electric-indent-mode' is enabled, this indents the final new line
that it adds, and reindents the preceding line.  To just insert
a newline, use \\[electric-indent-just-newline].

If `auto-fill-mode' is enabled, this may cause automatic line
breaking of the preceding line.  A non-nil ARG inhibits this.

If `use-hard-newlines' is enabled, the newline is marked with the
text-property `hard'.

A non-nil INTERACTIVE argument means to run the `post-self-insert-hook'."
  (interactive "P\np") ; DANGER: Only change with respect to original one
  (barf-if-buffer-read-only)
  (when (and arg
             (< (prefix-numeric-value arg) 0))
    (error "Repetition argument has to be non-negative"))
  ;; Call self-insert so that auto-fill, abbrev expansion etc. happen.
  ;; Set last-command-event to tell self-insert what to insert.
  (let* ((was-page-start (and (bolp) (looking-at page-delimiter)))
         (beforepos (point))
         (last-command-event ?\n)
         ;; Don't auto-fill if we have a prefix argument.
         (inhibit-auto-fill (or inhibit-auto-fill arg))
         (arg (prefix-numeric-value arg))
         (procsym (make-symbol "newline-postproc")) ;(bug#46326)
         (postproc
          ;; Do the rest in post-self-insert-hook, because we want to do it
          ;; *before* other functions on that hook.
          (lambda ()
            (remove-hook 'post-self-insert-hook procsym t)
            ;; Mark the newline(s) `hard'.
            (if use-hard-newlines
                (set-hard-newline-properties
                 (- (point) arg) (point)))
            ;; If the newline leaves the previous line blank, and we
            ;; have a left margin, delete that from the blank line.
            (save-excursion
              (goto-char beforepos)
              (beginning-of-line)
              (and (looking-at "[ \t]+$")
                   (> (current-left-margin) 0)
                   (delete-region (point)
                                  (line-end-position))))
            ;; Indent the line after the newline, except in one case:
            ;; when we added the newline at the beginning of a line that
            ;; starts a page.
            (or was-page-start
                (move-to-left-margin nil t)))))
    (fset procsym postproc)
    (if (not interactive)
        ;; FIXME: For non-interactive uses, many calls actually
        ;; just want (insert "\n"), so maybe we should do just
        ;; that, so as to avoid the risk of filling or running
        ;; abbrevs unexpectedly.
        (let ((post-self-insert-hook (list postproc)))
          (self-insert-command arg))
      (unwind-protect
          (progn
            (add-hook 'post-self-insert-hook procsym nil t)
            (self-insert-command arg))
        ;; We first used let-binding to protect the hook, but that
        ;; was naive since add-hook affects the symbol-default
        ;; value of the variable, whereas the let-binding might
        ;; protect only the buffer-local value.
        (remove-hook 'post-self-insert-hook procsym t))))
  nil)


;;;###autoload
(defun larumbe/next-error-find-buffer-function (&optional avoid-current
                                                          extra-test-inclusive
                                                          extra-test-exclusive)
  "Customized `next-error-find-buffer-function'.
Let flycheck and compilation-based coexist in a sensible manner.

Flycheck sets the value `flycheck-next-error-function' to `next-error-function' when mode is enabled.
This is needed so that `next-error-buffer-p' can detect current buffer as an active `next-error' buffer.
This does not happen since current buffer is only detected as an active `next-error' buffer when all the
rest of the default searches in `next-error' fail, if `next-error-find-buffer-function' is not set.
On the other hand, using `next-error-buffer-on-selected-frame' as `next-error-find-buffer-function' does
not work either, because it has the `_avoid-current' default set to t in `next-error-buffer-p' call,
effectively ignoring current-buffer, even if flycheck is enabled.

Summary: it is needed to set our own `next-error-find-buffer-function' that takes current buffer in
flycheck mode into account. This way, flycheck will add the value `flycheck-display-error-at-point' to
`next-error-hook', since the current buffer is selected as the error buffer.

Principles:

- 1st) If there is an active next-error search, e.g. any of the values in
`larumbe/compilation-based-search-buffer-list' (plus any compilation-derived mode
without the *Help* buffer), is active in current window, return this buffer.
- 2nd) If current buffer has flycheck enabled, use current buffer with flycheck.
- 3rd) Fallback to default behaviour returning nil and letting `next-error' do its magic.

Inspired by `next-error-buffer-on-selected-frame'."
  (let ((frame-windows-buffer-list (mapcar (lambda (win)
                                             (window-buffer win))
                                           (window-list)))
        (first-search-buffers '("*xref*" "*ag search*" "*ripgrep-search*")))
    (or (catch 'found
          ;; First check if there is an xref, ag, ripgrep buffer (in this order/priority)
          (dolist (buf frame-windows-buffer-list)
            (with-current-buffer buf
              (dolist (search-buf first-search-buffers)
                (when (string= (buffer-name buf) search-buf)
                  (throw 'found buf)))))
          ;; Otherwise look for next-error capable buffers in current window (e.g. *compilation*)
          ;; that are not flycheck buffers
          (dolist (buf frame-windows-buffer-list)
            (with-current-buffer buf
              (when (and (next-error-buffer-p buf nil extra-test-inclusive extra-test-exclusive) ; INFO: Do not ignore current buffer!
                         (not (eq next-error-function 'flycheck-next-error-function)))
                (throw 'found buf))))
          ;; Finally check ONLY current flycheck buffer
          (when (eq next-error-function 'flycheck-next-error-function)
            (throw 'found (current-buffer))))
        ;; Else return error of not found buffers
        (error "No buffers contain error message locations"))))


(provide 'simple-utils)

;;; simple-utils.el ends here
