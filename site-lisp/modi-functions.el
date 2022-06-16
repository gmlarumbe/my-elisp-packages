;;; modi-functions.el --- Kaushal Modi various functions  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defvar modi/rwins-max 100
  "Default maximum number of replacements.")

(defvar modi/rwins-incr 1
  "Default number by which the number suffixes will increment in the
replacements.")

;;;###autoload
(defun modi/replace-with-incr-num-suffix (start)
  "Replace selected region/symbol at point with incrementing number suffixes.

If START is non-nil, the replacements will be suffixes with the START number
and increment by 1 on each replacement.

If START is nil and if the selected region or symbol already ends in a number,
the replacements will use that number as the START number.

If START is nil and if the selected region or symbol does NOT end in a number,
the replacements will use 1 as the START number.

`modi/rwins-max' controls the maximum number till which the suffix number
increments. After the max number is reached, the suffixes will restart from
START (behavior of `map-query-replace-regexp').

`modi/rwins-incr' controls the increments between the number suffixes in
consecutive replacements.

  Example:
  Initial text:
     Here3 Here3 Here3 Here3 Here3
  After replacement text:
     Here3 Here4 Here5 Here6 Here7

Note that the selected region cannot contain any spaces."
  (interactive "p")
  (let (raw-str beg non-number-str to-strings)
    (cond ((use-region-p)
           (setq raw-str (buffer-substring-no-properties
                          (region-beginning) (region-end)))
           (setq beg (region-beginning)))
          ((symbol-at-point)
           (setq raw-str (substring-no-properties
                          (symbol-name (symbol-at-point))))
           (setq beg (car (bounds-of-thing-at-point 'symbol)))))
    (if (string-match "\\b\\(\\w*?\\)\\([0-9]+\\)$" raw-str)
        (progn
          (setq non-number-str (match-string-no-properties 1 raw-str))
          (when (null current-prefix-arg)
            (setq start (string-to-number (match-string-no-properties 2 raw-str)))))
      (setq non-number-str raw-str))
    ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Mapping-Functions.html
    (setq to-strings (mapconcat (lambda (x) (concat non-number-str (number-to-string x)))
                                (number-sequence
                                 start
                                 (+ start (* modi/rwins-incr (1- modi/rwins-max)))
                                 modi/rwins-incr)
                                " "))
    (goto-char beg) ; Go to the start of the selection/symbol
    (map-query-replace-regexp (regexp-quote raw-str) to-strings)))



;; http://emacs.stackexchange.com/q/7519/115
;;;###autoload
(defun modi/pull-up-line ()
  "Join the following line onto the current one.

This is analogous to \\[move-end-of-line] followed by
\\[delete-foward], or \\[universal-argument] \\[delete-indentation],
or \\[universal-argument] \\[join-line].

If the current line is a comment and the pulled-up line is also a
comment, remove the leading comment characters from that line."
  (interactive)
  (join-line -1)
  (when (nth 4 (syntax-ppss))           ;If the current line is a comment
    ;; Remove comment prefix chars from the pulled-up line if present.
    (save-excursion
      ;; Delete all comment-start and space characters, one at a time.
      (while (looking-at (concat "\\s<"  ;Comment-start char as per syntax table
                                 "\\|" (substring comment-start 0 1) ;First char of `comment-start'
                                 "\\|" "[[:blank:]]"))               ;Extra spaces
        (delete-forward-char 1))
      (insert-char ? ))))               ;Insert space



;; Example:
;; (modi/search-replace-pairs '(("larumbe/" . "someone/") ("modi/" . "kmodi/")))
;;;###autoload
(defun modi/search-replace-pairs (sr-pairs)
  "Search/replace in the buffer/region using SR-PAIRS.
SR-PAIRS is a list of cons (SEARCH-REGEX . REPLACE-EXPR) where
the cons elements are strings."
  (let ((cnt 0)
        (beg (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (dolist (pair sr-pairs)
      (let ((search-regex (car pair))
            (replace-expr (cdr pair)))
        (save-restriction
          (narrow-to-region beg end)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward search-regex nil :noerror)
              (replace-match replace-expr)
              (cl-incf cnt))))))
    (message "Finished %d replacements" cnt)))



;; https://emacs.stackexchange.com/questions/14403/how-can-i-copy-syntax-highlighted-code-as-rtf-or-html
(defvar modi/htmlize-output-directory
  (let ((dir (concat temporary-file-directory
                     (getenv "USER") "/.htmlize/"))) ; must end with /
    (make-directory dir :parents)
    dir)
  "Output directory for files exported by `modi/htmlize-region-to-file'.")

(defvar modi/htmlize-css-file (concat straight-base-dir "straight/repos/my-elisp-packages/site-lisp/leuven_theme.css")
  "CSS file to be embedded in the html file created using the `modi/htmlize-region-to-file' function.")

;;;###autoload
(defun modi/htmlize-region-to-file (option)
  "Export the selected region to an html file. If a region is not
selected, export the whole buffer.

The output file is saved to `modi/htmlize-output-directory' and its fontification
is done using `modi/htmlize-css-file'.

If OPTION is non-nil (for example, using `\\[universal-argument]' prefix), copy
the output file name to kill ring.
If OPTION is \\='(16) (using `\\[universal-argument] \\[universal-argument]' prefix),
do the above and also open the html file in the default browser."
  (interactive "P")
  (require 'ox-html) ; INFO: Added by Larumbe
  (let ((org-html-htmlize-output-type 'css)
        (org-html-htmlize-font-prefix "org-")
        (fname (concat modi/htmlize-output-directory
                       (if (buffer-file-name)
                           (file-name-nondirectory (buffer-file-name))
                         "temp")
                       ".html"))
        start end html-string)
    (if (use-region-p)
        (progn
          (setq start (region-beginning))
          (setq end (region-end)))
      (progn
        (setq start (point-min))
        (setq end (point-max))))
    (setq html-string (org-html-htmlize-region-for-paste start end))
    (with-temp-buffer
      ;; Insert the `modi/htmlize-css-file' contents in the temp buffer
      (insert-file-contents modi/htmlize-css-file nil nil nil :replace)
      ;; Go to the beginning of the buffer and insert comments and
      ;; opening tags for `html', `head' and `style'. These are
      ;; inserted *above* the earlier inserted css code.
      (goto-char (point-min))
      (insert (concat "<!-- This file is generated using the "
                      "`modi/htmlize-region-to-file' function\n"
                      "from https://github.com/kaushalmodi/.emacs.d/"
                      "blob/master/setup-files/setup-org.el -->\n"))
      (insert "<html>\n<head>\n<style media=\"screen\" type=\"text/css\">\n")
      ;; Go to the end of the buffer (end of the css code) and
      ;; insert the closing tags for `style' and `head' and opening
      ;; tag for `body'.
      (goto-char (point-max))
      (insert "</style>\n</head>\n<body>\n")
      ;; Insert the HTML for fontified text in `html-string'.
      (insert html-string)
      ;; Close the `body' and `html' tags.
      (insert "</body>\n</html>\n")
      (write-file fname)
      (when option
        (kill-new fname)
        (when (= 16 (car option))
          (browse-url-of-file fname))))))



(provide 'modi-functions)

;;; modi-functions.el ends here
