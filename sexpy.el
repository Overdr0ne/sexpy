;;; sexpy.el --- sexpy notes for lispers -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Sam

;; Author: Sam <scmorris.dev@gmail.com>
;; Keywords: lisp, data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl-lib))

(define-abbrev-table 'sexpy-mode-abbrev-table ()
  "Abbrev table for Sexpy mode.")

(eval-and-compile
 (defconst sexpy-mode-symbol-regexp "\\(?:\\sw\\|\\s_\\|\\\\.\\)+"))

;;;; Font-lock support.

(defun sexpy-adaptive-fill ()
  "Return fill prefix found at point.
Value for `adaptive-fill-function'."
  ;; Adaptive fill mode gets the fill wrong for a one-line paragraph made of
  ;; a single docstring.  Let's fix it here.
  (if (looking-at "\\s-+\"[^\n\"]+\"\\s-*$") ""))

(defun sexpy-mode-variables (&optional sexpy-syntax keywords-case-insensitive)
  "Common initialization routine for lisp modes.
KEYWORDS-CASE-INSENSITIVE non-nil means that for
font-lock keywords will not be case sensitive."
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'sexpy-fill-paragraph)
  (setq-local adaptive-fill-function #'sexpy-adaptive-fill)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because sexpy-fill-paragraph should do the job.
  ;;  I believe that newcomment's auto-fill code properly deals with it  -stef
  ;;(set (make-local-variable 'adaptive-fill-mode) nil)
  (setq-local indent-line-function 'sexpy-indent-line)
  (setq-local indent-region-function 'sexpy-indent-region)
  (setq-local comment-indent-function #'sexpy-comment-indent)
  (setq-local outline-regexp ";;;\\(;* [^ \t\n]\\|###autoload\\)\\|(")
  (setq-local outline-level 'sexpy-outline-level)
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1)		;default to `;;' in comment-region
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)
  (setq-local multibyte-syntax-as-symbol t)
  ;; (setq-local syntax-begin-function 'beginning-of-defun)  ;;Bug#16247.
  (setq-local electric-pair-skip-whitespace 'chomp)
  (setq-local electric-pair-open-newline-between-pairs nil))

(defun sexpy-outline-level ()
  "Sexpy mode `outline-level' function."
  (let ((len (- (match-end 0) (match-beginning 0))))
    (if (looking-at "(\\|;;;###autoload")
	1000
      len)))

(defvar sexpy-mode-shared-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (define-key map "\e\C-q" 'indent-sexp)
    (define-key map "\177" 'backward-delete-char-untabify)
    ;; This gets in the way when viewing a Sexpy file in view-mode.  As
    ;; long as [backspace] is mapped into DEL via the
    ;; function-key-map, this should remain disabled!!
    ;;;(define-key map [backspace] 'backward-delete-char-untabify)
    map)
  "Keymap for commands shared by all sorts of Sexpy modes.")

(defcustom sexpy-mode-hook nil
  "Hook run when entering Sexpy mode."
  :options '(imenu-add-menubar-index)
  :type 'hook
  :group 'lisp)

(defcustom sexpy-interaction-mode-hook nil
  "Hook run when entering Sexpy Interaction mode."
  :options '(eldoc-mode)
  :type 'hook
  :group 'lisp)

;;; Generic Sexpy mode.

(defvar sexpy-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap "Sexpy")))
    (set-keymap-parent map sexpy-mode-shared-map)
    (bindings--define-key map [menu-bar lisp] (cons "Sexpy" menu-map))
    (bindings--define-key menu-map [ind-sexp]
                          '(menu-item "Indent sexp" indent-sexp
		                      :help "Indent each line of the list starting just after point"))
    map)
  "Keymap for ordinary Sexpy mode.
All commands in `sexpy-mode-shared-map' are inherited by this map.")

(define-derived-mode sexpy-mode prog-mode "Sexpy"
  "Major mode for editing Sexpy code.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.

\\{sexpy-mode-map}"
  (sexpy-mode-variables nil t)
  (setq-local find-tag-default-function 'sexpy-find-tag-default)
  (setq-local comment-start-skip
	      "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq imenu-case-fold-search t))

(defun sexpy-find-tag-default ()
  (let ((default (find-tag-default)))
    (when (stringp default)
          (if (string-match ":+" default)
              (substring default (match-end 0))
	    default))))

(defun sexpy-comment-indent ()
  "Like `comment-indent-default', but don't put space after open paren."
  (or (when (looking-at "\\s<\\s<")
            (let ((pt (point)))
              (skip-syntax-backward " ")
              (if (eq (preceding-char) ?\()
                  (cons (current-column) (current-column))
                (goto-char pt)
                nil)))
      (comment-indent-default)))

(define-obsolete-function-alias 'sexpy-mode-auto-fill 'do-auto-fill "23.1")

(defcustom sexpy-indent-offset nil
  "If non-nil, indent second line of expressions that many more columns."
  :group 'lisp
  :type '(choice (const nil) integer))
(put 'sexpy-indent-offset 'safe-local-variable
     (lambda (x) (or (null x) (integerp x))))

(defun sexpy-ppss (&optional pos)
  "Return Parse-Partial-Sexp State at POS, defaulting to point.
Like `syntax-ppss' but includes the character address of the last
complete sexp in the innermost containing list at position
2 (counting from 0).  This is important for sexpy indentation."
  (unless pos (setq pos (point)))
  (let ((pss (syntax-ppss pos)))
    (if (nth 9 pss)
        (let ((sexp-start (car (last (nth 9 pss)))))
          (parse-partial-sexp sexp-start pos nil nil (syntax-ppss sexp-start)))
      pss)))

(cl-defstruct (sexpy-indent-state
               (:constructor nil)
               (:constructor sexpy-indent-initial-state
                             (&aux (ppss (sexpy-ppss))
                                   (ppss-point (point))
                                   (stack (make-list (1+ (car ppss)) nil)))))
              stack ;; Cached indentation, per depth.
              ppss
              ppss-point)

(defun sexpy-indent-calc-next (state)
  "Move to next line and return calculated indent for it.
STATE is updated by side effect, the first state should be
created by `sexpy-indent-initial-state'.  This function may move
by more than one line to cross a string literal."
  (pcase-let* (((cl-struct sexpy-indent-state
                           (stack indent-stack) ppss ppss-point)
                state)
               (indent-depth (car ppss)) ; Corresponding to indent-stack.
               (depth indent-depth))
              ;; Parse this line so we can learn the state to indent the
              ;; next line.
              (while (let ((last-sexp (nth 2 ppss)))
                       (setq ppss (parse-partial-sexp
                                   ppss-point (progn (end-of-line) (point))
                                   nil nil ppss))
                       ;; Preserve last sexp of state (position 2) for
                       ;; `calculate-sexpy-indent', if we're at the same depth.
                       (if (and (not (nth 2 ppss)) (= depth (car ppss)))
                           (setf (nth 2 ppss) last-sexp)
                         (setq last-sexp (nth 2 ppss)))
                       (setq depth (car ppss))
                       ;; Skip over newlines within strings.
                       (and (not (eobp)) (nth 3 ppss)))
                (let ((string-start (nth 8 ppss)))
                  (setq ppss (parse-partial-sexp (point) (point-max)
                                                 nil nil ppss 'syntax-table))
                  (setf (nth 2 ppss) string-start) ; Finished a complete string.
                  (setq depth (car ppss)))
                (setq ppss-point (point)))
              (setq ppss-point (point))
              (let* ((depth-delta (- depth indent-depth)))
                (cond ((< depth-delta 0)
                       (setq indent-stack (nthcdr (- depth-delta) indent-stack)))
                      ((> depth-delta 0)
                       (setq indent-stack (nconc (make-list depth-delta nil)
                                                 indent-stack)))))
              (prog1
                  (let (indent)
                    (cond ((= (forward-line 1) 1)
                           ;; Can't move to the next line, apparently end of buffer.
                           nil)
                          ((null indent-stack)
                           ;; Negative depth, probably some kind of syntax
                           ;; error.  Reset the state.
                           (setq ppss (parse-partial-sexp (point) (point))))
                          ((car indent-stack))
                          ((integerp (setq indent (calculate-sexpy-indent ppss)))
                           (setf (car indent-stack) indent))
                          ((consp indent)       ; (COLUMN CONTAINING-SEXP-START)
                           (car indent))
                          ;; This only happens if we're in a string, but the
                          ;; loop should always skip over strings (unless we hit
                          ;; end of buffer, which is taken care of by the first
                          ;; clause).
                          (t (error "This shouldn't happen"))))
                (setf (sexpy-indent-state-stack state) indent-stack)
                (setf (sexpy-indent-state-ppss-point state) ppss-point)
                (setf (sexpy-indent-state-ppss state) ppss))))

(defun sexpy-indent-region (start end)
  "Indent region as Lisp code, efficiently."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (beginning-of-line)
    ;; The default `indent-region-line-by-line' doesn't hold a running
    ;; parse state, which forces each indent call to reparse from the
    ;; beginning.  That has O(n^2) complexity.
    (let* ((parse-state (sexpy-indent-initial-state))
           (pr (unless (minibufferp)
                       (make-progress-reporter "Indenting region..." (point) end))))
      (let ((ppss (sexpy-indent-state-ppss parse-state)))
        (unless (or (and (bolp) (eolp)) (nth 3 ppss))
                (sexpy-indent-line (calculate-sexpy-indent ppss))))
      (let ((indent nil))
        (while (progn (setq indent (sexpy-indent-calc-next parse-state))
                      (< (point) end))
          (unless (or (and (bolp) (eolp)) (not indent))
                  (sexpy-indent-line indent))
          (and pr (progress-reporter-update pr (point)))))
      (and pr (progress-reporter-done pr))
      (move-marker end nil))))

(defun sexpy-indent-line (&optional indent)
  "Indent current line as Sexpy code."
  (interactive)
  (let ((pos (- (point-max) (point)))
        (indent (progn (beginning-of-line)
                       (or indent (calculate-sexpy-indent (sexpy-ppss))))))
    (skip-chars-forward " \t")
    (if (or (null indent) (looking-at "\\s<\\s<\\s<"))
	;; Don't alter indentation of a ;;; comment line
	;; or a line that starts in a string.
        ;; FIXME: inconsistency: comment-indent moves ;;; to column 0.
	(goto-char (- (point-max) pos))
      (if (and (looking-at "\\s<") (not (looking-at "\\s<\\s<")))
	  ;; Single-semicolon comment lines should be indented
	  ;; as comment lines, not as code.
	  (progn (indent-for-comment) (forward-char -1))
	(if (listp indent) (setq indent (car indent)))
        (indent-line-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))))

(defvar calculate-sexpy-indent-last-sexp)

(defun calculate-sexpy-indent (&optional parse-start)
  "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

PARSE-START may be a buffer position to start parsing from, or a
parse state as returned by calling `parse-partial-sexp' up to the
beginning of the current line.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-sexpy-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start) (beginning-of-defun))
            (t (setq state parse-start)))
      (unless state
              ;; Find outermost containing sexp
              (while (< (point) indent-point)
                (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry
		  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-sexpy-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-sexpy-indent-last-sexp
		 (> calculate-sexpy-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-sexpy-indent-last-sexp
					    indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-sexpy-indent-last-sexp)
	    ;; indent-point immediately follows open paren.
	    ;; Don't call hook.
            (setq desired-indent (current-column))
	  ;; Find the start of first element of containing sexp.
	  (parse-partial-sexp (point) calculate-sexpy-indent-last-sexp 0 t)
	  (cond ((looking-at "\\s(")
		 ;; First element of containing sexp is a list.
		 ;; Indent under that list.
		 )
		((> (save-excursion (forward-line 1) (point))
		    calculate-sexpy-indent-last-sexp)
		 ;; This is the first line to start within the containing sexp.
		 ;; It's almost certainly a function call.
		 (if (= (point) calculate-sexpy-indent-last-sexp)
		     ;; Containing sexp has nothing before this line
		     ;; except the first element.  Indent under that element.
		     nil
		   ;; Skip the first element, find start of second (the first
		   ;; argument of the function call) and indent under.
		   (progn (forward-sexp 1)
			  (parse-partial-sexp (point)
					      calculate-sexpy-indent-last-sexp
					      0 t)))
		 (backward-prefix-chars))
		(t
		 ;; Indent beneath first sexp on same line as
		 ;; `calculate-sexpy-indent-last-sexp'.  Again, it's
		 ;; almost certainly a function call.
		 (goto-char calculate-sexpy-indent-last-sexp)
		 (beginning-of-line)
		 (parse-partial-sexp (point) calculate-sexpy-indent-last-sexp
				     0 t)
		 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by sexpy-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
	       nil)
              ((and (integerp sexpy-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) sexpy-indent-offset))
              ;; in this case calculate-sexpy-indent-last-sexp is not nil
              (calculate-sexpy-indent-last-sexp
               (and (save-excursion
                      (goto-char indent-point)
                      (skip-chars-forward " \t")
                      (looking-at ":"))
                    ;; The last sexp may not be at the indentation
                    ;; where it begins, so find that one, instead.
                    (save-excursion
                      (goto-char calculate-sexpy-indent-last-sexp)
		      ;; Handle prefix characters and whitespace
		      ;; following an open paren.  (Bug#1012)
                      (backward-prefix-chars)
                      (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                    (line-beginning-position))
                                      (and containing-sexp
                                           (>= (1+ containing-sexp) (point)))))
                        (forward-sexp -1)
                        (backward-prefix-chars))
                      (setq calculate-sexpy-indent-last-sexp (point)))
                    (> calculate-sexpy-indent-last-sexp
                       (save-excursion
                         (goto-char (1+ containing-sexp))
                         (parse-partial-sexp (point) calculate-sexpy-indent-last-sexp 0 t)
                         (point)))
                    (let ((parse-sexp-ignore-comments t)
                          indent)
                      (goto-char calculate-sexpy-indent-last-sexp)
                      (or (and (looking-at ":")
                               (setq indent (current-column)))
                          (and (< (line-beginning-position)
                                  (prog2 (backward-sexp) (point)))
                               (looking-at ":")
                               (setq indent (current-column))))
                      indent))
               ;; another symbols or constants not preceded by a constant
               ;; as defined above.
               normal-indent)
              ;; in this case calculate-sexpy-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent))))))

(defun indent-sexp (&optional endpos)
  "Indent each line of the list starting just after point.
If optional arg ENDPOS is given, indent each line, stopping when
ENDPOS is encountered."
  (interactive)
  (let* ((parse-state (sexpy-indent-initial-state)))
    ;; We need a marker because we modify the buffer
    ;; text preceding endpos.
    (setq endpos (copy-marker
                  (if endpos endpos
                    ;; Get error now if we don't have a complete sexp
                    ;; after point.
                    (save-excursion
                      (forward-sexp 1)
                      (let ((eol (line-end-position)))
                        ;; We actually look for a sexp which ends
                        ;; after the current line so that we properly
                        ;; indent things like #s(...).  This might not
                        ;; be needed if Bug#15998 is fixed.
                        (when (and (< (point) eol)
                                   ;; Check if eol is within a sexp.
                                   (> (nth 0 (save-excursion
                                               (parse-partial-sexp
                                                (point) eol)))
                                      0))
                              (condition-case ()
                                  (while (< (point) eol)
                                    (forward-sexp 1))
                                ;; But don't signal an error for incomplete
                                ;; sexps following the first complete sexp
                                ;; after point.
                                (scan-error nil))))
                      (point)))))
    (save-excursion
      (while (let ((indent (sexpy-indent-calc-next parse-state))
                   (ppss (sexpy-indent-state-ppss parse-state)))
               ;; If the line contains a comment indent it now with
               ;; `indent-for-comment'.
               (when (and (nth 4 ppss) (<= (nth 8 ppss) endpos))
                     (save-excursion
                       (goto-char (sexpy-indent-state-ppss-point parse-state))
                       (indent-for-comment)
                       (setf (sexpy-indent-state-ppss-point parse-state)
                             (line-end-position))))
               (when (< (point) endpos)
                     ;; Indent the next line, unless it's blank, or just a
                     ;; comment (we will `indent-for-comment' the latter).
                     (skip-chars-forward " \t")
                     (unless (or (eolp) (not indent)
                                 (eq (char-syntax (char-after)) ?<))
                             (indent-line-to indent))
                     t))))
    (move-marker endpos nil)))

(defun indent-pp-sexp (&optional arg)
  "Indent each line of the list starting just after point, or prettyprint it.
A prefix argument specifies pretty-printing."
  (interactive "P")
  (if arg
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (progn (forward-sexp 1) (point)))
          (pp-buffer)
          (goto-char (point-max))
          (if (eq (char-before) ?\n)
              (delete-char -1)))))
  (indent-sexp))

;;;; Lisp paragraph filling commands.

(defun sexpy-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handle Emacs Lisp comments and docstrings.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial semicolons."
  (interactive "P")
  (or (fill-comment-paragraph justify)
      ;; Since fill-comment-paragraph returned nil, that means we're not in
      ;; a comment: Point is on a program line; we are interested
      ;; particularly in docstring lines.
      ;;
      ;; We bind `paragraph-start' and `paragraph-separate' temporarily.  They
      ;; are buffer-local, but we avoid changing them so that they can be set
      ;; to make `forward-paragraph' and friends do something the user wants.
      ;;
      ;; `paragraph-start': The `(' in the character alternative and the
      ;; left-singlequote plus `(' sequence after the \\| alternative prevent
      ;; sexps and backquoted sexps that follow a docstring from being filled
      ;; with the docstring.  This setting has the consequence of inhibiting
      ;; filling many program lines that are not docstrings, which is sensible,
      ;; because the user probably asked to fill program lines by accident, or
      ;; expecting indentation (perhaps we should try to do indenting in that
      ;; case).  The `;' and `:' stop the paragraph being filled at following
      ;; comment lines and at keywords (e.g., in `defcustom').  Left parens are
      ;; escaped to keep font-locking, filling, & paren matching in the source
      ;; file happy.  The `:' must be preceded by whitespace so that keywords
      ;; inside of the docstring don't start new paragraphs (Bug#7751).
      ;;
      ;; `paragraph-separate': A clever regexp distinguishes the first line of
      ;; a docstring and identifies it as a paragraph separator, so that it
      ;; won't be filled.  (Since the first line of documentation stands alone
      ;; in some contexts, filling should not alter the contents the author has
      ;; chosen.)  Only the first line of a docstring begins with whitespace
      ;; and a quotation mark and ends with a period or (rarely) a comma.
      ;;
      ;; The `fill-column' is temporarily bound to
      (let ((paragraph-start
             (concat paragraph-start
                     "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
	    (paragraph-separate
	     (concat paragraph-separate "\\|\\s-*\".*[,\\.]$")))
	(fill-paragraph justify))
      ;; Never return nil.
      t))

(defun indent-code-rigidly (start end arg &optional nochange-regexp)
  "Indent all lines of code, starting in the region, sideways by ARG columns.
Does not affect lines starting inside comments or strings, assuming that
the start of the region is not inside them.

Called from a program, takes args START, END, COLUMNS and NOCHANGE-REGEXP.
The last is a regexp which, if matched at the beginning of a line,
means don't indent that line."
  (interactive "r\np")
  (let (state)
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp)
	  (setq state (parse-partial-sexp (point)
					  (progn
					    (forward-line 1) (point))
					  nil nil state)))
      (while (< (point) end)
	(or (car (nthcdr 3 state))
	    (and nochange-regexp
		 (looking-at nochange-regexp))
	    ;; If line does not start in string, indent it
	    (let ((indent (current-indentation)))
	      (delete-region (point) (progn (skip-chars-forward " \t") (point)))
	      (or (eolp)
		  (indent-to (max 0 (+ indent arg)) 0))))
	(setq state (parse-partial-sexp (point)
					(progn
					  (forward-line 1) (point))
					nil nil state))))))

(provide 'sexpy)
;;; sexpy.el ends here
