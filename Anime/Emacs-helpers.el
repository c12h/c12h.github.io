;;; File:	/C/c12h.github.io/Anime/Emacs-helpers.el
;;;; Purpose:	Emacs commands etc for editing my HTML lists of Anime
;;;; By:	Chris Chittleborough
;;;;
;;;; NOTE: The c12h-check-anime-index function is intended for use from Emacs
;;;; batch mode -- hence its bizarre behaviour of exiting Emacs with an error
;;;; status if it finds a problem.  Suggested usage:
;;;;	emacs --batch -Q -l Emacs-helpers.elc index.html -f c12h-check-anime-index
;;;; or, less tersely:
;;;;	emacs --batch -Q  --load Emacs-helpers.elc  --visit index.html	\
;;;;						    --funcall c12h-check-anime-index
;;;;

;;; Modify MHTML mode and its JavaScript “submode” (which is actually js-mode)
;;; to understand the keystroke sequence “C-c a”.
;;;
;;; (Because I've configured Emacs to use js2-mode for .js files etc, “C-c a”
;;; will normally not do anything in those files.)
;;;
;;; Define the “C-c a” sequence to:
;;;	- insert JavaScript code calling add_show whenever in js-mode
;;;	- update the table of links to YY-*.html files when in ordinary HTML mode
;;;	  (which will signal an error if not in my "…/Anime/index.html" or a file
;;;	   greatly resembling it).
;;; Note the context dependence!
;;;
(when (boundp 'mhtml-mode-map)
  (define-key js-mode-map    "\^Ca" 'c12h-insert-add_show-call)
  (define-key mhtml-mode-map "\^Ca" 'c12h-update-anime-index))


;;; This command inserts JavaScript code calling add_show(), the core function defined
;;; by my "animelist.js" file.

;;; The inserted code will need editing: it will contain a JavaScript syntax
;;; error (no 8th argument) and have too many empty strings.
;;;
(defun c12h-insert-add_show-call ()
  "Insert JavaScript calling add_show() function, for Anime lists.
First, move forward to first line starting with anything other than tab or
space.  Leave one empty line at start and end of inserted text."
  (interactive "*")
  (save-match-data
    (re-search-forward "\n[^\t ]" nil t)
    (backward-char)
    (let ((bol (point)))
      (skip-chars-backward "\n\t ")
      (delete-region (point) bol))
    ;;
    (insert "\n\nadd_show(\"\", \"\",\n"
	    "\t rToTryMaybe, \"?\","
	    "\t UnknownSite, \"X\", UnknownSchedule,"
	    "\t , NoTvT, \"\",\n\t [ \"\", \"\", \"\", \"\" ]);\n\n")
    ))


;;; This command automagically updates the body of the (first) table in
;;; my …/Anime/index.html from files found in the same directory,
;;; or says that the table is already correct.
;;; It signals an error if the current buffer does not look like my
;;;  …/Anime/index.html file.
;;;
(defun c12h-update-anime-index ()
  "Regenerate body of <table id=l> element for …/Anime/index.html.
Signal an error if the current buffer does not look like that file.

Scan the current directory for YY-{01,04,07,10,[A-Za-z]*}.html files and
generate HTML text of table rows linking to them.  Replace the <tr><td>… rows
in the table body by that text unless they are already identical, and report in
the echo are whether any changes were made."
  (interactive "*")
  (c12h--check-maybe-update-anime-index 'yes-fix-the-HTML))


;;; This function is intended to be called only in Emacs’s batch mode.
;;; It generates HTML text for the body of the <table id=l> element
;;; and EXITS EMACS with an error status if the current body is not
;;; identical to that text.
;;; XXX???FIXME: WHAT HAPPENS IF THE THING SIGNALS?
;;;
(defun c12h-check-anime-index ()
  "For batch mode, check <table id=l> in …/Anime/index.html etc.
Generate table body HTML text containing links to any files in the current
directory named YY-{01,04,07,10,[A-Za-z]*}.html; signal an error if none.
Issue a warning and EXIT EMACS with error status if table body in current
buffer differs from that text.  Never changes current buffer."
  (when (c12h--check-maybe-update-anime-index nil)
    (message "Please update the table of links in %S" buffer-file-name)
    (kill-emacs 1)))


;;; This helper function checks or updates the <tbody> of the <table id=l>
;;; element in …/Anime/index.html (or a similar file); it signals an error if
;;; the current buffer does not match expectations.
;;; It generates the text of a <tbody> linking to all files named
;;; YY-{01,04,07,10,[A-Za-z]*}.html in the same directory (or signals an error
;;; if no such file exists).
;;; If and only if MAYBE-UPDATE is set:
;;; 	- If the current table body differs at all from the generated text,
;;;	  this function replaces the table body with that text and says so.
;;;	- Otherwise, this function just says that no change is needed.
;;; In any case, it returns t if the table body is (or was) different, or nil if
;;; the current text is OK.
;;;
;;; The buffer must look like
;;;	…
;;;	<table id=l>
;;;	…
;;;	<tbody>
;;;	…
;;;	</tbody>
;;;	…
;;;	</table>
;;;	…
;;; where “…” means any (non-matching) lines.
;;; Note that no extra white-space is allowed in the specified lines!
;;;
(defun c12h--check-maybe-update-anime-index (maybe-update)
  "Regenerate or check <table id=l> body for …/Anime/index.html.
Search current buffer for <tbody> element in a “<table id=l>” element.  If not
found, signal an error.

Look for files named YY-{01,04,07,10,[A-Za-z]*}.html in the current directory
and generate HTML of <tbody> containing rows linking to those files.  Check
whether <tbody> in buffer is identical to generated HTML text.

If MAYBE-UPDATE is nil, do not modify buffer.  Otherwise replace <tbody> in
buffer if difference found and say either that buffer has been changed or that
no change is needed.

Returns nil if no change needed, t if <tbody> is/was wrong."
  (save-match-data
    ;; Find the start and end of the table body and check for problems.
    (goto-char (point-min))
    (unless (search-forward "\n<table id=l>\n" nil t)
      (error "Cannot find “<table id=l>” in this buffer"))
    (backward-char)			; Cope with "\n<table id=l>\n<tbody>\n"
    (let ((eotable (save-excursion (search-forward "\n</table>\n") (point))))
      (unless eotable
	(error "Cannot find </table> for “<table id=l>” in this buffer"))
      (unless (search-forward "\n<tbody>\n" eotable t)
	(error "Cannot find <tbody> in <table id=l>"))
      (forward-line -1)
      (let ((start (point))		; Start of <tbody>
	    cur-dir)
	(unless (re-search-forward "\n</tbody>\n" eotable t)
	  (error "Cannot find </tbody> in <table id=l>"))
	;; Point is now at start of line after </tbody>
	(unless buffer-file-name
	  (error "Buffer does not have a filename (yet)"))
	(setq cur-dir (file-name-directory buffer-file-name))
	(unless cur-dir
	  (error "Cannot find current directory (!)"))
	;; Recreate HTML text of table body.
	(let* ((new-text (c12h--anime-index-html cur-dir))
	       (ok (string-equal new-text
				 (buffer-substring-no-properties start (point)))))
	  (when maybe-update
	    ;; Replace the table body or say that it needs no change.
	    (if ok
		(message "No changes needed.")
	      (delete-region start (point))
	      (insert new-text)
	      (message "Table of links has been updated.")))
	  (not ok))))))


;; This helper function looks for files named YY-{01,04,07,10,[A-Za-z]*}.html in the
;; current directory. If none are found, it signals an error, otherwise it returns
;; the HTML text of a <tbody> containing one or more table rows of the form
;;	<tr><td>YYYY
;;	 <td>—					; If no YY-01.html
;;	 <td><a href="YY-04.html">YY-04</a>	; If YY-04.html present
;;	 <td><a href="YY-07.html">YY-07</a>	; If YY-07.html present
;;	 <td>—					; If no YY-10.html
;;	 <td><a href="YY-XXX.html">YYYY</a>	; If YY-[A-Za-z]*.html present
;; Does not change the match data.
;;
;; Assumes file NN-* is for year 20NN.
;; FIXME???: treat 99-10.html as 1999-10, etc.
;;
(defun c12h--anime-index-html (cur-dir)
  "Returns HTML <tbody> text for my …/Anime/index.html file.
Generate HTML linking to every file named YY-{01,04,07,10,[A-Za-z]*}.html in
current directory.  Signals an error if no such files."
  ;; Get a list of matching files, sorted in Unicodal order.
  (let ((items (directory-files cur-dir nil
				"^[0-9][0-9]-\\(0[147]\\|10\\|[A-Za-z].*\\)\\.html$")))
    (unless items
      (error "No YY-{01,04,07,10,[A-Za-z]*}.html files in directory %S" cur-dir))
    (with-temp-buffer
      (let ((y (1- (read (substring (car items) 0 2))))
	    filename)
	(insert "<tbody>\n")
	(while items
	  (setq y (1+ y))
	  ;; Insert a row of the table body
	  ;;  - Column 1 is four-digit year.
	  (insert (format "<tr><td>%d\n" (+ 2000 y)))
	  ;;  - Columns 2-5 are em-dash or link to yy-{01,04,07,10}.html
	  (dolist (tail (list "-01.html" "-04.html" "-07.html" "-10.html"))
	    (setq filename (format "%02d%s" y tail))
	    (if (not (and items (string-equal filename (car items))))
		(insert " <td>—\n")
	      (insert (format " <td><a href=\"%s\">%02d%s</a>\n"
			      filename y (substring tail 0 3)))
	      (setq items (cdr items))))
	  ;; - Column 6 is em-dash or link to yy-[A-Za-z]*.html
	  (setq filename (if items (car items) ""))
	  (if (not (and items (string-match-p "^[0-9][0-9]-[A-Za-z]" filename)))
	      (insert " <td>—\n")
	    (insert (format " <td><a href=\"%s\">20%02d</a>\n"
			    filename y))
	    (setq items (cdr items))))
	(insert "</tbody>\n"))
      (buffer-substring-no-properties (point-min) (point-max)))))
