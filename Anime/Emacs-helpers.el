;;;; File:	/C/c12h.github.io/Anime/Emacs-helpers.el
;;;; Purpose:	Emacs commands etc for editing my HTML lists of Anime
;;;; By:	Chris Chittleborough
;;;;
;;;; The c12h-check-anime-index function is intended for use from Emacs batch mode --
;;;; hence its bizarre behaviour of exiting Emacs with an error status if it finds
;;;; a problem.  Suggested usage:
;;;;	emacs --batch -Q -l Emacs-helpers.elc index.html -f c12h-check-anime-index
;;;; or, for greater readability:
;;;;	emacs --batch -Q  --load Emacs-helpers.elc  --visit index.html	\
;;;;						    --funcall c12h-check-anime-index
;;;;

;;; Define key sequences for the commands in this file.
;;;
;;; Assume MHTML mode, in which you are in js-mode when inside a <script>
;;; element.
;;; Define the “C-c a” sequence to:
;;;	- insert JavaScript code calling add_show when inside a <script>
;;;	- update the table of links to YY-*.html files when in ordinary HTML mode
;;;	  (which will signal an error if not in my "…/Anime/index.html" or a file
;;;	   greatly resembling it).
;;;
(when (boundp 'mhtml-mode-map)
  (define-key js-mode-map    "\^Ca" 'c12h-insert-add_show-call)
  (define-key mhtml-mode-map "\^Ca" 'c12h-update-anime-index))


;;; This command inserts JavaScript code calling add_show(), the core function defined
;;; by my "animelist.js" file.
;;; The inserted code will need editing; for one thing, it will contain a syntax error.
;;;
(defun c12h-insert-add_show-call ()
  "Move forwards to ??? and insert JavaScript call to add_show()."
  (interactive "*")
  (save-match-data
    (skip-chars-backward "\n\t ")
    (re-search-forward "\n[^a/<	]" nil t)
    ;;
    (insert "\nadd_show(\"\", \"\",\n"
	    "\t rToTryMaybe, \"?\","
	    "\t UnknownSite, \"X\", UnknownSchedule,"
	    "\t , NoTvT, \"\",\n\t [ \"\", \"\", \"\", \"\" ]);\n")
    ))


;;; This command automagically updates the body of the (first) table in
;;; my …/Anime/index.html from files found in the same directory.
;;; It signals an error if the current buffer does not look like my
;;;  …/Anime/index.html file.
;;;
(defun c12h-update-anime-index ()
  "Regenerate body of <table id=l> element for …/Anime/index.html.
Generates HTML linking to YY-{01,04,07,10,[A-Za-z]*}.html files in
same directory. If that HTML is identical to current body, issue
message without changing buffer; otherwise replace table body."
  (interactive "*")
  (c12h--check-maybe-update-anime-index 'yes-fix-the-HTML))


;;; This function is intended to be called only in Emacs’s batch mode.
;;; It generates HTML text for the body of the <table id=l> element
;;; and EXITS EMACS with an error status if the current body is not
;;; identical to that text.
;;; XXX???FIXME: WHAT HAPPENS IF THE THING SIGNALS?
;;;
(defun c12h-check-anime-index ()
  "For batch mode: check body of <table id=l> element for …/Anime/index.html.

Generates table body text containing links to files named
YY-{01,04,07,10,[A-Za-z]*}.html in same directory.

Issues warning and exits Emacs with error status
if table body in current buffer differs from that text.
Never changes current buffer."
  (when (c12h--check-maybe-update-anime-index nil)
    (message "Please update the table of links in %S" buffer-file-name)
    (kill-emacs 1)))


;;; This helper function checks or updates the body of the <table id=l> element
;;; in …/Anime/index.html (or a similar file) by generated HTML text linking to
;;; all files named YY-{01,04,07,10,[A-Za-z]*}.html in the same directory.
;;; If MAYBE-UPDATE is set:
;;; 	- if the current table body differs at all from the generated text,
;;;	  this function replaces the table body with that text
;;;	- otherwise, this function just says that no change is needed.
;;; In any case, it returns t if the table body is or was different or nil
;;; if the current text is OK.
;;;
(defun c12h--check-maybe-update-anime-index (maybe-update)
  "Regenerate or check body of <table id=l> element for …/Anime/index.html.
Looks for files named YY-{01,04,07,10,[A-Za-z]*}.html in same directory.
Table body should contain links to every such file.
Returns nil if no change needed, t if table mode is/was wrong.
Leaves buffer unchanged unless MAYBE-UPDATE is non-nil."
  (save-match-data
    ;; Find the start and end of the table body and check for problems.
    (goto-char (point-min))
    (unless (search-forward "\n<table id=l>" nil t)
      (error "Cannot find “<table id=l>” in this buffer"))
    (forward-line)
    (while (looking-at "^\\(<tr[^>]*>$\\| *<th>\\)")
      (forward-line))
    (unless (looking-at "^<tr><td>")
      (error "Cannot find body of table"))
    (unless buffer-file-name
      (error "Buffer does not have a filename (yet)"))
    (let ((start (point))
	  (cur-dir (file-name-directory buffer-file-name)))
      (unless (re-search-forward "^</table>" nil t)
	(error "Cannot find end of table"))
      (goto-char (match-beginning 0))
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
	    (insert new-text)))
	(not ok)))))


;; This helper function looks for files named YY-{01,04,07,10,[A-Za-z]*}.html in the
;; current directory. If none are found, it signals an error, otherwise it returns
;; the HTML text of one or more table rows.
;;
;; Does not change the match data.
;;
;; Assumes file NN-* is for year 20NN.
;; FIXME???: treat 99-10.html as 1999-10, etc.
;;
(defun c12h--anime-index-html (cur-dir)
  "Returns HTML table row text for files named YY-{01,04,07,10,[A-Za-z]*}.html.
Signals an error if no such files."
  ;; Get a list of matching files, sorted in Unicodal order.
  (let ((items (directory-files cur-dir nil
				"^[0-9][0-9]-\\(0[147]\\|10\\|[A-Za-z].*\\)\\.html$")))
    (unless items
      (error "No YY-{01,04,07,10,[A-Za-z]*}.html files in directory %S" cur-dir))
    (with-temp-buffer
      (let ((y (1- (read (substring (car items) 0 2))))
	    filename)
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
	    (setq items (cdr items)))))
      (buffer-substring-no-properties (point-min) (point-max)))))
