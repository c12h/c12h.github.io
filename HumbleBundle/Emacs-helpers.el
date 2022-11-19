;;;; File:	/C/c12h.github.io/HumbleBundle/Emacs-helpers.el
;;;; Purpose:	Emacs commands for my HTML files describing Humble Bundles.
;;;; By:	Chris Chittleborough, November 2022
;;;;
;;;; NOTE: The c12h-check-bundles-index function is intended for use from Emacs
;;;; batch mode -- hence its bizarre behaviour of exiting Emacs with an error
;;;; status if it finds a problem.  Suggested usage:
;;;;	emacs --batch -Q -l Emacs-helpers.elc index.html -f c12h-check-bundles-index
;;;; or, less tersely:
;;;;	emacs --batch -Q  --load Emacs-helpers.elc  --visit index.html	\
;;;;						    --funcall c12h-check-bundles-index
;;;;

;;;;====== Checking and updating JSON data about files about bundles =======;;;;

;;; This command automagically updates the JSON in my …/HumbleBundle/index.html
;;; from files found in the same directory, or tells you that no changes are
;;; needed.
;;; It signals an error if the current buffer does not look like my
;;; …/HumbleBundle/index.html file.
;;;
(defun c12h-update-bundles-index ()
  "Check and maybe replace JSON data in …/HumbleBundle/index.html.
Generate JSON describing files named YY-MM-*-bundle.html in the current
directory and ensure that the <script id=\"bundles\" …> element contains
exactly that text. Say either that it updated the JSON or made no changes to
the buffer.

Signal an error if the current buffer or ANY of the YY-MM-*-bundle.html files
looks wrong."
  (interactive "*")
  (c12h--check-maybe-update-bundles-index 'yes-fix-the-JSON))

;;; This function is intended to be called only in Emacs’s batch mode.  It
;;; generates JSON describing all files named YY-MM-*-bundle.html in the current
;;; directory, and EXITS EMACS with an error status that to the contents of the
;;; <script id=\"bundles\" …> element is not identical to that JSON text.
;;;
;;; XXX???FIXME: WHAT HAPPENS IF THE THING SIGNALS?
;;;
(defun c12h-check-bundles-index ()
  "For batch mode, check JSON data for …/HumbleBundle/index.html.
Scan all files named YY-MM-*-bundle.html in the current directory, generate
JSON text describing them and check that buffer has a <script id=\"bundles\" …>
element containing exactly that text. If not, output a message and exit Emacs
with an error status.

Signal an error if the current buffer or ANY of the YY-MM-*-bundle.html files
looks wrong.

Will never modify the current buffer."
  (when (c12h--check-maybe-update-bundles-index nil)
    (message "Please update the JSON in %S" buffer-file-name)
    (kill-emacs 1)))

;;; This helper function checks or updates the JSON data in my
;;; …/HumbleBundle/index.html (or a similar file).  It generates JSON describing
;;; all files named YY-MM-*-bundle.html in the same directory.
;;; If and only if MAYBE-UPDATE is set:
;;;	- If the current JSON differs at all from the generated text, this
;;;	  function changes the <script id=\"bundles\" …> element to contain that
;;;	  text, changes the HTML comment preceding the <script> or inserts a new
;;;	  comment there, and says that it changed the JSON.
;;;	- Otherwise, this function just says that no change is needed.
;;; In any case, it returns t if JSON is or was different, or nil if the current
;;; text is OK.
;;;
;;; It assumes the current file contains lines of the form
;;;		<!-- JSON data generated «ISO8601-DATE-TIME» UTC: -->
;;;		<script id="bundles" …>
;;;		[
;;;			«JSON-ARRAYS»
;;;		]
;;;		</script><!--JSON-->
;;; and signals an error if not, or if anything else looks wrong in the buffer
;;; or in ANY of the YY-MM-*-bundle.html files it found.
;;;
(defun c12h--check-maybe-update-bundles-index (maybe-update)
  "Regenerate or check JSON data in my …/HumbleBundle/index.html.
Generate JSON data describing any files named YY-MM-*-bundle.html in the
current directory.  Search current buffer for “\\n<script id=\"bundles\" …>\\n”
and check whether the contents of that element are identical to the generated
JSON text.  Signal an error if the current buffer or any *-bundle.html is not
as expected.

If MAYBE-UPDATE is nil, do not modify buffer.  Otherwise replace JSON in buffer
if any difference found, then say either that buffer has been changed or that
no change is needed.

Return nil if no change needed, t if the JSON is/was wrong."
  (interactive "*")
  (save-match-data
    ;; Find the start and end of the <script ...> element and check for problems.
    (goto-char (point-min))
    (unless (re-search-forward "\n<script id=\"bundles\" [^<>]+>\n" nil t)
      (error "Cannot find “<script id=\"bundles\"…>” in this buffer"))
    (let ((boJSON (point))		; Point is now at b-o-l after <script …>
	  (cur-dir (file-name-directory buffer-file-name)))
      (unless (re-search-forward "\n</script><!--JSON-->" nil t)
	(error "Cannot find “</script><!--JSON-->” line"))
      (unless cur-dir
	(error "Cannot find current directory (!)"))
      (beginning-of-line)		; Point is now at "<" in "</script>"
      (unless (char-equal (char-after boJSON) ?\[)
	(error "<script id=\"bundles\" ...> does not start with ‘[’"))
      ; Get list of bundle files in cur-dir, sorted in Unicodal order.
      (let ((items (directory-files cur-dir nil
				    "^[0-9][0-9]-[01][0-9].*-bundle\\.html$"))
	    (prefix "[\n")
	    new-text)
	(unless items
	  (error "No YY-MM-*-bundle.html files found in directory %s" cur-dir))
	(with-temp-buffer
	  (dolist (fname items)
	    (let* (;; Get "MONTH, YEAR" from file name
		   (year-int (+ (read (substring fname 0 2)) 2000))
		   (mont-int (read (substring fname 3 5)))
		   (time-val (encode-time (list 0 0 0 17 mont-int year-int 0 0 0)))
		   (m-y-text (format-time-string "%B %Y" time-val))
		   ;; Get END-D-ISO, LINK-HTML, NOTE-HTML (all strings) from file
		   (b-f-data (c12h--parse-my-bundle-file fname))
		   (end-d-ISO (nth 0 b-f-data))
		   (link-html (nth 1 b-f-data))
		   (note-html (nth 2 b-f-data))
		   ;; Compute FOLL-HTML
		   (foll-html (concat "(" m-y-text note-html ")")))
	      (insert (format "%s [%S, %S,\n\t%S,\n\t%S]"
			      prefix
			      end-d-ISO fname link-html foll-html))
	      (setq prefix ",\n")))
	  (insert "\n]\n")
	  (setq new-text (buffer-substring-no-properties (point-min) (point-max))))
	;; Back to the target buffer
	(let ((ok (string-equal new-text
				(buffer-substring-no-properties boJSON (point)))))
	  (when maybe-update
	    (if ok
		(message "No changes needed.")
	      (delete-region boJSON (point))
	      (insert new-text)
	      (goto-char boJSON)
	      (forward-line -2)
	      ;; Update or insert any HTML comment line preceding <script ...> line.
	      (let ((gen-time-text (format-time-string "%F %T" nil t))) ; t here ⇒ UTC
		(if (looking-at
		     "<!-- JSON data generated \\([0-9-]+ [0-9:.+-]+\\) UTC: -->")
		    (replace-match gen-time-text t t nil 1)
		  (forward-line 1)
		  (insert "<!-- JSON data generated " gen-time-text " UTC: -->\n")))
	      (message "JSON data updated.")))
	  (not ok))))))


;;; This helper function parses one of my YY-MM-*-bundle.html files.  On
;;; success, it returns a list of 3 strings: the bundle’s end date, the URL of
;;; its page on Humblebundle.com and a note about the bundle (normally "").
;;;
;;; It signals an error unless the file looks like either
;;;	<h1><a …>«LINK-HTML»</a>…</h1>
;;;	<div id=end>end… <time datetime="«END-D-ISO»"
;;; in which case the third string returned will be "", or like this:
;;;	<h1><a …>«LINK-HTML»</a>…</h1>
;;;	<div id=end data-note="«NOTE-TEXT»">end… <time datetime="«END-D-ISO»"
;;; in which case the third result will be "; «NOTE-TEXT»".
;;;
;;; Exception: certain unusual bundles have to be handled by hard-coding their
;;; file name and metadata. Sigh.
;;;
(defun c12h--parse-my-bundle-file (fname)
  "Extract meta-data from one of my YY-MM-*-bundle.html files.
Signal an error if expected text not found, otherwise return a list of 3
strings: (END-D-ISO LINK-HTML NOTE-HTML).

END-D-ISO is when bundle becomes/became unavailable to buy (ISO 8601 format).

LINK-HTML is the bundle title as HTML text, used for text of link to FNAME in
index.html.

NOTE-HTML is \"\" or short HTML text starting with \"; \" explaining bundle
title."
  (cond
   ;; Some bundles are too weird to do programatically.
   ((string-equal fname "22-03-Kodansha-Comics-2022-bundle.html")
    (list "2022-04-17" "Humble 2022 Kodansha Manga Bundle" ""))
   (t
    (let ((re (concat "<h1><a [^<>]+>\\(.+?\\)</a>[^<>]+</h1>\\s-*"
		      "<div id=end\\(?: data-note=\"\\([^\"]+\\)\"\\)?>\\s-*"
		      "end[eds]+ <time datetime=\"\\([^\"]+\\)\""))
	  note)
      (save-match-data
	(with-temp-buffer
	  (insert-file-contents-literally fname)
	  (goto-char (point-min))
	  (unless (re-search-forward re nil t)
	    (error "Bad <h1>... sequence in file %s" fname))
	  (setq note (match-string 2))
	  (cond ((null note)
		 (setq note ""))
		((not (string-empty-p note))
		 (setq note (concat "; " note))))
	  (list (match-string 3) (match-string 1) note)))))))


;;;;=============== Parsing bundle pages at Humblebundle.com ===============;;;;

;;>>> USE "http://localhost:8080/C/Junk/+Transient/deck-builder-bundle.html"
;;>>> and "http://localhost:8080/C/Junk/+Transient/twin-sails-collection.html"
;;>>> for testing.


;;; When looking for the date a bundle ended, we only accept text of the form
;;;	<month> <day>, <year>
;;; and therefore do our own conversion to a 9-item-list time value.
(defconst c12h--months
  '((JAN .  1) (JANUARY   .  1)   (FEB .  2) (FEBRUARY  .  2)
    (MAR .  3) (MARCH     .  3)   (APR .  4) (APRIL     .  4)
    (MAY .  5)                    (JUN .  6) (JUNE      .  6)
    (JUL .  7) (JULY      .  7)   (AUG .  8) (AUGUST    .  8)
    (SEP .  9) (SEPTEMBER .  9)   (OCT . 10) (OCTOBER   . 10)
    (NOV . 11) (NOVEMBER  . 11)   (DEC . 12) (DECEMBER  . 12)))

;;; This function can extract the end date and title of a Humble Bundle from the HTML
;;; of its page, given the Bundle’s URL.
;;;
;;; WARNING: Since it scrapes HB’s HTML, it may start failing whenever they
;;; change their website!  It signals an error if it has any trouble.
;;;
;;; If the bundle is still active, it returns a list of the form
;;;	(t "yyyy-mm-ddThh:mm:ss…+00:00" "«TITLE»")
;;; or otherwise it returns
;;;	(nil "yyyy-mm-dd" "«TITLE»")
;;; where TITLE is the content of the <title> element with any leading and
;;; trailing white-space removed.
;;;
(defun c12h--parse-bundle-home-page (url)
  "Extracts end date & title from HTML of a Humble Bundle’s page."
  (save-match-data
    (let ((title-re "<title>\\s-*\\(\\S-[^<>]+\\S-\\)\\s-*</title>")
	  (avail-re (concat "<script\\>[^<>]+\\<"
			    "type=\\([\"']\\)?application/ld\\+json\\1[<>]*>[\n\t ]*{"))
	  (ended-re (concat "This bundle was live .*? to <strong>"
			   "\\([A-Za-z]+\\) \\([0-9]+\\), \\(20[0-9][0-9]\\)</strong>"))
	  (active t) title end-date
	  alist offers availEnds time-val)
      (let ((temp-buffer (url-retrieve-synchronously url nil t 30)))
	;;	WARNING: buffer gets ENTIRE response, including headers!
	(unless temp-buffer
	  (error "No data for URL protocol in %S" url))
	(with-current-buffer temp-buffer
	  ;; Search for a <title> element.
	  (goto-char (point-min))
	  (unless (re-search-forward title-re nil t)
	    (error "No <title> element (not really HTML?) in %S" url))
	  (setq title (match-string-no-properties 1))
	  ;; Look for a <script> element with type="application/ld+json"
	  ;; containing an object with an "offers" property
	  ;; whose value is an object containing an "availabilityEnds" property
	  ;; which has a string value.
	  (while (and (not end-date)
		      (re-search-forward avail-re nil t))
	    (backward-char)
	    (setq alist (json-parse-buffer :object-type 'alist))
	    (and (consp alist)
		 (setq offers (assq 'offers alist))
		 (consp offers)
		 (setq availEnds (assq 'availabilityEnds (cdr offers)))
		 (consp availEnds)
		 (setq availEnds (cdr availEnds))
		 (stringp availEnds)
		 (setq end-date availEnds)
		 ;; Maybe append "+00:00" ???FIXME
		 ))
	  ;; If that fails, look for a bundle-over notice.
	  (unless end-date
	    (setq active nil)
	    (goto-char (point-min))
	    (and (re-search-forward ended-re nil t)
		 (setq time-val (list 0 0 0			; SEC MIN HOUR
				      (read (match-string 2))	; DAY
				      (cdr time-val)		; MONTH
				      (read (match-string 3))	; YEAR
				      nil -1 nil))		; JUNK DST ZONE
	         (setq end-date (format-time-string "%F" (encode-time time-val)))))
	;; If both failed, we have the wrong URL or a bug.
	  (unless end-date
	    (error "Could not get bundle end time from %S" url)))
	;; On success, return a list.
	(list active end-date title)))))


;;;;;; BUG: the <script type=*application/*json*> we want is not necessarily the first!
;;;;;;
(defun c12h--find-end-date ()
  (interactive)
  (goto-char (point-min))
  (unless (re-search-forward
	   "<script type=['\"]?application/[^<>\";]*json[^<>]*>[\n\t ]*{" nil t)
    (error "Cannot find “<script type=\"application/…json\"…>…{”"))
  (backward-char)
  (let ((alist (json-parse-buffer :object-type 'alist))
	(standard-output t)
	offers end-date)
    (unless (consp alist)
      (error "Bad JSON data: got %S" end-date))
    (setq offers (assq 'offers alist))
    (unless offers
      (error "No \"offers\" key in JSON object"))
    (unless (consp offers)
      (error "Weird value %S for \"offers\" key in JSON object" offers))
    (let ((end-date (assoc 'availabilityEnds (cdr offers))))
      (unless end-date
	(error "No \"availabilityEnds\" key in JSON object"))
      (setq end-date (cdr end-date))
      (message "Ends %S" end-date)
      end-date)))


;;; Insert HTML text for two table rows describing a Steam app.
;;; For use in my "Humble-monthlies.html" file or similar.
;;; Requires the PRIMARY selection to be the full URL of a Steam app, like this:
;;;	https://store.steampowered.com/app/$NUM/$NAMEISH$"
;;; perhaps with a trailing "/" or "#$FRAGMENT" or both.
;;;
(defun insert-steam-info-from-primary-sel ()
  "Insert 2 HTML table rows re a Steam app from PRIMARY selection.
That selection must be text of the form
  https://store.steampowered.com/app/DDD/NAME[/][#frag]
where DDD is the numeric app ID and NAME is munged to be valid in a URL.
Partially un-munges NAME by replacing any “_” characters in NAME with “ ”.

For my Humble-monthlies.html file and look-alikes."
  (interactive "*")
  (let ((re (concat "^https://store[.]steampowered[.]com"
		   "\\(:?/\\(app\\|sub\\|bundle\\)/[0-9]+\\)/\\([^/#]+\\)/?#?")))
    (save-match-data
      (let ((url (gui-get-selection)) ; Defaults to (gui-get-selection 'PRIMARY 'STRING)
	    rel-url app-name)
	(unless (stringp url)
	  (error "Weird primary selection data %S" url))
	(when (string= url "")
	  (error "Primary selection is empty"))
	(unless (string-match re url)
	  (error "Primary selection %S does not look like canonical Steam app URL" url))
	;; Looks OK, so insert two <TR> elements
	(setq rel-url (match-string 1 url)
	      app-name (replace-regexp-in-string "_" " " (match-string 2 url)))
	(end-of-line)
	(insert "\n")
	(delete-blank-lines)
	(forward-char)
	(insert "<tr class=hr><td><a href=\"" rel-url "\"\n\t>" app-name "</a>\n"
		"\t<td>W/-/- <td class=ln>Gold\t\t\t\t<td class=n4me>\n"
		" <tr><td colspan=5>Action, \n\n")))))


;;;;==================== Creating my bundle HTML files =====================;;;;

;;; Use (defun url-expand-file-name (url "https://www.humblebundle.com/"))
;;; Result should begin with "https://www.humblebundle.com/games/"
;;; or with "https://www.humblebundle.com/books/".
