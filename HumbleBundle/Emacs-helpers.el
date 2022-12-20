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
    (let ((case-fold-search t))
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
	;; Get list of bundle files in cur-dir, sorted in Unicodal order.
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
	    (not ok)))))))


;;; This helper function parses one of my YY-MM-*-bundle.html files.  On
;;; success, it returns a list of 3 strings: the bundle’s end date, the URL of
;;; its page on Humblebundle.com and a note about the bundle (normally "").
;;;
;;; It signals an error unless the file contains text looking like either
;;;	<h1><a …>«LINK-HTML»</a>…</h1>
;;;	<div id=end>end… <time datetime="«END-DATE-ISO»"
;;; in which case the third string returned will be "", or like
;;;	<h1><a …>«LINK-HTML»</a>…</h1>
;;;	<div id=end data-note="«NOTE-TEXT»">end… <time datetime="«END-DATE-ISO»"
;;; in which case the third result will be "; «NOTE-TEXT»".
;;;
;;; Exception: certain unusual bundles have to be handled by hard-coding their
;;; file name and metadata. Sigh.
;;;
(defun c12h--parse-my-bundle-file (fname)
  "Extract metadata from one of my YY-MM-*-bundle.html files.
Signal an error if expected text not found, otherwise return a list of 3
strings: (END-DATE-ISO LINK-HTML NOTE-HTML).

END-DATE-ISO is when bundle becomes/became unavailable to buy (ISO 8601 format).

LINK-HTML is the bundle title as HTML text, used for text of link to FNAME in
index.html.

NOTE-HTML is \"\" or a short HTML text starting with \"; \" explaining the
bundle title."
  (cond
   ;; Some bundles are too weird to do programatically.
   ((string-equal fname "22-03-Kodansha-Comics-2022-bundle.html")
    (list "2022-04-17" "Humble 2022 Kodansha Manga Bundle" ""))
   (t
    (let ((case-fold-search t)
	  (re (concat "<h1><a [^<>]+>\\(.+?\\)</a>[^<>]+</h1>\\s-*"
		      "<div id=ends?\\(?: data-note=\"\\([^\"]+\\)\"\\)?>\\s-*"
		      "end[eds]+ <time datetime=\"\\([^\"]+\\)\""))
	  note)
      (save-match-data
	(with-temp-buffer
	  (insert-file-contents fname)
	  (goto-char (point-min))
	  (unless (re-search-forward re nil t)
	    (error "Bad <h1>... sequence in file %s" fname))
	  (setq note (match-string 2))
	  (cond ((null note)
		 (setq note ""))
		((not (string-empty-p note))
		 (setq note (concat "; " note))))
	  (list (match-string 3) (match-string 1) note)))))))


;;; Insert HTML text for two table rows describing a Steam app.
;;; For use in my "Humble-monthlies.html" file or similar.
;;; Requires the PRIMARY selection to be the full URL of a Steam app, like this:
;;;	https://store.steampowered.com/app/$NUM/$NAMEISH$"
;;; perhaps with a trailing "/" or "#$FRAGMENT" or both.
;;;
(defun c12h-insert-steam-info-from-primary-sel ()
  "Insert 2 HTML table rows re a Steam app from PRIMARY selection.
That selection must be text of the form
  https://store.steampowered.com/app/DDD/NAME[/][#frag]
where DDD is the numeric app ID and NAME is munged to be valid in a URL.
Partially un-munges NAME by replacing any “_” characters in NAME with “ ”.

For my Humble-monthlies.html file and look-alikes."
  (interactive "*")
  (let ((case-fold-search t)
	(re (concat "^https://store[.]steampowered[.]com"
		   "\\(/\\(?:app\\|sub\\|bundle\\)/[0-9]+\\)/\\([^/#]+\\)/?#?")))
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
(defconst c12h--re-html-title
  "<title>\\s-*\\(\\S-[^<>]+\\S-\\)\\s-*</title>")
(defconst c12h--re-HB-avail
  (concat "<script\\>[^<>]+\\<"
	  "type=\\([\"']\\)?application/ld\\+json\\1[<>]*>[\n\t ]*{"))
(defconst c12h--re-HB-ended
  ;; Could change "<strong>“ to ”<[a-z]+[^<>]*>“ for extra robustnes:
  (concat "This bundle was live .*? to <strong>"
	  "\\([A-Za-z]+\\) \\([0-9]+\\), \\(20[0-9][0-9]\\)</strong>"))
(defun c12h--parse-bundle-home-page (url)
  "Extracts end date & title from HTML of a Humble Bundle’s page.
URL must be HTTP or HTTPS."
  (save-match-data
    (let ((case-fold-search t)
	  (deactivate-mark nil)	; Don’t deactive mark in current buffer.
	  (active t) title end-date
	  (url-obj (url-generic-parse-url url))
	  temp-buffer b-o-resp alist offers availEnds time-val)
      (unless (and url-obj (member (url-type url-obj) '("http" "https")))
	(error "Need https://… or http://… url, not %S" url))
      (setq temp-buffer (url-retrieve-synchronously url-obj nil t 30))
	(unless temp-buffer
	  (error "No data for URL protocol in %S" url))
	(with-current-buffer temp-buffer
	  (goto-char (point-min))
	  ;; Check the HTTP response line and find the start of the response body.
	  (unless (looking-at "HTTP/[0-9.]+ \\([0-9][0-9][0-9]\\) \\([^\n]+\\)")
	    (forward-line)
	    (error "Get non-HTTP response line %S for URL %s"
		   (buffer-substring (point-min) (point)) url))
	  (unless (string-equal (match-string 1) "200")
	    (error "Cannot fetch %S: HTTP error %s %s"
		   url (match-string 1) (match-string 2)))
	  (unless (search-forward "\n\n" nil t)
	    (error "Cannot locate reponse body for %S" url))
	  (setq b-o-resp (match-beginning 0))
	  (goto-char (point-min))
	  ;; If we have a “Content-Type:” header, check for "text/html".
	  (when (re-search-forward "^content-type: \\([^; \n]+\\)" b-o-resp t)
	    (unless (string-equal (match-string 1) "text/html")
	      (error "Got %S content for %S, need \"text/html\""
		     (match-string 1) url)))
	  ;; Search for a <title> element.
	  (goto-char b-o-resp)
	  (unless (re-search-forward c12h--re-html-title nil t)
	    (error "No <title> element in %S" url))
	  (setq title (match-string-no-properties 1))
	  ;; Look for a <script> element with type="application/ld+json"
	  ;; containing an object with an "offers" property
	  ;; whose value is an object containing an "availabilityEnds" property
	  ;; which has a string value.
	  (while (and (not end-date)
		      (re-search-forward c12h--re-HB-avail nil t))
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
	    (goto-char b-o-resp)
	    (and (re-search-forward c12h--re-HB-ended nil t)
		 (setq time-val (assoc (upcase (match-string 1))
				       c12h--months 'string-equal))
		 (setq time-val (list 0 0 0			; SEC MIN HOUR
				      (read (match-string 2))	; DAY
				      (cdr time-val)		; MONTH
				      (read (match-string 3))	; YEAR
				      nil -1 nil))		; JUNK DST ZONE
	         (setq end-date (format-time-string "%F" (encode-time time-val)))))
	;; If both failed, we have the wrong URL or a bug in this very code.
	  (unless end-date
	    (error "Could not get bundle end time from %S" url)))
	;; On success, return a list.
	(list active end-date title))))


;;;;==================== Creating my bundle HTML files =====================;;;;

;;; NOTES
;;;
;;; AFIAK (as of November 2022), URLs of non-monthly humble bundles look like
;;; 	1. https://www.humblebundle.com/games/«BASENAME»
;;; 	2. https://www.humblebundle.com/books/«BASENAME»
;;; 	3. https://www.humblebundle.com/software/«BASENAME»
;;;  or	4. https://www.humblebundle.com/«BASENAME»
;;; where «BASENAME» contains no slashes.
;;; The fourth format is used for big multi-vendor bundles like the “Humble Conquer
;;; COVID-19 bundle” and the “Humble Stand with Ukraine Bundle” (for example,
;;; “https://www.humblebundle.com/conquer-covid19-bundle”).
;;;
;;; The monthly bundles from Humble Choice each have pages with URLs like
;;; 	https://www.humblebundle.com/subscription/january-2022
;;;  or	https://www.humblebundle.com/membership/february-2022
;;; whereas (IIRC) the bundles from the previous Humble Monthly feature did not
;;; get their own pages.

;;; This function checks a string which should/could be the URL of a Humble
;;; Bundle’s page at HumbleBundle.com.
;;; It returns nil if the string looks valid, returns a symbol if things look
;;; sus, or signals an error if (1) the string does not start with “^https?://”
;;; and (2) NOERROR is nil or omitted.
;;;
(defun c12h-check-HB-url (url-str &optional noerror)
  "Returns nil (if URL-STR seems OK for a HB page), or a symbol.
Can return 'not-hbcom, 'monthly, 'weird-path or 'not-http, the last only when
NOERROR is non-nil: the default is to signal an error if URL-STR does not begin
with \"https://\" or \"http://\"."
  (save-match-data
    (let* ((case-fold-search t)
	   url-obj url-path-etc)
      (if (not (string-match "^https?://" url-str))
	  (if noerror
	      'not-http
	    (signal "Need \"https://…\" or \"http://…\", not %S" url-str))
	(setq url-obj (url-generic-parse-url url-str)
		url-path-etc (url-filename url-obj))
	(cond ((not (string-match-p "^\\(?:www\\.\\)?humblebundle.com"
				    (url-host url-obj)))
	       'not-hbcom)
	      ((string-match-p "^/\\(?:subscription/\\|membership/\\)" url-path-etc)
	       'monthly)
	      ((string-match-p c12h--re-HB-url-path-etc url-path-etc)
	       nil)
	      ('weird-path))))))
(defconst c12h--re-HB-url-path-etc
  "^/\\(books/\\|games/\\|software/\\)?\\(?:[^?#]+\\)"
  "Regexp for `c12h-check-HB-url'")


;;; This helper function asks the user for the URL of a Humble Bundle.
;;;
;;; If the primary selection or the clipboard contain a fully-conforming URL
;;; (per `c12h-check-HB-url', those URL(s) are presented as default(s).
;;;
;;; We reject input that does is not a HTTPS or HTTP URL (for the sake of
;;; `c12h--parse-bundle-home-page' which requires HTTP-style responses: headers,
;;; empty line, response body). If the input looks sus in any other way, we ask
;;; the user for confirmation.
;;;
(defun c12h--ask-for-h-b-url ()
  "Ask the user for the URL of a Humble Bundle.
Checks the URL using `c12h-check-HB-url', and gets confirmation if it looks
sus.  Uses primary selection and clipboard for default(s), if they look like
bundle URLs."
  (save-match-data
    (let* ((defaults (cl-remove-if-not
		      (lambda (str) (c12h-check-HB-url str 'noerror))
		      (list (gui-get-selection 'PRIMARY)
			    (gui-get-selection 'CLIPBOARD))))
	   (ok t)
	   prompt input kind question)
      (setq prompt "URL for a Humble Bundle: ");??? Incorporate DEFAULT(s)
      (setq input (read-from-minibuffer prompt
					nil nil nil
					'c12h--history-bundle-url)
	    kind (c12h-check-HB-url input)
	    question (assq kind c12h--h-b-url-problems-alist))
      (when question
	(setq ok (not (y-or-n-p (format "%S %s. Are you sure? " input question)))))
      (if ok
	  input
	nil))))
(defvar c12h--history-bundle-url nil
  "Minibuffer history list for `c12h--ask-for-h-b-url'")
;; This alist maps symbols returned by c12h-check-HB-url to text to use
;; with `y-or-n-p' when getting user confirmation of dubious URLs.
(defconst c12h--h-b-url-problems-alist
  '((weird-path . "does not look like most bundle URLs")
    (monthly    . "is for a MONTHLY bundle")
    (not-hbcom  . "is not at Humble Bundle’s website"))
  "Controls which URLs user must confirm in `c12h--ask-for-h-b-url'.")


;;; Use (defun url-expand-file-name (url "https://www.humblebundle.com/"))
;;; which returns a string.
;;; Result should usually begin with "https://www.humblebundle.com/$KIND/" where
;;; $KIND is "games" or "books" or "software"
;;;
;;; Want some special keys for inserting stub of <li> describing a Linux game.
;;; Best to do this as a minor mode, since would be useful elsewhere.
;;;	(define-minor-mode SIK-mode	; "SIK" = Steam Info Keys. First-draft!
;;;	  "???"
;;;	  :lighter " +SIK"
;;;	  :keymap (("\^Cl" . 'c12h-insert-linux-native-game-info)
;;;		   ("\^Cp" . 'c12h-insert-proton-compat-game-info)
;;;		   ("\^Cx" . 'c12h-insert-non-linux-game-info)))
;;;	except maybe add `button-buffer-map' as parent keymap???
;;;
;;; Getting fancy with 'edit-here' indicators:
;;;   *	Info: (elisp)Buttons
;;;   *	(define-button-type 'c12h-fixme-button
;;;	  'action	…
;;;	  'follow-link  t	; ???
;;;	  'face		…
;;;	  'help-echo	…
;;;	  …)
;;;   * Face:	:height 0.9	;maybe
;;;		:box '(:line-width -1 :color ??? :style released-button)
;;;	OR	:inverse-video t
;;;   *	(insert-text-button "FIXME>" 'type 'c12h-fixme-button)
;;;
