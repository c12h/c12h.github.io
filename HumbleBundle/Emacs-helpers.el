;;;; File:	/C/c12h.github.io/HumbleBundle/Emacs-helpers.el
;;;; Purpose:	Emacs commands for my HTML files describing Humble Bundles.
;;;; By:	Chris Chittleborough

;;; Command to regenerate the body of the list(s) of files describing bundles in
;;; HTML files like …/HumbleBundle/index.html
;;; from files found in the same directory with names matching "YY-MM-*-bundle.html".
;;;
;;; Assumes the current file contains lines of the form
;;;		<!-- JSON data generated «ISO8601-DATE-TIME» UTC: -->
;;;		<script id="bundles" …>
;;;		[ «JSON-ARRAYS»
;;;		]
;;;		</script><!--JSON-->
;;;
(defun c12h-update-bundles-index ()
  "Regenerate JSON data in …/HumbleBundle/index.html from files in same dir."
  (interactive "*")
  (save-match-data
    ;; Find the start and end of the <script ...> element and check for problems.
    (goto-char (point-min))
    (unless (re-search-forward "\n<script id=\"bundles\" [^<>]+>\n" nil t)
      (error "Cannot find “<script id=\"bundles\"…>” in this buffer"))
    (let ((boJSON (point))
	  (cur-dir (file-name-directory buffer-file-name)))
      (unless cur-dir
	(error "Cannot find current directory (!)"))
      (unless (re-search-forward "\n</script><!--JSON-->" nil t)
	(error "Cannot find “</script><!--JSON-->” line"))
      (beginning-of-line)
      (unless (char-equal (char-after boJSON) ?\[)
	(error "<script id=\"bundles\" ...> does not start with ‘[’"))
      ; Get list of bundle files in cur-dir, sorted in Unicodal order.
      (let ((items (directory-files cur-dir nil
				    "^[0-9][0-9]-[01][0-9].*-bundle\\.html$"))
	    (prefix "[\n") new-text)
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
			      prefix end-d-ISO fname link-html foll-html))
	      (setq prefix ",\n")))
	  (insert "\n]\n")
	  (setq new-text (buffer-substring-no-properties (point-min) (point-max))))
	;; Back to the target buffer
	(if (string-equal new-text (buffer-substring-no-properties boJSON (point)))
	    (message "No changes made.")
	  (delete-region boJSON (point))
	  (insert new-text)
	  (goto-char boJSON)
	  (forward-line -2)
	  ;; Update or insert HTML comment line immediately before <script ...> line.
	  (let ((gen-time-text (format-time-string "%F %T" nil t))) ; t here ⇒ UTC
	    ;; ... comment ???
	    (if (looking-at "<!-- JSON data generated \\([0-9-]+ [0-9:.+-]+\\) UTC: -->")
		(replace-match gen-time-text t t nil 1)
	      (forward-line 1)
	      (insert "<!-- JSON data generated " gen-time-text " UTC: -->\n")))
	  (message "JSON data updated."))))))

;; This helper function looks for files named YY-MM-*-bundle.html in the current
;; directory. If none are found, it signals an error, otherwise it returns a
;; list of 3 strings (see function doc). It (usually) changes the match data.
;;
;; These files must have a <h1> element followed by a <div id=end>, like this:
;;	<h1><a …>«LINK-HTML»</a>…</h1>
;;	<div id=end>end… <time datetime="«END-D-ISO»"
;; in which case the third string returned will be "", or like this:
;;	<h1><a …>«LINK-HTML»</a>…</h1>
;;	<div id=end data-note="«NOTE-TEXT»">end… <time datetime="«END-D-ISO»"
;; in which case the third result will be "; «NOTE-TEXT»".
;;
;; Exception: certain unusual bundles have to be handled by hard-coding their
;; file name and metadata.
;;
;; Assumes file NN-MM-* is for year 20NN.
;; FIXME???: treat 99-10.html as 1999-10, etc.
;;
(defun c12h--parse-my-bundle-file (fname)
  "Reads one of my HTML files about a bundle; returns (END-D-ISO LINK-HTML NOTE-HTML).
All three list elements will be strings.
END-D-ISO is ISO8601 date-time when bundle becomes/became unavailable to buy.
LINK-HTML is bundle title as HTML text, used for link to FNAME in index.html.
NOTE-HTML is \"\" or short HTML text starting with \"; \" explaining bundle title.
Signals an error if anything goes wrong."
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

;;;
(defconst c12h--months
  '((JAN .  1) (JANUARY   .  1)   (FEB .  2) (FEBRUARY  .  2)
    (MAR .  3) (MARCH     .  3)   (APR .  4) (APRIL     .  4)
    (MAY .  5)                    (JUN .  6) (JUNE      .  6)
    (JUL .  7) (JULY      .  7)   (AUG .  8) (AUGUST    .  8)
    (SEP .  9) (SEPTEMBER .  9)   (OCT . 10) (OCTOBER   . 10)
    (NOV . 11) (NOVEMBER  . 11)   (DEC . 12) (DECEMBER  . 12)))

;;; This function fetches the HTML of the home page of a bundle, presumably from
;;; humblebundle.com.
;;;
;;; WARNING: Since it scrapes HB’s HTML, it may start failing whenever they
;;; change their website!  It signals an error if it has any trouble.
;;;
;;; If the bundle is still active, it returns a list of the form
;;;	(t "yyyy-mm-ddThh:mm:ss…+00:00" "«TITLE»")
;;; or otherwise it returns
;;;	(nil "yyyy-mm-dd" "«TITLE»")
;;; if the bundle has ended, where TITLE is the content of the <title> element with
;;; leading and trailing white-space removed.
;;;
(defun c12h--parse-bundle-home-page (url)
  "Extracts ending date and title from the HTML for a Humble Bundle’s home page."
  (save-match-data
    (let ((title-re "<title>\\s-*\\(\\S-[^<>]+\\S-\\)\\s-*</title>")
	  (avail-re (concat "<script\\>[^<>]+\\<"
			    "type=\\([\"']\\)?application/ld\\+json\\1[<>]*>[\n\t ]*{"))
	  (ended-re (concat "This bundle was live .*? to <strong>"
			   "\\([A-Za-z]+\\) \\([0-9]+\\), \\(20[0-9][0-9]\\)</strong>"))
	  title alist offers availEnds time-val (active t) end-date)
      ;;#D# <<< Hack for debugging
      (let (temp-buffer))
      (cond ((string-equal url "twin-sails-collection.html")
	     (setq temp-buffer (set-buffer "twin-sails-collection.html")))
	    ((string-equal url "deck-builder-bundle.html")
	     (setq temp-buffer (set-buffer "deck-builder-bundle.html")))
	    ((string-equal url "c12h-functions.el")
	     (setq temp-buffer (set-buffer "c12h-functions.el")))
	    (t
	     setq temp-buffer (url-retrieve-synchronously url nil t 30)))
      ;;>>>    (let ((temp-buffer (url-retrieve-synchronously url nil t 30)))
      (unless temp-buffer
	(error "No data for URL protocol in %S" url))
      (with-current-buffer temp-buffer
	;; Search for a <script> element containing json with .offers.availabilityEnds
	(goto-char (point-min))
	(unless (re-search-forward title-re nil t)
	  (error "No <title> element (not really HTML?) in %S" url))
	(setq title (match-string-no-properties 1))
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
	       ;; Maybe append "+00:00" ???
	       ))
	;; If that files, try to find the bundle-over notice
	(unless end-date
	  (setq active nil)
	  (goto-char (point-min))
	  (and (re-search-forward ended-re nil t)
	       (setq time-val (assoc-string (upcase (match-string 1)) c12h--months))
	       (setq time-val (list 0 0 0
				    (read (match-string 2))
				    (cdr time-val)
				    (read (match-string 3)) nil -1 nil))
	       (setq end-date (format-time-string "%F" (encode-time time-val)))))
	;; If both failed, we have the wrong URL or a bug.
	(unless end-date
	    (error "Could not get bundle end time from %S" url)))
	(list active end-date title))))


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
  "Insert 2 HTML table rows describing a Steam app given URL in PRIMARY selection.
For Humble-monthlies.html etc."
  (interactive "*")
  (set re (concat "^https://store[.]steampowered[.]com"
		  "\\(:?/\\(app\\|sub\\|bundle\\)/[0-9]+\\)/\\([^/#]+\\)/?#?"))
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
	      " <tr><td colspan=5>Action, \n\n"))))

;;;;; " id=end\(?: data-tag=\"\\([^\"]_\\)">\s-*End[eds]+\s-+<time datetime="\\(20[0-9][0-9]-[012][0-9]-[0-3][0-9]\\)\\(?:T[^\"]+\\)?">


;;  Ended </abbr><time title="2022-09-30 17:59 UTC" datetime="2022-09-30T17:59:00+00:00" class="bold">
;; Ends </abbr><time title="2022-11-26 01:56 UTC" datetime="2022-11-26T01:56:00+00:00" class="bold">
;;; WIP function to get bundle ending dates
(defun c12h-get-bundle-end ()
  ""
  (save-excursion
    (set-buffer "*eww-source*")
    (goto-char (point-min))
    (unless (re-search-forward " End\\(?:ed\\|s\\) \\(?:</abbr>\\)?<time title=\"\\([^\"]+\\)\" datetime=\"\\([-:T+.0-9]+\\)\"[^<>]*>"

(rx " End" (or "ed" "s") " "
    (opt "</abbr>")
    "<time title=\""
    (group (not ?\"))
    "\" datetime=\""

)))))
