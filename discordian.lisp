(in-package "RT-DISCORDIAN")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +exclamations+
    #("Hail Eris!" "All Hail Discordia!" "Kallisti!" "Fnord." "Or not."
      "Wibble." "Pzat!" "P'tang!" "Frink!" "Slack!" "Praise \"Bob\"!"
      "Or kill me." "Grudnuk demand sustenance!" "Keep the Lasagna flying!"
      "You are what you see." "Or is it?" "This statement is false."
      "Lies and slander, sire!" "Hee hee hee!" "Hail Eris, Hack Lisp!"))
  (defconstant +holy-5+
    #("Mungday" "Mojoday" "Syaday" "Zaraday" "Maladay"))
  (defconstant +holy-50+
    #("Chaoflux" "Discoflux" "Confuflux" "Bureflux" "Afflux"))
  (defconstant +day-names+
    #("St. Tib's Day" "Tibs"   "Sweetmorn" "SM"  "Boomtime" "BT"
      "Pungenday" "PD"         "Prickle-Prickle" "PP"
      "Setting Orange" "SO"))
  (defconstant +season-names+
    #("Chaos" "Chs"   "Discord" "Dsc"   "Confusion" "Cfn" 
      "Bureaucracy" "Bcy"    "The Aftermath" "Afm")))

(defstruct (discdate (:conc-name dd-))
  year yday day weekday season tibs)

(defun date-to-discdate (y m d)
  "take a Gregorian year-month-day date, and produce a discordian discdata"
  ;; if the year is negative (B.C.), increase it by 1 to account for no
  ;; year 0
  (when (< y 0) (incf y))
  (let* ((raw-yday (rt-dates:ymd-2-yday y m d))
	 (ly (rt-dates:leap-year-p y))
	 (adjusted-yday (- raw-yday
			   (if (and ly (> raw-yday 59)) 1 0)))
	 (tibs (and ly (eql raw-yday 59))))
    (multiple-value-bind (zb-ssn zb-day) (floor adjusted-yday 73)
      (make-discdate :year    (+ y 1166)
		     :yday    raw-yday
		     :day     (if tibs 0 (1+ zb-day))
		     :weekday (if tibs 0 (1+ (mod adjusted-yday 5)))
		     :season  (if tibs 1 (1+ zb-ssn))
		     :tibs    tibs))))

(defun time-to-discdate (&optional (utm (get-universal-time)))
  "take a universal time, utm, and produce a discordian discdata"
  (multiple-value-bind (_1 _2 _3 d m y) (decode-universal-time utm)
    (declare (ignore _1 _2 _3))
    (date-to-discdate y m d)))

(defun holyday-p (dd)
  "is this a holy day?"
  (or (eql (dd-day dd) 5) (eql (dd-day dd) 50)))

(defun holyday-name (dd)
  "the name of the holy day"
  (let ((d (dd-day dd)) (s (1- (dd-season dd))))
    (or (and (eql d 5)  (svref +holy-5+ s))
	(and (eql d 50) (svref +holy-50+ s)))))

(defun day-name (dd &key short)
  "get the (short) day name for a discordian date"
  (svref +day-names+ (+ (* 2 (dd-weekday dd)) (if short 1 0))))

(defun season-name (dd &key short)
  "get the (short) season name for a discordian date"
  (svref +season-names+ (+ (* 2 (1- (dd-season dd))) (if short 1 0))))

(defun ordinal-day (dd)
  "get the ordinal number for the day in the season"
  (let ((day (dd-day dd)))
    (multiple-value-bind (tens ones) (floor day 10)
      (let ((suffix
	      (cond ((eql tens 1) "th")
		    ((eql ones 1) "st")
		    ((eql ones 2) "nd")
		    ((eql ones 3) "rd")
		    (t            "th"))))
	(format nil "~d~a" day suffix)))))

(defun exclamation ()
  "produce a random discordian exclamation"
  (svref +exclamations+ (random (length +exclamations+))))

(defun days-to-xday (dd)
  "get the number of days from now until X-day,
which is Jul 5, 8661."
  (let ((year_e 8661)   (year_s (- (dd-year dd) 1166))
	(day_e  185)    (day_s  (dd-yday dd)))
    ;; if the given day is AFTER x-day, flip them and remember
    ;; that we did that.
    (let ((flip (or (< year_e year_s)
		    (and (eql year_e year_s) (< day_e day_s)))))
      (when flip
	(psetq year_e year_s  year_s year_e
	       day_e day_s    day_s  day_e))

      ;; calculate the days to X-Day now...
      (* (if flip -1 1)
	 (rt-dates:days-between `(,year_s ,day_s) `(,year_e ,day_e))))))

(defun format-day (fmt dd)
  "Format the discordian date according to `fmt`.
fmt can optionally start with a +, which I remove, to honor UNIX tradition."
  (let* ((flen (length fmt))
	 (str  (make-array (* 2 flen)
			   :fill-pointer 0
			   :element-type 'character
			   :adjustable t)))
    (do ((idx (if (eql (char fmt 0) #\+) 1 0)
	      (1+ idx)))
	((>= idx flen) str)
      (let ((ch (char fmt idx)))
	(if (eql ch #\%)
	    (case (char fmt (incf idx))
	      (#\% (vector-push-extend #\% str))
	      (#\A (format str "~a" (day-name dd)))
	      (#\a (format str "~a" (day-name dd :short t)))
	      (#\B (format str "~a" (season-name dd)))
	      (#\b (format str "~a" (season-name dd :short t)))
	      (#\d (format str "~d" (dd-day dd)))
	      (#\e (format str "~a" (ordinal-day dd)))
	      (#\H (format str "~a" (or (holyday-name dd) "")))
	      (#\n (format str "~%"))
	      (#\t (vector-push-extend #\Tab str))
	      (#\X (format str "~:d" (days-to-xday dd)))
	      (#\Y (format str "~d" (dd-year dd)))
	      (#\. (format str "~a" (exclamation)))
	      (#\} t)
	      (#\N (when (not (holyday-p dd))
		     (setq idx flen)))
	      (#\{ (when (dd-tibs dd)
		     (format str "~a" (day-name dd))
		     (setq idx
			   (1+ (or (search "%}" fmt :start2 idx)
				   flen))))))
	    ;; not a %.. just copy it to the output
	    (vector-push-extend ch str))))))
