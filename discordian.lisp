(in-package "RT-DISCORDIAN")

(defmacro divisible-p (y n) `(zerop (mod ,y ,n)))
(defun leap-year-p (y)
  "is Y a leap year?"
  (and (divisible-p y 4)
       (or (not (divisible-p y 100)) (divisible-p y 400))))

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant +dayz+
    (coerce (loop :for x :in '(0 31 28 31 30 31 30 31 31 30 31 30)
		  :summing x :into y :collecting y) 'vector))
  (defconstant +day-names+
    #("St. Tib's Day" "Tibs"   "Sweetmorn" "SM"  "Boomtime" "BT"
      "Pungenday" "PD"         "Prickle-Prickle" "PP"
      "Setting Orange" "SO"))
  (defconstant +season-names+
    #("Chaos" "Chs"   "Discord" "Dsc"   "Confusion" "Cfn" 
      "Bureaucracy" "Bcy"    "The Aftermath" "Afm")))

(defun day-of-year (y m d)
  "Give the 0-based day-of-the-year (0-364; 365 on leap-years)"
  (+ (svref +dayz+ (1- m))
     (1- d)
     (if (and (> m 2) (leap-year-p y)) 1 0)))

(defstruct (discdata (:conc-name dd-))
  year yday day weekday season tibs)

(defun time-to-discdata (utm)
  "take a universal time, utm, and produce a discordian discdata"
  (multiple-value-bind (_ _ _ d m y) (decode-universal-time utm)
    (let* ((raw-yday (day-of-year y m d))
	   (ly (leap-year-p y))
	   (adjusted-yday (- raw-yday
			     (if (and ly (> raw-yday 59)) 1 0)))
	   (tibs (and ly (eql raw-yday 59))))
      (multiple-value-bind (zb-ssn zb-day) (floor adjusted-yday 73)
	(make-discdata :year    (+ y 1166)
		       :yday    raw-yday
		       :day     (if tibs 0 (1+ zb-day))
		       :weekday (if tibs 0 (1+ (mod adjusted-yday 5)))
		       :season  (if tibs 1 (1+ zb-ssn))
		       :tibs    tibs)))))

(defun date-to-discdata (y m d)
  "take a Gregorian y-m-d date, and produce a discordian discdata"
  (time-to-discdata (encode-universal-time 0 0 0 d m y)))

(defun holyday-p (dd)
  "is this a holy day?"
  (or (eql (dd-day dd) 5) (eql (dd-day dd) 50)))

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
