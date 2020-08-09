;; packages for rt-discordian system
(in-package :cl-user)

(defpackage "RT-DISCORDIAN"
  (:use :cl)
  (:export #:date-to-discdate
	   #:time-to-discdate
	   #:holyday-p
	   #:holyday-name
	   #:day-name
	   #:season-name
	   #:ordinal-day
	   #:exclamation
	   #:days-to-xday
	   #:format-day))

(defpackage "RT-DDATE"
  (:use :cl "RT-DISCORDIAN")
  (:export #:MAIN))
