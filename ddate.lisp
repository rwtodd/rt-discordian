(in-package "RT-DDATE")

(defun main ()
  (let ((args #+ecl(cdr (ext:command-args))
              #-ecl(uiop:command-line-arguments)))
    (handler-case 
	(format t "~a~%"
		(ccase (length args)
		  (0 (format-day
		      "Today is %{%A, the %e day of %B%} in the YOLD %Y%N%nCelebrate %H"
		      (time-to-discdate (get-universal-time))))
		  (1 (format-day (first args) (time-to-discdate)))
		  (3 (format-day
		      "%{%A, %B %d%}, %Y YOLD"
		      (apply #'date-to-discdate
			     (mapcar #'parse-integer args))))
		  (4 (format-day
		      (first args)
		      (apply #'date-to-discdate
			     (mapcar #'parse-integer (rest args)))))))
      (t ()
	(format *error-output*
		"Usage: ddate [format-string] [year month day]~%")
	#+sbcl(sb-ext:exit :code 1)
	#+ecl(si:exit 1)))))
