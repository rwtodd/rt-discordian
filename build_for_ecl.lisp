;; a file I used to successfully build an .EXE from ECL for windows.
;; You will have to put ecl.dll in a place visible to the resulting exe.

(require 'asdf)
(require 'cmp)

(defvar *files* (list "packages" "discordian" "ddate"))

(mapc #'(lambda (fl)
	  (compile-file (concatenate 'string fl ".lisp")
			:system-p t))
      *files*)

(c:build-program "ddate"
		 :lisp-files
		 (mapcar #'(lambda (fl) (concatenate 'string fl ".obj"))
			 *files*)
		 :epilogue-code
		 '(progn (rt-ddate:main)(si:exit)))

(si:exit)
