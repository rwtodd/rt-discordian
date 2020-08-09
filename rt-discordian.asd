(defsystem "rt-discordian"
  :description "rt-discoridan: discordian date calculations"
  :version "1.0"
  :author "Richard Todd <richard.wesley.todd@gmail.com>"
  :licence "MIT"
  :build-operation "program-op" ;; leave as is
  :build-pathname "ddate"
  :entry-point "rt-ddate:main"
  ;;  :depends-on ("optima.ppcre" "command-line-arguments")
  :serial t
  :components ((:file "packages")
               (:file "discordian")
	       (:file "ddate")))

