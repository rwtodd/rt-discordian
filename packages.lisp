;; packages for rt-discordian system
(in-package :cl-user)

(defpackage "RT-DISCORDIAN"
  (:use :cl))

(defpackage "RT-DDATE"
  (:use :cl "RT-DISCORDIAN")
  (:export #:MAIN))
