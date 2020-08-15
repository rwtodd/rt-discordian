;; this helps to build via SBCL
(require 'asdf)

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression 9))

;; (asdf:load-system "rt-discordian")
(asdf:make "rt-discordian")
