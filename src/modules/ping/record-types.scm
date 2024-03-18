(define-module (ping record-types)
  #:use-module (srfi srfi-9)
  #:export (make-ball
	    ball
	    ball?
	    ball-pos
	    ball-radius
	    ball-direction-x
	    ball-direction-y
	    set-ball-pos!
	    set-ball-direction-x!
	    set-ball-direction-y!))

(define-record-type ball
  (make-ball pos radius direction-x direction-y)
  ball?
  (pos ball-pos set-ball-pos!)
  (radius ball-radius)
  (direction-x ball-direction-x set-ball-direction-x!)
  (direction-y ball-direction-y set-ball-direction-y!))
