(use-modules (srfi srfi-1)
	     (srfi srfi-9)
	     (system repl coop-server)
	     (chickadee)
	     (chickadee math vector)
	     (chickadee graphics text)
	     (chickadee graphics path)
	     (chickadee graphics color))

;;; INIT VALUES
(define w 640)

(define h 480)

;;; UTILITY FUNCTIONS
(define (centroid w h)
  (vec2 (/ w 2)
	(/ h 2)))

(define (vec2-rotate vec2 rad-angle)
  "Rotate a 2D vector by a given ANGLE (in radians)."
  (let ((x (vec2-x vec2))
	(y (vec2-y vec2))
	(c (cos rad-angle))
	(s (sin rad-angle)))
    (vec2 (- (* x c) (* y s))
	  (+ (* x s) (* y c)))))

;;; DATA TYPES
(define-record-type ball
  (make-ball pos radius direction-x direction-y)
  ball?
  (pos ball-pos)
  (radius ball-radius)
  (direction-x ball-direction-x set-ball-direction-x!)
  (direction-y ball-direction-y set-ball-direction-y!))

(define-record-type rect
  (make-rect pos width height)
  rect?
  (pos rect-pos)
  (width rect-width)
  (height rect-height))

(define (rect-left-x rect)
  (- (vec2-x (rect-pos rect)) (/ (rect-width rect) 2)))

(define (rect-right-x rect)
  (+ (vec2-x (rect-pos rect)) (/ (rect-width rect) 2)))

(define (rect-bottom-y rect)
  (- (vec2-y (rect-pos rect)) (/ (rect-height rect) 2)))

(define (rect-up-y rect)
  (+ (vec2-y (rect-pos rect)) (/ (rect-height rect) 2)))

(define (between? val a b)
  "Is VAL between A and B?"
  (and (>= val a)
       (<= val b)))

(define (inside-rect? rect x y)
  (and (between? x (rect-left-x rect) (rect-right-x rect))
       (between? y (rect-bottom-y rect) (rect-up-y rect))))

;;; ANIMATIONS
(define* (bounce-ball ball dt #:optional obstacles)
  "Bounce the ball off the walls and obstacles."
  (define (change-direction2 pos direction radius min max)
    (cond ((and (eqv? direction +)
		(<= (+ pos radius) max)) -)
	  ((and (eqv? direction -)
		(>= (- pos radius) min)) +)
	  (else direction)))
  (define (change-direction pos direction radius min max)
    (cond ((and (eqv? direction +)
		(>= (+ pos radius) max)) -)
	  ((and (eqv? direction -)
		(<= (- pos radius) min)) +)
	  (else direction)))
  (define (compute-pos pos direction dt speed)
    (direction pos (* speed dt)))
  (let ((x (vec2-x (ball-pos ball)))
	(y (vec2-y (ball-pos ball)))
	(dx (ball-direction-x ball))
	(dy (ball-direction-y ball))
	(r (ball-radius ball)))
    (set-ball-direction-x!
     ball
     (or (fold (lambda (o res)
		 (let ((points-inside
			(map (lambda (point-inside) (car point-inside))
			     (filter (lambda (point-inside)
				       (cdr point-inside))
				     (map (lambda (point)
					    (cons (car point) (let ((p (cdr point)))
								(inside-rect? o (vec2-x p) (vec2-y p)))))
					  (list (cons #:A (vec2 (+ x r) (+ y r)))
						(cons #:B (vec2 (+ x r) (- y r)))
						(cons #:C (vec2 (- x r) (- y r)))
						(cons #:D (vec2 (- x r) (+ y r)))))))))
		   (cond ((and (member #:A points-inside)
			       (member #:B points-inside))
			  (change-direction2 x dx r (rect-right-x o) w))
			 ((and (member #:C points-inside)
			       (member #:D points-inside))
			  (change-direction2 x dx r 0.0 (rect-left-x o)))
			 (else res))))
	       #f obstacles)
	 (change-direction x dx r 0 w)))
    (set-ball-direction-y!
     ball
     (or (fold (lambda (o res)
		 (let ((points-inside
			(map (lambda (point-inside) (car point-inside))
			     (filter (lambda (point-inside)
				       (cdr point-inside))
				     (map (lambda (point)
					    (cons (car point) (let ((p (cdr point)))
								(inside-rect? o (vec2-x p) (vec2-y p)))))
					  (list (cons #:A (vec2 (+ x r) (+ y r)))
						(cons #:B (vec2 (+ x r) (- y r)))
						(cons #:C (vec2 (- x r) (- y r)))
						(cons #:D (vec2 (- x r) (+ y r)))))))))
		   (cond ((and (member #:A points-inside)
			       (member #:D points-inside))
			  (change-direction2 y dy r (rect-up-y o) h))
			 ((and (member #:C points-inside)
			       (member #:B points-inside))
			  (change-direction2 y dy r 0.0 (rect-bottom-y o)))
			 (else res))))
	       #f obstacles)
	 (change-direction y dy r 0 h)))
    ;; (set-ball-direction-x! ball (change-direction x dx r 0 w))
    ;; (set-ball-direction-y! ball (change-direction y dy r 0 h))
    (set-vec2-x! (ball-pos ball) (compute-pos x dx dt 100))
    (set-vec2-y! (ball-pos ball) (compute-pos y dy dt 100))))

(define (tie-mouse-x gobject)
  "Tie a graphical object to the mouse on the X axis.
Note: now using a RECT-POS of a RECT object, but needs
an abstraction to work on any graphical object."
  (set-vec2-x! (rect-pos gobject) (max 0.0 (min (- w (rect-width gobject))
						(- (mouse-x) (/ (rect-width gobject) 2))))))

(define (tie-mouse-y gobject)
  "Tie a graphical object to the mouse on the Y ayis.
Note: now using a RECT-POS of a RECT object, but needs
an abstraction to work on any graphical object."
  (set-vec2-y! (rect-pos gobject) (max 0.0 (min (- h (rect-height gobject))
						(- h (mouse-y))))))

;;; REPL LOOP
(define repl (spawn-coop-repl-server))

;;; GAME VARS
(define ping-ball
  (make-ball (centroid w h)
	     10.0 + +))

(define ping-racket
  (make-rect (vec2 (vec2-x (centroid w h)) 5.0)
	     100.0 5.0))

(define (make-ball-canvas)
  (make-canvas
   (let ((ball (circle (ball-pos ping-ball)
		       (ball-radius ping-ball))))
     (with-style ((fill-color red))
		 (fill ball)))))

(define (make-racket-canvas)
  (make-canvas
   (let ((racket (rounded-rectangle (vec2 (rect-left-x ping-racket)
					  (rect-bottom-y ping-racket))
				    (rect-width ping-racket)
				    (rect-height ping-racket))))
     (with-style ((fill-color white))
		 (fill racket)))))

(define (make-gobject-boundaries-canvases gobject)
  (map (lambda (point) (let ((racket-point (circle point 2.0)))
			 (make-canvas (with-style ((fill-color black))
						  (fill racket-point)))))
       (list (vec2 (rect-left-x gobject) (rect-bottom-y gobject))
	     (vec2 (rect-right-x gobject) (rect-bottom-y gobject))
	     (vec2 (rect-left-x gobject) (rect-up-y gobject))
	     (vec2 (rect-right-x gobject) (rect-up-y gobject)))))

(define follow #t)

;;; GAME FUNCTIONS
(define (draw alpha)
  (draw-canvas (make-ball-canvas))
  (draw-canvas (make-racket-canvas)))

(define (updates dt)
  (bounce-ball ping-ball dt (list ping-racket))
  (when follow (tie-mouse-x ping-racket)))

(define (key-press key modifiers repeat?)
  (when (eq? key 'q)
    (abort-game)))

;;; LAUNCH!
(run-game #:update (lambda (dt) 
		     (poll-coop-repl-server repl)
		     (updates dt))
	  #:key-press (lambda (key modifiers repead?)
			(key-press key modifiers repead?))
	  #:mouse-press (lambda (button clicks x y)
			  (set! follow (not follow)))
	  #:draw (lambda (alpha)
		   (draw alpha))
	  #:window-title "PING"
	  #:window-width w
	  #:window-height h)
