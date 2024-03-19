(define-module (ping collisions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (chickadee)
  #:use-module (chickadee math)
  #:use-module (chickadee math rect)
  #:use-module (chickadee math vector)
  #:use-module (chickadee graphics text)
  #:use-module (chickadee graphics path)
  #:use-module (chickadee graphics color)
  #:use-module (ping record-types)
  #:export (bounce-ball
	    snippets))

(define* (bounce-ball ball window-width window-height dt #:optional obstacles)
  "Bounce the ball off the walls (window) and obstacles."
  (define (bounce-off-obstacle pos direction radius min max)
    (cond ((and (eqv? direction +)
		(<= (+ pos radius) max)) -)
	  ((and (eqv? direction -)
		(>= (- pos radius) min)) +)
	  (else direction)))
  (define (bounce-off-walls pos direction radius min max)
    (cond ((and (eqv? direction +)
		(>= (+ pos radius) max)) -)
	  ((and (eqv? direction -)
		(<= (- pos radius) min)) +)
	  (else direction)))
  (define (detect-obstacles x y r min max obstacles on-collision)
    (fold (lambda (o res)
		 (let ((points-inside
			(map (lambda (point-inside) (car point-inside))
			     (filter (lambda (point-inside)
				       (cdr point-inside))
				     (map (lambda (point)
					    (cons (car point) (let ((p (cdr point)))
								(rect-contains? o (vec2-x p) (vec2-y p)))))
					  (list (cons #:A (vec2 (+ x r) (+ y r)))
						(cons #:B (vec2 (+ x r) (- y r)))
						(cons #:C (vec2 (- x r) (- y r)))
						(cons #:D (vec2 (- x r) (+ y r)))))))))
		   (or (on-collision o points-inside res)
		       res)))
	  #f obstacles))
  (let ((x (vec2-x (ball-pos ball)))
	(y (vec2-y (ball-pos ball)))
	(dx (ball-direction-x ball))
	(dy (ball-direction-y ball))
	(r (ball-radius ball)))
    (set-ball-direction-x!
     ball
     (or (detect-obstacles x y r 0 window-width obstacles
			   (lambda (obstacle points-inside res)
			     (or (and (member #:A points-inside)
				      (member #:B points-inside)
				      ;; bounce off left side of obstacle
				      (bounce-off-obstacle x dx r (rect-right obstacle) window-width))
				 (and (member #:C points-inside)
				      (member #:D points-inside)
				      ;; bounce off right side of obstacle
				      (bounce-off-obstacle x dx r 0.0 (rect-left obstacle))))))
	 (bounce-off-walls x dx r 0 window-width)))
    (set-ball-direction-y!
     ball
     (or (detect-obstacles x y r 0 window-height obstacles
			   (lambda (obstacle points-inside res)
			     (or (and (member #:A points-inside)
				      (member #:D points-inside)
				      ;; bounce off top side of obstacle
				      (bounce-off-obstacle y dy r (rect-top obstacle) window-height))
				 (and (member #:C points-inside)
				      (member #:B points-inside)
				      ;; bounce off bottom side of obstacle
				      (bounce-off-obstacle y dy r 0.0 (rect-bottom obstacle))))))
	 (ball-direction-y ball)))))

;; Infinite Line
(define-record-type i-line
  (make-line base direction)
  line?
  ;; base: vec2
  (base line-base set-line-base!)
  ;; direction: vec2
  (direction line-direction set-line-direction!))

;; Line segment
(define-record-type line-segment
  (make-line-segment p1 p2)
  line-segment?
  ;; p1: vec2
  (p1 line-segment-p1 set-line-segment-p1!)
  ;; p2: vec2
  (p2 line-segment-p2 set-line-segment-p2!))

;; Circle
(define-record-type circle
  (make-circle center radius)
  circle?
  ;; center: vec2
  (center circle-center set-circle-center!)
  ;; radius: float
  (radius circle-radius set-circle-radius!))

;; Axis Aligned Rectangle
(define-record-type a-rect
  (make-a-rect origin size)
  a-rect?
  ;; origin: vec2
  (origin a-rect-origin set-a-rect-origin!)
  ;; size: vec2
  (size a-rect-size set-a-rect-size!))

(define (a-rect->rect a-rect)
  "Return a chickadee math rectangle from an axis aligned rectangle."
  (let* ((origin (a-rect-origin a-rect))
	 (top-right (vec2+ origin (a-rect-size a-rect)))
	 (width (abs (- (vec2-x top-right) (vec2-x origin))))
	 (height (abs (- (vec2-y top-right) (vec2-y origin)))))
    (rect (vec2-x origin) (vec2-y origin) width height)))

;; Oriented rectangle
(define-record-type o-rect
  (make-o-rect center half-extend rotation)
  o-rect?
  ;; center: vec2
  (center o-rect-center set-o-rect-center!)
  ;; half-extend: vec2
  (half-extend o-rect-half-extend set-o-rect-half-extend!)
  ;; rotation: float
  (rotation o-rect-rotation set-o-rect-rotation!))

(define (snippets w h)
  "Bits of code that are useful for preparing upcoming collision functions."
  ;; Vizualize the angle between two vectors (from origin)
  (let* ((ori (vec2 0 0))
	 (v1 (vec2 200 90)) 		; green
	 (l1 (line ori v1))
	 (v2 (vec2 100 100)) 		; white
	 (l2 (line ori v2))
	 (angle (radians->degrees (angle v1 v2))))
    (draw-canvas
     (make-canvas (with-style ((stroke-color green))
			      (stroke l1))))
    (draw-canvas
     (make-canvas (with-style ((stroke-color white))
			      (stroke l2))))
    (draw-text (format #f "Angle = ~a" angle)
	       (vec2 50 20)
	       #:color white)
    
    ;; Vizualize the projection of a vector onto another
    (let* ((v1->v2 (project v2 v1))
	   (l3 (line v2 v1->v2)))
      (draw-canvas
       (make-canvas (with-style ((stroke-color yellow))
				(stroke l3))))))
  ;; Show rectangles colliding
  (let* ((r1 (make-a-rect (vec2 200 200) (vec2 50 50)))
	 (dr1 (a-rect->rect r1))
	 (r2 (make-a-rect (if #f 	; change this to #t to play with the second rectangle
			      (vec2 (mouse-x) (- h (mouse-y)))
			      (vec2 170 180)) (vec2 50 50)))
	 (dr2 (a-rect->rect r2))
	 (collide? (rects-collide? r1 r2))
	 (draw-rect1 (rectangle (vec2 (rect-left dr1) (rect-bottom dr1))
				(rect-width dr1)
				(rect-height dr1)))
	 (draw-rect2 (rectangle (vec2 (rect-left dr2) (rect-bottom dr2))
				(rect-width dr2)
				(rect-height dr2)))
	 (dr1-color (if collide? red blue))
	 (dr2-color (if collide? red green)))
    (draw-canvas
     (make-canvas (with-style ((fill-color dr1-color)
			       (stroke-color white))
			      (fill-and-stroke draw-rect1))))
    (draw-canvas
     (make-canvas (with-style ((fill-color dr2-color)
			       (stroke-color white))
			      (fill-and-stroke draw-rect2))))
    (draw-canvas
     (make-canvas (with-style ((fill-color dr2-color))
			      (fill draw-rect2))))
    (draw-canvas
     (make-canvas (with-style ((fill-color dr1-color))
			      (fill draw-rect1))))))

(define (project v1 v2)
  "Return the projection of v1 onto v2."
  (let* ((d (vec2-dot v2 v2)))
    (if (< 0 d)
	(let ((dp (vec2-dot v1 v2)))
	  (vec2* v2 (/ dp d)))
	v2)))

(define (angle v1 v2)
  "Return the angle between two vectors."
  (let* ((unit-v1 (vec2-normalize v1))
	 (unit-v2 (vec2-normalize v2))
	 (dotp (vec2-dot unit-v1 unit-v2)))
    (acos dotp)))

;;; homogeneous collision detection
(define (overlapping min-a max-a min-b max-b)
  (and (<= min-b max-a)
       (<= min-a max-b)))

(define (rects-collide? rect-a rect-b)
  (let* ((a-left (vec2-x (a-rect-origin rect-a)))
	 (a-right (+ a-left (vec2-x (a-rect-size rect-a))))
	 (b-left (vec2-x (a-rect-origin rect-b)))
	 (b-right (+ b-left (vec2-x (a-rect-size rect-b))))
	 (a-bottom (vec2-y (a-rect-origin rect-a)))
	 (a-top (+ a-bottom (vec2-y (a-rect-size rect-a))))
	 (b-bottom (vec2-y (a-rect-origin rect-b)))
	 (b-top (+ b-bottom (vec2-y (a-rect-size rect-b)))))
    (and (overlapping a-left a-right b-left b-right)
	 (overlapping a-bottom a-top b-bottom b-top))))
