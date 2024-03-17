(use-modules (srfi srfi-1)
	     (srfi srfi-9)
	     (system repl coop-server)
	     (chickadee)
	     (chickadee math rect)
	     (chickadee math vector)
	     (chickadee graphics text)
	     (chickadee graphics path)
	     (chickadee graphics color)
	     (chickadee audio))

;;; INIT VALUES
(define w 640)

(define h 480)

;;; UTILITY FUNCTIONS
(define (centroid w h)
  (vec2 (/ w 2)
	(/ h 2)))

;;; DATA TYPES
(define-record-type ball
  (make-ball pos radius direction-x direction-y)
  ball?
  (pos ball-pos set-ball-pos!)
  (radius ball-radius)
  (direction-x ball-direction-x set-ball-direction-x!)
  (direction-y ball-direction-y set-ball-direction-y!))

;;; ANIMATIONS
(define* (bounce-ball ball dt #:optional obstacles)
  "Bounce the ball off the walls and obstacles."
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
     (or (detect-obstacles x y r 0 w obstacles
			   (lambda (obstacle points-inside res)
			     (or (and (member #:A points-inside)
				      (member #:B points-inside)
				      ;; bounce off left side of obstacle
				      (bounce-off-obstacle x dx r (rect-right obstacle) w))
				 (and (member #:C points-inside)
				      (member #:D points-inside)
				      ;; bounce off right side of obstacle
				      (bounce-off-obstacle x dx r 0.0 (rect-left obstacle))))))
	 (bounce-off-walls x dx r 0 w)))
    (set-ball-direction-y!
     ball
     (or (detect-obstacles x y r 0 h obstacles
			   (lambda (obstacle points-inside res)
			     (or (and (member #:A points-inside)
				      (member #:D points-inside)
				      ;; bounce off top side of obstacle
				      (bounce-off-obstacle y dy r (rect-top obstacle) h))
				 (and (member #:C points-inside)
				      (member #:B points-inside)
				      ;; bounce off bottom side of obstacle
				      (bounce-off-obstacle y dy r 0.0 (rect-bottom obstacle))))))
	 (ball-direction-y ball)))))

(define (tie-mouse-x rect)
  "Tie a RECT to the mouse on the X axis."
  (set-rect-x! rect (max 0 (min (- (mouse-x) (/ (rect-width rect) 2))
				(- w (rect-width rect))))))

;;; REPL LOOP
(define repl (spawn-coop-repl-server))

;;; GAME VARS
(define ping-ball
  (make-ball (centroid w h)
	     10.0 + +))

(define ping-racket
  (rect 0.0 5.0 100.0 5.0))

(define ping-racket2
  (rect 0.0 (- h 10.0) 100.0 5.0))

(define (make-ball-canvas)
  (make-canvas
   (let ((ball (circle (ball-pos ping-ball)
		       (ball-radius ping-ball))))
     (with-style ((fill-color red))
		 (fill ball)))))

(define (make-racket-canvas racket)
  (make-canvas
   (let ((r (rounded-rectangle (vec2 (rect-x racket)
				     (rect-y racket))
			       (rect-width racket)
			       (rect-height racket))))
     (with-style ((fill-color white))
		 (fill r)))))

(define follow #t)

(define game-over #f)

;;; GAME FUNCTIONS
(define (draw alpha)
  (draw-canvas (make-ball-canvas))
  (draw-canvas (make-racket-canvas ping-racket))
  (draw-canvas (make-racket-canvas ping-racket2))
  (when game-over
    (draw-text "     You Lost!\nType R to try again!"
	       (centroid (- w 200) h)
	       #:color white)))

(define (updates dt)
  (define (compute-pos pos direction dt speed)
    (direction pos (* speed dt)))

  ;; bounce the ball off the walls and the two rackets
  (bounce-ball ping-ball dt (list ping-racket ping-racket2))
  (when follow (tie-mouse-x ping-racket))
  (when follow (tie-mouse-x ping-racket2))
  
  ;; move the ball
  (let ((x (vec2-x (ball-pos ping-ball)))
	(y (vec2-y (ball-pos ping-ball)))
	(dx (ball-direction-x ping-ball))
	(dy (ball-direction-y ping-ball))
	(speed 200))
    (set-vec2-x! (ball-pos ping-ball) (compute-pos x dx dt speed))
    (set-vec2-y! (ball-pos ping-ball) (compute-pos y dy dt speed)))

  ;; check if the ball is out of the screen
  (let ((ball-pos (ball-pos ping-ball))
	(screen-rect (rect 0.0 0.0 w h)))
    (when (not (rect-contains? screen-rect (vec2-x ball-pos) (vec2-y ball-pos)))
      (set! game-over #t))))

(define (key-press key modifiers repeat?)
  (when (eq? key 'r)
    (set! game-over #f)
    (set-ball-pos! ping-ball (centroid w h))
    (set-ball-direction-x! ping-ball +)
    (set-ball-direction-y! ping-ball +))
  (when (eq? key 'q)
    (abort-game)))

;;; LAUNCH!
;; (define sample (load-audio "assets/audio.mp3"))
;; (audio-play sample)

(run-game #:update (lambda (dt) 
		     (poll-coop-repl-server repl)
		     (updates dt))
	  #:key-press (lambda (key modifiers repead?)
			(key-press key modifiers repead?))
	  #:mouse-press (lambda (button clicks x y)
			  (set! follow (not follow)))
	  #:draw (lambda (alpha)
		   (draw alpha))
	  #:clear-color black
	  #:window-title "PING"
	  #:window-width w
	  #:window-height h)
