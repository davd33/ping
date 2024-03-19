(use-modules (srfi srfi-1)
	     (srfi srfi-9)
	     (system repl coop-server)
	     (chickadee)
	     (chickadee math)
	     (chickadee math rect)
	     (chickadee math vector)
	     (chickadee graphics text)
	     (chickadee graphics path)
	     (chickadee graphics color)
	     (chickadee graphics texture)
	     (chickadee graphics sprite)
	     (chickadee graphics particles)
	     (chickadee audio)
	     (ping collisions)
	     (ping record-types))

;;; UTILITY FUNCTIONS
(define (centroid w h)
  (vec2 (/ w 2)
	(/ h 2)))

;;; ANIMATIONS
(define (tie-mouse-x rect)
  "Tie a RECT to the mouse on the X axis."
  (set-rect-x! rect (max 0 (min (- (mouse-x) (/ (rect-width rect) 2))
				(- (window-width (current-window)) (rect-width rect))))))

;;; REPL LOOP
(define repl (spawn-coop-repl-server))

;;; GAME VARS
(define ping-ball #:not-init)

(define ping-racket #:not-init)

(define ping-racket2 #:not-init)

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

(define game #:FROG)

(define frog-idle-index 0)
(define (init-frog-pos) (vec2 10 (vec2-y (centroid (window-width (current-window)) (window-height (current-window))))))
(define frog-position #:not-init)
(define frog-status #:IDLE)

(define frog-wall (rect 0 0 320 320))

;;; DRAW FUNCTIONS
(define (draw-frog)
  ;; draw the frog idle animation
  (draw-text "FROG" (vec2 10 10) #:color white)
  (make-)
  (cond ((eq? frog-status #:IDLE)
	 (let ((frog-idle (load-image "assets/images/frog_idle.png")))
	   (draw-sprite
	    (make-texture-region frog-idle (rect (* 32 frog-idle-index) 0 32 32))
	    frog-position
	    #:scale (vec2 4 4))))
	((eq? frog-status #:WALKING-RIGHT)
	 (let ((frog-walking (load-image "assets/images/frog_run.png")))
	   (draw-sprite
	    (make-texture-region frog-walking (rect (* 32 frog-idle-index) 0 32 32))
	    frog-position
	    #:scale (vec2 4 4))))
	((eq? frog-status #:WALKING-LEFT)
	 (let ((frog-walking (load-image "assets/images/frog_run.png")))
	   (draw-sprite
	    (make-texture-region frog-walking (rect (* 32 frog-idle-index) 0 32 32))
	    frog-position
	    #:scale (vec2 4 4))))))

;;; GAME FUNCTIONS
(define (draw alpha)
  (cond ((eq? game #:PING)
	 (begin
	   (draw-canvas (make-ball-canvas))
	   (draw-canvas (make-racket-canvas ping-racket))
	   (draw-canvas (make-racket-canvas ping-racket2))
	   (when game-over
	     (draw-text "     You Lost!\nType R to try again!"
			(centroid (- (window-width (current-window)) 200) (window-height (current-window)))
			#:color white))))
	((eq? game #:SNIPPETS) (snippets (window-width (current-window)) (window-height (current-window))))
	((eq? game #:FROG)
	 (draw-frog))))

(define (updates dt)
  ;; change frogs idle animation
  (when (and (eq? game #:FROG))
    (set! frog-idle-index (if (= frog-idle-index 11)
			      0
			      (+ frog-idle-index 1)))
    (cond ((key-pressed? 'l)
	   (set! frog-position (init-frog-pos)))
	  ((key-pressed? 'a)
	   (set-vec2-x! frog-position (if (< (vec2-x frog-position) 0)
					  (window-width (current-window))
					  (- (vec2-x frog-position) (* 2 10))))
	   (set! frog-status #:WALKING-LEFT))
	  ((key-pressed? 'e)
	   (set-vec2-x! frog-position (if (> (vec2-x frog-position) (window-width (current-window)))
					 0
					  (+ (vec2-x frog-position) (* 2 10))))
	   (set! frog-status #:WALKING-RIGHT))
	  (else (set! frog-status #:IDLE))))

  ;; move the racket
  (define (compute-pos pos direction dt speed)
    (direction pos (* speed dt)))

  ;; bounce the ball off the walls and the two rackets
  (bounce-ball ping-ball (window-width (current-window)) (window-height (current-window)) dt (list ping-racket ping-racket2))
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
	(screen-rect (rect 0.0 0.0 (window-width (current-window)) (window-height (current-window)))))
    (when (not (rect-contains? screen-rect (vec2-x ball-pos) (vec2-y ball-pos)))
      (set! game-over #t))))

(define (key-press key modifiers repeat?)
  ;; change the game
  (when (eq? key 'f)
    (set! game #:FROG))
  (when (eq? key 's)
    (set! game #:SNIPPETS))
  (when (eq? key 'p)
    (set! game #:PING))

  ;; ping game
  (when (and  (eq? game #:PING)
	      (eq? key 'r))
    (set! game-over #f)
    (set-ball-pos! ping-ball (centroid (window-width (current-window)) (window-height (current-window))))
    (set-ball-direction-x! ping-ball +)
    (set-ball-direction-y! ping-ball +))
  
  (when (eq? key 'q)
    (abort-game)))

;;; LAUNCH!
;; (define sample (load-audio "assets/audio.mp3"))
;; (audio-play sample)

(define (init)
  (set! ping-ball
	(make-ball (centroid (window-width (current-window)) (window-height (current-window)))
		   10.0 + +))
  (set! ping-racket
	(rect 0.0 5.0 100.0 5.0))
  (set! ping-racket2
	(rect 0.0 (- (window-height (current-window)) 10.0) 100.0 5.0))
  (set! frog-position (init-frog-pos)))

(run-game
 #:load (lambda () (init))
 #:update (lambda (dt) 
	    (poll-coop-repl-server repl)
	    (updates dt))
 #:key-press (lambda (key modifiers repead?)
	       (key-press key modifiers repead?))
 #:mouse-press (lambda (button clicks x y)
		 (set! follow (not follow)))
 #:draw (lambda (alpha)
	  (draw alpha))
 #:clear-color black
 #:window-fullscreen? #t
 #:window-title "PING")
