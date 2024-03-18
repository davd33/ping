(define-module (ping collisions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (chickadee)
  #:use-module (chickadee math rect)
  #:use-module (chickadee math vector)
  #:use-module (ping record-types)
  #:export (bounce-ball))

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


