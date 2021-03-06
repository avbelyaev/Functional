
(define (interval+ a b)
  (let* ((a1 (+ (car a) (car b)))
         (b1 (+ (cadr a) (cadr b)))
         (xs (list a1 b1)))
    xs))
(define (interval- a b)
  (let* ((a1 (- (car a) (car b)))
         (b1 (- (cadr a) (cadr b)))
         (xs (list a1 b1)))
    xs))
(define (interval* a b)
  (let* ((a-c (* (car a) (car b)))
         (a-d (* (car a) (cadr b)))
         (b-c (* (cadr a) (car b)))
         (b-d (* (cadr a) (cadr b)))
         (umn (list a-c a-d b-c b-d))
         (xs (list (apply min umn) (apply max umn))))
    xs))
(define (interval/ a b)
  (let* ((a1 (* 1.0 (/ (car a) (car b))))
         (a2 (* 1.0 (/ (car a) (cadr b))))
         (b1 (* 1.0 (/ (cadr a) (car b))))
         (b2 (* 1.0 (/ (cadr a) (cadr b))))
         (del (list a1 a2 b1 b2))
         (xs (list (apply min del) (apply max del))))
    xs))
 
(define (includes-zero? xs)
   (let  ((a (car xs))
         (b (cadr xs)))
         (or (zero? a) (zero? b) (and (< a 0) (> b 0)))))
 
 
(define (avg-dev%->interval x d)
  (let* ((a (* 1.0 (- x (* (/ x 100) d))))
         (b (* 1.0 (+ x (* (/ x 100) d))))
         (xs (list a b)))
    xs))
(define (apply-to-intervals formula a b)
  (list (formula (car a) (car b)) (formula (cadr a) (cadr b))))

