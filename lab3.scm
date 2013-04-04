(define (f lst)
  (if (null? lst)
    '()
    (cons (+ 1 (car lst)) (f (cdr lst)))))

