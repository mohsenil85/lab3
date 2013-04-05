(define (f lst)
  (if (null? lst)
    '()
    (cons (+ 1 (car lst)) (f (cdr lst)))))

(define (good-set? lst)
  (if (not (memq #f (map integer? lst)))
    #t
    #f))
