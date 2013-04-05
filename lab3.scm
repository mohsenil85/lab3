(define (f lst)
  (if (null? lst)
    '()
    (cons (+ 1 (car lst)) (f (cdr lst)))))

(define (all-ints? lst)
  (if  (memq #f (map integer? lst))
    #f
    #t))

;returns true if there are duplicates and false if there are not
(define (dupes? lst)
  (cond ((null? lst) #f)
        ((member (car lst) (cdr lst) ) #t)
        (else (dupes? (cdr lst)))))

(define (good-set? lst)
  (and (all-ints? lst) (not (dupes? lst))))

(define (member? e lst)
  (memq #t
    (map (lambda (x)
           (eq? e x))
         lst)))

(define (union lst1 lst2)
  (cond ((or (null? lst1) (null? lst2))
         "null list!")
        ((
 
