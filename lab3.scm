;;logan Mohseni
;;-01449704
;;lab 3 -- scheme
;;includes code for good-set?, member? union, and intersect
;;MIT/GNU SCHEME
(define (f lst)
  (if (null? lst);;base condition:  returns an empty list literal when it finds the end of the list
    '()
    (cons (+ 1 (car lst)) (f (cdr lst)))));;maps 'plus 1' to each element in the list


;this is kind of weird.  first it maps the predicate 
  ;;integer to every member of the list.  this returns a list of boolean literals
  ;;that looks something like (#t #t #t #f).  in this case, the predicate integer 
  ;;would have detected a non integer.
  ;;then, the memq function checks each the list for the presence of a literal false.
;;Ideally, I would have liked to do something like (reduce (and (#t #t #t #t))).
;;where (#t #t #t) would have been the out put of (map (integer? lst)).  unfortunatel;;y, I could not get this to work, hence the awkward application of memq.
(define (all-ints? lst)
  (if  (memq #f (map integer? lst))
    #f
    #t))

;returns true if there are duplicates and false if there are not.  the cond function
;can act like a pattern matcher, or if-elseif-else branches in imperitive code.
;the fist branch returns false if lst is null, so that we can terminate the recursion
;;;  the secnond branch checks if the car in list is contained in the cdr of list.  
;;;  if that returns false, then the else branch of cond is hit, which simply discard;;; the car of list, and then recursively applies dupes? to the cdr of list.
(define (dupes? lst)
  (cond ((null? lst) #f)
        ((member (car lst) (cdr lst) ) #t)
        (else (dupes? (cdr lst)))))

;;;so good-set is basically the previous 2 functions anded together.  because 
;;; dupes? basically returns the opposite  of what i want, i had to wrap him in a not;;; I think it's easy to tell that most of this code was written from the repl, which;;; is a strength of lispy languages.  I work on a smaller peice of the problem, get ;;; something that works, and then add it together.
(define (good-set? lst)
  (and (all-ints? lst) (not (dupes? lst))))

;;;  this is my implementation of the member function.  it takes an element e and 
;;;  checks to see if it is in lst. it uses the lambda to check each element by turn ;;; in list, and since lambda is a function that returns, the entire list is converte;;; to another list of booleans.  it seems like this is a pattern we run into in
;;;  scheme a lot
(define (member? e lst)
  (memq #t
    (map (lambda (x)
           (eq? e x))
         lst)))

;;remove-dupes is a helper function for union.  it uses cond to branch between all 
;;the cases.  if it is null, it is the base case and it bottoms out the recusion.  if
;;the car of lst is a member of the cdr of list, it just applies remove dupes 
;;recursively to the cdr of lst.  otherwise, it keeps the car of list, and conses it 
;;onto the cdr of list to which remove-dupes has been recusivly applied
(define (remove-dupes lst)
  (cond
    ((null? lst) '())
    ((member? (car lst) (cdr lst))
     (remove-dupes (cdr lst)))
     (else (cons (car lst) (remove-dupes (cdr lst))))))

;;so with the remove-dupes function sorted, taking the union of 2 lists is pretty
;;simple.  we just append the lists together, and then remove any duplicates.
(define (union lst1 lst2)
  (remove-dupes (append lst1 lst2)))

;;this was probably the hardest funciton to write, because it needs a local
;;variable to store the result in.  first, it does the usual bottoming out of 
;;the recusion.  then, it checks to see if the car of list1 is contained in list 2
;;using the member function, and stores that result in accumulator.  if we do get 
;; results stored in accumulator, then we hit the second branch of the inner if state
;; statement, in which case it conses the car of list1 onto a recursive call of the 
;; cdr of list1 onto list2.  otherwise it just calls intersect on the cdr of list 1 
;; to list 2.  
(define (intersect lst1 lst2)
  (if (null? lst1) '()
    (let ((accumulator (member? (car lst1) lst2)))
      (if (null? (cdr lst1))
        (if accumulator lst1 '())
        (if accumulator (cons (car lst1 ) ( intersect (cdr lst1) lst2))
          (intersect (cdr lst1 ) lst2))))))

;;to flatten a list, we use a cond to branch between the different possibilities,
;;since this is (another) recursive function .  if not pair? comes back true, it
;;that lst is a cons cell, and not a list.  so it is sort of a another base case where ;;we want to come back false from it.  otherwsie, we just append the flattened car of;; 
;; list to the flattened cdr of list.
;; note that this does not work on eg a list like (((())) (()) ()) since it would 
;; flatten them all into nil

(define (flatten lst)
  (cond ((null? lst) '())
       ((not (pair? lst)) (list lst))
       (else (append (flatten (car lst)) (flatten (cdr lst))))))
