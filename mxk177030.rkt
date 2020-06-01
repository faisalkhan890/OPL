#lang racket
(provide (all-defined-out))
;function definition for divisible-by-x? 
;Returns true, if the n is divisible by x
;returns false, otherwise
(define ((divisible-by-x? n) x)
    ;Check whether n is divisible by x or not
    (if (= 0 (remainder x n))
        ;If n is divisible by x, return true
        #t
        ;otherwise, return false
        #f)
)



;function definition for function-9
(define function-9
(lambda (number)
    (number 9)))



; function definition for my-map
(define (my-map funct myList)
    ; check the condition
    (cond
        ; if the myList is empty return empty
        [(empty? myList) empty]
        ; otherwise append the values to the myList
        ; Here the first from the list is retrieved
        ; performs the operation of the funct and adds
        ; to the myList
        [else (cons (funct (first myList))
            ; call the my-map function by passing
            ; the funct followed by rest of elements in the list
            (my-map funct (rest myList)))]))
           


;function definition for pair-up 
(define (pair-up list1 list2)
; check whehther the first list is empty or not
(cond ((null? list1) '())
    ; check whether the second list is empty
    ;then return an empty list
    ((null? list2) '())
    ; otherwise
    (else
      ;with two elements
      ;get the first element from the first list list
      ;get the first element from the second list list2
      (cons (list (car list1) (car list2))
      ; call pair-up function in recursive apporach
      ;cdr it returs the first item of list that is
      ;first item from list1 and list2.
      (pair-up (cdr list1) (cdr list2))))))
    

;function definition for  separate
(define (separate boolean lst)
(cond ((null? lst) '())
((equal? boolean even?) (list (even lst) (odd lst)))
((equal? boolean real?) (list (real lst) (non-real lst)))
((equal? boolean integer?) (list (integer lst) (non-integer lst)))))
;;SUB_Function to find even elements
(define (even lst)
(cond ((null? lst) '())
((even? (car lst)) (cons (car lst) (even (cdr lst))))
(else (even (cdr lst)))))
;SUB_Function to find odd elements
(define (odd lst)
(cond ((null? lst) '())
((odd? (car lst)) (cons (car lst) (odd (cdr lst))))
(else (odd (cdr lst)))))
;SUB_Function to find real elements
(define (real lst)
(cond ((null? lst) '())
((real? (car lst)) (cons (car lst) (real (cdr lst))))
(else (real (cdr lst)))))
;SUB_Function to find non-real elements
(define (non-real lst)
(cond ((null? lst) '())
((real? (car lst)) (non-real (cdr lst)))
(else (cons (car lst) (non-real (cdr lst))))))
;SUB_Function to find integer elements
(define (integer lst)
(cond ((null? lst) '())
((integer? (car lst)) (cons (car lst) (integer (cdr lst))))
(else (integer (cdr lst)))))
;SUB_Function to find non-integer elements
(define (non-integer lst)
(cond ((null? lst) '())
((integer? (car lst)) (non-integer (cdr lst)))
(else (cons (car lst) (non-integer (cdr lst))))))


;function definition for is-member?
(define (is-member? x list)
(cond ((null? list) #f) 
((equal? x (car list)) #t) 
(else (is-member? x (cdr list))))) 


;function definition for my-sorted 
(define (my-sorted? < lst)
  (or (null? lst)
      (let loop ((e (car lst)) (lst (cdr lst)))
        (or (null? lst)
            (and (not (< (car lst) e)) 
                 (loop (car lst) (cdr lst)))))))
                

;function definition for my-flatten
(define (my-flatten lst)
   (cond ((null? lst) '())
             ((pair? lst)
               (append (my-flatten (car lst)) (my-flatten (cdr lst))))
             (else (list lst))))
          


;function definition for upper-threshold 
(define (upper-threshold lst theshold)
  (cond
    ((null? lst) '())
    ((< (car lst) theshold)
     (cons (car lst)
           (upper-threshold (cdr lst) theshold)))
    (else (upper-threshold (cdr lst) theshold))))
   


; function definition for my-list-ref
(define (my-list-ref lst pos)
; condition to check whether the list is empty or not.
(cond ((null? lst) (display "ERROR: Index out of bounds"))
;check if the position is zero.
((= pos 0) (car lst))
; Recursive call to the function
; display the number
(else (my-list-ref (cdr lst) (- pos 1)))))


;function definition for deep-reverse
(define (deep-reverse ls)
  (define (deep-reverse-2 ls acc)
    (if (null? ls)
        acc
        (if (list? (car ls))
            (deep-reverse-2 (cdr ls) (cons (deep-reverse (car ls)) acc))
            (deep-reverse-2 (cdr ls) (cons (car ls) acc)))))
  (deep-reverse-2 ls '()))
      