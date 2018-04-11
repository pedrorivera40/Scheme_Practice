#lang racket

; @author Pedro Luis Rivera
; Programming Languages Ch. 15 - Programming Problems

; Exercise 1: Write a Scheme function that compute the volume of a sphere given its radius...

(define pi 3.14159)
(define (sphere_volume radius) (* (/ 4 3) pi radius radius radius))
(display "Exercise 1\n")
(sphere_volume 3)

; Exercise 2: Write a Scheme function that computes the real roots of a given quadratic equation using the IF function.
; If the roots are complex, print an appropriate message indicating this.

(define (discriminant a b c) (- (* b b) (* 4 a c)))

(define (solve_quadratic_if a b c)
  (if (> (discriminant a b c) 0)
      (list (/ (+ (- 0 b) (sqrt (discriminant a b c))) 2 a) (/ (- (- 0 b) (sqrt (discriminant a b c))) 2 a))
      (if (= (discriminant a b c) 0)
            (list (/ (- 0 b) 2 a))
            (display "COMPLEX ROOTS!"))
   )
 )
(display "Exercise 2\n")
(solve_quadratic_if 1 2 0)
(solve_quadratic_if 1 -2 1)
(solve_quadratic_if 1 -2 100)
(newline)
; Exercise 3: Repeat exercise 2 using a COND function rather than an IF function.
(define (solve_quadratic_cond a b c)
  (cond
    ((> (discriminant a b c) 0) (list (/ (+ (- 0 b) (sqrt (discriminant a b c))) 2 a) (/ (- (- 0 b) (sqrt (discriminant a b c))) 2 a)))
    ((= (discriminant a b c) 0) (list (/ (- 0 b) 2 a)))
    (else (display "COMPLEX ROOTS!"))
   ))
(display "Exercise 3\n")
(solve_quadratic_cond 1 2 0)
(solve_quadratic_cond 1 -2 1)
(solve_quadratic_cond 1 -2 100)
(newline)

; Exercise 4: Write a Scheme function that returns the number of zeros in a given simple list of integers.

(define (zero_count data)
  (if (null? data) 0
      (if (eq? (car data) 0) (+ 1 (zero_count (cdr data)))
          (+ 0 (zero_count (cdr data))))
       )
 )
(display "Exercise 4\n")
(zero_count '(1 0 0 1 0 1))

; Exercise 5: Write a Scheme function that delete all top-level instances of a given atom from a given list.

(define (remove_top_level atom data) 
  (if (null? data) '()
      (if (eq? atom (car data)) (remove_top_level atom (cdr data))
          (cons (car data) (remove_top_level atom (cdr data)))
          )
      )
 )
(display "Exercise 5\n")
(remove_top_level 1 '(1 0 0 1 1 0 0 0 1))

; Exercise 6: Write a Scheme function that removes the last element from a given list.

(define (remove_last data)
  (if (null? data) (display "EMPTY LIST!")
      (if (null? (cdr data)) '()
          (cons (car data) (remove_last (cdr data)))
          )
      )
  )
(display "Exercise 6\n")
(remove_last '(0 1 1 0))

; Exercise 7: Repeat exercise 5, except that the atom can be either an atom or a list.
(define (remove_occurrences values data)
  (if (null? values) data
      (if (list? values) (remove_occurrences (cdr values) (remove_top_level (car values) data))
          (remove_top_level values)
       )
   )
 )
(display "Exercise 7\n")
(remove_occurrences '(1 2) '(1 2 3 1 1 1 2 2 1 2 1 0 0 1 2 1))

; Exercise 8: Write a Scheme function that takes two atoms and a list as parameters and replaces
; all occurrences of the first given atom in the list with the second given atom no matter how deeply the first atom is nested.

; Sol. 1
(define (deep_replace atom1 atom2 data)
  (if (null? data) '()
      (if (list? data) (cons (deep_replace atom1 atom2 (car data)) (deep_replace atom1 atom2 (cdr data)))
          (if (eq? data atom1) atom2
              data
           )
          )
   )
  )
(display "Exercise 8: IF\n")
(deep_replace 1 2 '(1 '(1 2 2) 3 3 4 4 1))

; Sol. 2
(define (deep_replace_cond atom1 atom2 data)
  (cond ((null? data) '())
        ((list? data) (cons (deep_replace_cond atom1 atom2 (car data)) (deep_replace_cond atom1 atom2 (cdr data))))
        ((eq? atom1 data) atom2)
        (else data)
   )
  )
(display "Exercise 8: COND\n")
(deep_replace_cond 1 2 '(1 '(1 2 2) 3 3 4 4 1))

; Exercise 9: Write a Scheme function that returns the reverse of its simple list parameter:
(define (reverse_simple_list data)
  (if (null? data) '()
      (append (reverse_simple_list (cdr data)) (list (car data))))
 )
(display "Exercise 9\n")
(reverse_simple_list '(1 2 3 2 1 5))

; Exercise 10: Write a Scheme predicate function that tests for the structural equality of two given lists.
; Two lists are structurally equal if they have the same list structure, although their atoms may be different.

(define (struct_helper atom1 atom2)
  (cond
    ((eq? atom1 atom2) #T)
    ((null? atom1) #F)
    ((null? atom2) #F)
    ((list? atom1)
     (if (list? atom2) #T #F))
    ((list? atom1) #F)
    ((list? atom2) #F)
    (else #T)
   )
 )

(define (struct_equivalence data1 data2)
  (cond
    ((eq? data1 data2) #T)
    ((null? data1) #F)
    ((null? data2) #F)
    ((struct_helper (car data1) (car data2)) (struct_equivalence (cdr data1) (cdr data2)))
    (else #F)
   )
 )
(display "Exercise 10\n")
(struct_equivalence '(1 '(1)) '(5 2))

; Exercise 11: Write a Scheme function that returns the union of two simple list parameters that represent sets.

(define (is_member value data)
  (cond
   ((null? data) #F)
   ((null? value) #T)
   ((eq? value (car data)) #T)
   (else (is_member value (cdr data)))
   )
 )
(display "Exercise 11: SET-MEMBERSHIP\n")
(is_member 1 '(1 2 3))

(define (remove_repetitions data)
  (cond
   ((null? data) '())
   ((is_member (car data) (cdr data)) (cons (car data) (remove_repetitions (remove_top_level (car data) (cdr data)))))
   (else (cons (car data) (remove_repetitions (cdr data))))
   )
 )

(define (set_union data1 data2)
  (if (not (list? data1))
      (display "data1 is not a list.")
      (if (not (list? data2))
          (display "data2 is not a list.")
          (remove_repetitions (append data1 data2)))
   )
 )
(display "Exercise 11: SET-UNION\n")
(set_union '(1 2 3) '(1 1 1 4 5 1))