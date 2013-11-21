
; Some utility functions that you may find useful.
(define nil '())
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Problem 18

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
    (cond 
        ((null? list1) list2)
        ((null? list2) list1)
        ((comp (car list1) (car list2)) (cons (car list1) (merge comp (cdr list1) list2)))
        (else (cons (car list2) (merge comp (cdr list2) list1)))
    ))

(merge < '(1 5 7 9) '(4 8 10))
; expect (1 4 5 7 8 9 10)
(merge > '(9 7 5 1) '(10 8 4 3))
; expect (10 9 8 7 5 4 3 1)

;; Sort a list of lists of numbers to be in decreasing lexicographic
;; order. Relies on a correct implementation of merge.
(define (sort-lists lsts)
  (if (or (null? lsts) (null? (cdr lsts)))
      lsts
      (let ((sublsts (split lsts)))
        (merge greater-list
               (sort-lists (car sublsts))
               (sort-lists (cdr sublsts))))))

(define (greater-list x y)
  (cond ((null? y) #t)
        ((null? x) #f)
        ((> (car x) (car y)) #t)
        ((> (car y) (car x)) #f)
        (else (greater-list (cdr x) (cdr y)))))

(define (split x)
  (cond ((or (null? x) (null? (cdr x))) (cons x nil))
        (else (let ((sublsts (split (cdr (cdr x)))))
                (cons (cons (car x) (car sublsts))
                      (cons (car (cdr x)) (cdr sublsts)))))))

(merge greater-list '((3 2 1) (1 1) (0)) '((4 0) (3 2 0) (3 2) (1)))
; expect ((4 0) (3 2 1) (3 2 0) (3 2) (1 1) (1) (0))


; Problem 19

; def count_partitions(n, m):
;  """Partition n into pieces no larger than m"""
;   if n == 0:
;     return 1
;   elif n < 0:
;     return 0
;   elif m == 0:
;     return 0
;   else:
;     with_m = count_partitions(n-m, m)
;     without_m = count_partitions(n, m-1)
;     return with_m + without_m

;; A list of all ways to partition TOTAL, where  each partition must
;; be at most MAX-VALUE and there are at most MAX-PIECES partitions.
(define (list-partitions total max-pieces max-value)
  (define (partition tracker-list total max-pieces max-value)
    (cond
      ((or (< max-pieces 1) (< max-value 1) (< total 0)) nil)
      ((= total 0) (list tracker-list))
      (else
        (append (partition (append tracker-list (list max-value)) (- total max-value) (- max-pieces 1) max-value)
                (partition tracker-list total max-pieces (- max-value 1)))
      )
    )
  )

  (partition nil total (+ 1 max-pieces) max-value)
)

; Problem 19 tests rely on correct Problem 18.
(sort-lists (list-partitions 5 2 4))
; expect ((4 1) (3 2))
(sort-lists (list-partitions 7 3 5))
; expect ((5 2) (5 1 1) (4 3) (4 2 1) (3 3 1) (3 2 2))


; Problem 20

;; The Tree abstract data type has an entry and a list of children.
(define (make-tree entry children)
  (cons entry children))
(define (entry tree)
  (car tree))
(define (children tree)
  (cdr tree))

;; An example tree:
;;                5
;;       +--------+--------+
;;       |        |        |
;;       6        7        2
;;    +--+--+     |     +--+--+
;;    |     |     |     |     |
;;    9     8     1     6     4
;;                      |
;;                      |
;;                      3
(define tree
  (make-tree 5 (list
                (make-tree 6 (list
                              (make-tree 9 nil)
                              (make-tree 8 nil)))
                (make-tree 7 (list
                              (make-tree 1 nil)))
                (make-tree 2 (list
                              (make-tree 6 (list
                                            (make-tree 3 nil)))
                              (make-tree 4 nil))))))

;; Takes a TREE of numbers and outputs a list of sums from following each
;; possible path from root to leaf.
(define (tree-sums tree)
  
    (define (flatten lst)
      ; Helper function--takes a list that potentially has lists
      ; inside of it and flattens it

        (define (flatten-complex lst)
         (cond 
           ((null? (cdr lst)) (car lst))
           (else (append (flatten (car lst)) (flatten (cdr lst))))
         )
       )
    
       (define (no-nesting lst)
         (cond 
           ((null? lst) #t)
           ((not (number? (car lst))) #f)
           (else (no-nesting (cdr lst)))
         )
       )

       (cond 
         ((no-nesting lst) lst)
         (else (flatten-complex lst))
       )
     )
    (if (null? (children tree))
        (entry tree)
        (map (lambda (child) (+ child (entry tree))) (flatten (map tree-sums (children tree))))
    )
 )

(tree-sums tree)
; expect (20 19 13 16 11)


; Problem 21 (optional)

; Draw the hax image using turtle graphics.
(define (hax d k)
  ; *** YOUR CODE HERE ***
  nil)
