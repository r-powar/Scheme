;Duc Nguyen and Raj Powar
; CS 152

; problem 1
; Predicate procedure list-of-symbols tests if a given list lst
; is a list of top-level symbols.
(define (list-of-symbols? lst)
    (cond
        ((null? lst) #f)
        ((and (symbol? (car lst)) (null? (cdr lst))) #t)
        ((not (symbol? (car lst))) #f)
        (else (and (symbol? (car lst)) (list-of-symbols? (cdr lst))))))

; problem 2's helper procedure        
; Predicate procedure same-type? tests if its two arguments 
; have the same type.
(define (same-type? a b)
    (if (or (and (boolean? a) (boolean? b))
            (and (integer? a) (integer? b))
            (and (rational? a) (rational? b))
            (and (real? a) (real? b))
            (and (complex? a) (complex? b))
            (and (char? a) (char? b))
            (and (symbol? a) (symbol? b))
            (and (string? a) (string? b))
            (and (vector? a) (vector? b)))
        #t
        #f))
        
; problem 2
; Predicate procedure same-structure? tests if its two arguments 
; have the same structure;  i.e., corresponding elements between the two arguments 
; have the same type.
(define (same-structure? lst1 lst2)
    (cond
        ((and (null? lst1) (null? lst2)) #t)
        ((or (null? lst1) (null? lst2)) #f)
        ((same-type? (car lst1) (car lst2))
            (same-structure? (cdr lst1) (cdr lst2)))
        ((and (pair? (car lst1)) (pair? (car lst2)))
            (and (same-structure? (car lst1) (car lst2))
                 (same-structure? (cdr lst1) (cdr lst2))))
        (else #f)))
        
; problem 3
; Procedurehash-get has two arguments, an item key and a list hash-table. 
; List hash-table is a list of pairs of the form (key value). Search the hash 
; table for the first pair with a matching key and return that pair. The key can be 
; any type.
(define (hash-get key hash-table)
    (cond   
        ((null? hash-table) '())
        ((and (pair? (car hash-table)) (equal? key (caar hash-table))) 
            (car hash-table))
        (else (hash-get key (cdr hash-table)))))

; problem 4
; Procedure subst-1st has three parameters: an item new, an item old, and a 
; list. Replace the first top-level occurrence of item old in the list with item new. 
; Items new and old can be lists. 
(define (subst-1st new old lst)
    (cond
        ((null? lst) '())
        ((equal? old (car lst)) (cons new (cdr lst)))
        (else (cons (car lst) (subst-1st new old (cdr lst))))))
        
; problem 5
; Procedure replace replaces each top-level item in a list of items by a given item. 
; The given item can itself be a list. 
(define (replace item lst)
    (cond
        ((null? lst) '())
        (else (cons item (replace item (cdr lst))))))

; problem 6
; Procedure list-of-first-items has an argument that is a list of nonempty 
; sublists of items. The return value is a list of the first top-level item in each of the 
; sublists. A top-level item can itself be a list. 
(define (list-of-first-items lst)
    (cond
        ((null? lst) '())
        (else (cons (caar lst) (list-of-first-items (cdr lst))))))

; problem 7's helper procedure.
; Procedure remove-1st removes the first occurrence of a given item from a 
; list of items.
(define (remove-1st item lst)
    (cond
        ((null? lst) '())
        ((equal? item (car lst)) (cdr lst))
        (else (cons (car lst) (remove-1st item (cdr lst))))))
        
; problem 7
; Procedure remove-2nd removes the second occurrence of a given item from a 
; list of items. The given item can itself be a list. 
(define (remove-2nd item lst)
    (cond
        ((null? lst) '())
        ((equal? item (car lst)) (cons item (remove-1st item (cdr lst))))
        (else (cons (car lst) (remove-2nd item (cdr lst))))))
        
; problem 8
; Procedure remove-last removes the last occurrence of a given item from a list 
; of items. The given item can itself be a list.
(define (remove-last item lst)
    (cond 
        ((null? lst) '())
        ((equal? item (car lst))
            (if (member item (cdr lst))
                (cons (car lst) (remove-last item (cdr lst)))
                (cdr lst)))
        (else (cons (car lst) (remove-last item (cdr lst))))))

; problem 9
; Predicate procedure all-same? tests whether a given list has all top-level 
; elements the same.
(define (all-same? lst)
    (cond 
        ((null? lst) #t) 
        ((null? (cdr lst)) #t)
        (else (and (equal? (car lst) (cadr lst))
                   (all-same? (cdr lst))))))
                 
; problem 10
; Procedure sandwich-1st takes two items aand band a list as its arguments. 
; Replace the first occurrence of two successive b’s in the list with b a b. 
(define (sandwich-1st a b lst)
    (cond 
        ((null? lst) '())
        ((null? (cdr lst)) (cons (car lst) '()))
        ((and (equal? b (car lst)) (equal? b (cadr lst))) 
            (cons b (cons a (cons b (cddr lst)))))
        (else (cons (car lst) (sandwich-1st a b (cdr lst))))))