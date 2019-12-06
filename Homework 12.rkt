;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 12|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; HOMEWORK 12 ---------------------------------------
; Batyr Kornusov, Harry (Henry) Rose

; EXERCISE 1 ----------------------------------------------------------

; remove-consec-dupl : [List-of String] -> [List-of String]
; takes a list of string and eliminates consecutive duplicate elements

(check-expect (remove-consec-dupl (list "a" "a" "b" "c" "c" "c" "a" "b" "b"))
              (list "a" "b" "c" "a" "b"))
(check-expect (remove-consec-dupl '()) '())
(check-expect (remove-consec-dupl (list "a" "a" "a" "a" "b")) (list "a" "b"))

; without accumulators
#|(define (remove-consec-dupl los)
  (cond [(empty? los) '()]
        [(empty? (rest los)) los]
        [(string=? (first los) (second los)) (remove-consec-dupl (rest los))]
        [else (cons (first los) (remove-consec-dupl (rest los)))]
        ))|#

; with accumulators
(define (remove-consec-dupl los)
  (cond [(empty? los) '()]
        [else
         (local
           [; remove-consec-dupl/acc : [List-of String] [List-of String] String -> [List-of String] 
            ; removes consecutive duplicates in a list by accumulating the output list
            ; Accumulator : The list with consecutive duplicates removed so far
            ; Accumuator : the previous element of the list
            (define (remove-consec-dupl/acc los out prev)
              (cond [(empty? los) out]
                    [(string=? (first los) prev) (remove-consec-dupl/acc (rest los) out prev)]
                    [else (remove-consec-dupl/acc (rest los)
                                                  (append out (list (first los)))
                                                  (first los))]))]
           (cons (first los) (remove-consec-dupl/acc (rest los) '() (first los))))]))

; EXERCISE 2 ----------------------------------------------------------

; biggest-consec-streak : [NEList-of String] -> String
; determines the string with the biggest consecutive streak in the non-empty list

(check-expect (biggest-consec-streak (list "b" "a" "a" "b" "c" "c" "c" "b" "b")) "c")
(check-expect (biggest-consec-streak (list "a" "a" "a")) "a")
(check-expect (biggest-consec-streak (list "b" "a" "b" "c" "a" "d")) "b")

(define (biggest-consec-streak los)
  (local [; current-streak : [List-of String] String Number Number -> String
          ; compares the current streak to the max string in the list
          ; Accumulator : The value of the longest streak
          ; Accumulator : The length of the longest streak
          ; Accumulator : The length of the current streak
          (define (current-streak los max-val max current) 
            (cond [(empty? (rest los)) max-val]
                  [(string=? (first los) (second los))
                   (if (> (add1 current) max)
                       (current-streak (rest los) (first los) (add1 current) (add1 current))
                       (current-streak (rest los) max-val max (add1 current)))]
                  [else (current-streak (rest los) max-val max 1)]))]
    (current-streak los (first los) 1 1)))

; EXERCISE 3 ----------------------------------------------------------

; rotation? : [List-of String] [List-of String] -> Boolean
; checks whether the two lists are a rotation of each other

(check-expect (rotation? (list "a" "b" "c") (list "c" "a" "b")) #t)
(check-expect (rotation? '() '()) #t)
(check-expect (rotation? (list "a" "b") (list "a" "b" "c")) #f)
(check-expect (rotation? (list "a" "b" "d") (list "a" "b" "c")) #f)
(check-expect (rotation? (list "b" "a" "d") (list "d" "a" "b")) #f)

(define (rotation? los1 los2)
  (local [; rotate : [List-of String] -> [List-of String] 
          ; rotates the list by one position
          (define (rotate los)
            (append (rest los) (list (first los))))

          ; compare-list : [List-of String] [List-of String] Number -> Boolean
          ; compares the current list with the second rotated list
          ; Accumulator : number of rotations left
          (define (compare-list los1 los2 count)
            (cond [(and (empty? los1) (empty? los2)) #t]
                  [(zero? count) #f]
                  [else (or (list=? los1 los2 string=?)
                            (compare-list los1 (rotate los2) (sub1 count)))]))]
    (compare-list los1 los2 (length los1))))

; list=? : (X) [List-of X] [List-of X] [X X -> Boolean] -> Boolean
; determines whether the two lists have the same contents
; taken from Homework 9

(check-expect (list=? (list "a" "bc")
                      (list "d" "ef")
                      (λ (a b) (= (string-length a) (string-length b))))
              true)
(check-expect (list=? (list 4 5 6)
                      (list 13 14 15)
                      =)
              false)
(check-expect (list=? '() '() string=?) true)


(define (list=? l1 l2 func)
  (cond [(not (= (length l1) (length l2))) #f]
        [else (andmap identity (map func l1 l2))]))

; EXERCISE 4 ----------------------------------------------------------

; my-build-list : (X) Number [Number -> X] -> [List-of X]
; implements the build-list function

(check-expect (my-build-list 5 identity) (build-list 5 identity))
(check-expect (my-build-list 10 add1) (build-list 10 add1))
(check-expect (my-build-list 4 number->string) (build-list 4 number->string))
(check-expect (my-build-list 0 sub1) '())

(define (my-build-list n f)
  (local [; current-element : (X) Number -> [List-of X]
          ; accumulator function, helper function
          ; Accumulator : the index of the current element
          (define (current-element k)
            (cond [(= k n) '()]
                  [else (cons (f k) (current-element (add1 k)))]))]
    (current-element 0)))

; EXERCISE 5 ----------------------------------------------------------

(define-struct ntnode [data children])
 
; An [NTree X] is one of:
; - false
; - (make-ntnode X [NEList-of [NTree X]])
; representing an n-ary tree or a leaf node
 
; A [Forest X] is a [List-of [NTree X]]
; and represents a forest of n-ary trees

(define NT0 (make-ntnode "a" (list (make-ntnode "b"
                                                (list (make-ntnode "c" (list #f))
                                                      (make-ntnode "d" (list #f))))
                                   (make-ntnode "e"
                                                (list (make-ntnode "f" (list #f)))))))
(define NT1 (make-ntnode "g" (list (make-ntnode "h" (list #f)))))

(define NT0-D (make-ntnode 0 (list (make-ntnode 1
                                                (list (make-ntnode 2 (list #f))
                                                      (make-ntnode 2 (list #f))))
                                   (make-ntnode 1
                                                (list (make-ntnode 2 (list #f)))))))
(define NT1-D (make-ntnode 0 (list (make-ntnode 1 (list #f)))))
(define EMPTYFOREST '())
(define FOREST0 (list NT0 NT1))
(define FOREST0-D (list NT0-D NT1-D))

#; (define (ntree-temp t)
     (cond [(false? t) ...]
           [(ntnode? t) ... (ntnode-data t) ... (list-temp (ntnode-children t)) ...]))

#; (define (forest-temp f)
     (list-temp f))

; depth-trees : [Forest X] -> [Forest Number]
; converts a forest into one that has the same structure of each tree,
; but the data represents the depth of each node

(check-expect (depth-trees '()) '())
(check-expect (depth-trees FOREST0) FOREST0-D)

(define (depth-trees forest)
  (local [; tree-depth : [Tree X] Number -> [Tree Number]  
          ; converts the tree into the tree where the data represents its depth
          ; Accumulator : current depth of the tree
          (define (tree-depth tree depth)
            (cond [(false? tree) #f]
                  [(ntnode? tree)
                   (make-ntnode depth (map (λ (t) (tree-depth t (add1 depth)))
                                           (ntnode-children tree)))]))]
    (map (λ (tree) (tree-depth tree 0)) forest)))

; EXERCISE 6 -------------------------------------------------------------------

; base16->string : Nat[0, 15] -> 1String
; Converts a base16 digit to a string
 
(check-expect (base16->string 0) "0")
(check-expect (base16->string 5) "5")
(check-expect (base16->string 9) "9")
(check-expect (base16->string 10) "A")
(check-expect (base16->string 15) "F")
 
(define (base16->string nat)
  (local [(define BASE-ASCII
            (if (< nat 10) 48 55))]
    (string (integer->char (+ BASE-ASCII nat)))))

; base10->16 : Nat -> String
; Converts the given non-negative integer from decimal to hexidecimal

(check-expect (base10->16 10) "A")
(check-expect (base10->16 5) "5")
(check-expect (base10->16 15) "F")
(check-expect (base10->16 30) "1E")
(check-expect (base10->16 15) "F")

(define (base10->16 nat)
  (local [(define QUOTIENT (quotient nat 16))
          (define REMAINDER (modulo nat 16))]
    (if (zero? QUOTIENT)
        (base16->string REMAINDER)
        (string-append (base10->16 QUOTIENT)
                       (base16->string REMAINDER)))))

; Termination : since the initial number is finite, and each recursion happens on a smaller number,
; eventually the number must become 0, and therefore the program must terminate

; EXERCISE 7 -------------------------------------------------------------------

; mergesort : [List-of Number] -> [List-of Number]
; sorts the given list of numbers using Mergesort

(check-expect (mergesort (list 5 1 2 4 3)) (list 1 2 3 4 5))
(check-expect (mergesort '()) '())
(check-expect (mergesort (list 10)) (list 10))
(check-expect (mergesort (list 1 2 3 4)) (list 1 2 3 4))

(define (mergesort lon)
  (cond [(< (length lon) 2) lon]
        [else (local [(define HALF (quotient (length lon) 2))
                      (define LON1 (take lon HALF))
                      (define LON2 (drop lon HALF))]
                (merge (mergesort LON1) (mergesort LON2)))]))

; Termination : since the list is finite, and the recursion happens on a smaller portion of the list,
; then we will eventually reach a list of length 1 or 0

; take : (X) [List-of X] Number -> [List-of X]
; returns the given list with the first Number elements

(check-expect (take (list 4 12 14 15) 2) (list 4 12))
(check-expect (take '() 12) '())
(check-expect (take (list "a" "b" "c" "d" "e") 15) (list "a" "b" "c" "d" "e"))

(define (take lox n)
  (cond
    [(or (zero? n) (empty? lox)) '()]
    [else (cons (first lox) (take (rest lox) (sub1 n)))]))

; drop : (X) [List-of X] Number -> [List-of X]
; returns the given list with the first Number elements removed

(check-expect (drop (list 1 2 3 4 5) 2) (list 3 4 5))
(check-expect (drop '() 15) '())
(check-expect (drop (list "a" "b" "c") 16) '())

(define (drop lox n)
  (cond
    [(or (zero? n) (empty? lox)) lox]
    [else (drop (rest lox) (sub1 n))]))

; merge : [List-of Number] [List-of Number] -> [List-of Number]
; merges the two lists together, while sorting it

(check-expect (merge (list 1 2 4) (list 3 5)) (list 1 2 3 4 5))
(check-expect (merge '() '()) '())
(check-expect (merge (list 4 5 6) '()) (list 4 5 6))

(define (merge lon1 lon2)
  (cond
    [(and (empty? lon1) (cons? lon2)) lon2]
    [(and (empty? lon2) (cons? lon1)) lon1]
    [(and (empty? lon1) (empty? lon2)) '()]
    [(and (cons? lon1) (cons? lon2))
     (if (< (first lon1) (first lon2))
         (cons (first lon1) (merge (rest lon1) lon2))
         (cons (first lon2) (merge lon1 (rest lon2))))]))