;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 11|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; EXERCISE 1 ---------------------------------------------------------------------------------------
(define-struct branch [in])
(define-struct fork [left right])
 
; A FT (FunkyTree) is one of:
; - "end"
; - (make-branch FT)
; - (make-fork FT FT)

(define FT_END "end")
(define FT1 (make-branch FT_END))
(define FT2 (make-fork FT_END FT_END))
(define FT3 (make-fork FT1 FT2))
(define FT4 (make-fork FT3 FT_END))
(define FT5 (make-branch FT4))

(define (FT-temp ft)
  (... (cond [(string? ft) ...]
             [(branch? ft) (FT-temp (branch-in ft))]
             [(fork? ft) (FT-temp (fork-left ft)) ... (FT-temp (fork-right ft))]
             ...)))

; ft-same? : FunkyTree FunkyTree -> Boolean
; determines whether two FunkyTrees are equal

(check-expect (ft-same? FT5 FT5) #t)
(check-expect (ft-same? FT3 FT5) #f)
(check-expect (ft-same? FT1 FT2) #f)
(check-expect (ft-same? FT_END FT_END) #t)

(define (ft-same? ft1 ft2)
  (cond [(and (string? ft1) (string? ft2)) #t]
        [(and (branch? ft1) (branch? ft2)) (ft-same? (branch-in ft1) (branch-in ft2))]
        [(and (fork? ft1) (fork? ft2)) (and (ft-same? (fork-left ft1) (fork-left ft2))
                                            (ft-same? (fork-right ft1) (fork-right ft2)))]
        [else #f]))

;; EXERCISE 2 ---------------------------------------------------------------------------------------

(define-struct bstnode [data left right])
 
; A StringBST is one of:
; - "leaf"
; - (make-bstnode String StringBST StringBST)
; representing a binary search tree of strings, wherein any nodes
; to the left of a node have a smaller string (according to string<=?)
; and any strings to the right are greater.
 
(define SBST-LEAF "leaf")
(define SBST-A (make-bstnode "a" SBST-LEAF SBST-LEAF))
(define SBST-R (make-bstnode "r" SBST-LEAF SBST-LEAF))
(define SBST-S (make-bstnode "s" SBST-LEAF SBST-LEAF))
(define SBST-SS (make-bstnode "s" SBST-S SBST-LEAF))
(define SBST-RS (make-bstnode "r" SBST-LEAF SBST-S))
(define SBST-RSS (make-bstnode "r" SBST-LEAF SBST-SS))
(define SBST-MRS (make-bstnode "m" SBST-LEAF SBST-RS))
(define SBST-MAR (make-bstnode "m" SBST-A SBST-R))
(define SBST-MARS (make-bstnode "m" SBST-A SBST-RS))
(define SBST-MARSS (make-bstnode "m" SBST-A SBST-RSS))
 
(define (sbst-temp sbst)
  (...
   (cond
     [(string? sbst) ...]
     [(bstnode? sbst)
      (...
       (bstnode-data sbst) ...
       (sbst-temp (bstnode-left sbst)) ...
       (sbst-temp (bstnode-right sbst)) ...)])))
 
; add-to-sbst : String SBST -> SBST
; Adds a string to an SBST
 
(check-expect (add-to-sbst "a" SBST-LEAF) SBST-A)
(check-expect (add-to-sbst "s" SBST-R) SBST-RS)
(check-expect (add-to-sbst "s" SBST-MAR) SBST-MARS)
(check-expect (add-to-sbst "a" SBST-MRS) SBST-MARS)
(check-expect (add-to-sbst "s" SBST-MARS) SBST-MARSS)
 
(define (add-to-sbst str sbst)
  (cond
    [(string? sbst) (make-bstnode str SBST-LEAF SBST-LEAF)]
    [(bstnode? sbst)
     (if (string<=? str (bstnode-data sbst))
         (make-bstnode
          (bstnode-data sbst)
          (add-to-sbst str (bstnode-left sbst))
          (bstnode-right sbst))
         (make-bstnode
          (bstnode-data sbst)
          (bstnode-left sbst)
          (add-to-sbst str (bstnode-right sbst))))]))

; count-words-in-file : [List-of String] String -> [List-of Number]
; produces a list of counts of how many times each search term appears as a word in the file

(check-expect
 (count-words-in-file (list "A" "a" "f" "F" "n" "N" "z" "Z" "foo") "alpha.txt")
 (list 1 1 6 6 14 14 26 26 0))
 
(check-expect
 (count-words-in-file (list "peter") "peter.txt")
 (list 4))
 
(check-expect
 (count-words-in-file (list "wood" "would") "wood.txt")
 (list 2 1))

(define (count-words-in-file los file)
  (local [
          (define STRINGS (read-words file))
          (define BST (foldr add-to-sbst SBST-LEAF STRINGS))
          ; wordcount : String StringBST -> Number
          ; counts the number of times a string appears in the BST
          (define (wordcount s bst)
            (cond
              [(string? bst) 0]
              [(bstnode? bst)
               (+
                (1-if-equal (string-downcase (bstnode-data bst)) (string-downcase s))
                (wordcount s (bstnode-left bst))
                (wordcount s (bstnode-right bst)))]))]
    (map (λ (str) (wordcount str BST)) los)))

; 1-if-equal : String String -> Number
; returns 1 if two strings are equal, otherwise - 0

(check-expect (1-if-equal "a" "a") 1)
(check-expect (1-if-equal "foo" "bar") 0)
(check-expect (1-if-equal "" "") 1)

(define (1-if-equal s1 s2)
  (if (string=? s1 s2) 1 0))

;; EXERCISE 3 ---------------------------------------------------------------------------------------

(define-struct graph [nodes neighbors])
 
; A Graph is a (make-graph [List-of Nat] [Nat -> [List-of Nat]])
; and represents the nodes and edges in a graph, where each node
; is identified by a unique natural number.
 
; All of the numbers in nodes are assumed to be unique, as are the numbers in
; any list returned by neighbors, and all of the numbers returned by neighbors
; are assumed to be in nodes.
 
(define G1
  (make-graph (build-list 7 add1)
              (λ (n)
                (cond [(= n 1) (list 2 5)]
                      [(= n 2) (list 5 6)]
                      [(= n 3) (list 4)]
                      [(= n 4) empty]
                      [(= n 5) (list 3 6 1)]
                      [(= n 6) (list 4 7)]
                      [(= n 7) empty]))))
 
(define G2
  (make-graph (build-list 3 add1)
              (λ (n)
                (cond [(= n 1) (list 2)]
                      [(= n 2) (list 1)]
                      [(= n 3) empty]))))
 
(define G3
  (make-graph (build-list 3 add1)
              (λ (n)
                (cond [(= n 1) (list 2 3)]
                      [(= n 2) (list 1 3)]
                      [(= n 3) (list 1 2)]))))
 
(define (graph-temp g)
  (... (list-template (graph-nodes g))
       (graph-neighbors g) ...))

; neighbor-of? : Graph Number Number -> Boolean
; determines whether one of the numbers is the neighbor of the other

(check-expect (neighbor-of? G1 6 7) #t)
(check-expect (neighbor-of? G1 2 7) #f)
(check-expect (neighbor-of? G2 1 2) #t)
(check-expect (neighbor-of? G3 1 1) #f)

(define (neighbor-of? g n1 n2)
  (or (member? n1 ((graph-neighbors g) n2))
      (member? n2 ((graph-neighbors g) n1))))

;; EXERCISE 4 ---------------------------------------------------------------------------------------

; undirected? : Graph -> Boolean
; determines whether each edge in the graph has a matching edge going in the opposite direction

(check-expect (undirected? G1) #f)
(check-expect (undirected? G2) #t)
(check-expect (undirected? G3) #t)

(define (undirected? g)
  (andmap (λ (node) (mutual-with? g node)) (graph-nodes g)))

; mutual-with? : Graph Nat -> Boolean
; checks whether the number is a neighbor of each of its neighbors

(check-expect (mutual-with? G2 1) #t)
(check-expect (mutual-with? G1 1) #f)
(check-expect (mutual-with? G3 1) #t)

(define (mutual-with? g n)
  (andmap (λ (neighbor) (member? n ((graph-neighbors g) neighbor))) ((graph-neighbors g) n)))


;; EXERCISE 5 ---------------------------------------------------------------------------------------


; fully-connected? : Graph -> Boolean
; checks whether each node in a graph is connected to every other node

(check-expect (fully-connected? G1) #f)
(check-expect (fully-connected? G2) #f)
(check-expect (fully-connected? G3) #t)

(define (fully-connected? g)
  (and (= (length ((graph-neighbors g) (first (graph-nodes g)))) (sub1 (length (graph-nodes g))))
       (andmap (λ (node) (andmap (λ (node2) (mutual? g node node2)) ((graph-neighbors g) node)))
               (graph-nodes g))))


; mutual? : Graph Number Number -> Boolean
; determines whether two nodes are connected both ways

(check-expect (mutual? G1 1 1) #f)
(check-expect (mutual? G2 1 2) #t)
(check-expect (mutual? G1 1 7) #f)

(define (mutual? g n1 n2)
  (and (member? n1 ((graph-neighbors g) n2))
       (member? n2 ((graph-neighbors g) n1))))

;; EXERCISE 6 ---------------------------------------------------------------------------------------

; same-set? : (X) [List-of X] [List-of X] -> Boolean
; Does the second list contain all the items in the
; first (assumed to be distinct) and nothing else?
 
(check-expect (same-set? (list 1 2) (list 2 1)) true)
(check-expect (same-set? (list 1 2) (list 2 1 3)) false)
(check-expect (same-set? (list 1 2) (list 1)) false)
 
(define (same-set? l1 l2)
  (and
   (= (length l1) (length l2))
   (andmap (λ (x) (member? x l2)) l1)))

; reverse-edges : Graph -> Graph
; reverses all edges in the graph

; check-expects are with the reverse-compare function below

(define (reverse-edges g)
  (make-graph (graph-nodes g)
              (foldr (λ (node f) (add-to-f g node f))
                     (λ (n) '()) (graph-nodes g))))

; reverse-compare : Graph Graph -> Boolean
; checks if the graph is reversed

(check-expect (reverse-compare G1 (reverse-edges G1)) #t)
(check-expect (reverse-compare G2 (reverse-edges G2)) #t)
(check-expect (reverse-compare G3 (reverse-edges G3)) #t)
(check-expect (reverse-compare G1 (reverse-edges G2)) #f)

(define (reverse-compare g1 g2)
  (and (same-set? (graph-nodes g1) (graph-nodes g2))
       (andmap (λ (node)
                 (same-set? (get-neighbors-to g1 node) ((graph-neighbors g2) node)))
               (graph-nodes g1))))

; add-to-f : Graph Number [Nat -> [List-of Nat]] -> [Nat -> [List-of Nat]]
; builds up the neighbor function

(define (add-to-f g node f)
  (λ (n) (if (= n node)
             (get-neighbors-to g node)
             (f n))))

; get-neighbors-to : Graph Number -> [List-of Nat]
; gets the list of all the nodes who have the given number as a neighbor

(check-expect (get-neighbors-to G1 1) (list 5))
(check-expect (get-neighbors-to G1 2) (list 1))
(check-expect (get-neighbors-to G2 2) (list 1))
(check-expect (get-neighbors-to G2 3) '())

(define (get-neighbors-to g n)
  (filter (λ (node) (member? n ((graph-neighbors g) node))) (graph-nodes g)))

;; EXERCISE 7 ---------------------------------------------------------------------------------------

; collapse : Nat Nat Nat Graph -> Graph
; The function collapses the first two nodes into one new node, which is named by the third node.
; All nodes in the graph that were pointing to either of the first two given nodes
; should now point to this new one,
; and the new node should point to all nodes either of the first two nodes were pointing to.

; check-expects are with the compare-collapse function below

(define (collapse n1 n2 n3 g)
  (local [(define NEW-NODES (remove-and-add n1 n2 n3 (graph-nodes g)))]
    (make-graph NEW-NODES
                (build-neighbor-func n1 n2 n3 (graph-neighbors g) NEW-NODES))))

(define G2-COLLAPSED
  (make-graph (list 3 4)
              (λ (n)
                (cond [(= n 3) '()]
                      [(= n 4) '()]))))

(define G3-COLLAPSED
  (make-graph (list 3 4)
              (λ (n) (cond [(= n 3) (list 4)]
                           [(= n 4) (list 3)]))))

; compare-collapse : Graph Graph -> Boolean
; compares the collapsed graph

(check-expect (compare-collapse G2-COLLAPSED (collapse 1 2 4 G2)) #t)
(check-expect (compare-collapse G2-COLLAPSED (collapse 2 3 5 G2)) #f)
(check-expect (compare-collapse G3-COLLAPSED (collapse 1 2 4 G3)) #t)
(check-expect (compare-collapse G3-COLLAPSED (collapse 1 2 3 G3)) #f)

(define (compare-collapse g1 g2)
  (and (same-set? (graph-nodes g1) (graph-nodes g2))
       (andmap (λ (node)
                 (same-set? (get-neighbors-to g1 node) ((graph-neighbors g2) node)))
               (graph-nodes g1))))

; replace-n1-n2-with-n3 : Nat Nat Nat [List-of Nat] -> [List-of Nat]
; in the given list, replaces the numbers n1 and n2 with n3

(check-expect (replace-n1-n2-with-n3 1 2 3 (list 1 2 4)) (list 3 4))
(check-expect (replace-n1-n2-with-n3 4 5 6 (list 1 2 3)) (list 1 2 3))
(check-expect (replace-n1-n2-with-n3 7 8 9 '()) '())

(define (replace-n1-n2-with-n3 n1 n2 n3 lon)
  (if (or (member? n1 lon) (member? n2 lon))
      (remove-and-add n1 n2 n3 lon)
      lon))

; remove-and-add Nat Nat Nat [List-of Nat] -> [List-of Nat]
; in a given list removes the first two numbers and replaces them with the third

(check-expect (remove-and-add 1 2 3 (list 1 2 4)) (list 3 4))
(check-expect (remove-and-add 4 5 6 (list 1 2 3)) (list 6 1 2 3))
(check-expect (remove-and-add 7 8 9 '()) (list 9))

(define (remove-and-add n1 n2 n3 lon)
  (cons n3 (remove-n1-n2 n1 n2 lon)))

; build-neighbor-func : Nat Nat Nat [Nat -> [List-of Nat]] [List-of Nat] -> [Nat -> [List-of Nat]]
; returns the new function for finding neighbors in a graph

(define (build-neighbor-func n1 n2 n3 old-f lon)
  (foldr (λ (node f) (build-neighbor-f lon old-f n1 n2 n3 node f))
         (λ (n) '()) lon))

; build-neighbor-f : [List-of Nat] [Nat -> [List-of Nat]]
;                     Nat Nat Nat Nat [Nat -> [List-of Nat]]
;                     -> [Nat -> [List-of Nat]]
; helper function for the build-neighbor-func
; builds up a new neighbor function for the collapsed graph

(define (build-neighbor-f lon old-f n1 n2 n3 node f)
  (λ (n) (cond
           [(and (= n node) (not (= node n3))) (replace-n1-n2-with-n3 n1 n2 n3 (old-f n))]
           [(and (= n node) (= node n3))
            (remove-duplicates (remove-n1-n2 n1 n2 (append (old-f n1) (old-f n2))))]
           [else (f n)])))

; remove-n1-n2 : Nat Nat [List-of Nat] -> [List-of Nat]
; removes all instances of two numbers in the given list

(check-expect (remove-n1-n2 1 2 '()) '())
(check-expect (remove-n1-n2 1 2 (list 1 2 3)) (list 3))
(check-expect (remove-n1-n2 3 4 (list 1 2)) (list 1 2))

(define (remove-n1-n2 n1 n2 lon)
  (filter (λ (n) (not (or (= n n1) (= n n2)))) lon))

; remove-duplicates : (X) [List-of X] -> [List-of X]
; removes all duplicate entries from a list

(check-expect (remove-duplicates (list 1 1 2 2 3 3 4 5 6)) (list 1 2 3 4 5 6))
(check-expect (remove-duplicates '()) '())
(check-expect (remove-duplicates (list "a" "b" "foo" "foo" "bar")) (list "a" "b" "foo" "bar"))
(check-expect (remove-duplicates (list 1 1 1 1 1 1 1)) (list 1))

(define (remove-duplicates lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon) (if (member? (first lon) (rest lon))
                     (remove-duplicates (rest lon))
                     (cons (first lon) (remove-duplicates (rest lon))))]))