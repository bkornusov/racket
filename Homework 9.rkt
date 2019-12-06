;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework 9|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Homework 9
; Batyr Kornusov, Harry (Henry) Rose

(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)

; EXERCISE 1 --------------------------------------------------------

; read-lolon : String -> [List-of [List-of Number]]
; produces a list of lists of numbers from a supplied file name,
; where each inner list is a row in the file

(check-expect (read-lolon "numbers.txt")
              (list
               (list 0 0 0 0)
               (list 1 2 3 4)
               (list 2 1 0 -0.5)))

(define (read-lolon file)
  (map (λ (los) (map string->number los)) (read-words/line file)))
  

; EXERCISE 2 ---------------------------------------------------------

; map-2list : (X Y Z) [X Y -> Z] [List-of X] [List-of Y] -> [List-of Z]
; takes a function and applies it to the first element of both lists,
; then the second of both lists, ...

(check-expect (map-2list + (list 1 2) (list 3 4)) (list 4 6))
(check-expect (map-2list * '() '()) '())
(check-error (map-2list / (list 3) (list 12 14 5 6)))
(check-error (map-2list - (list 4 5 6) (list 3)))

(define (map-2list func lox loy)
  (cond [(and (empty? lox) (empty? loy)) '()]
        [(or (empty? lox) (empty? loy)) (error "Lists not same size")]
        [(and (cons? lox) (cons? loy))
         (cons
          (func (first lox) (first loy)) (map-2list func (rest lox) (rest loy)))]))

; EXERCISE 3 ----------------------------------------------------------

; list=? : (X) [List-of X] [List-of X] [X X -> Boolean] -> Boolean
; determines whether the two lists have the same contents

; Without abstactions

(check-expect (list=? (list "a" "bc")
                      (list "d" "ef")
                      (λ (a b) (= (string-length a) (string-length b))))
              true)
(check-expect (list=? (list 5 6)
                      (list (- 6 1) (+ 3 3))
                      =)
              true)
(check-expect (list=? (list 5 6 13)
                      (list 5 6)
                      =)
              false)
(check-expect (list=? (list 5 6 13)
                      (list 5 6)
                      =)
              false)
(check-expect (list=? '() '() string=?) true)

(define (list=? l1 l2 func)
  (cond [(and (empty? l1) (empty? l2)) true]
        [(or (empty? l1) (empty? l2)) false]
        [(and (list? l1) (list? l2))
         (and (func (first l1) (first l2)) (list=? (rest l1) (rest l2) func))]))

; With abstactions

(check-expect (list-abs=? (list "a" "bc")
                          (list "d" "ef")
                          (λ (a b) (= (string-length a) (string-length b))))
              true)
(check-expect (list-abs=? (list 5 6)
                          (list (- 6 1) (+ 3 3))
                          =)
              true)
(check-expect (list-abs=? (list 4 5 6)
                          (list 13 14 15)
                          =)
              false)
(check-expect (list-abs=? (list 5 6 13)
                          (list 5 6)
                          =)
              false)
(check-expect (list-abs=? '() '() string=?) true)


(define (list-abs=? l1 l2 func)
  (cond [(not (= (length l1) (length l2))) false]
        [else (not (ormap false? (map func l1 l2)))]))


; EXERCISE 4 ----------------------------------------------------------

; interleave : (X) [List-of X] [List-of X] -> [List-of X]
; takes two lists and produces a list of their items, alternating from each list

(check-expect (interleave (list 1 2) (list 3 4 5)) (list 1 3 2 4 5))
(check-expect (interleave (list 1 2 3) '()) (list 1 2 3))
(check-expect (interleave '() '()) '())
(check-expect (interleave (list 4 5) (list 1 2 3)) (list 4 1 5 2 3))
(check-expect (interleave '() (list 1 2 3)) (list 1 2 3))

(define (interleave l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) '()]
    [(and (empty? l1) (cons? l2)) l2]
    [(and (cons? l1) (empty? l2)) l1]
    [(and (cons? l1) (cons? l2))
     (cons (first l1) (interleave l2 (rest l1)))]))

; EXERCISE 5 ----------------------------------------------------------

(define-struct test [unanswered correct answered?])
; a Test is a (make-test [List-of String] [List-of String] Boolean), where
; - unanswered is a list of unanswered questions
; - correct is the list of correctly asnwered questions
; - answered? represents the current question state

(define test1 (make-test
               (list "T, question1"
                     "T, question2"
                     "F, question3"
                     "F, question4")
               (list "T, question2"
                     "F, question3")
               false))
(define test2 (make-test
               (list "F, question5"
                     "F, question6")
               '()
               true))

#; (define (test-temp t)
     (... (los-temp (test-unanswered t)) ... (los-temp (test-correct t)) ...))

; take-tftest : String -> Number
; returns the proportion of correctly answered questions in the given test

(define (take-tftest file)
  (local
    [(define TEST (big-bang (make-test (read-lines file) '() false)
                    [on-key answer]
                    [to-draw draw-test]
                    [stop-when done?]))]
    (/ (length (test-correct TEST)) (length (read-lines file)))))

; answer : Test KeyEvent -> Test
; returns an updated test based on the pressed key

(check-expect (answer test1 " ") test1)
(check-expect (answer test1 "\r")
              (make-test
               (list "T, question1"
                     "T, question2"
                     "F, question3"
                     "F, question4")
               (list "T, question1"
                     "T, question2"
                     "F, question3")
               true))
(check-expect (answer test1 "escape")
              (make-test
               (list "T, question1"
                     "T, question2"
                     "F, question3"
                     "F, question4")
               (list "T, question2"
                     "F, question3")
               true))
(check-expect (answer test2 "escape") test2)
(check-expect (answer test2 " ")
              (make-test (list
                          "F, question6")
                         '()
                         false))

(define (answer test key)
  (cond [(test-answered? test)
         (if (string=? key " ")
             (make-test (rest (test-unanswered test)) (test-correct test) false)
             test)]
        [else
         (cond
           [(string=? key "\r") (check-answer test "T")]
           [(string=? key "escape") (check-answer test "F")]
           [else test])]))

; check-answer : Test String -> Test
; helper function that checks whether a given answer is correct


(check-expect (check-answer test1 "T")
              (make-test
               (list "T, question1"
                     "T, question2"
                     "F, question3"
                     "F, question4")
               (list "T, question1"
                     "T, question2"
                     "F, question3")
               true))
(check-expect (check-answer test1 "F")
              (make-test
               (list "T, question1"
                     "T, question2"
                     "F, question3"
                     "F, question4")
               (list "T, question2"
                     "F, question3")
               true))

(define (check-answer test ans)
  (if (correct? (first (test-unanswered test)) ans)
      (make-test (test-unanswered test)
                 (cons (first (test-unanswered test)) (test-correct test))
                 true)
      (make-test (test-unanswered test)
                 (test-correct test)
                 true)))

; correct? : String String -> Boolean
; checks whether the answer for the given question is correct

(check-expect (correct? "T, question10" "T") true)
(check-expect (correct? "F, question11" "T") false)
(check-expect (correct? "T, question12" "F") false)
(check-expect (correct? "F, question13" "F") true)

(define (correct? question answer)
  (string=? (substring question 0 1) answer))

(define BACKGROUND (rectangle 600 200 "solid" "white"))

; draw-test : Test -> Image
; displays the test to the screen

(check-expect (draw-test test1) (underlay
                                 BACKGROUND
                                 (draw-question test1)))
(check-expect (draw-test test2) (underlay
                                 BACKGROUND
                                 (above (draw-question test2) (draw-answer test2))))

(define (draw-test test)
  (underlay BACKGROUND
            (cond
              [(test-answered? test) (above (draw-question test) (draw-answer test))]
              [else (draw-question test)])))

; draw-question : Test -> Image
; displays the current question of the test

(check-expect (draw-question test1) (text "question1" 16 "black"))
(check-expect (draw-question test2) (text "question5" 16 "black"))

(define (draw-question test)
  (text (substring (first (test-unanswered test)) 3) 16 "black"))

; draw-answer : Test -> Image
; displays the current answer and its correctness (red - wrong, green - correct)

(check-expect (draw-answer test1) (text "T" 32 "red"))
(check-expect (draw-answer test2) (text "F" 32 "red"))
(check-expect (draw-answer (make-test (list
                                       "T, q1"
                                       "T, q2")
                                      (list
                                       "T, q1")
                                      false)) (text "T" 32 "green"))
(check-expect (draw-answer (make-test (list
                                       "F, q1"
                                       "T, q2")
                                      (list
                                       "F, q1")
                                      false)) (text "F" 32 "green"))

(define (draw-answer test)
  (cond [(empty? (test-correct test))
         (text (substring (first (test-unanswered test)) 0 1) 32 "red")]
        [(string=? (first (test-unanswered test)) (first (test-correct test)))
         (text (substring (first (test-unanswered test)) 0 1) 32 "green")]
        [else
         (text (substring (first (test-unanswered test)) 0 1) 32 "red")]))

; done? : Test -> Boolean
; checks whether the test is over

(check-expect (done? test1) false)
(check-expect (done? (make-test '() '() false)) true)

(define (done? test)
  (empty? (test-unanswered test)))
