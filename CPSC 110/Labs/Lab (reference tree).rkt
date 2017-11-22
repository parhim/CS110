;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Lab (reference tree)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Lab 6

;; =================
;; Data definitions:
(define-struct s (txt nexts))
;; Sentence is (make-s String ListOfSentence)
;; interp. a sentence with text and the possible sub-texs

;; ListOfSentence is one of:
;; - empty
;; - (cons Sentence ListOfSentence)
;; interp. a list of sentences

(define S-JOKE (make-s "joking " (list (make-s "about jealousy." empty))))
(define S-LIKE (make-s "like " (list (make-s "YOU REALLY MEAN IT." empty) (make-s "WE ARE IN A BACK TO SCHOOL SPECIAL ABOUT MONO." empty) (make-s "WE ARE PERCHED ON THE TIP OF A SINKING SHIP." empty))))
(define S-TO (make-s "to " (list (make-s "FREEZE TIME." empty) (make-s "MY FAVOURITE SONG ON REPEAT." empty))))
(define LOS-0 empty)
(define LOS1 (list (make-s "go to the left." empty) (make-s "go to the right." empty)))
(define S0 (make-s "LOL" empty))
(define S1 (make-s "Kiss me" (list S-JOKE S-LIKE S-TO)))
(define S9 (make-s "I want to" LOS1))
#;#;
(define (fn-for-s s)
  (... (s-txt s)
       (fn-for-los (s-nexts s))))

(define (fn-for-los los)
  (cond [(empty? los) ...]
        [else
         (... (fn-for-s (first los))
              (fn-for-los (rest los)))]))


;; =================
;; Functions:

;; Sentence -> Natural
;; ListOfSentence -> Natural
;; produces the amount of sentences in a sentence tree

(check-expect (count--s S0) 0)
(check-expect (count--s S1) 6)
(check-expect (count--los LOS1) 2)

(define (count--s s)
  (if (not (string-contains? "." (s-txt s)))
      (count--los (s-nexts s))
      1))

(define (count--los los)
  (cond [(empty? los) 0]
        [else
         (+ (count--s (first los))
            (count--los (rest los)))]))



;; Sentence -> Image
;; ListOfSentence -> Image
;; renders the tree

(check-expect (render--s S9) (beside (text "I want to" 20 "black") (above/align "left" (text "go to the left." 20 "black") (text "go to the right." 20 "black"))))

(define (render--s s)
  (beside (text (s-txt s) 20 "black")
          (render--los (s-nexts s))))

(define (render--los los)
  (cond [(empty? los) empty-image]
        [else
         (above/align "left" (render--s (first los))
                (render--los (rest los)))]))


 (render--s S1)