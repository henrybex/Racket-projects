;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; Usual Instructions:
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the _design recipe_ for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2023F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get no errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether or Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New Instructions                                                           ;;
;; 1. Many problems have provided signatures and purpose statements that you  ;;
;;    should not modify.                                                      ;;
;; 2. When we write "complete the following function design", you should      ;;
;;    write the function definition and check-expects.                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;! Problem 1

;; Complete the following function design.

;;! average-of-two-lists : [List-of Number] [List-of Number] -> [List-of Number]
;;! Produces a list of numbers where each number is the average of the
;;! corresponding numbers in the two lists.

(define LIST1 (list 2 4 6 8))
(define LIST2 (list 1 2 3 4))
(define LIST3 (list 0 0 0 0))

(check-expect (average-of-two-lists LIST1 LIST2)
              (list 1.5 3 4.5 6))
(check-expect (average-of-two-lists LIST1 LIST3)
              (list 1 2 3 4))
(check-expect (average-of-two-lists LIST2 LIST3)
              (list 0.5 1 1.5 2))

(define (average-of-two-lists l1 l2)
  (if (and (cons? l1) (cons? l2))
      (cons (/ (+ (first l1) (first l2)) 2)
            (average-of-two-lists (rest l1) (rest l2)))
      '()))


;;! Problem 2

;; Complete the following function design *without using the builtin function
;; replicate*.

;;! repeat-strings-solo : Nat [List-of String] -> [List-of String]
;;! (repeat-strings-solo n slist) produces a produces a list of strings where
;;! each output string is the corresponding input string repeated n times.
(check-expect (repeat-strings-solo 3 (list "a" "b" "c"))
              (list "aaa" "bbb" "ccc"))
(check-expect (repeat-strings-solo 5 (list "i"))
              (list "iiiii"))
(check-expect (repeat-strings-solo 2 (list "m" "a" "y" "a"))
              (list "mm" "aa" "yy" "aa"))
(define (repeat-strings-solo n slist)
  (local [(define (repeat z s string)
            (if (> z 0)
            (string-append s (repeat (- z 1) s string))
            string))]
  (map (lambda (x) (repeat n x "")) slist)))

;;! Problem 3

;; Complete the following function design *and you may use the builtin
;; replicate.*

;;! repeat-strings : [List-of String] [List-of Nat] -> [List-of String]
;;! (repeat-strings slist nlist) produces a list of strings from slist, where
;;! each is duplicated N times, where N is the corresponding number in
;;! nlist. However:
;;!
;;! 1. If there  are more strings than numbers, assume that the extra strings
;;!    should be repeated twice each.
;;! 2. If there are more numbers than strings, for each extra number N,
;;!    repeat the the string "Extra!" N times.

(define STRING1 (list "a" "b" "c"))
(define NUM1 (list 1 2 3))
(define STRING2 (list "hello" "good morning"))
(define NUM2 (list 2 4 6))
(define STRING3 (list "do" "re" "me" "fa" "so"))
(define NUM3 (list 1 2 3 4))

(check-expect (repeat-strings STRING1 NUM1)
              (list "a" "bb" "ccc"))
(check-expect (repeat-strings STRING2 NUM2)
              (list
               "hellohello"
               "good morninggood morninggood morninggood morning"
               "Extra!Extra!Extra!Extra!Extra!Extra!"))
(check-expect (repeat-strings STRING3 NUM3)
              (list "do" "rere" "mememe" "fafafafa" "soso"))

(define (repeat-strings slist nlist)
  (cond [(and (empty? slist) (empty? nlist))
         '()]
        [(and (cons? slist) (empty? nlist))
         (cons (replicate 2 (first slist)) (repeat-strings (rest slist) '()))]
        [(and (empty? slist) (cons? nlist))
         (cons (replicate (first nlist) "Extra!") (repeat-strings '() (rest nlist)))]
        [(and (cons? slist) (cons? nlist))
         (cons (replicate (first nlist) (first slist)) (repeat-strings (rest slist) (rest nlist)))]))

;;! Problem 4

;; Consider the following data definitions (we have omitted examples and
;; templates).

(define-struct student [name nuid])
;;! A Student is a (make-student String Number)
;;! Interpretation: represents a student

(define-struct grade [nuid course value])
;;! A Grade is a (make-grade Number String Number)
;;! (make-grade nuid course grade) represents the grade that
;;! a student received in a course.

(define-struct student-grades [name grades])
;;! A StudentGrades is a (make-student-grades String [List-of Number]).
;;! (make-student-grades name grades) represents the grades
;;! that a student has received in all courses.

;; Complete the following function design.

;;! students->student-grades: [List-of Student] [List-of Grade] -> [List-of StudentGrades]
;;! Produces a StudentGrade for each student, with the list of grades that
;;! student received. The list produced should have an item for every student in the
;;! input list, even if there are no grades for that student.
(define STUDENT1 (make-student "Maya" 123456))
(define STUDENT2 (make-student "Maggie" 234567))
(define STUDENT3 (make-student "Kate" 345678))

(define GRADE1 (make-grade 123456 "Fundies 1" 100))
(define GRADE2 (make-grade 123456 "Public Speaking" 99))
(define GRADE3 (make-grade 234567 "Comm 1" 98))
(define GRADE4 (make-grade 234567 "Music" 97))
(define GRADE5 (make-grade 345678 "Music Industry" 99))

(check-expect (students->student-grades (list STUDENT1) (list GRADE1 GRADE2 GRADE3))
              (list (make-student-grades "Maya" (list GRADE1 GRADE2))))
(check-expect (students->student-grades (list STUDENT1 STUDENT2) (list GRADE1 GRADE2 GRADE3 GRADE4))
              (list (make-student-grades "Maya" (list GRADE1 GRADE2))
                    (make-student-grades "Maggie" (list GRADE3 GRADE4))))
(check-expect (students->student-grades (list STUDENT1 STUDENT2 STUDENT3)
                                              (list GRADE1 GRADE2 GRADE4 GRADE5))
              (list (make-student-grades "Maya" (list GRADE1 GRADE2))
                    (make-student-grades "Maggie" (list GRADE4))
                    (make-student-grades "Kate" (list GRADE5))))
(define (students->student-grades slist glist)
  (local [(define (match-id nuid glist)
            (filter (lambda (x) (= nuid (grade-nuid x))) glist))]
    (map (lambda (x) (make-student-grades (student-name x) (match-id (student-nuid x) glist))) slist)))
