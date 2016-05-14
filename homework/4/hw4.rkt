
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (cond [(> low high) null]
        [(<= (+ low stride) high)
             (cons low (sequence (+ low stride) high stride))]
        [#t (cons low null)]))

(define (string-append-map xs suffix)
  (map (λ (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs)  (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

(define (stream-for-n-steps s n)
  (cond [(> n 0) (cons (car (s))
                       (stream-for-n-steps (cdr (s)) (- n 1)))]
        [#t null]))

(define (funny-number-stream)
  (letrec ([foo (λ (num) (cons (if (= 0 (remainder num 5)) (- num) num)
                               (λ () (foo (+ 1 num)))))])
    (foo 1)))

(define (dan-then-dog)
  (letrec ([foo (λ (name) (cons (string-append name ".jpg")
                                (λ () (foo (if (string=? name "dan")
                                               "dog" "dan")))))])
    (foo "dan")))
(define (stream-add-zero s)
  (letrec ([foo (λ (stream) (cons
                             (cons 0 (car (stream)))
                             (λ () (foo (cdr (stream))))))])
  (λ () (foo s))))

(define (cycle-lists xs ys)
  (letrec ([foo (λ (num) (cons (cons (list-nth-mod xs num)
                                     (list-nth-mod ys num))
                               (λ () (foo (+ 1 num)))))])
    (λ () (foo 0))))
(define (vector-assoc v vec)
  (letrec ([find (λ (iter) (cond [(= iter (vector-length vec)) #f]
                                 [(pair? (vector-ref vec iter))
                                  (if (equal? (car (vector-ref vec iter)) v)
                                      (vector-ref vec iter)
                                      (find (+ 1 iter)))]
                                 [#t #f]))])
  (find 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [iter 0]
           [assoc-with-set (λ (v) (let ([val-by-assoc (assoc v xs)])
                                    (begin (vector-set! cache iter val-by-assoc)
                                           (if (= n (+ 1 iter)) (set! iter 0)
                                               (set! iter (+ 1 iter)))
                                           val-by-assoc)))]
           [find (λ (v) (let ([val-in-cache (vector-assoc v cache)])
                          (if val-in-cache val-in-cache (assoc-with-set v))))])
    find))

(define-syntax while-less
  (syntax-rules (do)
  [(while-less e1 do e2)
   (letrec ([to e1]
         [foo (λ () (if (< e2 e1) (foo) #t))])
     (foo))]))
         

