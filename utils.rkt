#lang racket
(require srfi/13)
(provide (all-defined-out))

(define (getmaxlength strings)
  (let loop ((lst strings) (max 0))
    (if (null? lst) max
        (if (> (string-length (first lst)) max) (loop (rest lst) (string-length (first lst)))
            (loop (rest lst) max)))))

(define (string-list->fixed-width-array width items)
   (string-join (map (lambda (i) (string-pad-right i width #\null)) items)))

(define (gen-output-string width)
  (string-pad-right "" width #\null))
