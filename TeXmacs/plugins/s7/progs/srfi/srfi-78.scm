; <PLAINTEXT>
; Copyright (c) 2005-2006 Sebastian Egner.
; 
; Permission is hereby granted, free of charge, to any person obtaining
; a copy of this software and associated documentation files (the
; ''Software''), to deal in the Software without restriction, including
; without limitation the rights to use, copy, modify, merge, publish,
; distribute, sublicense, and/or sell copies of the Software, and to
; permit persons to whom the Software is furnished to do so, subject to
; the following conditions:
; 
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
; 
; -----------------------------------------------------------------------
; 
; Lightweight testing (reference implementation)
; ==============================================
;
; Sebastian.Egner@philips.com
; in R5RS + SRFI 23 (error) + SRFI 42 (comprehensions)
;
; history of this file:
;   SE, 25-Oct-2004: first version based on code used in SRFIs 42 and 67
;   SE, 19-Jan-2006: (arg ...) made optional in check-ec
;
; Naming convention "check:<identifier>" is used only internally.
;
; Copyright (c) 2024 Da @ Liii Network Inc.
; Follow the same License as the original one

(provide 'srfi-78)
(provide 'check)
(provide 'check-set-mode!)
(provide 'check-report)
(provide 'check-reset!)

(define check:write write)

(define check:mode #f)

(define (check-set-mode! mode)
  (set! check:mode
        (case mode
          ((off)           0)
          ((summary)       1)
          ((report-failed) 10)
          ((report)        100)
          (else (error "unrecognized mode" mode)))))


(check-set-mode! 'report)

(define check:correct #f)
(define check:failed   #f)

(define (check-reset!)
  (set! check:correct 0)
  (set! check:failed   '()))

(define (check:add-correct!)
  (set! check:correct (+ check:correct 1)))

(define (check:add-failed! expression actual-result expected-result)
  (set! check:failed
        (cons (list expression actual-result expected-result)
              check:failed)))

(check-reset!)

(define (check:report-expression expression)
  (newline)
  (check:write expression)
  (display " => "))

(define (check:report-actual-result actual-result)
  (check:write actual-result)
  (display " ; "))

(define (check:report-correct cases)
  (display "correct")
  (if (not (= cases 1))
      (begin (display " (")
             (display cases)
             (display " cases checked)")))
  (newline))

(define (check:report-failed expected-result)
  (display "*** failed ***")
  (newline)
  (display "; expected result: ")
  (check:write expected-result)
  (newline))

(define (check-report)
  (if (>= check:mode 1)
      (begin
        (newline)
        (display "; *** checks *** : ")
        (display check:correct)
        (display " correct, ")
        (display (length check:failed))
        (display " failed.")
        (if (or (null? check:failed) (<= check:mode 1))
            (newline)
            (let* ((w (car (reverse check:failed)))
                   (expression (car w))
                   (actual-result (cadr w))
                   (expected-result (caddr w)))                  
              (display " First failed example:")
              (newline)
              (check:report-expression expression)
              (check:report-actual-result actual-result)
              (check:report-failed expected-result))))))

(define (check-passed? expected-total-count)
  (and (= (length check:failed) 0)
       (= check:correct expected-total-count)))

(define (check:proc expression thunk equal expected-result)
  (case check:mode
    ((0) #f)
    ((1)
     (let ((actual-result (thunk)))
       (if (equal actual-result expected-result)
           (check:add-correct!)
           (check:add-failed! expression actual-result expected-result))))
    ((10)
     (let ((actual-result (thunk)))
       (if (equal actual-result expected-result)
           (check:add-correct!)
           (begin
             (check:report-expression expression)
             (check:report-actual-result actual-result)
             (check:report-failed expected-result)
             (check:add-failed! expression actual-result expected-result)))))
    ((100)
     (check:report-expression expression)
     (let ((actual-result (thunk)))
       (check:report-actual-result actual-result)
       (if (equal actual-result expected-result)
           (begin (check:report-correct 1)
                  (check:add-correct!))
           (begin (check:report-failed expected-result)
                  (check:add-failed! expression 
                                     actual-result 
                                     expected-result)))))
    (else (error "unrecognized check:mode" check:mode))))

(define-macro (check expr => expected)
  `(check:proc ',expr (lambda () ,expr) equal? ,expected))

