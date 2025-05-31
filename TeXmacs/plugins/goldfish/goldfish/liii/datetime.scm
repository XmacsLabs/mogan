;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii datetime)
(import (liii oop))
(export datetime date years)
(begin

(define-object years

(define (@leap? year)
  (when (not (integer? year))
    (type-error "years@leap? must accept integer"))
  (or (and (zero? (modulo year 4)) 
           (not (zero? (modulo year 100))))
      (zero? (modulo year 400))))

)

(define-case-class datetime
  ((year integer?)
   (month integer?)
   (day integer?)
   (hour integer? 0)
   (minute integer? 0)
   (second integer? 0)
   (micro-second integer? 0))

(chained-define (@now)
  (let ((time-vec (g_datetime-now)))
    (datetime 
      :year (vector-ref time-vec 0)
      :month (vector-ref time-vec 1)
      :day (vector-ref time-vec 2)
      :hour (vector-ref time-vec 3)
      :minute (vector-ref time-vec 4)
      :second (vector-ref time-vec 5)
      :micro-second (vector-ref time-vec 6))))

(define (%to-string)
  (define (pad2 n)  ; 补零到 2 位
    (if (< n 10)
        (string-append "0" (number->string n))
        (number->string n)))
  (define (pad6 n)  ; 补零到 6 位（微秒）
    (let ((s (number->string n)))
      (string-append (make-string (- 6 (string-length s)) #\0) s)))
  
  (let ((date-part (string-append (number->string year) "-"
                                 (pad2 month) "-"
                                 (pad2 day)))
        (time-part (string-append (pad2 hour) ":"
                                 (pad2 minute) ":"
                                 (pad2 second))))
    (if (zero? micro-second)
        (string-append date-part " " time-part)
        (string-append date-part " " time-part "." (pad6 micro-second)))))

(define (%plus-days days-to-add)
  
  (define (days-in-month m y)
    (cond ((member m '(4 6 9 11)) 30)
          ((= m 2) (if (years :leap? y) 29 28))
          (else 31)))
  
  (let loop ((y year)
             (m month)
             (d day)
             (remaining-days days-to-add))
    (cond
      ;; No more days to add
      ((zero? remaining-days)
       (datetime :year y
                 :month m
                 :day d
                 :hour hour
                 :minute minute
                 :second second
                 :micro-second micro-second))
      
      ;; Adding days (positive)
      ((> remaining-days 0)
       (let ((days-in-current-month (days-in-month m y)))
         (if (<= (+ d remaining-days) days-in-current-month)
             ;; Simple case: result is within the same month
             (loop y m (+ d remaining-days) 0)
             ;; Complex case: need to move to next month
             (let ((next-month (if (= m 12) 1 (+ m 1)))
                   (next-year (if (= m 12) (+ y 1) y)))
               (loop next-year 
                     next-month 
                     1 
                     (- (+ remaining-days d) days-in-current-month 1))))))
      
      ;; Subtracting days (negative)
      (else
       (if (> d (abs remaining-days))
           ;; Simple case: result is within the same month
           (loop y m (+ d remaining-days) 0)
           ;; Complex case: need to move to previous month
           (let* ((prev-month (if (= m 1) 12 (- m 1)))
                  (prev-year (if (= m 1) (- y 1) y))
                  (days-in-prev-month (days-in-month prev-month prev-year)))
             (loop prev-year 
                   prev-month 
                   days-in-prev-month 
                   (+ remaining-days d))))))))

(define (%plus-months months-to-add)
  (define (days-in-month m y)
    (cond ((member m '(4 6 9 11)) 30)
          ((= m 2) (if (years :leap? y) 29 28))
          (else 31)))
  
  ;; Calculate new year and month
  (let* ((total-months (+ (+ (* year 12) month -1) months-to-add))
         (new-year (quotient total-months 12))
         (new-month (+ (remainder total-months 12) 1))
         ;; Adjust day if necessary
         (days-in-new-month (days-in-month new-month new-year))
         (new-day (min day days-in-new-month)))
    
    (datetime :year new-year
              :month new-month
              :day new-day
              :hour hour
              :minute minute
              :second second
              :micro-second micro-second)))

(define (%plus-years years-to-add)
  (define (days-in-month m y)
    (cond ((member m '(4 6 9 11)) 30)
          ((= m 2) (if (years :leap? y) 29 28))
          (else 31)))
  
  ;; Calculate new year
  (let* ((new-year (+ year years-to-add))
         ;; Adjust day if necessary (for Feb 29 in leap years)
         (days-in-new-month (days-in-month month new-year))
         (new-day (min day days-in-new-month)))
    
    (datetime :year new-year
              :month month
              :day new-day
              :hour hour
              :minute minute
              :second second
              :micro-second micro-second)))

)

(define-case-class date
  ((year integer?)
   (month integer?)
   (day integer?))

(chained-define (@now)
  (let ((time-vec (g_date-now)))
    (date 
      :year (vector-ref time-vec 0)
      :month (vector-ref time-vec 1)
      :day (vector-ref time-vec 2))))

(define (%to-string)
  (define (pad2 n)  ; 补零到 2 位
    (if (< n 10)
        (string-append "0" (number->string n))
        (number->string n)))
  
  (let ((date-part (string-append (number->string year) "-"
                                  (pad2 month) "-"
                                  (pad2 day))))
       date-part))

)

) ; end of begin
) ; end of define-library

