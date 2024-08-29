;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) John Cowan (2015). All Rights Reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

;;;; Main part of the SRFI 114 reference implementation

;;; "There are two ways of constructing a software design: One way is to
;;; make it so simple that there are obviously no deficiencies, and the
;;; other way is to make it so complicated that there are no *obvious*
;;; deficiencies." --Tony Hoare

(define-library (srfi srfi-128)
(export boolean<? boolean-hash)
(begin

(define (boolean<? a b)
  ;; #f < #t but not otherwise
  (and (not a) b))

(define %salt% (lambda () 16064047))

(define (boolean-hash obj)
  (if obj (%salt%) 0))

) ; end of begin
) ; end of define-library

