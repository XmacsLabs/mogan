;
; BSD License by Peter Danenberg
;

(define-library (liii alist)
(import (liii base)
        (liii list)
        (liii error)
        (scheme case-lambda))
(export alist? alist-cons alist-ref alist-ref/default)
(begin

(define (alist? l)
  (and (list? l)
       (every pair? l)))

(define alist-ref
  (case-lambda
    ((alist key)
     (alist-ref
       alist
       key
       (lambda () (key-error "alist-ref: key not found " key))))
    ((alist key thunk)
     (alist-ref alist key thunk eqv?))
    ((alist key thunk =)
     (let ((value (assoc key alist =)))
       (if value
           (cdr value)
           (thunk))))))

(define alist-ref/default
  (case-lambda
    ((alist key default)
     (alist-ref alist key (lambda () default)))
    ((alist key default =)
     (alist-ref alist key (lambda () default) =))))

) ; end of begin
) ; end of library

