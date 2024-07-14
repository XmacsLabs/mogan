
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : logic-test.scm
;; DESCRIPTION : examples of logical programs
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel logic logic-test)
  (:use (kernel logic logic-rules) (kernel logic logic-query)))

(import (srfi srfi-78))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples of rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-rules
  ((son% Joris Piet))
  ((son% Piet Opa))
  ((daughter% Geeske Opa))
  ((daughter% Jekke Opa))
  ((child% 'x 'y) (son% 'x 'y))
  ((child% 'x 'y) (daughter% 'x 'y))
  ((descends% 'x 'y) (child% 'x 'y))
  ((descends% 'x 'z) (child% 'x 'y) (descends% 'y 'z)))


;; (logic-query (child% 'x Opa))
;; (logic-query (descends% 'x 'y))
;; (logic-query (descends% 'x 'y) (daughter% Joleen Opa))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Addional assumptions <-> creating modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-rules
  (assume family%) ; added to constraints for all rules below
  ((son% Joris Piet))
  ((son% Piet Opa))
  ((daughter% Geeske Opa))
  ((daughter% Jekke Opa))
  ((child% 'x 'y) (son% 'x 'y))
  ((child% 'x 'y) (daughter% 'x 'y))
  ((descends% 'x 'y) (child% 'x 'y))
  ((descends% 'x 'z) (child% 'x 'y) (descends% 'y 'z)))

;; (logic-query (child% 'x Opa))
;; (logic-query (child% 'x Opa) family%)

(tm-define (regtest-logic)
  (display (logic-query (child% 'x Opa))))
