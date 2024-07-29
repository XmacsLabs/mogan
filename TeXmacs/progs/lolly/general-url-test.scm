;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : general-url-test.scm
;; DESCRIPTION : Test suite for URL operations
;; COPYRIGHT   : (C) 2024  ATQlove
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (srfi srfi-78))

;; Test function for URL descendants
(define (regtest-url-descendants)
   (let ((base-url (string->url "$TEXMACS_PATH")))
     (display (string-append "Base URL: " (url->string base-url) "\n"))
     (let ((descendants (url-descendants base-url)))
       (display "Debugging descendants list:\n")
       (display descendants)
       (check
        (not (null? descendants)) => #t))))

;; Test function for URL or operation
(define (regtest-url-or)
   (let* ((url1 (string->url "$TEXMACS_PATH"))
          (url2 (string->url "$TEXMACS_PATH/tests"))
          (combined-url (url-or url1 url2)))
     (check
      (or (url-exists? combined-url) (null? combined-url)) => #t)
     (check
      (url-exists? url1) => #t)
     (check
      (url-exists? url2) => #t)))

;; Test function for URL expand operation
(define (regtest-url-expand)
   (let ((input-url (string->url (string-append "$TEXMACS_PATH" "/path_not_exists"))))
     ;; Check if the expanded URL does not exist
     (check
      (url-exists? (url-expand input-url)) => #f)))

;; Test function for URL complete operation
(define (regtest-url-complete)
      ;; complete the URL based on the specified filter "fr" to find matching file resources.
      ;; "fr": "File Resource."  "dr": "Directory Resource." "r": "Resource.(including files & directories)" 
   (let ((complete-url (url-complete (string->url "$TEXMACS_PATH") "dr")))
     (check
      (url-exists? complete-url) => #t)))

;; Test function for URL existence
(define (regtest-url-exists)
   (let ((existing-url (string->url "$TEXMACS_PATH"))
         (non-existing-url (string->url "nonexistentpath")))
     ;; Check if existing URL exists
     (check
      (url-exists? existing-url) => #t)
     ;; Check if non-existing URL does not exist
     (check
      (not (url-exists? non-existing-url)) => #t)))

(define (regtest-general-url)
  (regtest-url-descendants)
  (regtest-url-or)
  (regtest-url-expand)
  (regtest-url-complete)
  (regtest-url-exists)
  (check-report)
  (if (check-failed?)
      (exit -1))
  (display "Test suite of general-url-test end\n"))
