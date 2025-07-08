
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 201_5.scm
;; DESCRIPTION : Tests for tabpage-list and view->window-of-tabpage-url
;; COPYRIGHT   : (C) 2024  JimZhouZZY
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (liii check))

(check-set-mode! 'report-failed)

(define (get-filename-from-url url)
  ;; Extract the filename from a URL by splitting on '/' and taking the last element"
  (let* ((url-str (url->string url))
         (parts (string-split url-str #\/)))
    (car (reverse parts))))

(define (test-initial-state)
  (check (length (tabpage-list #t)) => 1))

(define (test-after-loading)
  (load-buffer "$TEXMACS_PATH/tests/tm/201_5.tm")
  (check (length (tabpage-list #t)) => 2)
  (check (get-filename-from-url (list-ref (tabpage-list #t) 1)) 
         => "201_5.tm"))

(define (test-same-window-loading)
  (load-buffer "$TEXMACS_PATH/tests/tm/201_5_in_same_window.tm")
  (check (length (tabpage-list #t)) => 3)
  (check (get-filename-from-url (list-ref (tabpage-list #t) 2)) 
         => "201_5_in_same_window.tm"))

(define (test-new-window-loading)
  (load-buffer-in-new-window "$TEXMACS_PATH/tests/tm/201_5_in_new_window_1.tm")
  (load-buffer "$TEXMACS_PATH/tests/tm/201_5_in_new_window_2.tm")
  (check (length (tabpage-list #t)) => 2)
  (check (get-filename-from-url (list-ref (tabpage-list #t) 0)) 
         => "201_5_in_new_window_1.tm")
  (check (get-filename-from-url (list-ref (tabpage-list #t) 1)) 
         => "201_5_in_new_window_2.tm"))

(define (test-view-to-window)
  (let* ((tabpages (tabpage-list #t))
         (first-view (list-ref tabpages 0)))
    (display* "Testing view->window-of-tabpage with view: " 
            (get-filename-from-url first-view) "\n")
    (let ((window-result (url->string (view->window-of-tabpage first-view))))
    (display* "Result: " window-result "\n")
    (check window-result => "tmfs://window/1"))))

(tm-define (test_201_5)
  (test-view-to-window)
  (test-initial-state)
  (test-after-loading)
  (test-same-window-loading)
  (test-new-window-loading)
  (check-report))
