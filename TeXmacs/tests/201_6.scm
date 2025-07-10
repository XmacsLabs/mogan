
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 201_6.scm
;; DESCRIPTION : Tests for move-tabpage
;; COPYRIGHT   : (C) 2025  JimZhouZZY
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

(define (debug-print-tabpages)
  "Print all current tabpage filenames for debugging"
  (let* ((tabpages (tabpage-list #t))
         (filenames (map get-filename-from-url tabpages)))
    (display* "Current tabpages (" (length tabpages) "): [")
    (for (i (.. 0 (length filenames)))
      (when (< i (length filenames))
        (display* (number->string i) ":" (list-ref filenames i))
        (when (< (+ i 1) (length filenames))
          (display* ", "))))
    (display* "]\n")))

(define (test-same-window)
  (load-buffer "$TEXMACS_PATH/tests/tmu/201_6.tmu")
  (load-buffer "$TEXMACS_PATH/tests/tmu/201_6_in_same_window.tmu")
  (load-buffer "$TEXMACS_PATH/tests/tmu/201_6_in_same_window_2.tmu")
  ;; [_] A B C
  (debug-print-tabpages)
  (move-tabpage 3 2)
  (debug-print-tabpages)
  ;; [_] A C B
  (check (length (tabpage-list #t)) => 4)
  (check (get-filename-from-url (list-ref (tabpage-list #t) 1))
         => "201_6.tmu")
  (check (get-filename-from-url (list-ref (tabpage-list #t) 2))
         => "201_6_in_same_window_2.tmu")
  (check (get-filename-from-url (list-ref (tabpage-list #t) 3))
         => "201_6_in_same_window.tmu")
  (move-tabpage 3 1)
  (debug-print-tabpages)
  ;; [_] B A C
  (check (length (tabpage-list #t)) => 4)
  (check (get-filename-from-url (list-ref (tabpage-list #t) 1))
         => "201_6_in_same_window.tmu")
  (check (get-filename-from-url (list-ref (tabpage-list #t) 2))
         => "201_6.tmu")
  (check (get-filename-from-url (list-ref (tabpage-list #t) 3))
         => "201_6_in_same_window_2.tmu")
  (move-tabpage 1 2)
  (debug-print-tabpages)
  ;; [_] A B C
  (check (length (tabpage-list #t)) => 4)
  (check (get-filename-from-url (list-ref (tabpage-list #t) 1))
         => "201_6.tmu")
  (check (get-filename-from-url (list-ref (tabpage-list #t) 2))
         => "201_6_in_same_window.tmu")
  (check (get-filename-from-url (list-ref (tabpage-list #t) 3))
         => "201_6_in_same_window_2.tmu"))

(define (test-multi-window)
  (load-buffer-in-new-window "$TEXMACS_PATH/tests/tmu/201_6_in_new_window_1.tmu")
  (load-buffer "$TEXMACS_PATH/tests/tmu/201_6_in_new_window_2.tmu")
  (load-buffer "$TEXMACS_PATH/tests/tmu/201_6_in_new_window_3.tmu")
  ;; D E F (4 5 6)
  (debug-print-tabpages)
  (move-tabpage 2 1)
  (debug-print-tabpages)
  ;; D F E
  (check (length (tabpage-list #t)) => 3)
  (check (get-filename-from-url (list-ref (tabpage-list #t) 0))
         => "201_6_in_new_window_1.tmu")
  (check (get-filename-from-url (list-ref (tabpage-list #t) 1))
         => "201_6_in_new_window_3.tmu")
  (check (get-filename-from-url (list-ref (tabpage-list #t) 2))
         => "201_6_in_new_window_2.tmu")
  (move-tabpage 2 0)
  (debug-print-tabpages)
  ;; E D F
  (check (length (tabpage-list #t)) => 3)
  (check (get-filename-from-url (list-ref (tabpage-list #t) 0))
         => "201_6_in_new_window_2.tmu")
  (check (get-filename-from-url (list-ref (tabpage-list #t) 1))
         => "201_6_in_new_window_1.tmu")
  (check (get-filename-from-url (list-ref (tabpage-list #t) 2))
         => "201_6_in_new_window_3.tmu")
  (move-tabpage 0 1)
  (debug-print-tabpages)
  ;; D E F
  (check (length (tabpage-list #t)) => 3)
  (check (get-filename-from-url (list-ref (tabpage-list #t) 0))
         => "201_6_in_new_window_1.tmu")
  (check (get-filename-from-url (list-ref (tabpage-list #t) 1))
         => "201_6_in_new_window_2.tmu")
  (check (get-filename-from-url (list-ref (tabpage-list #t) 2))
         => "201_6_in_new_window_3.tmu"))

(tm-define (test_201_6)
  (test-same-window)
  (test-multi-window)
  (check-report))
