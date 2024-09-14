
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tabpage-menu.scm
;; DESCRIPTION : The tab bar below the main icon bar
;; COPYRIGHT   : (C) 2024 Zhenjun Guo
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus tabpage-menu)
  (:use (kernel gui menu-widget)
        (texmacs menus file-menu)))

(define (index-of-buffer lst buf)
  (let find ((lst lst) (index 0))
    (cond
      ((null? lst) -1)
      ((== (car lst) buf) index)
      (else (find (cdr lst) (+ index 1))))))

(define (move-buffer i j)
  ;; move the buffer at index i to index j
  (if (!= i j)
    (let ((next (if (< i j) (+ i 1) (- i 1))))
      (swap-buffer-index i next)
      (move-buffer next j))))

(tm-define (move-buffer-to-index buf j)
  (define (transform lst index) (- (length lst) 1 index))
  ;; lst is the reversed buffer list of cpp buffer array
  (let* ((lst  (buffer-menu-unsorted-list 99))
         ;; so we need to transform the index to true index in cpp buffer array
         (from (transform lst (index-of-buffer lst buf)))
         (to   (transform lst j)))
    (if (and (!= from -1) (!= from to))
        (move-buffer from to))))

(tm-menu (texmacs-tab-pages)
  (for (buf (buffer-menu-unsorted-list 99)) ; buf is the url
    (let* ((title  (buffer-get-title buf))
        (title*    (if (== title "") (url->system (url-tail buf)) title))
        (mod?      (buffer-modified? buf))
        (tab-title (string-append title* (if mod? " *" "")))
        (doc-path  (url->system buf))
        (active?   (== (current-buffer) buf)))
      (tab-page
        (eval buf)
        ((balloon (eval `(verbatim ,tab-title)) (eval `(verbatim ,doc-path))) (switch-to-buffer* buf))
        ((balloon "✕" "Close") (safely-kill-buffer-by-url buf))
        (eval active?)
      ))))