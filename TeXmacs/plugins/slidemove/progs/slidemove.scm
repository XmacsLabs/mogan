;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : slidemove.scm
;; DESCRIPTION : Move slides in a beamer presentation
;; COPYRIGHT   : (C) 2022 Jeroen Wouters
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (slidemove))

; (tm-define (nr-slides)
;   (:require (inside? 'slideshow))
;   (with-innermost s 'slide
;     (length (tree-children (tree-ref s :up)))))

(tm-define (move-slide-up)
  (noop))

(tm-define (move-slide-up)
  (:require (inside? 'slideshow))
  (with-innermost s 'slide
    (if (tree-ref s :previous)
      (let* ((p (tree->path s)) ;; path to the current slide
             (i (last p)) ;; index of the slide
             (t (tree-ref s :up))) ;; parent node of the slide
        ;; insert a copy of slide i before its predecessor
        (tree-insert t (- i 1) (list (tree->stree s)))
        ;; remove original slide, now at i+1
        (tree-remove t (+ i 1) 1)
        (tree-go-to t (- i 1) 0)))))

(tm-define (move-slide-down)
  (noop))

(tm-define (move-slide-down)
  (:require (inside? 'slideshow))
  (with-innermost s 'slide
    (if (tree-ref s :next)
      (let* ((p (tree->path s)) ;; path to the current slide
             (i (last p)) ;; index of the slide
             (t (tree-ref s :up))) ;; parent node of the slide
        ;; insert a copy of slide i after its successor
        (tree-insert t (+ i 2) (list (tree->stree s)))
        ;; remove original slide
        (tree-remove t i 1)
        (tree-go-to t (+ i 1) 0)))))

(tm-menu (standard-focus-icons t)
    (:require (slideshow-context? t))
    (former t)
    ((balloon (icon "tm_slide_move_up.xpm") "move slide up") (move-slide-up))
    ((balloon (icon "tm_slide_move_down.xpm") "move slide down") (move-slide-down))
    )
