;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm_outline.scm
;; DESCRIPTION : Outlining module for texmacs : reorganizing texts and lists
;; COPYRIGHT   : (C) 2019  Philippe Joyez
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sectional tags are converted to folds, and various routines are provided to 
;; reorganize the text 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(texmacs-module (utils outline outline))

(texmacs-module (outline)
  (:use (dynamic dynamic-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; utility functions (not specific for the plugin)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-preference "debug-outline" "true")
(define (debug-outline* . l)
  (if (in? (get-preference "debug-outline") '("default" "#f" "false")) (for-each display l)))

;; list functions

(define (map-index f l)
  ;f is a function that takes an element and its index in the list as second argument
  ; l is a list
  (let* ((acc '())
         (ll (length l)))
              
     (do ((i 0 (+ i 1)))
               ((= i ll) acc)
               (set! acc (rcons acc (f (list-ref l i) i))))))  

(define (lists-equal? a . b)
  ; if b is absent and a is a list, check all elements in a are identical
  ; if more than 1 arguments are passed check they are all identical
  ; (whatever the nested list structure of what is compared)
  (if (and (nnull? a) (list? a) (null? b)) 
        (if (list>1? a) ; compare them pair-wise                      
         (and (lists-equal? (car a) (cadr a)) (if (> (length a) 2) (lists-equal? (cdr a)) #t))
         #t) ; list with single element is OK
  (if (list>1? b) (lists-equal? (rcons b a)) ; calling with more than two args. compare them all
      (with c (car b) 
  (cond 
    ((and (null? a) (null? c) #t))
    ((and (list? a) (list? c) (= (length a) (length c))) 
         (and (lists-equal? (car a) (car c)) (lists-equal? (cdr a) (cdr c))))
    ((not (or (list? a) (list? c))) (equal? a c))
    (#t #f))
))))
               
;;;; tree utility functions (general)

(tm-define (tree-arity-1? t) (== 1 (tree-arity t)))

(define (remove-node! node)
  ;; removes node (with children if any)
  (tree-remove! (tree-ref node :up) (tree-index node) 1))

(define (copy-node! node parent-dest pos)
  ;; insert an existing node (with children if any) as new child of parent-dest
  ;; FIXME: No sanity check! parent should not be in node's subtree!
  (tree-insert! parent-dest pos `(,node)))

(define (move-node! node parent-dest pos)
  ;; moves an existing node (with children if any) and
  ;; insert it as new child of parent-dest
  ;; FIXME: No sanity check! parent should not be in node's subtree!
  (copy-node! node parent-dest pos)
  (remove-node! node))

(define (remove-node-raise-children! node . firstlast)
  ;; similar to "remove tag" operation in the editor
  ;; if no extra args are given all children are raised
  ;; if only one extra arg is given all children starting from this one are raised
  ;; if last is specified, children up to last (included) are raised
  (let* ((parent (tree-ref node :up))
         (pos (tree-index node))
         (lastindex+1 (if (< (length firstlast) 2) (tree-arity node) (+ (cadr firstlast) 1) ))
         (firstindex (if (null? firstlast) 0 (car firstlast)))
         (cl (select node `((:range ,firstindex ,lastindex+1)))))
    (tree-insert! parent (+ pos 1) cl)
    (tree-remove! parent pos 1)
    ))

(define (replace-leaftext! leaf newtext)
  ;; replace a node's content by a new string.
  ;; Makes the node a leaf if it wasn't one
  (tree-assign! leaf (string->tree newtext)))

(define (find-next-tag-in-tree lab t forward? . p) 
  ;;for some reason path-next-tag (and go-to-next-tag, based on it) does not work reliably
  ;;coming up with my own, inefficient, code
  ;(display* "path-next-tag result: " (map (lambda (x) (path-next-tag (root-tree) (cursor-path) x)) (cdr lab)) "\n")
  ; if p not specified look from cursor path
  (with cp (if (null? p) (if forward? (cursor-path) (cursor-path*)) (car p))
  
    ;(display* "search dir " forward? "\n") 
  (let* ((specs (cond ((and (list>0? lab) (!= (car lab) :or)) (cons :* lab))
                      (else `(:* ,lab ))))
         (headers (select t specs))
         (pl (map tree->path headers))
         (pl (if forward? (list-filter pl (lambda (x) (path-before? cp x)))
                     (list-filter pl (lambda (x) (path-before? x cp)))))
         (pl (sort pl path-before?) )
         )
        ;(display* "find next:#" (length pl) "  " pl " | " cp "\n")
        (if (list>0? pl) (path->tree (if forward? (car pl) (cAr pl))) #f)
    )))
   
;;;; path functions

; for some reason, when comparing paths with the  built-in functions path-less? and path-inf? 
; the output does not always correspond to their order of appearance in the document...
; (path-less? '(1 1) '(2 0))       => #t  correct
; (path-less? '(2 1 1 1) '(2 1))   => #t  WRONG
; (path-less?  '(2 1) '(2 1 1))    => #f  WRONG
; (path-inf? '(1 1) '(2 0))        => #t  correct
; (path-inf? '(2 1 1 1) '(2 1))    => #f  correct
; (path-inf?  '(2 1) '(2 1 1))     => #f  WRONG
; So I wrote a function that does that
; (path-before? '(1 1) '(2 0))     => #t  correct
; (path-before? '(2 1 1 1) '(2 1)) => #f  correct
; (path-before?  '(2 1) '(2 1 1))  => #t  correct


(define (path-before? a b)
  (cond ((< (car a) (car b)) #t)
        ((> (car a) (car b)) #f)
        ((and (list>1? a) (list>1? b)) (path-before? (cdr a) (cdr b)))
        ((and (list>1? b) (!= (cdr b) '(0))) #t)
        (else #f)))

(define (path-before-eq? a b)
  ;(display* a b "\n")
  (cond ((< (car a) (car b)) #t)
        ((> (car a) (car b)) #f)
        ((and (list>1? a) (list>1? b)) (path-before-eq? (cdr a) (cdr b)))
        ((list>1? b)  #t)
        ((and (nlist>1? a) (nlist>1? b) (== (car a) (car b))) #t)
        ((and (list>1? a) (nlist>0? (list-filter (cdr a) (lambda (x) (!= x 0)))))  #t)
        (else #f))
  )

(define (shallower-or-before? p1 p2)
  (cond ((< (length p1) (length p2)) #t)
        ((> (length p1) (length p2)) #f)
        (else (path-before? p1 p2))))

;(define (shorten-path p)  not presently used
;  ;removes ending zeros
;  (if (or (nlist>0? p) (!= 0 (cAr p))) p (shorten-path (cDr p))))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plugin logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; constants 
(define special ;cf section-base.ts "Names of special sections." prologue epilogue table-of-contents index glossary list-of-figures list-of-tables 
  '(doc-data abstract-data table-of-contents the-index the-glossary list-of-figures list-of-tables bibliography))
(define section-like '(part chapter section subsection subsubsection paragraph subparagraph)) ;variants-of 'section except appendix that needs special handling
(define section*-like '(part* chapter* section* subsection* subsubsection* paragraph* subparagraph*))
(define all-headers (append special section-like section*-like '(appendix appendix*)))

;levels : alist giving the depth in the hierarchy of a given header type
(define levels (map-index (lambda (x y) `(,x . ,y)) section-like))
(append! levels (map-index (lambda (x y) `(,x . ,y)) section*-like))
(append! levels (map (lambda (x) `(,x . 0 )) special))
;;in customize-sections.en.tm "From a global point of view, an important predicate macro is "sectional-short-style". When it evaluates to "true", then appendices, tables of contents, etc. are considered to be at the same level as sections. In the contrary case, they are at the same level as chapters. Typically, articles use the short sectional style whereas book use the long style."
(append! levels (if (!= (get-init-tree "sectional-short-style") (tree 'macro "false")) '((appendix . 1) (appendix* . 1)) '((appendix . 2) (appendix* . 2))))


(define-fold
  folded-outline unfolded-outline)
(define-fold
  folded-outline-special unfolded-outline-special)
(define-fold
  folded-outline-text unfolded-outline-text)
(define oc?-alist '()) ; alist for memorizing open/close state of sections while refreshing or toggling outline on/off

(tm-define (outline-toolbar)
  (menu-bind texmacs-extra-icons 
         ((check (balloon "Outline" "toggle outline mode") "v" (outline?)) (toggle-outline))
         ((balloon (icon "tm_refresh.png") "refresh hierarchy") (local-refresh))
         ((balloon "Close others" " Close all other folds") (close-others))
    (=> (balloon "Show to..." "Show hierarchy down to...")
            ("Parts" (show-hierarchy-to 'part))
            ("Chapters" (show-hierarchy-to 'chapter))
            ; appendix will show either with chapters or sections, depending on "sectional-short-style" 
            ("Sections" (show-hierarchy-to 'section))
            ("Subsections" (show-hierarchy-to 'subsection))
            ("Subsubsections" (show-hierarchy-to 'subsubsection))
            ("Paragraphs" (show-hierarchy-to 'paragraph))
            ("Subparagraphs" (show-hierarchy-to 'subparagraph))
            ("All" (show-hierarchy-to  #t)))
    ((balloon (icon "tm_new_subsection.png") "New sublevel here") (add-subsection-here))
    (mini #t
      (inert ("Move node:" (noop))))
     ((balloon (icon "tm_outline_downgrade_node.png") "Lower node") (change-hierarchy #f #f))
     ((balloon (icon "tm_outline_upgrade_node.png") "Raise node") (change-hierarchy #t #f))
     (mini #t
      (inert ("Move branch:" (noop))))
     ((balloon (icon "tm_outline_downgrade_branch.png") "Lower branch") (change-hierarchy #f #t))
     ((balloon (icon "tm_outline_upgrade_branch.png") "Raise branch") (change-hierarchy #t #t))
     ((balloon (icon "tm_outline_move_up.png") "Move branch up") (move-up-down #t))
     ((balloon (icon "tm_outline_move_down.png") "Move branch down") (move-up-down #f))
        )
      (show-icon-bar 3 #t))
    
(tm-define (toggle-outline-toolbar)
  ;(:check-mark "v" (visible-icon-bar? 3)) ; do not activate, breaks tools menu!
  (if (visible-icon-bar? 3) (show-icon-bar 3 #f)
      (outline-toolbar)))

;;;; utility functions for reorganizing itemize and similar tags

(define (select-in-selection crit)
   (let* ((s (selection-get-start))
          (e (selection-get-end))
          (t (path->tree (selection-path)))
          (t (if (tm-is? t 'document) t (tree-ref t :up)))
          (void (debug-outline* "sel-tree: " t "\n"))
          ( l (select t crit))
          (void (debug-outline* "unfiltered found: " l "\n"))
          (l (list-filter l (lambda (x) (and (path-before-eq? (tree->path x) e)
                                             (path-before-eq? s (tree->path x)))))))
     (debug-outline* "filtered found: " l "\n")
     l))

(define (split-itemize-at itz x . y) 
  (debug-outline* "splitting " itz " " x " " y "\n")
  (cond ((nnull? y) 
           (with p (tree->path itz)
             (tree-split! itz 0 (cAr y))
             (tree-split! (tree-ref itz :up) (tree-index itz) 1)
             (if (list>1? y) 
               (split-itemize-at (path->tree p) x  (cDr y) )
               (split-itemize-at (path->tree p) x ) )))
        ((> x 0)
           (begin
             (tree-split! itz 0 x)
             (tree-split! (tree-ref itz :up) (tree-index itz) 1))))
  )

(define (find-items-and-operate where func up? branch?)
              (with sa (selection-active?)  
              (save-cursor)
              (debug-outline* "finding items to move\n")
              (debug-outline* "(selection-active?)" sa "\n")
              (if sa 
                (let* ((lit (append (select-in-selection '(:%0 concat (:or item item*) :up))
                               (select-in-selection '(:%0 (:or item item*)))))
                       (void (debug-outline* lit "\n"))
                       ;(lit (reverse (sort lit (lambda (x y) (path-before? (tree->path x) (tree->path y))))))
                       (lit (sort lit (lambda (x y) (path-before? (tree->path x) (tree->path y)))))
                       )
                  (debug-outline* "*****************\n" lit "\n")
                  (map-in-order (lambda (x) (func x up? branch?)) lit)
                )       
                (let* ((it (tree-search-upwards (cursor-tree) 'item*)) ;in item*?
                       (it (or it 
                               (with ac (cursor-tree*) ; item right of cursor?
                                  (if (in? (tree-label ac) '(item item*)) ac #f))))
                       (it (or it (find-next-tag-in-tree '(:or item item*) where #f))) ; 1st item left of the cursor
                       (it (and it (with tu (tree-up it) (if (tm-is? tu 'concat) tu it))))) ;select enclosing concat if present
              (if it (func it up? branch?)))
              
         )     

        (restore-cursor)
        )
)

(define (move-item-up-down it up? . ignored)
        (let* ((void (debug-outline* "node moved: " it "\n"))
              (pos (tree-index it))
              (doc (tree-up it))
              (l (sort (map tree-index 
                          (append 
                            (select doc '(concat (:or item item*) :up))
                            (select doc '((:or item item*))))
                            ) < )) ;other items in doc
              (li (list-index l pos))
              (void (debug-outline* "l: " l "\n"))
              (void (debug-outline* "it: " it " pos:" pos "\n" "doc:" doc "\n"))
              (nexti (if (< li (- (length l) 1)) (list-ref l (+ li 1)) (tree-arity doc)))
              (nnexti (and (< li (- (length l) 2)) (list-ref l (+ li 2))))
              (prev (and (> li 0) (list-ref l (- li 1))))
              (void (debug-outline* "next:" nexti "\n" "prev:" prev "\n"))
              (dest (if up? prev
                           (or nnexti (tree-arity doc))))
              (cl (select doc `((:range ,pos ,nexti))))
              (lcl (length cl))
              )
            (if (not dest) (debug-outline* "can't move (don't know where to...)\n")
                (begin 
                  (tree-insert! doc dest cl)
                  (tree-remove! doc (+ pos (if up? lcl 0)) lcl)
                  )
              ))
 )


(define (up-downgrade-item it up? branch?)
  ;upgrade or downgrade list items
  ;if branch? downgrade any sublist together with the item, otherwise move the item into the sublist
  ;on upgrade branch? is irrelevant: cannot leave sublist "hanging" at a level no longer existing
  (debug-outline* "up/downgrading item: " it "\n")
  (let* ((lst (tree-search-upwards it (append '(:or) (variants-of 'enumerate) (variants-of 'itemize) (variants-of 'description))))
          
              (new (tree-label lst))
              (void (debug-outline* new "\n"))         
              (pos (tree-index it))
              (doc (tree-up it))
              (l (sort (map tree-index 
                            (append (select doc '(concat (:or item item*) :up))
                                    (select doc '(:%0 (:or item item*)))));other items in doc
                       < )) 
              (li (list-index l pos))
              (void (debug-outline* "l: " l "\n"))
              (void (debug-outline* "it: " it " pos:" pos "\n" "doc:" doc "\n")))
            (if (not up?)
                ;downgrading item
              (if (> pos 0) ;if no previous item cannot become subitem
              (let* ( 
              (prev (tree-ref doc (- pos 1)))
              (nexti (if (or #f; branch?
                         (== li (- (length l) 1))) (tree-arity doc) (list-ref l (+ li 1)) ))
              (cl (select doc `((:range ,pos ,nexti))))
              (lcl (length cl))
              (void (debug-outline* "length " lcl "\n" )) 
              ;test if there is a compatible list just above, in which case we go into it  
              (prevlist  (if (tm-is? prev new) prev #f)) 
              (previtemize (or prevlist (begin (tree-insert! doc pos (list `(,new (document )))) (tree-ref doc pos))))
              (prevdoc (tree-ref previtemize 0))
              (void (debug-outline* "previtemize " previtemize "\n" ))
              (ip (tree-arity prevdoc)))
              (tree-insert! prevdoc ip cl)
              (tree-remove! doc (+ pos (if prevlist 0 1)) lcl)
              ;does our item have a compatible sublist?
              (with subitemize (with si (list-filter (select prevdoc `((:range ,ip ,(tree-arity prevdoc) ) )) (lambda (x) (tm-is? x new)))
              (debug-outline* "subitemize " si "\n" )   
                                 
                                 (if (list>0? si) si #f)) 
              (if subitemize 
                  (if branch? 
                      (noop);(map (lambda (x) (variant-circulate x #t)) (list-filter (select prevdoc `((:range ,ip ,(tree-arity prevdoc) :*) )) (lambda (x) (in? (tree-label x) vs)))) 
                      (map (lambda (x) (let* ((parent (tree-ref x :up))
         (pos1 (tree-index x))
         (cl1 (select x '(document :%1))))
    (tree-insert! parent (+ pos1 1) cl1)
    (tree-remove! parent pos1 1))) subitemize))))))
              
              ;upgrading item 
              (let* ((outerlist (tree-search-upwards (tree-ref lst :up) (append '(:or) (variants-of 'enumerate) (variants-of 'itemize) (variants-of 'description))))
                     ;(outerlist (tree-ref doc :up :up)) FIXME!
                     (void (debug-outline* "outerlist " outerlist "\n"))
                     ;(outerlist (and outerlist (in? (tree-label outerlist) (append (variants-of 'enumerate) (variants-of 'itemize)))))
                     )
              (if outerlist
                  ; if item is not alone we need to split the list that contains it
                (begin
                  ;(display* "outerlist " outerlist)
                  (cond 
                         ;((and (> pos 0) (or branch? (== li (- (length l) 1))) (split-itemize-at lst pos )))
                         ((and (> pos 0) (== li (- (length l) 1)) (split-itemize-at lst pos )))
                         ((> pos 0);(and (> pos 0) (not branch?) ) 
                            (split-itemize-at lst pos (list-ref l (+ li 1))))
                         ((and (= pos 0) (< li (- (length l) 1)));(and (= pos 0) (not branch?) (< li (- (length l) 1)))
                            (split-itemize-at lst (list-ref l (+ li 1))))
                        )
                       (debug-outline* "outerlist compatible? " (tm-is? outerlist new) "\n")
                      
                  (if (tm-is? outerlist new)
                      ;if the outerlist is the right type go into it, otherwise break it up or deny move
                      (begin
                        (let* (
                        (olddoc (tree-ref it :up))
                        ;(void (display* "olddoc " olddoc))
                        (olditemize (tree-ref olddoc :up))
                        ;(void (map (lambda (x) (variant-circulate x #f)) (select olddoc `(:* ,(append '(:or) vs)))))
                        (void (debug-outline* "olditemize " olditemize "\n"))
                        (newdoc (tree-ref outerlist 0))
                        (void (debug-outline* "newdoc " newdoc "\n"))
                        (void (debug-outline* (tree->path olditemize) (tree->path newdoc) "\n"))
                        (newpos (with p (tree->path olditemize) (list-ref p (list-common-left p (tree->path newdoc)))))
                        (cl1 (select olddoc `(:%1)))
                        (lcl1 (length cl1))
                        (void (debug-outline* "length " lcl1 "\n" )))
                        (tree-insert! newdoc newpos cl1)
                        (tree-remove! newdoc (+ newpos lcl1) 1)
                        ))))))
              
                         
                         
              
              ))
              
        
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utility functions for outlining
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (outline-context? t)
  (in? (tree-label t) '(unfolded-outline folded-outline unfolded-outline-special folded-outline-special unfolded-outline-text folded-outline-text)))

;must overload toggle because our folds may have only one argument
(tm-define (alternate-toggle t)
  (:require (outline-context? t))
  (variant-set t (symbol-toggle-alternate (tree-label t))))


(tm-define (mouse-unfold t)
 ;automatically fold/unfold subtext depending on existing subheaders 
  ;(:type (-> void))
  (:require (tm-is? (tree-up t) 'folded-outline))
  ;(:secure #t)
  ;(display* "in mouse unfold\n")
  (mouse-unfold-aux (tree-up t)))

(define (mouse-unfold-aux tu)
  (let* ((subs (list>0? (select tu '((:or unfolded-outline folded-outline)))))
         (txt (tree-ref tu 1)))
         ;(display* txt "\n")
         (cond ((and (tm-is? txt 'folded-outline-text) (not subs)) (variant-set txt 'unfolded-outline-text))
               ((and (tm-is? txt 'unfolded-outline-text) subs) (variant-set txt 'folded-outline-text)))
         
         (and tu (variant-set tu 'unfolded-outline))
         (tree-go-to tu 0)
         ))

(tm-define (mouse-fold t)
 ;overload to keep cursor there
  (:require (with tu (tree-up t) (and tu (in? (tree-label tu) '(unfolded-outline unfolded-outline-text unfolded-outline-special) ))))
  ;(:secure #t)
  (let* ((tu (tree-up t)))
         (and tu (variant-set tu (symbol-toggle-alternate (tree-label tu))))
         (tree-go-to tu 0)
         ))
 

(tm-define (short-special-form var)
  ;what to display for folded "special" tags
  (:secure #t)
  (with lab (tree->string var)
  (cond ((== lab "doc-data") '(section-title (localize "Front Matter")))
        ((== lab "abstract-data") '(section-title(localize "Abstract")))
        (else `(section-title (localize ,(upcase-first (string-recompose (string-decompose lab "-") " ")))))
        )))

(define (split-long-concat t)
  ;check whether the sectional tag (and possible label) is concatenated with its following text, and if so, split header from text
  (with conc (tree-ref t :up)
    (if (tm-is? conc 'concat)
      (with cutat (if (tm-is? (tree-ref t :next) 'label) 2 1) 
        (if (> (tree-arity conc) cutat) 
             (tree-split! (tree-ref conc :up) (tree-index conc) cutat) ;split it 
    )))))


(define (parents lt)
  ;input is a (list <tree>) (single tree) ; and returns the hierarchical list of outline folds enclosing the initial element 
  (let* ((p (tree-ref (car lt) :up))
         (r (and p (tree-search-upwards p '(:or unfolded-outline folded-outline)))))
          (if r (parents (cons r lt)) lt)))


(define (header-in-outline t)
  ; finds first sectional tag in t 
  ; if t is a sectional tag then return it
  ; else it's a outline fold and/or a concat containing one: return the first enclosed sectional tag
  (if (in? (tree-label t) all-headers )
    t
    (or (tree-ref t :%0 (append '(:or) all-headers))
        (tree-ref t :%1 (append '(:or) all-headers))
        (tree-ref t :%2 (append '(:or) all-headers))) ; should not be any deeper, hopefully
  )
 )                                                          


(define (remove-outline-folds-in t)
  (let* ((headers (select t '(:* (:or unfolded-outline folded-outline unfolded-outline-special folded-outline-special unfolded-outline-text folded-outline-text))))
         (uhp (map-in-order (lambda (x) (with p (tree->path x) (if (== 0 (cAr p)) p (rcons p 0))))  headers))
         (shp (sort uhp shallower-or-before?))
         (headers (map-in-order (lambda (x) (list-ref headers (list-index uhp x))) shp))
         (lh (length shp))
         )
        (do ((i (- lh 1) (- i 1)))
      ((= i -1) #f) 
      (let* ((ofold (list-ref headers i))
             (sec (header-in-outline ofold))
             (fu? (tree-label ofold)))
        (if sec (set! oc?-alist (assoc-set! oc?-alist sec fu?)))
        ;(display* sec " || " (object->string* sec) " memorized " oc?-alist "\n")
        (remove-node-raise-children! ofold)
        )
      )
   )
)


(define (outline-level t)
  ; returns the depth of the sectional tag t in hierarchy (using alist levels)
  ; returns #f if tag not found
  (with h (and t (with h1 (header-in-outline t) (and h1 (tree-label h1))))
     (and h (assoc-ref levels h))))

(define (cleanup-outlined t)
  (with p (tree->path t)
    (debug-outline* "entering cleanup-outlined; path to clean" p "\n"))
    ;(display* "tree to clean" (tree->stree t) "\n")
  (let* ((lt (select t '(:* (:or document unfolded-outline folded-outline unfolded-outline-special folded-outline-special unfolded-outline-text folded-outline-text)  document)))
          (void (debug-outline* "found " (length lt) " documents inside folds \n"))
          ;(void (display* "parents : " (map (lambda (x) (tree-ref x :up)) lt)))
          (lh (sort (map tree->path lt) path-before?)))
          
    (if (list>0? lh) 
        (begin 
          (debug-outline* "removing 'documents at first level of outline folds\n")
          (do ((i (- (length lh) 1) (- i 1)))
            ((= i -1) #f)
            (remove-node-raise-children! (path->tree (list-ref lh i)))
          ))))        
  (with lh (sort (map tree->path (select t '(:* (:or document concat) (:match :tree-empty?)))) path-before?)
    (if (list>0? lh) 
      (begin 
        (debug-outline* "empty doc found\n")
        (do ((i (- (length lh) 1) (- i 1)))
          ((= i -1) (debug-outline* "finished removing empty docs\n"))
          (remove-node! (path->tree (list-ref lh i))))
      ))) 
  ;(display* "tree :" (tree->stree t) "\n")
;  (with lh (sort (map tree->path (select t '(:* document :%1 (:match :tree-empty?)))) path-before?)
;    (if (list>0? lh) 
;      (begin
;        (debug-outline* "blank lines found\n")
;        (do ((i (- (length lh) 1) (- i 1)))
;          ((= i -1) (debug-outline* "finished removing blank lines\n"))
;          (remove-node! (path->tree (list-ref lh i)))
;        ))))
;  (with lh (sort (map tree->path (select t '(:* document (:match :tree-arity-1?)))) path-before?)
;    (if (list>0? lh) 
;        (begin 
;          (debug-outline* "singleton documents found\n")
;          (do ((i (- (length lh) 1) (- i 1)))
;            ((= i -1) (debug-outline* "finished removing singleton documents\n"))
;            (remove-node-raise-children! (path->tree (list-ref lh i)))
 ;         ))))
  ;Clear singleton concats? merge child of doc == doc?
  
)

(define (change-hierarchy-1node t up?)
  ;(display* t "\n")
  (let* ((n (header-in-outline t))
         (typ (and n (tree-label n))))
    ;(display* n "\n")
    (if (and n (or (in? typ section-like) (in? typ section*-like))) 
      (let* (
         (void (debug-outline* typ " -> "))
         (vs (if (in? typ section-like) section-like section*-like))   
         (ll (length vs))
         (i (list-index vs typ))
         (old (tree-label t))
         (rot (list-search-rotate vs typ))
         (new (if (and (not up?) (nnull? rot)) (cadr rot) (cAr rot)))
         (void (debug-outline* new "\n"))         
        )
        (if i
           (if (and up? (== i 0)) (noop) ; at the top, cannot go higher
             (if (and (not up?) (== i (- ll 1))) ; at bottom : turn subsubparagraph to normal text : remove tag
                 (begin
                   (with t1 (tree-ref t 1) (if (and t1 (in? (tree-label t1) '(unfolded-outline-text folded-outline-text))) (remove-node-raise-children! t1)));folded text should not be there at that last level
                   (remove-node-raise-children! n)
                   (remove-node-raise-children! t)
                 )
                 (variant-set n new)))))
      
  (begin ; not a header that we know how to shift
    (if (in? n '(appendix appendix*))
        (display "Promoting/Demoting an Appendix does not really make sense. Change it to a normal section first \n")
    (display "not a sectional tag\n")))
      )))

(define (refresh-part t)
  ;Not sure it's completely OK (used to often crash )
  (debug-outline* "entering refresh \n")
  (save-cursor)
  (system-wait "Refreshing Outline, " "please wait")
     
  (set! oc?-alist '())
  (debug-outline* "in refresh, cursor saved \n")
  (with bt? (== t  (buffer-tree))
        (or bt? (tree-set! t `(document ,t))) ; temporary enclosure to work in STILL NEEDED??
        (cleanup-outlined t) ; removing spurious 'documents in outline-folds in particular
        (remove-outline-folds-in t)
        (debug-outline* "alist (part) :" oc?-alist "\n")
        (debug-outline* "finished removing \n")
        ;(display* "content : " (header-in-outline t) " \n")
        (do-outlining t #f)
        (debug-outline* "finished re-hierarchizing \n")
        ;(cleanup-outlined t)
        (or bt? (remove-node-raise-children! t)) ; remove temporary enclosure
      
    (restore-cursor)
  )) 
      

(define (check-hierarchy2 t)
  (with headers (select t '(:* (:or unfolded-outline folded-outline)))
    ;for each, check that the fold enclosing is higher in hierarchy and that folds preceeding in the enclosure are lower or equal
    ; (one can always make a non-sensical hierarchy by making manual changes!)
    (check-hierarchy2-aux headers)
    ))
      
(define (check-hierarchy2-aux l)
  (with h (car l)
   (let* ((levl (outline-level h))
          (enc (tree-search-upwards (tree-ref h :up) '(:or unfolded-outline folded-outline)))
          (pred (begin (go-to-path (tree->path h)) (find-next-tag-in-tree '(:or unfolded-outline folded-outline) (if enc enc (buffer-tree)) #f)))
          (void (debug-outline* h " & " enc " & " pred " \n"))
          (encOK (and enc (< (outline-level enc) levl)))
          (predOK (and pred (>= (outline-level pred) levl)))
          (test (and (or (not enc) encOK) (or (not pred) predOK)))
          (void (debug-outline*  encOK predOK test "\n"))
          )
          (if test (if (list>1? l) (check-hierarchy2-aux (cdr l)) #t) #f)
    )))

(define (structure-flat? tl)
  ;given a list of sectional headers check they are all in the same 'document 
  (let* ((docs (map (lambda (x) (tree-search-upwards (tree-ref x :up) 'document)) tl))
         (pl (map tree->path docs)))
    (lists-equal? pl)))

(define (check-structure2 tl )
    ;given a list of sectional headers check they are all in the same 'document
    ; if not we may have problems getting the correct hierarchy
    (let* ((docs (map-in-order (lambda (x) (tree-search-upwards (tree-ref x :up) 'document)) tl))
           (pl (map-in-order tree->path docs))
           (al '()))
      ; now count how many times a given path occurs
      (do ((i 0 (+ i 1)))
          ((= i (length tl)) 
              ;termination function 
              (if (list>1? al)   
                (begin 
                  (set! al (sort al (lambda (x y) (shallower-or-before? (car x) (car y)))))
                  (with d (path->tree (car (cAr  al)))
                    (set! return-to-initial-cursor #f)
                      (tree-go-to d :start)
                      (tree-select d)
                      (dialogue-window widget2-buttons 
                        (lambda (ans) (if ans (do-outlining-cont tl) (noop)))
                        "Warning")
                      ; note that any code here is executed *before* the user answers
                      ; and this whole function returns to its caller that continues as well
                  )
                )
                (do-outlining-cont tl)
              ))
          ; loop body
          (let*((p (list-ref pl i))
                (x (assoc-ref al p)))
            (if x (assoc-set! al p (+ 1 x)) (set! al (assoc-set! al p 1))))
      )
    ))

(tm-widget (widget2-buttons cmd)
  (vlist (centered(text "Some sectional tags are not in the top level document."))
         (centered(text "Resulting outline may be inconsistent and/or not reorganize as expected.")))
  (centered (bottom-buttons  ("Proceed anyway" (cmd #t)) // ("Cancel" (cmd #f) ))))

    
(tm-widget (widget1-buttons cmd)
  (text "Found nested sectional tags.\nCannot continue. \nPlease fix")
  (bottom-buttons (centered ("Cancel" (cmd)))))

(define (check-structure1 tl )
;check for nested sectional tags
  (let* ((l1 `(:* ,(append '(:or) all-headers)))
         (l3 (list-filter tl (lambda (x) (list>0? (select x l1))) ))
         )
        (if (list>0? l3)
          (begin 
            (set! return-to-initial-cursor #f)
            (tree-go-to (car  l3) :start)
            (tree-select (car  l3))
            (dialogue-window widget1-buttons noop "Error")
            #f)
        #t)
  )
)

(define (do-outlining enct . opt)
  ;enct enclosing tree to work in
  ; when opt is not null, means we are refreshing a possibly altered structure, don't do check-structure2 
  (debug-outline* "hierarchizing in " (tree->path enct) "\n\n")
  (debug-outline* "all-headers : " all-headers "\n\n")
  ;(split-sections-headers-concat enct)
  (with headers1 (select enct `(:* ,(append '(:or) all-headers)))
      (if (check-structure1 headers1) ; else abort (nested sectional tags)
           (if (null? opt) 
             (check-structure2 headers1)
             (do-outlining-cont headers1))))
)


(define (do-outlining-cont headers1)
   ;(display* "structure ok, continuing\n")
   (or (outline?)
      (begin
       (zoom-out 1.4)
       (set-init-env "outline-mode" "true")
       (init-page-medium "automatic")))

   (let* 
     ((headers (map (lambda (x) (with l (tree-ref x :up) (if (tm-is? l 'concat) l x))) headers1)) ; when a sectional header has a label attached, they are concatenated. Pick the concat in that case.
      ; should we also consider "with" tags? or anything in between the header and the enclosing 'document?
      ; headers1 is the true headers list, but the structure we want to make hierachical is headers 
      ;(void (display* "unsorted headers:" headers "\n"))
      ;(uhp (map-in-order (lambda (x) (shorten-path (tree->path x)) )  headers))
      (uhp (map-in-order tree->path headers))
      ;(void (display* "unsorted headers paths :" uhp "\n"))
      (shp (sort uhp path-before?))
      ; sort headers tree list according to their path
      (headers (map-in-order (lambda (x) (list-ref headers (list-index uhp x))) shp))
      (headers1 (map-in-order (lambda (x) (list-ref headers1 (list-index uhp x))) shp))
      (void (debug-outline* "sorted headers:" headers1 "\n"))
      (void (debug-outline* "sorted headers paths :" shp "\n"))
      (levl (map-in-order outline-level headers))
      (void (debug-outline* "sorted headers levels :" levl "\n"))
      (lh (length shp))
      (nexthindex (and (> lh 1) (map-index 
                (lambda (x y) 
                   (with i (list-find-index (sublist levl (+ 1 y) lh) (lambda (z) (<= z (list-ref levl y))))
                      (if i (+ y i 1) #f))) levl))) ;a list where for each item in the sorted header list, gives the index of the following header whose level is <= (i.e. higher in hierarchy), or #f
                      ; note that it does not matter on which list (of correct length) you map
      
      (void (debug-outline* "following headers index :" nexthindex "\n"))
     )
     (do ((i (- lh 1) (- i 1))) ; walk the sorted header list from the end, making them folds 
      ((= i -1) #f) ; #f is the return value of do, evaluated after the successful termination test
      (let* 
        ((h (list-ref headers i))
         (h1 (list-ref headers1 i))
         (void (debug-outline* "********** h:" h1 ":" (in? (tree-label h1) special) "\n"))
        )
        (if (in? (tree-label h1) special)
          (tree-set! h `(,(or (assoc-ref oc?-alist h) 'folded-outline-special) ,h)) ;unless memorized open, special headers are folded by default (they have no inner hierarchy, so folding them does not hide anything of the structure)
          (let* ; it's not a special header
           (;(void (split-long-concat h1))
            (oc (or (assoc-ref oc?-alist h) 'unfolded-outline)) ;use the memorized fold state if exists, otherwise unfolded for viewing the inner hierarchy
            (doc (tree-up h)) ;we work inside doc (may not be what is expected if there is e.g. a "with" enclosing h)
            (inext (and (> lh 1) (list-ref nexthindex i)))
            (nextt (and inext (list-ref headers inext)))
            (posi (tree-index h))
            ;(void (display* "inext :" inext "nextt " nextt " " (and nextt (tree-index nextt)) " " (and nextt(tree-outer nextt)) " " (and nextt (tree-inside? nextt doc)) " " (tree-arity doc)  "\n"))
            (nexti2 (if (and inext (tree-inside? nextt doc))  (tree-index nextt)  (tree-arity doc))) ; index of next header in doc 
            (cl (select doc `((:range ,posi ,nexti2))))
            (void (debug-outline* posi ":" nexti2 "length cl " (length cl)   "\n"))
           )
            (tree-set! h (append `(,oc) cl)) ;make the fold in place of the header
            (if (list>1? cl) (tree-remove! doc (+ posi 1) (- (length cl) 1)))
            ;now handle possible text in-between the header and 1st child fold outline
            ; if any, place it in a unfolded-outline-text
            (with sub (tree-ref h :%0 '(:or unfolded-outline folded-outline)) ; first child that is a fold outline
              ;(display* "enclosed :" sub "\n")
              (let* (
                   (subi (if sub (tree-index sub) (tree-arity h)))
                   (cl1 (and (> subi 1) (select h `((:range 1 ,subi)))))
                   (h2nd (and (> subi 1) (tree-ref h 1)))
                   )
                  (if cl1 (begin
                    (tree-set! h2nd (append '(unfolded-outline-text) cl1))
                    (if (list>1? cl1) (tree-remove! h 2 (- (length cl1) 1)))
                    )))
            )
          )
        )
        (list-set! headers i h) ;? refreshing the item in the list, not sure it's needed (dynamic tree?)
      )
     );do
    )
  )
  
       
(define return-to-initial-cursor #t)

(define (save-cursor)
  ;save cursor in the text before reorganizing the tree
  (debug-outline* "saving cursor in text \n")
  (with cp (cursor-path)
    (if cp 
      (let* ((scp (rcons (cDr cp) 0))
           (pos (number->string (cAr cp)))
           (sel? (selection-active?))
           (ss (and sel? (selection-get-start)))
           (se (and sel? (selection-get-end))))
        (if (path-exists? scp) 
           (begin (go-to-path scp)
              (set! return-to-initial-cursor #t)
              (insert `(cursor-at ,pos))
              (if sel? (selection-set ss se))
              (debug-outline* "cursor saved\n"))
           (debug-outline* "cursor not saved - could not find line start path!\n"))
      )
      (debug-outline* "cursor path does not exist\n")
   )
))


(define (restore-cursor)
  (let* ((l (select (buffer-tree) '(:* cursor-at)))
         (mem (if (list>0? l) (car l) #f))
         (sel? (selection-active?))
         (ss (and sel? (selection-get-start)))
         (se (and sel? (selection-get-end)))
         )
    (if mem 
      (begin
       (if return-to-initial-cursor
        (with cp (rcons (tree->path mem) (tree->number (tree-ref mem 0)))
          (debug-outline* "restoring cursor at " cp "\n") 
          (go-to-path cp)))
       (map remove-node! l)
       (if sel? (selection-set ss se))
       )
    )))

(define (outline?) (== (get-init "outline-mode") "true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; main functions for outlining (entry-point functions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (toggle-outline)
  (with o (get-init "outline-mode")
    (cond ((== o "true") (end-outline-mode))
          ((or (== o "") (== o "false")) (start-outline-mode))
     )))     

(define (end-outline-mode)
  (save-cursor)
  (system-wait "Ending outline, " "please wait")
                                               
  (let* ((mem (get-init-env "mem-page-medium"))
         (mz (get-init-env "mem-page-zoom"))
         (mybuf (current-buffer))
         (buftree (buffer-get-body mybuf))
         )
    (remove-outline-folds-in buftree)
    (if mem   (init-page-medium mem))
    (if mz (change-zoom-factor (string->number mz)))
  )
  (remove-style-package "section-outline")
  (restore-cursor)
  (set-init-env "outline-mode" "false")
  (set! oc?-alist '())
  )

(define (start-outline-mode)
  (if (not-has-style-package? "outline") (add-style-package "outline"))
  (add-style-package "section-outline")
  (set-init-env "mem-page-medium" (get-init-env "page-medium"))
  (set-init-env "mem-page-zoom" (number->string (get-window-zoom-factor)))
  (with bt (buffer-get-body (current-buffer))
    (system-wait "Outlining document, " "please wait")  
   ; (split-sections-headers-concat bt)
    (if (list>0? (select bt '(:* (:or unfolded-outline folded-outline unfolded-outline-special folded-outline-special))))
        (user-ask '("The document seems already outlined. Clear?" "question" "Yes" )
                (lambda (x) (if (== x "Yes") (start-outlining-aux #t))))
        (start-outlining-aux #f)))
  )

(define (start-outlining-aux clean?)
  (with bt (buffer-get-body (current-buffer))
   (save-cursor)
   (if clean? (remove-outline-folds-in bt)) 
   (do-outlining bt)
   ;(cleanup-outlined bt)
   (restore-cursor)))


  
(tm-define (local-refresh . opt)
  ;search up until hiercharchy not broken and refresh 
  (with enc (if (null? opt)
                (tree-search-upwards (if (selection-active?) (path->tree (selection-path)) (cursor-tree)) '(:or unfolded-outline folded-outline))
                (car opt))
    ;(display* "local refreshing, considering " (header-in-outline enc) "\n")
    ;(display* "path : " (tree->path enc) "\n")
    (if enc
       (let* ((inlev (outline-level enc))
              (out (tree-search-upwards (tree-ref enc :up) '(:or unfolded-outline folded-outline)))
              (outlev (and out (outline-level out)))
              (prev (find-next-tag-in-tree '(:or unfolded-outline folded-outline) out #f (tree->path enc)))
              (prevlev (and prev (outline-level prev)))
              (void (debug-outline* "out " (and out (header-in-outline out)) " level " outlev "\nprev " (and prev (header-in-outline prev)) " level " prevlev "\n"))
             )         
            (if (or (and out inlev (>= outlev inlev)) (and prev inlev (< prevlev inlev)))
              (local-refresh out)
              (begin 
                (debug-outline* "now refreshing " (header-in-outline enc) "$$$$$$$$$$$$$$$\n")
                ;(display* "(tree-active? enc)" (tree-active? enc) "\n")
                ;(refresh-part (tree->path enc)) ; path NEEDED? check if active tree
                (refresh-part enc)
              )))
       (refresh-part (buffer-tree))
    )
    
  ))
 
 (tm-define (show-hierarchy-to s)
   ;s = section name or #f #t
   ;if #f close everything (only highest level items in doc will appear, closed)
   ;if #t open everything
   ;if s = section name close every level >= s  (s = 'Part equivalent to s = #f)
   (cond ((== s #t)
            (with l (select (buffer-tree) '(:* (:or folded-outline folded-outline-special folded-outline-text)))
            (map alternate-unfold l)))
         ((== s #f)
            (with l (select (buffer-tree) '(:* (:or unfolded-outline unfolded-outline-special unfolded-outline-text)))
            (map alternate-fold l)))
         (else 
  (let* ((ll (length section-like))
         ;(i (if s (list-index vs s) ll))
         (i (list-index section-like s))
         (lo (append '(:or) (sublist section-like 0 i) (sublist section*-like 0 i)))
         (lc (append '(:or) (sublist section-like i ll) (sublist section*-like i ll)))
         ;handle case appendix
         (test (>= i (assoc-ref levels 'appendix)))
         (lo (if test (append lo '(appendix appendix*)) lo))
         (lc (if test lc (append lc '(appendix appendix*))))
         (l1 (select (buffer-tree) `(:* ,lo)))
         (l2 (select (buffer-tree) `(:* ,lc)))
        )
        
    (map (lambda (x) 
           (let* ((of (tree-search-upwards x '(:or unfolded-outline folded-outline)))
                  (st (tree-ref of 'unfolded-outline-text)))
                  (and st (alternate-fold st))
                  (and of (alternate-fold of))))
           l2)
    (map (lambda (x) 
           (let* ((of (tree-search-upwards x '(:or unfolded-outline folded-outline)))
                  (st (tree-ref of 'unfolded-outline-text)))
                  (and st (alternate-fold st))
                  (and of (alternate-unfold of))))
         l1)
    )
  ))
  ) 

 (tm-define (close-others)
  (with enc (tree-search-upwards (cursor-tree) '(:or unfolded-outline folded-outline unfolded-outline-special folded-outline-special))
    ;(display* "node active:" enc "\n")   
    (if enc 
        
       (let* ((cp (cursor-path))
              (par (parents (list enc)))
              )
              (show-hierarchy-to #f)
              ;(display* "parents" (map header-in-outline par) "\n")
              ;(map (lambda (x) (with st (tree-ref x 1) (and (tm-is? st 'folded-outline-text) (alternate-unfold st)) (alternate-unfold x))) par)
              (map mouse-unfold-aux par)
              (go-to-path cp)
              ))))
 
 (tm-define (add-subsection-here) 
   (if (outline?)
  (let* ((cur (tree-ref (cursor-tree) ))
         (encs (and cur (tree-search-upwards cur '(:or unfolded-outline folded-outline))))
         (enct (and cur (tree-search-upwards cur '(:or unfolded-outline-text folded-outline-text))))
         (levl (and encs (+ 1 (outline-level encs))))
         (newsec (and levl (list-ref section-like levl)))
         )
    (if encs (begin
       (make-section newsec)
       ;(cleanup-outlined encs)
       (if enct (remove-node-raise-children! enct))
       (refresh-part encs)))
        )))
 
 
(tm-define (move-up-down up?)
  ;moves the outline fold enclosing the cursor
  ;the move is realized only if a fold of same hierarchy level is immediatly before or after in the tree 
  ;make it work for reorganizing lists too, while we're at it
  ;(display (cursor-tree*))
    (with node (tree-search-upwards (cursor-tree) 
                (append '(:or unfolded-outline folded-outline unfolded-outline-special folded-outline-special)
                        (variants-of 'enumerate)
                        (variants-of 'itemize)
                        (variants-of 'description)))

    (cond ((not node) (noop))
      ((in? (tree-label node)  '(unfolded-outline folded-outline unfolded-outline-special folded-outline-special))      
         (let* ((out (tree-ref node :up))
                ;FIXME in weird cases, after adding a sublevel to a header concatenated with its following text, the outline-fold is not directly in the document (encapsulated in another doc)
             (void (debug-outline* "node moved:" (header-in-outline node) "\n"))
             ;(void (display* "outer : " (tree-label out) " : " (tree-ref out 1 0) "\n"))
             (levl (outline-level node))
             (pos (tree-index node))
             ;(void (display* "parent:" (tree-ref node :up) "\n"))
             ;(void (display* "parent kids :" (map-index (lambda (x y) (list y (tree-ref node :up y) ) ) (iota (tree-arity (tree-ref node :up))))  "\n"))
             (bro (tree-ref out ((if up? - +) pos 1)))
             ;(void (display* "bro:" bro "\n"))
             (brolevl (outline-level bro))
             (cp (cursor-path))
             (bp (tree->path bro))
              )
             (debug-outline* "levl:" levl " pos:" pos " bro:" (and bro (tree-label bro)) " brolevl:" brolevl "\n")
        (cond ( (and up? brolevl (== brolevl levl))
              (move-node! node out (- pos 1))
              (go-to-path (append bp (sublist cp (length bp) (length cp)))) ;restoring cursor 
              )
              ((and (not up?) brolevl (== brolevl levl))
               (move-node! bro out pos)
               ;WEIRD: if using save/restore-cursor we almost always get a segfault/invalid history at that point
               ;this is why I use another way to keep the cursor position
               (go-to-path (append bp (sublist cp (length bp) (length cp)))) ;restoring cursor
               )
              (else (debug-outline* "can't move (don't know where to...)\n")))
        ))
      ( else ;  (variants-of enumerate) & (variants-of itemize) : reordering lists
         (find-items-and-operate node move-item-up-down up? #f)
)
    
    )))



 (tm-define (change-hierarchy up? branch?)
  (with node (tree-search-upwards (cursor-tree) 
                (append '(:or unfolded-outline folded-outline)
                        (variants-of 'enumerate)
                        (variants-of 'itemize)
                        (variants-of 'description)))
    (cond ((not node) (noop))
          ((in? (tree-label node)  '(unfolded-outline folded-outline))
             (let* ((l (if branch?
                         (append (list node) (select node '(:* (:or unfolded-outline folded-outline))))
                         (list node)) )
                 (out (or (tree-search-upwards (tree-ref node :up) 
                                               '(:or unfolded-outline folded-outline))
                          (buffer-tree)))
                 )
                (debug-outline* "nodes moved:" (map header-in-outline l) "\n")
                (map (lambda (x) (change-hierarchy-1node x up?)) l)
                (debug-outline* "now refreshing outer node $$$$$$$$$$$$$$$\n")
                (refresh-part out)
             ) 
          )
          (else ;itemize and list variants
            (find-items-and-operate node up-downgrade-item up? branch?)
          )
        
  )))
