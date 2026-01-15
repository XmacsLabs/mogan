;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE      : r-edit.scm
;; DESCRIPTION : editing R programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code r-edit)
  (:use (prog prog-edit)
        (code r-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation policy
;; - R community style is often 2 spaces (tidyverse), sometimes 4.
;; - Keep it configurable by overriding get-tabstop if desired.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (r-tabstop) 2)

(tm-define (get-tabstop)
  (:mode in-prog-r?)
  (r-tabstop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers (lightweight, line-based)
;;
;; We intentionally keep indentation heuristics simple:
;; - Increase indent after '{' and after an opening bracket/paren that is not
;;   closed on the same line.
;; - Decrease indent if line starts with '}'.
;; - Continuation indent if previous line ends with an operator or has an
;;   unmatched open paren/bracket.
;;
;; This is not a full R parser; it is a pragmatic editor indentation scheme.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (r-string-prefix? s p)
  (and (>= (string-length s) (string-length p))
       (== (substring s 0 (string-length p)) p)))

(define (r-trim-left s)
  (let loop ((i 0) (n (string-length s)))
    (if (or (>= i n)
            (not (char-whitespace? (string-ref s i))))
        (substring s i n)
        (loop (+ i 1) n))))

(define (r-trim-right s)
  (let loop ((i (- (string-length s) 1)))
    (if (< i 0) ""
        (if (char-whitespace? (string-ref s i))
            (loop (- i 1))
            (substring s 0 (+ i 1))))))

(define (r-trim s) (r-trim-right (r-trim-left s)))

;; Very small set of “line ends with operator” cues for continuation indentation.
;; Includes base pipe |>, magrittr %>%, and common binary operators.
(define (r-line-continues? line)
  (let* ((t (r-trim-right line))
         (n (string-length t)))
    (if (<= n 0) #f
        (or
          (r-string-prefix? (r-trim-left t) "|>")
          ;; Ends with an operator token (rough heuristic)
          (let ((ops '("+" "-" "*" "/" "^" "=" "<-" "<<-" "->" "->>" "&" "|" "&&" "||" ":" ",")))
            (let loop ((xs ops))
              (if (null? xs) #f
                  (let* ((op (car xs))
                         (m (string-length op)))
                    (if (and (>= n m)
                             (== (substring t (- n m) n) op))
                        #t
                        (loop (cdr xs)))))))
          ;; Ends with %something% (user-defined infix)
          (and (>= n 2)
               (== (string-ref t (- n 1)) #\%)
               (let ((j (- n 2)))
                 (let loop ((k j))
                   (cond
                     ((< k 0) #f)
                     ((== (string-ref t k) #\%) #t)
                     (else (loop (- k 1)))))))))))

(define (r-line-starts-with-closing-brace? line)
  (let ((t (r-trim-left line)))
    (and (> (string-length t) 0)
         (== (string-ref t 0) #\}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation computation
;;
;; Fallback logic:
;; - Start from previous line indentation.
;; - If current line starts with '}', decrease one level.
;; - If previous line contains an opening '{' not closed on same line, increase.
;; - If previous line looks like it continues, increase one level.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following helpers depend on the generic prog-edit infrastructure.
;; We keep calls conservative; if your build exposes different primitives,
;; adjust read-line / line indentation accessors accordingly.

(define (r-get-line doc row)
  ;; Best-effort wrapper; many TeXmacs/Mogan builds expose something like this.
  (with-optional (prog-line-string doc row) ""))

(define (r-leading-spaces-count s)
  (let loop ((i 0) (n (string-length s)))
    (if (or (>= i n)
            (not (char-whitespace? (string-ref s i))))
        i
        (loop (+ i 1) n))))

(define (r-prev-nonempty-row doc row)
  (let loop ((r (- row 1)))
    (if (< r 0) -1
        (let* ((line (r-get-line doc r))
               (t (r-trim line)))
          (if (== t "") (loop (- r 1)) r)))))

(define (r-indent-level-from-prev doc row)
  (let* ((pr (r-prev-nonempty-row doc row)))
    (if (< pr 0) 0
        (let* ((pline (r-get-line doc pr))
               (base (r-leading-spaces-count pline))
               (tab (get-tabstop))
               (trimmed (r-trim pline))
               (inc? (or
                      (r-line-continues? pline)
                      ;; crude: if previous line ends with "{", indent next
                      (and (> (string-length trimmed) 0)
                           (== (string-ref trimmed (- (string-length trimmed) 1)) #\{)))))
          (+ base (if inc? tab 0))))))

(tm-define (program-compute-indentation doc row col)
  (:mode in-prog-r?)
  (let* ((tab (get-tabstop))
         (line (r-get-line doc row))
         (base (r-indent-level-from-prev doc row)))
    (if (r-line-starts-with-closing-brace? line)
        (max 0 (- base tab))
        base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commenting
;;
;; R uses '#' as the line comment marker.
;; We expose a basic toggle suitable for prog-edit.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (program-comment-start)
  (:mode in-prog-r?)
  "#")

(tm-define (program-toggle-comment)
  (:mode in-prog-r?)
  (prog-toggle-line-comment "#"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paste import hook
;;
;; Keep minimal and safe: treat pasted text as R source.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-paste)
  (:mode in-prog-r?)
  (clipboard-paste-import "r" "primary"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic insertion, highlighting and selection of brackets and quotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (r-bracket-open lbr rbr)
  ;; Insert a pair of brackets/quotes and position cursor between them
  (bracket-open lbr rbr "\\"))

(tm-define (r-bracket-close lbr rbr)
  ;; Handle closing bracket/quote and position cursor correctly
  (bracket-close lbr rbr "\\"))

(tm-define (notify-cursor-moved status)
  (:require prog-highlight-brackets?)
  (:mode in-prog-r?)
  ;; Highlight matching brackets when cursor moves
  (select-brackets-after-movement "([{" ")]}" "\\"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard mappings for R programming mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-prog-r?)
  ;; R programming mode keyboard shortcuts
  ("A-tab" (insert-tabstop))                 ;; Alt+Tab: insert tabstop
  ("cmd S-tab" (remove-tabstop))             ;; Cmd+Shift+Tab: remove tabstop
  ("{" (r-bracket-open "{" "}" ))            ;; Auto-insert matching braces
  ("}" (r-bracket-close "{" "}" ))           ;; Handle closing brace
  ("(" (r-bracket-open "(" ")" ))            ;; Auto-insert matching parentheses
  (")" (r-bracket-close "(" ")" ))           ;; Handle closing parenthesis
  ("[" (r-bracket-open "[" "]" ))            ;; Auto-insert matching brackets
  ("]" (r-bracket-close "[" "]" ))           ;; Handle closing bracket
  ("\"" (r-bracket-open "\"" "\"" ))         ;; Auto-insert matching double quotes
  ("'" (r-bracket-open "'" "'" )))           ;; Auto-insert matching single quotes
