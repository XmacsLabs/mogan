;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE      : bash-edit.scm
;; DESCRIPTION : editing bash scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code bash-edit)
  (:use (prog prog-edit)
        (code bash-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation policy
;; - Bash community style is often 2 or 4 spaces. We'll use 2.
;; - Keep it configurable by overriding get-tabstop if desired.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (bash-tabstop) 2)

(tm-define (get-tabstop)
  (:mode in-prog-bash?)
  (bash-tabstop))

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
;; This is not a full bash parser; it is a pragmatic editor indentation scheme.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bash-string-prefix? s p)
  (and (>= (string-length s) (string-length p))
       (== (substring s 0 (string-length p)) p)))

(define (bash-trim-left s)
  (let loop ((i 0) (n (string-length s)))
    (if (or (>= i n)
            (not (char-whitespace? (string-ref s i))))
        (substring s i n)
        (loop (+ i 1) n))))

(define (bash-trim-right s)
  (let loop ((i (- (string-length s) 1)))
    (if (< i 0) ""
        (if (char-whitespace? (string-ref s i))
            (loop (- i 1))
            (substring s 0 (+ i 1))))))

(define (bash-trim s) (bash-trim-right (bash-trim-left s)))

;; Very small set of "line ends with operator" cues for continuation indentation.
;; Includes line continuation backslash, pipe |, &&, ||, etc.
(define (bash-line-continues? line)
  (let* ((t (bash-trim-right line))
         (n (string-length t)))
    (if (<= n 0) #f
        (or
          ;; Ends with backslash (line continuation)
          (== (string-ref t (- n 1)) #\\)
          ;; Ends with an operator token (rough heuristic)
          (let ((ops '("|" "||" "&&" ";" "&" ">" ">>" "<" "<<" "2>" "2>>" "1>" "1>>")))
            (let loop ((xs ops))
              (if (null? xs) #f
                  (let* ((op (car xs))
                         (m (string-length op)))
                    (if (and (>= n m)
                             (== (substring t (- n m) n) op))
                        #t
                        (loop (cdr xs)))))))
          ;; Ends with ` then` or ` do` (but we check trimmed line)
          ))))

(define (bash-line-starts-with-closing-brace? line)
  (let ((t (bash-trim-left line)))
    (and (> (string-length t) 0)
         (== (string-ref t 0) #\}))))

(define (bash-line-starts-with-fi-done-esac? line)
  (let ((t (bash-trim-left line)))
    (or (bash-string-prefix? t "fi")
        (bash-string-prefix? t "done")
        (bash-string-prefix? t "esac")
        (bash-string-prefix? t "elif")
        (bash-string-prefix? t "else"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation computation
;;
;; Fallback logic:
;; - Start from previous line indentation.
;; - If current line starts with '}', decrease one level.
;; - If current line starts with fi, done, esac, elif, else, decrease one level.
;; - If previous line contains an opening '{' not closed on same line, increase.
;; - If previous line looks like it continues, increase one level.
;; - If previous line contains if, while, until, for, case (without then/do), increase.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following helpers depend on the generic prog-edit infrastructure.
;; We keep calls conservative; if your build exposes different primitives,
;; adjust read-line / line indentation accessors accordingly.

(define (bash-get-line doc row)
  ;; Best-effort wrapper; many TeXmacs/Mogan builds expose something like this.
  (with-optional (prog-line-string doc row) ""))

(define (bash-leading-spaces-count s)
  (let loop ((i 0) (n (string-length s)))
    (if (or (>= i n)
            (not (char-whitespace? (string-ref s i))))
        i
        (loop (+ i 1) n))))

(define (bash-prev-nonempty-row doc row)
  (let loop ((r (- row 1)))
    (if (< r 0) -1
        (let* ((line (bash-get-line doc r))
               (t (bash-trim line)))
          (if (== t "") (loop (- r 1)) r))))

(define (bash-indent-level-from-prev doc row)
  (let* ((pr (bash-prev-nonempty-row doc row)))
    (if (< pr 0) 0
        (let* ((pline (bash-get-line doc pr))
               (base (bash-leading-spaces-count pline))
               (tab (get-tabstop))
               (trimmed (bash-trim pline))
               (inc? (or
                      (bash-line-continues? pline)
                      ;; crude: if previous line ends with "{", indent next
                      (and (> (string-length trimmed) 0)
                           (== (string-ref trimmed (- (string-length trimmed) 1)) #\{))
                      ;; if previous line contains if, for, while, until, case without then/do
                      (let ((words '("if" "for" "while" "until" "case")))
                        (let loop ((ws words))
                          (if (null? ws) #f
                              (let ((w (car ws)))
                                (if (and (bash-string-prefix? trimmed w)
                                         (not (bash-string-prefix? trimmed (string-append w " then")))
                                         (not (bash-string-prefix? trimmed (string-append w " do"))))
                                    #t
                                    (loop (cdr ws))))))))))
          (+ base (if inc? tab 0))))))

(tm-define (program-compute-indentation doc row col)
  (:mode in-prog-bash?)
  (let* ((tab (get-tabstop))
         (line (bash-get-line doc row))
         (base (bash-indent-level-from-prev doc row)))
    (cond
      ((or (bash-line-starts-with-closing-brace? line)
           (bash-line-starts-with-fi-done-esac? line))
       (max 0 (- base tab)))
      (else base))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commenting
;;
;; Bash uses '#' as the line comment marker.
;; We expose a basic toggle suitable for prog-edit.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (program-comment-start)
  (:mode in-prog-bash?)
  "#")

(tm-define (program-toggle-comment)
  (:mode in-prog-bash?)
  (prog-toggle-line-comment "#"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paste import hook
;;
;; Keep minimal and safe: treat pasted text as bash source.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-paste)
  (:mode in-prog-bash?)
  (clipboard-paste-import "bash" "primary"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic insertion, highlighting and selection of brackets and quotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (bash-bracket-open lbr rbr)
  ;; Insert a pair of brackets/quotes and position cursor between them
  (bracket-open lbr rbr "\\"))

(tm-define (bash-bracket-close lbr rbr)
  ;; Handle closing bracket/quote and position cursor correctly
  (bracket-close lbr rbr "\\"))

(tm-define (notify-cursor-moved status)
  (:require prog-highlight-brackets?)
  (:mode in-prog-bash?)
  ;; Highlight matching brackets when cursor moves
  (select-brackets-after-movement "([{" ")]}" "\\"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard mappings for bash programming mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-prog-bash?)
  ;; bash programming mode keyboard shortcuts
  ("A-tab" (insert-tabstop))                 ;; Alt+Tab: insert tabstop
  ("cmd S-tab" (remove-tabstop))             ;; Cmd+Shift+Tab: remove tabstop
  ("{" (bash-bracket-open "{" "}" ))            ;; Auto-insert matching braces
  ("}" (bash-bracket-close "{" "}" ))           ;; Handle closing brace
  ("(" (bash-bracket-open "(" ")" ))            ;; Auto-insert matching parentheses
  (")" (bash-bracket-close "(" ")" ))           ;; Handle closing parenthesis
  ("[" (bash-bracket-open "[" "]" ))            ;; Auto-insert matching brackets
  ("]" (bash-bracket-close "[" "]" ))           ;; Handle closing bracket
  ("\"" (bash-bracket-open "\"" "\"" ))         ;; Auto-insert matching double quotes
  ("'" (bash-bracket-open "'" "'" )))           ;; Auto-insert matching single quotes