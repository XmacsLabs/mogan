(define-library (liii lint)
  (export lint-check-brackets)
  (import (liii base)
          (liii list)
          (liii string)
          (liii stack))

  (begin

    (define *open-paren* (integer->char 40))
    (define *close-paren* (integer->char 41))

    (define (skip-whitespace chars)
      (let loop ((chars chars))
        (if (and (not (null? chars)) 
                 (char-whitespace? (car chars)))
            (loop (cdr chars))
            chars)))

    (define (read-symbol chars)
      (let loop ((chars chars) (result '()))
        (cond
          ((null? chars) (values (list->string (reverse result)) '()))
          ((or (char-whitespace? (car chars))
               (char=? (car chars) *open-paren*)
               (char=? (car chars) *close-paren*))
           (values (list->string (reverse result)) chars))
          (else (loop (cdr chars) (cons (car chars) result))))))

#|
lint-check-brackets
检查字符串中的括号是否平衡匹配，提供精确的符号上下文。

返回值：
- '(matched) - 所有括号正确匹配  
- '(unmatched (error-type location context)) - 存在不匹配的括号

location: (line col)
context: (symbol parent-line parent-col) 或 'anonymous
|#
    (define (lint-check-brackets content)
      (let ((chars (string->list content))
            (line 1)
            (col 1)
            (bracket-stack (stack '()))
            (symbol-stack (stack '())))
        
        (let loop ((pos 0)
                   (line line)
                   (col col)
                   (chars chars)
                   (br-st bracket-stack)
                   (sy-st symbol-stack)
                   (in-string? #f)
                   (in-char? #f)
                   (escaped? #f))
          (cond
            ((null? chars)
             (if (= (br-st :size) 0)
                 (cons 'matched '())
                 (let ((last-open (br-st :top)))
                   (let ((location (car last-open))
                         (context (cadr last-open)))
                     (cons 'unmatched (list (list 'unclosed location context)))))))
            
            ((char=? (car chars) #\newline)
             (loop (+ pos 1) (+ line 1) 1 (cdr chars) br-st sy-st in-string? in-char? #f))
            
            ((and (not in-string?) (not in-char?) (char=? (car chars) #\;))
             ;; 跳过注释到行尾
             (let skip-comment ((chars chars))
               (if (or (null? chars) (char=? (car chars) #\newline))
                   (if (null? chars)
                       (loop pos line col chars br-st sy-st in-string? in-char? #f)
                       (loop (+ pos 1) (+ line 1) 1 (cdr chars) br-st sy-st in-string? in-char? #f))
                   (skip-comment (cdr chars)))))

            ((and (not in-string?) (not in-char?) (char=? (car chars) #\#) 
                 (not (null? (cdr chars))) (char=? (cadr chars) #\|))
             ;; Skip block comments #|...|#
             (let skip-block-comment ((chars (cddr chars)) (line line) (col (+ col 2)))
               (cond
                 ((null? chars)
                  (loop pos line col chars br-st sy-st in-string? in-char? #f))
                 ((and (not (null? (cdr chars))) (char=? (car chars) #\|) (char=? (cadr chars) #\#))
                  (loop (+ pos 2) line (+ col 2) (cddr chars) br-st sy-st in-string? in-char? #f))
                 ((char=? (car chars) #\newline)
                  (skip-block-comment (cdr chars) (+ line 1) 1))
                 (else
                  (skip-block-comment (cdr chars) line (+ col 1))))))
            
            ((and (not in-string?) (char=? (car chars) #\#))
             ;; Handle # prefixed literals including character literals #\) #\( etc.
             (cond
               ((and (not (null? (cdr chars))) (char=? (cadr chars) #\\))
                ;; Handle character literals like #\) #\( #\[ #\] etc.
                (if (null? (cddr chars))
                    ;; Only #\ present, invalid but handle gracefully
                    (loop (+ pos 2) line (+ col 2) (cddr chars) br-st sy-st in-string? in-char? #f)
                    ;; Skip #\ and the following character, even if it's a parenthesis
                    (loop (+ pos 3) line (+ col 3) (cdddr chars) br-st sy-st in-string? in-char? #f)))
               (else
                ;; Handle other # prefixed literals (like #t, #f, etc.)
                (loop (+ pos 1) line (+ col 1) (cdr chars) br-st sy-st in-string? in-char? #f))))
            
            ((char=? (car chars) #\")
             (cond
               (escaped? (loop (+ pos 1) line (+ col 1) (cdr chars) br-st sy-st in-string? in-char? #f))
               (else (loop (+ pos 1) line (+ col 1) (cdr chars) br-st sy-st (not in-string?) in-char? #f))))
            
            ((and in-string? (char=? (car chars) #\\))
             (loop (+ pos 1) line (+ col 1) (cdr chars) br-st sy-st in-string? in-char? (not escaped?)))
            
            (in-string?
             (loop (+ pos 1) line (+ col 1) (cdr chars) br-st sy-st in-string? in-char? #f))
            
            ((char=? (car chars) *open-paren*)
             ;; 查找下一个符号作为这个括号的上下文
             (let* ((next-chars (skip-whitespace (cdr chars)))
                    (context (if (null? next-chars)
                               'anonymous
                               (let-values (((symbol next-next-chars) (read-symbol next-chars)))
                                 (if (string-null? symbol)
                                     'anonymous
                                     `(,symbol ,line ,col)))))
                    (new-br-st (br-st :push (list (list line col) context)))
                    (new-sy-st (if (and (not (eq? context 'anonymous))
                                       (not (null? next-chars)))
                                  (sy-st :push context)
                                  sy-st)))
               (loop (+ pos 1) line (+ col 1) (cdr chars) new-br-st new-sy-st in-string? in-char? #f)))
            
            ((char=? (car chars) *close-paren*)
             (if (= (br-st :size) 0)
                 (let ((context (if (> (sy-st :size) 0) 
                                   (sy-st :top)
                                   'anonymous)))
                   (cons 'unmatched (list (list 'unmatched-close (list line col) context))))
                 (let ((new-br-st (br-st :pop))
                       (context (cadr (br-st :top))))
                   (loop (+ pos 1) line (+ col 1) (cdr chars) new-br-st sy-st in-string? in-char? #f))))
            
            (else
             (loop (+ pos 1) line (+ col 1) (cdr chars) br-st sy-st in-string? in-char? #f))))))))