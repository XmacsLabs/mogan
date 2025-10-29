(define-library (liii pp)
(export pp-parse pp-format pp-post format-file format-single-file format-file-in-place)
(import (liii base)
        (liii string)
        (liii sys)
				(liii list)
				(liii pretty-print)
				(liii path)) 
(begin

(define (is-newline? str pos)
  (char=? (str pos) #\newline))

(define (count-newline str pos)
  (let loop ((n 0) (i pos))
    (cond ((>= i (string-length str))
           n)
          ((not (is-newline? str i))
           n)
          (else
           (loop (+ n 1) (+ i 1))))))

(define (encode-newlines n)
  (cond ((= n 1)
         "\n")
        ((>= n 2)
         (string-append "\n" (object->string (list '*PP_NEWLINE* n)) "\n"))
        (else (value-error "encode-newline: n must >= 1, but got" n))))

;; 当前状态：start
;; 含义：位置在代码的行首
(define (next-state-from-start str pos result)
  ; (display* "start: " pos result "\n")
  (if (>= pos (string-length str))
      (values 'end pos result)
      (cond ((is-newline? str pos)
             ;; start -> start | #\newline
             (let* ((n (count-newline str pos))
                    (next-pos (+ pos n)))
               (values 'start next-pos (string-append result (encode-newlines n)))))
            ((char=? (str pos) #\;)
             ;; start -> comment | ;
             (values 'comment (+ pos 1) result))
            ((and (< (+ pos 1) (string-length str))
                  (char=? (str pos) #\#)
                  (char=? (str (+ pos 1)) #\|))
             ;; start -> multi-comment | #|
             (values 'multi-comment (+ pos 2) result))
            (else
             (values 'normal (+ pos 1) (string-append result (string (str pos))))))))

;; 当前状态： normal
;; 含义：位置在代码的行中
(define (has-non-whitespace-on-line-before-comment str comment-pos)
  ;; Check if there is non-whitespace content on the same line before the comment
  (let loop ((i (- comment-pos 1)))
    (cond ((< i 0) #f)  ; Start of string, no content found
          ((is-newline? str i) #f)  ; Found start of line, no content found
          ((not (char-whitespace? (str i))) #t)  ; Found non-whitespace content
          (else (loop (- i 1))))))

(define (find-newline-after-comment str comment-pos)
  ;; Find the position of newline after the comment, or end of string
  (let loop ((i comment-pos))
    (cond ((>= i (string-length str)) i)
          ((is-newline? str i) i)
          (else (loop (+ i 1))))))

(define (next-state-from-normal str pos result)
  ; (display* "normal: " pos result "\n")
  (if (>= pos (string-length str))
      (values 'end pos result)
      (cond ((is-newline? str pos)
             (let* ((n (count-newline str pos))
                    (next-pos (+ pos n)))
               (values 'start next-pos (string-append result (encode-newlines n)))))
            ((char=? (str pos) #\;)
             ;; Check if this is an end-of-line comment by looking for non-whitespace before it
             (if (has-non-whitespace-on-line-before-comment str pos)
                 ;; This is an end-of-line comment, skip it
                 (let ((newline-pos (find-newline-after-comment str pos)))
                   (if (and newline-pos (< newline-pos (string-length str)))
                       (let* ((n (count-newline str newline-pos))
                              (next-pos (+ newline-pos n)))
                         (values 'start next-pos (string-append result (encode-newlines n))))
                       (values 'end (string-length str) result)))
                 ;; This is a standalone comment, process normally
                 (values 'comment (+ pos 1) result)))
            ((and (< (+ pos 1) (string-length str))
                  (char=? (str pos) #\#)
                  (char=? (str (+ pos 1)) #\|))
             ;; normal -> multi-comment | #| 
             (values 'multi-comment (+ pos 2) result))
            (else (values 'normal (+ pos 1) (string-append result (string (str pos))))))))

(define (next-state-from-comment str pos result)
  (let loop ((pos pos) (current "") (started? #f) (has-content? #f))
    (cond ((>= pos (string-length str))
           ;; End reached, terminate with comment content
           ;; 即使是空白注释也应该保留，使用特殊标记
           (values 'end pos (string-append result (object->string (list '*PP_SINGLE_COMMENT* current)))))
          ((is-newline? str pos)
           ;; Found newline, comment ends, return to start state
           ;; Let the start state handle the newline to avoid double-processing
           (values 'start pos (string-append result (object->string (list '*PP_SINGLE_COMMENT* current)))))
          ((not started?)
           ;; Skip leading whitespace after semicolon
           (if (char-whitespace? (str pos))
               (loop (+ pos 1) current #f #f)
               (loop (+ pos 1) (string-append current (string (str pos))) #t #t)))
          (else
           ;; Continue collecting comment content until newline or end
           (loop (+ pos 1) (string-append current (string (str pos))) #t #t)))))


(define (next-state-from-multi-comment str pos result)
  (let loop ((pos pos) (current "") (lines '()) (line-start pos))
    (cond ((>= pos (string-length str))
           ;; End reached, terminate
           (values 'end pos (object->string (cons '*PP_MULTI_COMMENT* (reverse (cons current lines))))))
          ((and (< (+ pos 1) (string-length str))
                (char=? (str pos) #\|)
                (char=? (str (+ pos 1)) #\#))
           ;; Found |#, add accumulated content to result and terminate
           (values 'multi-comment-end (+ pos 2) 
                   (string-append result (object->string (cons '*PP_MULTI_COMMENT* (reverse (cons current lines)))))))
          ((is-newline? str pos)
           ;; Found newline, add current content to lines and reset current
           (loop (+ pos 1) "" (cons current lines) (+ pos 1)))
          (else
           ;; Continue collecting characters for current line
           (loop (+ pos 1) (string-append current (string (str pos))) lines line-start)))))

(define (next-state-from-multi-comment-end str pos result)
  (if (>= pos (string-length str))
      (values 'end pos result)
      (cond ((is-newline? str pos)
             ;; multi-comment-end -> start | #\newline 
             (let* ((n (count-newline str pos))
                    (next-pos (+ pos n)))
               (values 'start next-pos (string-append result (encode-newlines n)))))
            ((char-whitespace? (str pos))
             ;; multi-comment-end -> normal | #\whitespace
             (values 'normal (+ pos 1) result))
            (else
             ;; multi-comment-end -> normal | char
             (values 'normal pos result)))))

(define (pp-parse str)
  (let loop ((state 'start) (pos 0) (result ""))
    (if (string-null? str)
        ""
        (case state 
          ((start)
           (receive (next-state next-pos next-result)
                    (next-state-from-start str pos result)
             (loop next-state next-pos next-result)))
          ((comment)
           (receive (next-state next-pos next-result)
                    (next-state-from-comment str pos result)
             (loop next-state next-pos next-result)))
          ((multi-comment)
           (receive (next-state next-pos next-result)
                    (next-state-from-multi-comment str pos result)
             (loop next-state next-pos next-result)))
          ((multi-comment-end)
           (receive (next-state next-pos next-result)
                    (next-state-from-multi-comment-end str pos result)
             (loop next-state next-pos next-result)))
          ((end) result)
          (else
           (receive (next-state next-pos next-result)
                    (next-state-from-normal str pos result)
             (loop next-state next-pos next-result)))))))

; 格式化文件相关函数

(define (clean-empty-lines str)
  ;; 清理空行中的空白字符：将只包含空格和制表符的行转换为纯换行
  (let ((result str)
        (max-iterations 10000)) ;; 防止无限循环
    (let loop ((s result) (count 0))
      (if (>= count max-iterations)
          s
          (let ((whitespace-match (find-whitespace-line s 0)))
            (if whitespace-match
                (loop (string-replace-one s whitespace-match) (+ count 1))
                s))))))

(define (find-whitespace-line str start-pos)
  ;; 查找 \n[空格或制表符]+\n 的模式，返回匹配位置或 #f
  (let ((len (string-length str)))
    (let loop ((i start-pos))
      (cond ((>= i (- len 2)) #f)  ;; 至少需要2个字符：\n和\n
            ((and (char=? (string-ref str i) #\newline)
                  (let ((ws-end (skip-whitespace str (+ i 1))))
                    (and ws-end
                         (< ws-end len)
                         (char=? (string-ref str ws-end) #\newline)
                         (> ws-end (+ i 1)))))  ;; 确保至少有一个空白字符
             i)
            (else (loop (+ i 1)))))))

(define (skip-whitespace str pos)
  ;; 跳过连续的空格和制表符，返回第一个非空白字符位置或字符串末尾
  (let ((len (string-length str)))
    (let loop ((i pos))
      (cond ((>= i len) i)
            ((or (char=? (string-ref str i) #\space)
                 (char=? (string-ref str i) #\tab))
             (loop (+ i 1)))
            (else i)))))

(define (string-replace-one str match-pos)
  ;; 替换指定位置的 \n[空白]+\n 为 \n\n
  (let* ((ws-start (+ match-pos 1))
         (ws-end (skip-whitespace str ws-start))
         (end-pos ws-end))
    (string-append
     (substring str 0 match-pos)
     "\n\n"
     (substring str (+ end-pos 1) (string-length str)))))

(define (pp-post str)
  ;; 后处理只保留清理空行的功能
  (clean-empty-lines str))

(define (format-file filename)
  (let* ((content (path-read-text filename))
         (preprocessed (pp-parse content))
         (port (open-input-string preprocessed)))
    (if (not port)
        #f
        (let ((output (open-output-string)))
          (let loop ()
            (let ((expr (read port)))
              (cond 
               ((eof-object? expr) 
                (let ((formatted (get-output-string output)))
                  (pp-post formatted)))
               (else 
                (display (pp expr) output)
                (newline output)
                (loop)))))))))

(define (format-single-file filename)
  (let ((formatted (format-file filename)))
    (if formatted
        (begin
          (display formatted)
          #t)  ; Return success
        (begin
          (display (string-append "Error: Failed to format file: " filename "\n"))
          #f))))  ; Return failure

(define (format-file-in-place filename)
  (let ((formatted (format-file filename)))
    (if formatted
        (begin
          (path-write-text filename formatted)
          #t)  ; Return success
        (begin
          (display (string-append "Error: Failed to format file: " filename "\n"))
          #f))))  ; Return failure

(define (pp-format)
  (let* ((args (argv))
         (argc (length args)))
    (cond 
     ((<= argc 2) 
      (display "Usage: format.scm [-i] <file1> [file2] ...\n")
      (display "  -i  Format files in-place\n")
      #t)  ; Return success for usage display
     (else
      (let ((first-arg (and (> argc 2) (list-ref args 2))))
        (if (and first-arg (string=? first-arg "-i"))
            (if (<= argc 3)
                (begin
                  (display "Error: -i option requires at least one file\n")
                  #f)  ; Return failure for error
                (let ((files (list-tail args 3)))  ; Skip program name and -i option
                  (let ((results (map format-file-in-place files)))
                    (if (every (lambda (x) x) results)
                        #t  ; All succeeded
                        #f)))) ; At least one failed
            (format-single-file first-arg)))))))


) ; end of begin
) ; end of define-library
