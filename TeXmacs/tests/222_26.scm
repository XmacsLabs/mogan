;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : 222_26_markdown.scm
;; DESCRIPTION : Unit tests for markdown format detection
;; COPYRIGHT   : (C) 2026 Mingshen Chu
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (liii check))

(check-set-mode! 'report-failed)

;; ============================================================================
;; Test cases for Markdown format detection
;; ============================================================================

;; 应该检测为Markdown的文本
(define md-text1 "# 标题\n这是Markdown文档的第一段。\n\n## 二级标题\n这是第二段。")

(define md-text2 (string-append "```python\\ndef hello():\n" "print(\"Hello, World!\")\n```\n\n这是代码块后面的文本。"))

(define md-text3 "> 这是引用块\n> 引用块的第二行\n\n普通文本。")

(define md-text4 "- 无序列表项1\n- 无序列表项2\n  - 子列表项\n- 无序列表项3")

(define md-text5 "1. 有序列表项1\n2. 有序列表项2\n3. 有序列表项3")

(define md-text6 "| 表头1 | 表头2 |\n|-------|-------|\n| 单元格1 | 单元格2 |\n| 单元格3 | 单元格4 |")

(define md-text7 "**粗体文本** 和 *斜体文本* 以及 `行内代码`。")

(define md-text8 "[链接文本](https://example.com) 和 ![图片描述](image.png)")

(define md-text9 "---\n\n这是水平线下面的文本。\n\n===")

(define md-text10 "# 混合Markdown文档\n\n这是一个包含多种Markdown元素的文档。\n\n## 列表部分\n- 项目1\n- 项目2\n\n## 代码部分\n```bash\necho \"Hello\"\n```\n\n## 引用部分\n> 名人名言\n\n---\n\n结束。")

;; 不应该检测为Markdown的文本
(define non-md-text1 "这是普通文本，没有任何Markdown格式。")

(define non-md-text2 "#这是没有空格的标题\n普通文本。")

(define non-md-text3 "def function():\n    # 这是Python注释\n    return 42")

(define non-md-text4 "C:\\Users\\Name\\Documents\\file.txt\nD:/Projects/code/src/main.py")

(define non-md-text5 "email@example.com\nuser.name@domain.co.uk")

(define non-md-text6 (string-append "var x = 10;\nvar y = 20;\n// 这是JavaScript注释\nconsole.log(x + y);"))

(define non-md-text7 "<?xml version=\"1.0\"?>\n<root>\n  <element attr=\"value\">text</element>\n</root>")

(define non-md-text8 "\\documentclass{article}\n\\begin{document}\n\\section{LaTeX标题}\n这是LaTeX文档。\n\\end{document}")

(define non-md-text9 "{\n  \"name\": \"John\",\n  \"age\": 30,\n  \"city\": \"New York\"\n}")

(define non-md-text10 "<!DOCTYPE html>\n<html>\n<head>\n  <title>HTML文档</title>\n</head>\n<body>\n  <h1>HTML标题</h1>\n</body>\n</html>")

;; ============================================================================
;; Helper functions (copied from 222_25.scm with modifications for Markdown)
;; ============================================================================

(define (string-split-lines s)
  (let ((len (if (>= (string-length s) 1000) 1000 (string-length s))))
    (let loop ((i 0)
              (start 0)
              (result '()))
      (cond ((>= i len)
             (reverse (cons (substring s start i) result)))
            ((char=? (string-ref s i) #\newline)
             (loop (+ i 1)
                   (+ i 1)
                   (cons (substring s start i) result)))
            (else (loop (+ i 1) start result))))))

;; Count occurrences of a character in string
(define (string-count-char s ch)
  (let loop ((i 0)
             (count 0))
    (if (>= i (string-length s))
        count
        (loop (+ i 1)
              (if (char=? (string-ref s i) ch)
                  (+ count 1)
                  count)))))

;; Count occurrences of a substring in string
(define (string-count-substring s sub)
  (let ((sub-len (string-length sub)))
    (if (zero? sub-len)
        0
        (let loop ((i 0)
                   (count 0))
          (if (>= i (- (string-length s) sub-len -1))
              count
              (if (string=? (substring s i (+ i sub-len)) sub)
                  (loop (+ i sub-len) (+ count 1))
                  (loop (+ i 1) count)))))))

;; ============================================================================
;; Markdown-specific helper functions
;; ============================================================================

;; Check if a line starts with Markdown features
(define (markdown-line-start? line)
  (let ((len (string-length line)))
    (cond
      ;; Headers: #, ##, ###, etc. (must be followed by space)
      ((and (>= len 2) (char=? (string-ref line 0) #\#) (char-whitespace? (string-ref line 1))) #t)
      ((and (>= len 3) (string=? (substring line 0 2) "##") (char-whitespace? (string-ref line 2))) #t)
      ((and (>= len 4) (string=? (substring line 0 3) "###") (char-whitespace? (string-ref line 3))) #t)
      ((and (>= len 5) (string=? (substring line 0 4) "####") (char-whitespace? (string-ref line 4))) #t)
      ((and (>= len 6) (string=? (substring line 0 5) "#####") (char-whitespace? (string-ref line 5))) #t)
      ((and (>= len 7) (string=? (substring line 0 6) "######") (char-whitespace? (string-ref line 6))) #t)

      ;; Code blocks: ``` or ~~~
      ((and (>= len 3) (or (string=? (substring line 0 3) "```")
                           (string=? (substring line 0 3) "~~~"))) #t)

      ;; Blockquote: > followed by space
      ((and (>= len 2) (char=? (string-ref line 0) #\>) (char-whitespace? (string-ref line 1))) #t)

      ;; Unordered list: -, *, + followed by space
      ((and (>= len 2)
            (or (char=? (string-ref line 0) #\-)
                (char=? (string-ref line 0) #\*)
                (char=? (string-ref line 0) #\+))
            (char-whitespace? (string-ref line 1))
            ;; Check it's not a horizontal rule (--- or ***)
            (not (and (>= len 3)
                      (or (string=? (substring line 0 3) "---")
                          (string=? (substring line 0 3) "***"))))) #t)

      ;; Ordered list: number followed by . and space
      ((and (>= len 3)
            (char-numeric? (string-ref line 0))
            (char=? (string-ref line 1) #\.)
            (char-whitespace? (string-ref line 2))) #t)

      ;; Table row: starts with | and contains at least one more |
      ((and (>= len 2)
            (char=? (string-ref line 0) #\|)
            (> (string-count-char line #\|) 1)) #t)

      ;; Horizontal rule: ---, ===, *** (at least 3 of the same character)
      ((and (>= len 3)
            (let ((first-char (string-ref line 0)))
              (and (or (char=? first-char #\-)
                       (char=? first-char #\=)
                       (char=? first-char #\*))
                   (string=? (make-string 3 first-char) (substring line 0 3))
                   (or (= len 3)
                       (char-whitespace? (string-ref line 3)))))) #t)

      (else #f))))

;; Calculate Markdown symbol density (more intelligent counting)
(define (markdown-symbol-density s)
  (if (string-null? s)
      0
      (let* ((len (string-length s))
             (limit (if (>= len 1000) 1000 len))
             (substr (substring s 0 limit)))
        ;; Count Markdown symbols more intelligently
        (let ((count (+
                      ;; # only counts if followed by space (for headers)
                      (let loop ((i 0) (cnt 0))
                        (if (>= i (- len 1))
                            cnt
                            (if (and (char=? (string-ref substr i) #\#)
                                     (char-whitespace? (string-ref substr (+ i 1))))
                                (loop (+ i 1) (+ cnt 1))
                                (loop (+ i 1) cnt))))
                      ;; * and - count normally (but avoid counting as XML/HTML tags)
                      (string-count-char substr #\*)
                      (string-count-char substr #\-)
                      ;; ` counts for inline code
                      (string-count-char substr #\`)
                      ;; > only counts if at line start followed by space
                      (let loop ((i 0) (cnt 0))
                        (if (>= i (- len 1))
                            cnt
                            (if (and (or (= i 0) (char=? (string-ref substr (- i 1)) #\newline))
                                     (char=? (string-ref substr i) #\>)
                                     (char-whitespace? (string-ref substr (+ i 1))))
                                (loop (+ i 1) (+ cnt 1))
                                (loop (+ i 1) cnt))))
                      ;; | counts for tables
                      (string-count-char substr #\|)
                      ;; ~ counts for code blocks
                      (string-count-char substr #\~)
                      ;; [ and ] for links
                      (string-count-char substr (string-ref "[" 0))
                      (string-count-char substr (string-ref "]" 0))
                      ;; ( and ) only count if they appear in link context: ]( or ![
                      (let loop ((i 0) (cnt 0))
                        (if (>= i (- len 1))
                            cnt
                            (if (and (char=? (string-ref substr i) #\()
                                     (or (and (> i 0) (char=? (string-ref substr (- i 1)) #\]))
                                         (and (> i 1) (string=? (substring substr (- i 2) i) "!["))))
                                (loop (+ i 1) (+ cnt 1))
                                (loop (+ i 1) cnt))))
                      (let loop ((i 0) (cnt 0))
                        (if (>= i (- len 1))
                            cnt
                            (if (and (char=? (string-ref substr i) #\))
                                     (let find-open-paren ((j (- i 1)))
                                       (if (< j 0)
                                           #f
                                           (if (char=? (string-ref substr j) #\() 
                                               (let find-bracket ((k (- j 1)))
                                                 (if (< k 0)
                                                     #f
                                                     (or (char=? (string-ref substr k) #\])
                                                         (and (> k 0) (string=? (substring substr (- k 1) (+ k 1)) "![")))))
                                               (find-open-paren (- j 1))))))
                                (loop (+ i 1) (+ cnt 1))
                                (loop (+ i 1) cnt))))
                      ;; ! for images
                      (string-count-char substr #\!))))
          (/ count len)))))

;; Calculate line-start feature density
(define (markdown-line-start-density s)
  (let ((lines (string-split-lines s)))
    (if (null? lines)
        0
        (let loop ((remaining lines)
                   (count 0)
                   (total 0))
          (if (null? remaining)
              (if (> total 0) (/ count total) 0)
              (let ((line (car remaining)))
                (loop (cdr remaining)
                      (if (markdown-line-start? line) (+ count 1) count)
                      (+ total 1))))))))

;; Calculate lines containing Markdown features (more intelligent)
(define (markdown-feature-line-density s)
  (let ((lines (string-split-lines s)))
    (if (null? lines)
        0
        (let loop ((remaining lines)
                   (count 0)
                   (total 0))
          (if (null? remaining)
              (if (> total 0) (/ count total) 0)
              (let ((line (car remaining)))
                (loop (cdr remaining)
                      (if (or (markdown-line-start? line)
                              ;; Bold and italic
                              (> (string-count-substring line "**") 0)
                              (> (string-count-substring line "__") 0)
                              ;; Inline code
                              (> (string-count-substring line "`") 0)
                              ;; Links: [text] or [text](url) or ![alt](url)
                              (let ((open-bracket (string-count-substring line "["))
                                    (close-bracket (string-count-substring line "]"))
                                    (open-paren (string-count-substring line "("))
                                    (close-paren (string-count-substring line ")")))
                                (or (> (string-count-substring line "](") 0)
                                    (> (string-count-substring line "![") 0)
                                    (and (> open-bracket 0) (> close-bracket 0)
                                         (> open-paren 0) (> close-paren 0))))
                              ;; Italic with * or _ (but avoid false positives)
                              (and (> (string-count-substring line "*") 0)
                                   (= (string-count-substring line "**") 0))  ; single * for italic, not **
                              (and (> (string-count-substring line "_") 0)
                                   (= (string-count-substring line "__") 0)))  ; single _ for italic, not __
                          (+ count 1)
                          count)
                      (+ total 1))))))))

;; ============================================================================
;; Main detection functions (modeled after LaTeX detection)
;; ============================================================================

(define (determine-short-markdown-string s)
  (let* ((len (string-length s)))
    (cond
      ;; Very short strings with clear Markdown structure
      ((markdown-line-start? s) #t)
      ;; Check for inline Markdown features in short strings
      ((or (> (string-count-substring s "**") 0)
           (> (string-count-substring s "__") 0)
           (> (string-count-substring s "*") 0)   ; italic
           (> (string-count-substring s "_") 0)   ; italic
           (> (string-count-substring s "`") 0)   ; inline code
           (> (string-count-substring s "![") 0)  ; image
           (> (string-count-substring s "](") 0)  ; link
           ;; More flexible link detection: [text] followed by (url) anywhere
           (let loop ((i 0))
             (if (>= i (- len 1))
                 #f
                 (if (and (char=? (string-ref s i) #\[)
                          (let find-bracket ((j (+ i 1)))
                            (if (>= j len)
                                #f
                                (if (char=? (string-ref s j) #\])
                                    (let find-paren ((k (+ j 1)))
                                      (if (>= k len)
                                          #f
                                          (if (char=? (string-ref s k) #\()
                                              #t
                                              (find-paren (+ k 1)))))
                                    (find-bracket (+ j 1))))))
                     #t
                     (loop (+ i 1)))))
           ;; Check for balanced brackets and parentheses
           (and (> (string-count-substring s "[") 0)
                (> (string-count-substring s "]") 0)
                (> (string-count-substring s "(") 0)
                (> (string-count-substring s ")") 0)))
       #t)
      ;; Short strings with high Markdown symbol density
      ((>= (markdown-symbol-density s) 0.03) #t)  ; Lower threshold for short strings
      (else #f))))

(define (is-short-markdown-string? s)
  (if (<= (string-length s) 100)
      (determine-short-markdown-string s)
      #f))

(define (is-markdown-string? s)
  (let* ((lines (string-split-lines s))
         (line-count (length lines))
         (line-start-density (markdown-line-start-density s))
         (symbol-density (markdown-symbol-density s))
         (feature-line-density (markdown-feature-line-density s)))

    ;; Special handling for single-line texts with rich inline Markdown
    (if (= line-count 1)
        ;; For single line: check if it has significant Markdown features
        (if (or (>= symbol-density 0.05)  ;; Higher symbol density threshold for single line
                (and (>= symbol-density 0.02)
                     (or (> (string-count-substring s "**") 0)
                         (> (string-count-substring s "__") 0)
                         (> (string-count-substring s "](") 0)
                         (> (string-count-substring s "![") 0))))
            #t
            #f)
        ;; For multi-line texts: use original statistical detection
        (if (>= line-start-density 0.15)  ;; At least 15% of lines start with Markdown features
            (if (and (>= symbol-density 0.02)   ;; At least 2% Markdown symbols
                     (<= symbol-density 0.20))  ;; But not more than 20% (avoid code/other formats)
                (if (>= feature-line-density 0.30)  ;; At least 30% of lines contain Markdown features
                    #t
                    #f)
                #f)
            #f))))

(define (markdown-recognizes-at? s pos)
  ;; Skip leading spaces
  (let loop ((i pos))
    (if (and (< i (string-length s)) (char-whitespace? (string-ref s i)))
        (loop (+ i 1))
        (set! pos i)))
  (cond
    ;; Check for Markdown line-start features (check first 100 chars)
    ((markdown-line-start? (substring s pos (min (string-length s) (+ pos 100)))) #t)
    ;; Check for short strings
    ((is-short-markdown-string? s) #t)
    ;; Check for long strings with statistical detection
    ((is-markdown-string? s) #t)
    (else #f)))

(define (markdown-recognizes? s)
  (and (string? s) (markdown-recognizes-at? s 0)))

;; ============================================================================
;; Test function
;; ============================================================================

(define (test-markdown-format-determine)

  ;; Should be detected as Markdown
  (check (markdown-recognizes-at? md-text1 0) => #t)
  (check (markdown-recognizes-at? md-text2 0) => #t)
  (check (markdown-recognizes-at? md-text3 0) => #t)
  (check (markdown-recognizes-at? md-text4 0) => #t)
  (check (markdown-recognizes-at? md-text5 0) => #t)
  (check (markdown-recognizes-at? md-text6 0) => #t)
  (check (markdown-recognizes-at? md-text7 0) => #t)
  (check (markdown-recognizes-at? md-text8 0) => #t)
  (check (markdown-recognizes-at? md-text9 0) => #t)
  (check (markdown-recognizes-at? md-text10 0) => #t)

  ;; Should NOT be detected as Markdown
  (check (markdown-recognizes-at? non-md-text1 0) => #f)
  (check (markdown-recognizes-at? non-md-text2 0) => #f)
  (check (markdown-recognizes-at? non-md-text3 0) => #f)
  (check (markdown-recognizes-at? non-md-text4 0) => #f)
  (check (markdown-recognizes-at? non-md-text5 0) => #f)
  (check (markdown-recognizes-at? non-md-text6 0) => #f)
  (check (markdown-recognizes-at? non-md-text7 0) => #f)
  (check (markdown-recognizes-at? non-md-text8 0) => #f)
  (check (markdown-recognizes-at? non-md-text9 0) => #f)
  (check (markdown-recognizes-at? non-md-text10 0) => #f))

(tm-define (test_222_26)
  (test-markdown-format-determine)
  (check-report))