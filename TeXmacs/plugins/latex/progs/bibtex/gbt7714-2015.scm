
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gbt7714-2015.scm
;; DESCRIPTION : GBT 7714-2015 style for BibTeX files
;; COPYRIGHT   : (C) 2025 Yuki Lu
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (bibtex gbt7714-2015)
  (:use (bibtex bib-utils) (bibtex plain)))

(bib-define-style "gbt7714-2015" "gbt7714-2015")

;; GBT 7714-2015 特定格式化函数
;; TODO: 实现正确的中英文作者姓名格式化
;; TODO: 添加文献类型标识符 [J], [M], [C] 等
;; TODO: 根据标准调整标点符号和顺序

;; 检测字符串是否包含中文字符的辅助函数
(define (contains-chinese? str)
  (if (bib-null? str) #f
      (let loop ((i 0) (len (string-length str)))
        (if (>= i len) #f
            (let ((c (string-ref str i)))
              (if (or (and (>= (char->integer c) #x4e00)
                           (<= (char->integer c) #x9fff))
                      (and (>= (char->integer c) #x3400)
                           (<= (char->integer c) #x4dbf))
                      (and (>= (char->integer c) #x20000)
                           (<= (char->integer c) #x2a6df)))
                  #t
                  (loop (+ i 1) len)))))))

;; 文献类型标识符函数
(tm-define (bib-document-type-identifier x type)
  (:mode bib-gbt7714-2015?)
  ;; 根据文献类型和是否有在线访问信息返回标识符
  ;; type: "article", "book", "inproceedings", 等
  ;; 检查是否有url或doi字段来判断是否为在线文献
  (let* ((has-url (not (bib-null? (bib-field x "url"))))
         (has-doi (not (bib-null? (bib-field x "doi"))))
         (online (or has-url has-doi)))
    (cond
      ((equal? type "article") (if online "[J/OL]" "[J]"))
      ((equal? type "book") (if online "[M/OL]" "[M]"))
      ((equal? type "inproceedings") (if online "[C/OL]" "[C]"))
      ((equal? type "phdthesis") (if online "[D/OL]" "[D]"))
      ((equal? type "mastersthesis") (if online "[D/OL]" "[D]"))
      ((equal? type "techreport") (if online "[R/OL]" "[R]"))
      (else ""))))

(tm-define (bib-format-name x)
  (:mode bib-gbt7714-2015?)
  ;; 西文作者：姓在前，名缩写（如 "Yu H B"）
  ;; 中文作者：姓在前，名在后（全称）
  (let* ((first-name-raw (if (bib-null? (list-ref x 1)) "" (list-ref x 1)))
         (last-name-raw (if (bib-null? (list-ref x 3)) "" (list-ref x 3)))
         (chinese? (or (contains-chinese? first-name-raw) (contains-chinese? last-name-raw)))
         (first-name (if chinese?
                         first-name-raw  ;; 中文名不缩写
                         (if (bib-null? first-name-raw) "" (bib-abbreviate first-name-raw "." " "))))
         (last-name (if (bib-null? last-name-raw) "" (bib-purify last-name-raw))))
    (if (bib-null? last-name)
        first-name
        (if (bib-null? first-name)
            last-name
            `(concat ,last-name " " ,first-name)))))

(tm-define (bib-format-editor x)
  (:mode bib-gbt7714-2015?)
  ;; 对于 GBT 7714-2015，编者后应跟随 ", 主编" 或 ", 编"
  (let* ((a (bib-field x "editor")))
    (if (or (bib-null? a) (nlist? a))
        ""
        (if (equal? (length a) 2)
            `(concat ,(bib-format-names a) ,(bib-translate ", editor"))
            `(concat ,(bib-format-names a) ,(bib-translate ", editors"))))))

(tm-define (bib-format-date x)
  (:mode bib-gbt7714-2015?)
  ;; GBT 7714-2015 仅使用年份，不含月份
  (let* ((y (bib-field x "year")))
    (if (bib-null? y) "" y)))

(tm-define (bib-format-vol-num-pages x)
  (:mode bib-gbt7714-2015?)
  ;; GBT 7714-2015 格式：年份,卷(期):页码
  (let* ((y (bib-field x "year"))
         (v (bib-field x "volume"))
         (n (bib-field x "number"))
         (p (bib-field x "pages"))
         (year (if (bib-null? y) "" y))
         (vol (if (bib-null? v) "" v))
         (num (if (bib-null? n) "" `(concat "(" ,n ")")))
         (pag (if (or (bib-null? p) (nlist? p))
                  ""
                  (cond
                    ((equal? 1 (length p)) "")
                    ((equal? 2 (length p)) `(concat ":" ,(list-ref p 1)))
                    (else
                     `(concat ":" ,(list-ref p 1)
                              ,bib-range-symbol ,(list-ref p 2)))))))
    (if (and (== vol "") (== num "") (== pag ""))
        year  ;; 只有年份，没有卷期页码
        (if (bib-null? year)
            `(concat ,vol ,num ,pag)  ;; 没有年份，只有卷期页码
            `(concat ,year "," ,vol ,num ,pag)))))  ;; 年份,卷(期):页码

(tm-define (bib-format-bibitem n x)
  (:mode bib-gbt7714-2015?)
  ;; 使用数字标签，如 [1], [2], ...
  `(bibitem* ,(number->string n)))

;; 重写文章格式以添加文献类型标识符 [J]
(tm-define (bib-format-article n x)
  (:mode bib-gbt7714-2015?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block (bib-format-author x))
         ,(bib-new-block
           `(concat ,(bib-format-field-Locase x "title")
                    " "
                    ,(bib-document-type-identifier x "article")))
         ,(bib-new-block
           (if (bib-empty? x "crossref")
               (bib-new-sentence
                `(,(bib-format-field x "journal")
                  ,(bib-format-vol-num-pages x)))
               (bib-new-sentence
                `((concat ,(bib-translate "in ")
                          (cite ,(bib-field x "crossref")))
                  ,(bib-format-pages x)))))
         ,(bib-new-block (bib-format-field x "note"))))))

;; 重写图书格式以添加文献类型标识符 [M]
(tm-define (bib-format-book n x)
  (:mode bib-gbt7714-2015?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block
           (if (bib-empty? x "author")
               (bib-format-editor x)
               (bib-format-author x)))
         ,(bib-new-block
           (bib-new-sentence
            `((concat ,(bib-format-field x "title")
                      " "
                      ,(bib-document-type-identifier x "book"))
              ,(bib-format-bvolume x))))
         ,(bib-new-block
           (if (bib-empty? x "crossref")
               (bib-new-list-spc
                `(,(bib-new-sentence
                    `(,(bib-format-number-series x)))
                  ,(bib-new-sentence
                    `(,(bib-format-field x "publisher")
                      ,(bib-format-field x "address")
                      ,(if (bib-empty? x "edition") ""
                           `(concat ,(bib-format-field x "edition")
                                    ,(bib-translate " edition")))
                      ,(bib-format-date x)))))
               (bib-new-sentence
                `((concat ,(bib-translate "in ")
                          (cite ,(bib-field x "crossref")))
                  ,(bib-format-field x "edition")
                  ,(bib-format-date x)))))
         ,(bib-new-block (bib-format-field x "note"))))))

;; 根据需要添加其他文献类型标识符：
;; [C] 用于会议论文集，[D] 用于学位论文，[R] 用于报告等。

;; 重写 booktitle 格式化函数（符合 GBT 7714-2015 标准）
(tm-define (bib-format-in-ed-booktitle x)
  (:mode bib-gbt7714-2015?)
  (let* ((b (bib-default-field x "booktitle"))
         (e (bib-field x "editor")))
    (if (bib-null? b)
        ""
        (if (bib-null? e)
            `(concat ,(bib-translate "in ") ,b)
            `(concat ,(bib-translate "in ") ,(bib-format-editor x) ", " ,b)))))

;; 重写会议论文格式以添加文献类型标识符 [C]
(tm-define (bib-format-inproceedings n x)
  (:mode bib-gbt7714-2015?)
  `(concat
    ,(bib-format-bibitem n x)
    ,(bib-label (list-ref x 2))
    ,(bib-new-list-spc
      `(,(bib-new-block (bib-format-author x))
        ,(bib-new-block
          `(concat ,(bib-format-field-Locase x "title")
                   " "
                   ,(bib-document-type-identifier x "inproceedings")))
        ,(bib-new-block
          (if (bib-empty? x "crossref")
              (bib-new-list-spc
               `(,(bib-new-sentence
                   `(,(bib-format-in-ed-booktitle x)
                     ,(bib-format-bvolume x)
                     ,(bib-format-number-series x)
                     ,(bib-format-pages x)))
                 ,(if (bib-empty? x "address")
                      (bib-new-sentence
                       `(,(bib-format-field x "organization")
                         ,(bib-format-field x "publisher")
                         ,(bib-format-date x)))
                      (bib-new-list-spc
                       `(,(bib-new-sentence
                           `(,(bib-format-field x "address")
                             ,(bib-format-date x)))
                         ,(bib-new-sentence
                           `(,(bib-format-field x "organization")
                             ,(bib-format-field x "publisher"))))))))
              (bib-new-sentence
               `((concat ,(bib-translate "in ")
                         (cite ,(bib-field x "crossref")))
                 ,(bib-format-pages x)))))
        ,(bib-new-block (bib-format-field x "note"))))))

;; 检测作者列表是否包含中文作者
(define (authors-contain-chinese? a)
  (if (or (bib-null? a) (nlist? a)) #f
      (let loop ((i 1) (n (length a)))
        (if (>= i n) #f
            (let* ((author (list-ref a i))
                   (last-name (if (>= (length author) 4) (list-ref author 3) ""))
                   (first-name (if (>= (length author) 2) (list-ref author 1) "")))
              (if (or (contains-chinese? last-name) (contains-chinese? first-name))
                  #t
                  (loop (+ i 1) n)))))))

;; 重写作者列表格式化函数以符合 GBT 7714-2015 标准
(tm-define (bib-format-names a)
  (:mode bib-gbt7714-2015?)
  (if (or (bib-null? a) (nlist? a))
      ""
      (let* ((n (length a))
             (chinese? (authors-contain-chinese? a))
             ;; GBT 7714-2015: 最多显示3个作者
             (max-authors 3)
             (author-count (- n 1))  ;; 减去第0个元素
             (show-count (min author-count max-authors)))
        (cond
          ((equal? author-count 1)  ;; 只有1个作者
           (bib-format-name (list-ref a 1)))
          (else
           (let* ((first (bib-format-name (list-ref a 1)))
                  ;; 收集中间作者（第2到第show-count个）
                  (middle (let loop ((i 2) (result ""))
                            (if (or (>= i n) (>= i (+ show-count 1)))  ;; 限制显示数量
                                result
                                (loop (+ i 1)
                                      (if (equal? result "")
                                          (bib-format-name (list-ref a i))
                                          `(concat ,result ", " ,(bib-format-name (list-ref a i))))))))
                  (has-more (> author-count show-count))
                  (last-part (if has-more
                                 (if chinese? "等" "et al.")
                                 (if (>= author-count 2)
                                     (bib-format-name (list-ref a (- n 1)))
                                     "")))
                  ;; 分隔符：中文用逗号，英文用"and"（最后一个作者前）
                  (separator (if has-more
                                 ","  ; "等"或"et al."前用逗号
                                 (if chinese? "," " and "))))  ;; 中文用逗号，英文用"and"
             (cond
               ((and (equal? middle "") (equal? last-part "")) first)
               ((equal? middle "") `(concat ,first ,separator ,last-part))
               ((equal? last-part "") `(concat ,first ", " ,middle))
               (else `(concat ,first ", " ,middle ,separator ,last-part)))))))))