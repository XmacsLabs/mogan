
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

;; 子字符串检查函数
(define (substring? pat text)
  (let ((pat-len (string-length pat))
        (text-len (string-length text)))
    (let outer ((i 0))
      (cond
        ((> (+ i pat-len) text-len) #f)
        (else
         (let inner ((j 0))
           (cond
             ((>= j pat-len) #t)
             ((char=? (string-ref pat j)
                      (string-ref text (+ i j)))
              (inner (+ j 1)))
             (else (outer (+ i 1))))))))))

;; 检测字符串是否包含中文字符的辅助函数
(define (contains-chinese? str)
  (if (bib-null? str) #f
      (let ((s (if (string? str) str "")))
        ;; 检查是否包含中文 Unicode 字符的内部表示模式
        (or (substring? "<#4" s)
            (substring? "<#5" s)
            (substring? "<#6" s)
            (substring? "<#7" s)
            (substring? "<#8" s)
            (substring? "<#9" s)
            ;; 检查实际的中文字符（基本中文范围 U+4E00-U+9FFF）
            (let loop ((i 0) (len (string-length s)))
              (if (>= i len) #f
                  (let* ((c (string-ref s i))
                         (code (char->integer c)))
                    (if (and (>= code #x4e00) (<= code #x9fff))
                        #t
                        (loop (+ i 1) len)))))))))

;; 检测作者列表是否包含中文作者
(define (authors-contain-chinese? a)
  (if (or (bib-null? a) (nlist? a)) #f
      (let loop ((i 1) (n (length a)))
        (if (>= i n) #f
            (let* ((author (list-ref a i))
                   ;; 将作者的所有字符串部分连接成一个字符串，然后检查是否包含中文
                   (author-str (cond
                                 ((string? author) author)
                                 ((list? author)
                                  (let part-loop ((j 1) (m (length author)) (result ""))
                                    (if (>= j m) result
                                        (let ((part (list-ref author j)))
                                          (part-loop (+ j 1) m
                                                     (if (string? part)
                                                         (string-append result part)
                                                         result))))))
                                 (else ""))))
              (if (contains-chinese? author-str)
                  #t
                  (loop (+ i 1) n)))))))

;; 作者列表格式
(tm-define (bib-format-names a)
  (:mode bib-gbt7714-2015?)
  (if (or (bib-null? a) (nlist? a))
      ""
      (let* ((n (length a))
             (chinese? (authors-contain-chinese? a))
             ;; GBT 7714-2015: 最多显示3个作者
             (max-authors 3)
             (author-count (- n 1))
             (show-count (min author-count max-authors))
             ;; 逗号分隔符：中文不加空格，英文加空格
             (comma-sep (if chinese? "," ", ")))
        (cond
          ((equal? author-count 1)
           (bib-format-name (list-ref a 1)))
          (else
           (let* ((first (bib-format-name (list-ref a 1)))
                  (has-more (> author-count max-authors))
                  ;; 收集中间作者
                  ;; 如果有更多作者（>3）：收集第2到第3个作者（共2个中间作者）
                  ;; 如果没有更多作者（<=3）：收集第2到第author-count-1个作者
                  (middle (let loop ((i 2) (result ""))
                            (if (or (>= i n)
                                    (if has-more
                                        (> i max-authors)
                                        (>= i author-count)))
                                result
                                (loop (+ i 1)
                                      (if (equal? result "")
                                          (bib-format-name (list-ref a i))
                                          `(concat ,result ,comma-sep ,(bib-format-name (list-ref a i))))))))
                  (last-part (if has-more
                                 (if chinese? "<#7b49>" "et al") ; Unicode U+7B49 "等"
                                 (if (>= author-count 2)
                                     (bib-format-name (list-ref a (- n 1)))
                                     "")))
                  ;; 分隔符：无论是否有更多作者，都使用逗号分隔符
                  ;; 中文：作者1,作者2,作者3,等
                  ;; 英文：Author1, Author2, Author3, et al
                  (separator comma-sep))
             (cond
               ((and (equal? middle "") (equal? last-part "")) first)
               ((equal? middle "") `(concat ,first ,separator ,last-part))
               ((equal? last-part "") `(concat ,first ,comma-sep ,middle))
               (else `(concat ,first ,comma-sep ,middle ,separator ,last-part)))))))))


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
      ((equal? type "article") (if online "[J/OL]" "[J]"))         ;; 期刊
      ((equal? type "book") (if online "[M/OL]" "[M]"))            ;; 普通图书
      ((equal? type "inproceedings") (if online "[C/OL]" "[C]"))   ;; 会议录
      ((equal? type "phdthesis") (if online "[D/OL]" "[D]"))       ;; 学位论文-博士
      ((equal? type "mastersthesis") (if online "[D/OL]" "[D]"))   ;; 学位论文-硕士
      ((equal? type "techreport") (if online "[R/OL]" "[R]"))      ;; 报告
      ((equal? type "misc") (if online "[EB/OL]" "[EB]"))          ;; 电子公告
      ((equal? type "collection") (if online "[G/OL]" "[G]"))      ;; 汇编
      ((equal? type "proceedings") (if online "[C/OL]" "[C]"))     ;; 会议录（同inproceedings）
      ((equal? type "manual") (if online "[S/OL]" "[S]"))          ;; 标准（常用manual表示标准）
      ((equal? type "standard") (if online "[S/OL]" "[S]"))        ;; 标准
      ((equal? type "patent") (if online "[P/OL]" "[P]"))          ;; 专利
      ((equal? type "database") (if online "[DB/OL]" "[DB]"))      ;; 数据库
      ((equal? type "software") (if online "[CP/OL]" "[CP]"))      ;; 计算机程序
      ((equal? type "program") (if online "[CP/OL]" "[CP]"))       ;; 计算机程序
      ((equal? type "electronic") (if online "[EB/OL]" "[EB]"))    ;; 电子公告
      ((equal? type "archive") (if online "[A/OL]" "[A]"))         ;; 档案
      ((equal? type "map") (if online "[CM/OL]" "[CM]"))           ;; 舆图
      ((equal? type "dataset") (if online "[DS/OL]" "[DS]"))       ;; 数据集
      ((equal? type "newspaper") (if online "[N/OL]" "[N]"))       ;; 报纸
      ((equal? type "other") (if online "[Z/OL]" "[Z]"))           ;; 其他
      (else ""))))

;; 地址:机构格式
(tm-define (bib-format-address-institution x)
  (:mode bib-gbt7714-2015?)
  (let* ((addr (bib-field x "address"))
         (inst-field (cond
                       ((not (bib-empty? x "school")) (bib-field x "school"))
                       ((not (bib-empty? x "organization")) (bib-field x "organization"))
                       ((not (bib-empty? x "publisher")) (bib-field x "publisher"))
                       ((not (bib-empty? x "institution")) (bib-field x "institution"))
                       (else "")))
         (addr-val (if (bib-empty? x "address") "" addr))
         (inst-val (if (or (bib-null? inst-field) (equal? inst-field "")) "" inst-field)))
    (cond
      ((and (not (equal? addr-val "")) (not (equal? inst-val "")))
       `(concat ,addr-val ": " ,inst-val))
      ((not (equal? addr-val "")) addr-val)
      ((not (equal? inst-val "")) inst-val)
      (else ""))))

;; 作者姓名格式
(tm-define (bib-format-name x)
  (:mode bib-gbt7714-2015?)
  ;; 西文作者：姓在前，名缩写（如 "Yu H B"）
  ;; 中文作者：姓在前，名在后（全称）
  (let* ((first-name-raw (if (bib-null? (list-ref x 1)) "" (list-ref x 1)))
         (last-name-raw (if (bib-null? (list-ref x 3)) "" (list-ref x 3)))
         (chinese? (or (contains-chinese? first-name-raw) (contains-chinese? last-name-raw)))
         (first-name (if chinese?
                         first-name-raw  ;; 中文名不缩写
                         (if (bib-null? first-name-raw) "" (bib-abbreviate first-name-raw "" " "))))
         (last-name (if (bib-null? last-name-raw) "" (bib-purify last-name-raw))))
    (if (bib-null? last-name)
        first-name
        (if (bib-null? first-name)
            last-name
            `(concat ,last-name " " ,first-name)))))

;; 日期格式
(tm-define (bib-format-date x)
  (:mode bib-gbt7714-2015?)
  ;; 优先使用 date 字段；若没有则回退到 year；返回空字符串表示没有可用的日期信息
  (let* ((d (bib-field x "date"))
         (y (bib-field x "year"))
         (date-str (if (bib-null? d) y d)))
    (if (bib-null? date-str)
        ""
        `(concat "(" ,date-str ")"))))

;; 书名格式
(tm-define (bib-format-in-ed-booktitle x)
  (:mode bib-gbt7714-2015?)
  (let* ((b (bib-default-field x "booktitle"))
         (e (bib-field x "editor")))
    (if (bib-null? b)
        ""
        `(concat ,(bib-translate "in ") ,b))))

;; 年份,卷(期):页码格式
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
                    ((equal? 2 (length p)) `(concat ": " ,(list-ref p 1)))
                    (else
                     `(concat ": " ,(list-ref p 1)
                              ,bib-range-symbol ,(list-ref p 2)))))))
    (if (and (== vol "") (== num "") (== pag ""))
        year
        (if (bib-null? year)
            `(concat ,vol ,num ,pag)
            `(concat ,year ", " ,vol ,num ,pag)))))

;; 数字标签格式
(tm-define (bib-format-bibitem n x)
  (:mode bib-gbt7714-2015?)
  ;; 使用数字标签，如 [1], [2], ...
  `(bibitem* ,(number->string n)))

;; 卷-册格式
(tm-define (bib-format-bvolume x)
  (:mode bib-gbt7714-2015?)
  (let* ((v (bib-field x "volume"))
	 (s (bib-default-field x "series")))
    (if (bib-null? v)
	""
	(let ((series (if (bib-null? s) ""
			  `(concat ,(bib-translate " of ") ,s)))
	      (sep (if (< (bib-text-length v) 3) `(nbsp) " ")))
	  `(concat ,(bib-translate "volume") ,sep ,v ,series)))))

;; URL/DOI 信息格式
(tm-define (bib-format-url-doi x)
  (:mode bib-gbt7714-2015?)
  (let* ((url (bib-field x "url"))
         (doi (bib-field x "doi"))
         (urldate (bib-field x "urldate"))
         (has-url (not (bib-null? url)))
         (has-doi (not (bib-null? doi)))
         (has-urldate (not (bib-null? urldate))))
    (cond
      ((and has-url has-doi)
       ;; 同时有 URL 和 DOI：引用日期放在 URL 前，DOI 在 URL 后
       (if has-urldate
           `(concat "[" ,urldate "]. " ,url ". DOI: " ,doi)
           `(concat ,url ". DOI: " ,doi)))
      (has-url
       ;; 只有 URL
       (if has-urldate
           `(concat "[" ,urldate "]. " ,url)
           `(concat ,url)))
      (has-doi
       ;; 只有 DOI
       (if has-urldate
           `(concat "[" ,urldate "]. DOI: " ,doi)
           `(concat "DOI: " ,doi)))
      (else ""))))

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
         ,(bib-new-block (bib-format-url-doi x))))))

;; 重写图书格式以添加文献类型标识符 [M]
(tm-define (bib-format-book n x)
  (:mode bib-gbt7714-2015?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block
           (bib-format-author x))
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
                    `(,(bib-format-address-institution x)
                      ,(if (bib-empty? x "edition") ""
                           `(concat ,(bib-format-field x "edition")
                                    ,(bib-translate " edition")))
                      ,(bib-format-date x)))))
               (bib-new-sentence
                `((concat ,(bib-translate "in ")
                          (cite ,(bib-field x "crossref")))
                  ,(bib-format-field x "edition")
                  ,(bib-format-date x)))))
         ,(bib-new-block (bib-format-url-doi x))))))

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
                       `(,(bib-format-address-institution x)
                         ,(bib-format-date x))))))
              (bib-new-sentence
               `((concat ,(bib-translate "in ")
                         (cite ,(bib-field x "crossref")))
                 ,(bib-format-pages x)))))
        ,(bib-new-block (bib-format-url-doi x))))))

;; 重写会议录格式以添加文献类型标识符 [C]
(tm-define (bib-format-proceedings n x)
  (:mode bib-gbt7714-2015?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block (bib-format-field x "editor"))
         ,(bib-new-block
           `(concat ,(bib-format-field-Locase x "title")
                    " "
                    ,(bib-document-type-identifier x "proceedings")))
         ,(bib-new-block
           (bib-new-sentence
            `(,(bib-format-address-institution x)
              ,(bib-format-date x))))
         ,(bib-new-block (bib-format-url-doi x))))))

;; 重写手册格式以添加文献类型标识符 [S]
(tm-define (bib-format-manual n x)
  (:mode bib-gbt7714-2015?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block
           (if (bib-empty? x "author")
               (bib-new-sentence `(,(bib-format-address-institution x)))
               (bib-format-author x)))
         ,(bib-new-block
           `(concat ,(bib-format-field-Locase x "title")
                    " "
                    ,(bib-document-type-identifier x "manual")))
         ,(bib-new-block
           (bib-new-sentence
            `(,(bib-format-address-institution x))))
         ,(bib-new-block (bib-format-url-doi x))))))

;; 重写杂项文献格式以添加文献类型标识符 [EB]
(tm-define (bib-format-misc n x)
  (:mode bib-gbt7714-2015?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block (bib-format-author x))
         ,(bib-new-block
           `(concat ,(bib-format-field-Locase x "title")
                    " "
                    ,(bib-document-type-identifier x "misc")))
         ,(bib-new-case-preserved-block
           (bib-new-case-preserved-sentence
            `(,(bib-format-field-preserve-case x "howpublished")
              ,(bib-format-date x))))
         ,(bib-new-block (bib-format-url-doi x))))))

;; 重写博士论文格式以添加文献类型标识符 [D]
(tm-define (bib-format-phdthesis n x)
  (:mode bib-gbt7714-2015?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block (bib-format-author x))
         ,(bib-new-block
           `(concat ,(bib-format-field-Locase x "title")
                    " "
                    ,(bib-document-type-identifier x "phdthesis")))
         ,(bib-new-block
           (bib-new-sentence
            `(,(if (bib-empty? x "type")
                   (bib-format-field-Locase x "type"))
              ,(bib-format-address-institution x)
              ,(bib-format-date x))))
         ,(bib-new-block (bib-format-url-doi x))))))

;; 重写硕士论文格式以添加文献类型标识符 [D]
(tm-define (bib-format-mastersthesis n x)
  (:mode bib-gbt7714-2015?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block (bib-format-author x))
         ,(bib-new-block
           `(concat ,(bib-format-field-Locase x "title")
                    " "
                    ,(bib-document-type-identifier x "mastersthesis")))
         ,(bib-new-block
           (bib-new-sentence
            `(,(if (bib-empty? x "type")
                   (bib-format-field-Locase x "type"))
              ,(bib-format-address-institution x)
              ,(bib-format-date x))))
         ,(bib-new-block (bib-format-url-doi x))))))

;; 重写报告格式以添加文献类型标识符 [R]
(tm-define (bib-format-techreport n x)
  (:mode bib-gbt7714-2015?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block (bib-format-author x))
         ,(bib-new-block
           `(concat ,(bib-format-field-Locase x "title")
                    " "
                    ,(bib-document-type-identifier x "techreport")))
         ,(bib-new-block
           (bib-new-sentence
            `(,(bib-format-address-institution x)
              ,(bib-format-date x))))
         ,(bib-new-block (bib-format-url-doi x))))))
