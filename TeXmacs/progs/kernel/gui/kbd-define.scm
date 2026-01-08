
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : kbd-define.scm
;; DESCRIPTION : Definition of keyboard shortcuts/wildcards
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui kbd-define)
  (:use (kernel texmacs tm-define)))

(import (liii hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy keyboard bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lazy-keyboard-waiting '())
(tm-define lazy-keyboard-done (make-ahash-table))

(tm-define (lazy-keyboard-do module mode*)
  (with mode (texmacs-mode-mode mode*)
    (set! lazy-keyboard-waiting (acons mode module lazy-keyboard-waiting))))

(tm-define-macro (lazy-keyboard module . modes)
  (for-each (lambda (mode) (lazy-keyboard-do module mode)) modes)
  `(delayed
     (:idle 250)
     (ahash-set! lazy-keyboard-done ',module #t)
     (module-provide ',module)))

(define lazy-force-all? #f)
(define lazy-force-busy? #f)

(define (lazy-keyboard-force-do l)
  (cond ((null? l) l)
        ((ahash-ref lazy-keyboard-done (cdar l))
         (lazy-keyboard-force-do (cdr l)))
        ((or lazy-force-all? (texmacs-in-mode? (caar l)))
         (module-provide (cdar l))
         (ahash-set! lazy-keyboard-done (cdar l) #t)
         (lazy-keyboard-force-do (cdr l)))
        (else (cons (car l) (lazy-keyboard-force-do (cdr l))))))

(tm-define (lazy-keyboard-force . opt)
  (set! lazy-force-all? (or lazy-force-all? (nnull? opt)))
  (when (not lazy-force-busy?)
    (set! lazy-force-busy? #t)
    (let* ((l1 (reverse lazy-keyboard-waiting))
           (l2 (lazy-keyboard-force-do l1)))
      (set! lazy-keyboard-waiting (reverse l2))
      (set! lazy-force-busy? #f)
      (when (null? lazy-keyboard-waiting)
        (set! lazy-force-all? #f))
      (when (and lazy-force-all? (nnull? lazy-keyboard-waiting))
        (lazy-keyboard-force #t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of keyboard wildcards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kbd-wildcards-sub l post)
  (if (nnull? l)
      (let* ((w (car l))
             (key (car w))
             (im (cadr w))
             (left (if (>= (length w) 3) (caddr w) #f))
             (right (if (>= (length w) 4) (cadddr w) #t)))
        (insert-kbd-wildcard key im post left right)
        (kbd-wildcards-sub (cdr l) post))))

(tm-define (kbd-wildcards-body l)
  (:synopsis "Helper routine for kbd-wildcards macro")
  (cond ((null? l) (noop))
        ((== (car l) 'pre) (kbd-wildcards-sub (cdr l) #f))
        ((== (car l) 'post) (kbd-wildcards-sub (cdr l) #t))
        (else (kbd-wildcards-sub l #t))))

(tm-define-macro (kbd-wildcards . l)
  (:synopsis "Add entries in @l to the keyboard wildcard table")
  `(kbd-wildcards-body ,(list 'quasiquote l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for the definition of keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define kbd-map-table (make-ahash-table))
(define kbd-inv-table (make-ahash-table))
(define kbd-rev-table (make-ahash-table))
(define (kbd-set-map! key im) (ahash-set! kbd-map-table key im))
(define (kbd-set-inv! key im) (ahash-set! kbd-inv-table key im))
(define (kbd-set-rev! key im) (ahash-set! kbd-rev-table key im))
(define (kbd-get-map key) (ahash-ref kbd-map-table key))
(define (kbd-get-inv key) (ahash-ref kbd-inv-table key))
(tm-define (kbd-get-rev key) (ahash-ref kbd-rev-table key))
(define (kbd-remove-map! key) (ahash-remove! kbd-map-table key))

(define (kbd-source cmd)
  (if (procedure? cmd) (promise-source cmd) cmd))

(define (simple-insert l x)
  (if (nlist? l) (list x)
      (list-union (list x) l)))

(define (simple-remove l x)
  (if (nlist? l) (list)
      (list-difference l (list x))))

(define (kbd-insert-key-binding conds key im)
  (let* ((com (kbd-source (car im)))
         (cmd (if (string? com) com (object->string com))))
    ;;(display* "Binding '" key "' when " conds " to " com "\n")
    (kbd-delete-key-binding2 conds key)
    (kbd-set-map! key (ctx-insert (kbd-get-map key) im conds))
    (kbd-set-inv! com (ctx-insert (kbd-get-inv com) key conds))
    (kbd-set-rev! cmd (simple-insert (kbd-get-rev cmd) key))
    ;;(display* key " > " (kbd-get-map key) "\n")
    ;;(display* com " < " (kbd-get-inv com) "\n")
    ;;(display* cmd " < " (kbd-get-rev cmd) "\n")
    ))

(tm-define (kbd-delete-key-binding2 conds key)
  ;;(display* "Deleting binding '" key "' when " conds "\n")
  (with im (ctx-find (kbd-get-map key) conds)
    (if im
        (let* ((com (kbd-source (car im)))
               (cmd (object->string com)))
          (kbd-set-map! key (ctx-remove (kbd-get-map key) conds))
          (kbd-set-inv! com (ctx-remove (kbd-get-inv com) conds))
          (kbd-set-rev! cmd (simple-remove (kbd-get-inv cmd) key))))))

(tm-define (kbd-find-key-binding key)
  (:synopsis "Find the command associated to the keystroke @key")
  ;;(display* "Find binding '" key "'\n")
  (lazy-keyboard-force)
  (ctx-resolve (kbd-get-map key) #f))

(tm-define (kbd-find-inv-binding com)
  (:synopsis "Find keyboard binding for command @com")
  ;;(display* "Find inverse binding '" com "'\n")
  (lazy-keyboard-force)
  (with r (ctx-resolve (kbd-get-inv com) #f)
    (if r r "")))

(tm-define (kbd-find-inv-system-binding com)
  (:synopsis "Find system keyboard binding for command @com")
  (and-with b (tm->stree (kbd-system-rewrite (kbd-find-inv-binding com)))
    (when (tm-is? b 'render-key) (set! b (tm-ref b 0)))
    (when (tm-is? b 'with) (set! b (tm-ref b (- (tm-arity b) 1))))
    (and (string? b) b)))

(tm-define (kbd-find-rev-binding cmd)
  (:synopsis "Find modeless keyboard binding for command @cmd")
  ;;(display* "Find reverse binding '" com "'\n")
  (lazy-keyboard-force)
  (cond ((tree? cmd)
         (kbd-find-rev-binding (tree->stree cmd)))
        ((string? cmd)
         (with l (kbd-get-rev (object->string (string->object cmd)))
           (and l (nnull? l) (string? (car l)) (string->tmstring (car l)))))
        (else #f)))

(define (kbd-find-key-binding2 conds key)
  ;;(display* "Find binding '" key "' when " conds "\n")
  ;; FIXME: we really need an ctx-find which does mode inference
  (or (ctx-find (kbd-get-map key) conds)
      (ctx-find (kbd-get-map key) '())))

(tm-define (kbd-base-sequence comb)
  (let loop ((s comb))
    (if (string-ends? s " tab")
        (loop (substring s 0 (- (string-length s) 4)))
        s)))

(tm-define (kbd-find-prefix-tab-inner prefix)
  (let* ((pairs (filter 
                  (lambda (pair)
                    (let* ((key (car pair))
                           (val (cdr pair)))
                      (and (string? key)
                           (string-ends? key " tab")
                           (let* ((base (kbd-base-sequence key))
                                  (resolved (ctx-resolve val #f)))
                             (and (string=? base prefix)
                                  resolved)))))
                  (ahash-table->list kbd-map-table)))
         ;; 将 (key . context-map) 转换为 (key . resolved-value)
         (resolved-pairs (map (lambda (pair)
                                (cons (car pair) (ctx-resolve (cdr pair) #f)))
                              pairs)))
    ; 按 tab 数量升序排序（少的在前）
    (sort resolved-pairs
          (lambda (x y)
            (let ((len-x (string-length (car x)))
                  (len-y (string-length (car y))))
              (< len-x len-y))))))

(define kbd-find-prefix-tab-cache (make-hash-table))

(tm-define (kbd-find-prefix-tab prefix)
  (let ((cached (hash-table-ref/default kbd-find-prefix-tab-cache prefix #f)))
    (if cached
        cached
        (let ((result (kbd-find-prefix-tab-inner prefix)))
          (hash-table-set! kbd-find-prefix-tab-cache prefix result)
          result))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yet more subroutines for the definition of keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kbd-append prefix s1)
  (let* ((s2 (string-replace s1 " " ""))
         (s3 (string-replace s2 "<" "<."))
         (s4 (string-replace s3 ">" "<gtr>"))
         (s5 (string-replace s4 "<." "<less>")))
    (string-append prefix s5)))

(define (kbd-sub-binding conds s prev-end end)
  (let* ((this-ss (substring s 0 end))
         (this (kbd-find-key-binding2 conds this-ss)))
    (if (not this)
        (let* ((prev-ss (substring s 0 prev-end))
               (prev (kbd-find-key-binding2 conds prev-ss)))
          (if (and (list? prev) (= (length prev) 2)) (set! prev (car prev)))
          (if (or (not prev) (nstring? prev)) (set! prev prev-ss))
          (with im (kbd-append prev (substring s prev-end end))
            (kbd-insert-key-binding conds this-ss (list im "")))))))

(define (kbd-sub-bindings-sub conds s prev-end end)
  (cond ((== end (string-length s)) (noop))
        ((== (string-ref s end) #\space)
         (kbd-sub-binding conds s prev-end end)
         (kbd-sub-bindings-sub conds s end (+ end 1)))
        (else (kbd-sub-bindings-sub conds s prev-end (+ end 1)))))

(define (kbd-sub-bindings conds s)
  (kbd-sub-bindings-sub conds s 0 0))

(tm-define (kbd-binding conds key2 cmd help)
  (:synopsis "Helper routine for kbd-map macro")
  ;;(display* conds ", " key2 ", " cmd ", " help "\n")
  (with key (kbd-pre-rewrite key2)
    (kbd-sub-bindings conds key)
    (kbd-insert-key-binding conds key (list cmd help))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kbd-add-condition conds opt)
  ;;(display* "Add condition " opt "\n")
  (cond ((== (car opt) :mode)
         (ctx-add-condition conds 0 (cadr opt)))
        ((== (car opt) :require)
         (ctx-add-condition conds 0 `(lambda () ,(cadr opt))))
        (else (texmacs-error "kbd-add-condition"
                             "Bad keyboard option ~S" opt))))

(define (kbd-map-one conds l)
  (if (not (and (pair? l) (string? (car l)) (pair? (cdr l))))
      (texmacs-error "kbd-map-pre-one" "Bad keymap in: ~S" l))
  (with (key action . opt) l
    (if (string? action)
        (with help (if (null? opt) "" (car opt))
          `(kbd-binding (list ,@conds) ,key ,action ,help))
        `(kbd-binding (list ,@conds) ,key (lambda () ,action ,@opt) ""))))

(define (kbd-map-body conds l)
  (cond ((null? l) '())
        ((symbol? (car l))
         (kbd-map-body (list 0 (car l)) (cdr l)))
        ((and (pair? (car l)) (== (caar l) :profile))
         (if (not (has-look-and-feel? (cdar l))) '((noop))
             (kbd-map-body conds (cdr l))))
        ((and (pair? (car l)) (keyword? (caar l)))
         (kbd-map-body (kbd-add-condition conds (car l)) (cdr l)))
        (else (map (lambda (x) (kbd-map-one conds x)) l))))

(tm-define-macro (kbd-map . l)
  (:synopsis "Add entries in @l to the keyboard mapping")
  `(begin ,@(kbd-map-body '() l)))

(define (kbd-remove-one conds key)
  `(kbd-delete-key-binding2 (list ,@conds) ,key))

(define (kbd-remove-body conds l)
  (cond ((null? l) '())
        ((symbol? (car l))
         (kbd-remove-body (list 0 (car l)) (cdr l)))
        ((and (pair? (car l)) (keyword? (caar l)))
         (kbd-remove-body (kbd-add-condition conds (car l)) (cdr l)))
        (else (map (lambda (x) (kbd-remove-one conds x)) l))))

(tm-define-macro (kbd-unmap . l)
  (:synopsis "Remove entries in @l from keyboard mapping")
  `(begin ,@(kbd-remove-body '() l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of keyboard (backslashed) commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define kbd-command-table (make-ahash-table))

(define (kbd-set-command! key im)
  (ahash-set! kbd-command-table key im))

(tm-define (kbd-get-command key)
  (lazy-keyboard-force)
  (ahash-ref kbd-command-table key))

(tm-define (kbd-command-pre arg)
  (:synopsis "Helper routine for kbd-commands macro")
  (with (cmd help . action) arg
    (list cmd help (list 'unquote `(lambda () ,@action)))))

(tm-define (kbd-command arg)
  (:synopsis "Helper routine for kbd-commands macro")
  (with (cmd help action) arg
    (kbd-set-command! cmd (cons help action))))

(tm-define-macro (kbd-commands . l)
  (:synopsis "Add backslashed commands in @l to keyboard mapping")
  `(for-each kbd-command ,(list 'quasiquote (map kbd-command-pre l))))

(tm-define-macro (kbd-symbols . l)
  (:synopsis "Add symbols in @l to keyboard mapping")
  (define (fun s)
    (list s (string-append "insert#<" s ">")
          (list 'kbd-insert (string-append "<" s ">"))))
  `(kbd-commands ,@(map fun l)))

(tm-define (emulate-keyboard k)
  (delayed (raw-emulate-keyboard k)))

#|
extract-code-str
从过程对象（Procedure）或列表中提取源码对象（S-Expression）。
这是用于调试快捷键绑定的核心辅助函数，能够“透视”匿名 Lambda 函数的内部逻辑，
并返回可供进一步程序处理的原始代码结构。

语法
----
(extract-code-str proc)

参数
----
proc : procedure | any
    需要提取源码的目标对象。通常是 `kbd-get-map` 返回列表中的条件闭包或命令闭包。
    如果是 procedure，尝试获取其源码；如果是普通列表或其他数据，则按原样处理。

返回值
----
any (list | symbol)
- 返回对象源码的原始 Scheme 结构（S-Expression）。
- 如果输入是 `(lambda () (func))`，则返回列表 `(func)`（保留括号结构）。
- 如果输入是 `(lambda () (func arg))`，则返回列表 `(func arg)`。

逻辑
----
1. 源码获取：首先检查输入是否为过程，如果是则调用 `procedure-source` 获取源码列表。
2. Lambda 识别：
   - 检查源码是否为列表 (pair)。
   - 检查第一个元素是否为符号 `lambda`。
   - 检查列表长度是否足够包含函数体 (cddr)。
3. 剥离外壳：
   - 如果确认为 Lambda 结构，直接提取第三个元素 `(caddr src)` 作为函数体代码。
   - 注意：不进行额外的 `car` 拆包，完整保留函数体内的逻辑结构。
|#
(define (extract-code-str proc)
  (let* ((src (if (procedure? proc) (procedure-source proc) proc))
         (code (if (and (pair? src) 
                        (eq? (car src) 'lambda)
                        (pair? (cddr src)))
                   (caddr src)
                   src)))
    code))

#|
get-kbd-bindings
获取指定按键序列的所有绑定详情，并以结构化数据的形式返回。
这是一个高层调试工具，用于查看某个快捷键在不同上下文（条件）下的行为定义。

语法
----
(get-kbd-bindings key-str)

参数
----
key-str : string
    按键序列字符串，例如 "return"、"C-x C-f" 或 "tab"。

返回值
----
list
- 如果按键未定义：返回 `(not-bound)`。
- 如果按键已定义：返回一个列表的列表，格式为：
  `(( (条件代码列表...) 命令代码 "帮助文档" ) ...)`
  示例：`(( ((inside-replace-buffer?)) (replace-one ...) "" ) ...)`
  注意：这里的条件和命令是 Scheme 代码对象（List），而非字符串。

逻辑
----
1. 获取原始映射：调用 `kbd-get-map` 获取按键对应的原始关联列表 (Association List)。
2. 空值检查：如果 `kbd-get-map` 返回 #f，则返回 `(not-bound)`。
3. 遍历处理：使用 `map` 遍历原始列表中的每一项绑定：
   - 条件处理：原始数据的 car 部分是条件闭包列表，对其中每个元素调用 `extract-code-str` 获取源码对象。
   - 命令处理：原始数据的 cadr 部分是命令闭包，对其调用 `extract-code-str` 获取源码对象。
   - 帮助文档：原始数据的 caddr 部分是字符串，保持原样。
4. 结果组装：将处理后的条件代码列表、命令代码对象和帮助文档重新组装成一个新的列表返回。

注意
----
1. 本函数使用 `tm-define` 定义，使其在 Mogan/TeXmacs 的模块系统中可见。
2. 返回结果中的条件部分始终是一个列表（如 `((cond1) (cond2))`），
   因为 Mogan 允许一个快捷键绑定同时依赖多个条件（逻辑与关系）。
|#
(tm-define (get-kbd-bindings key-str)
  (let ((raw-map (kbd-get-map key-str)))
    (if (not raw-map)
        (list 'not-bound)
        (map (lambda (item)
               (let ((conds (car item))
                     (cmd   (cadr item))
                     (help  (caddr item)))
                 (list 
                   (map extract-code-str conds)
                   (extract-code-str cmd)
                   help)))
             raw-map))))