
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : comment-widgets.scm
;; DESCRIPTION : special widgets for editing comments
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (various comment-widgets)
  (:use (various comment-edit)
        (utils library cursor)
        (generic document-style)
        (generic generic-edit)
        (kernel gui menu-widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing a simple comment in a separate widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define comment-quit-command ignore)
(define comment-window-table (make-ahash-table))
(define comment-text "comment")


;; 设置comment编辑器窗口状态
(tm-define (set-comment-window-state opened?)
  (set-auxiliary-widget-state opened? 'comment-editor))

;; 获取当前窗口的comment编辑器状态
(tm-define (get-comment-window-state)
  (let ((state (get-auxiliary-widget-state)))
    (if state
        (let ((opened? (car state))
              (widget-type (cadr state)))
          (and (== widget-type 'comment-editor) opened?))
        #f)))

(define (comment-editor-done)
  (and-let* ((popup  (current-buffer))
             (master (buffer-get-master popup))
             (pc     (tree-innermost any-comment-context? #t))
             (mid    (and pc (tm->string (tm-ref pc 1))))
             (body   (and pc (tm-ref pc :last))))
    (with-buffer master
      (let* ((mb (buffer-get-body master))
             (l  (tree-search
                   mb
                   (lambda (t)
                     (and (any-comment-context? t)
                          (== (tm->string (tm-ref t 1)) mid))))))
        (when (nnull? l)
          (tree-set! (car l) :last (tree-copy body))))))
  (when (defined? 'mirror-treat-pending)
    (mirror-treat-pending))
  (when (defined? 'mirror-synchronize)
    (mirror-synchronize))
  (set-comment-window-state #f)
  (show-auxiliary-widget #f)
  (comment-quit-command))

;; 用于auxiliary-widget的comment编辑器widget
(tm-widget ((comment-aux-widget u packs doc) quit)
  (padded
    (resize "480px" "300px"
      (texmacs-input doc `(style (tuple ,@packs)) u))
    ===
    (hlist
      >>
      (explicit-buttons
        ("Done" (comment-editor-done))))))

;; Comment编辑器取消函数
(tm-define ((comment-cancel u) . args)
  (set-comment-window-state #f))

(define (allow-init? init)
  (and (string? (car init))
       (not (string-starts? (car init) "page-"))))

(tm-define (open-comment-editor-aux)
  (and-let* ((c (tm->stree (tree-innermost any-comment-context? #t)))
             (b (current-buffer-url))
             (u (string->url "tmfs://aux/edit-comment"))
             (packs (embedded-style-list))
             (pre (document-get-preamble (buffer-tree)))
             (inits* (map cdr (cdr (tm->stree (get-all-inits)))))
             (inits (list-filter inits* allow-init?))
             (env (apply append inits))
             (com (mirror-comment c 'carbon-comment))
             (doc `(with ,@env (document (hide-preamble ,pre) ,com))))
    (buffer-set-master u b)
    (auxiliary-widget (comment-aux-widget u packs doc)
                      (comment-cancel b)
                      (translate comment-text) u)
    (buffer-focus "tmfs://aux/edit-comment" #t)
    (go-end)))

(tm-define (open-comment-editor)
  (:applicable (behind-folded-comment?))
  (:interactive #t)
  (set-comment-window-state #t)
  (open-comment-editor-aux))

(tm-define (kbd-control-return)
  (:require (behind-folded-comment?))
  (let ((state (get-comment-window-state)))
    (if state
        (begin
          (set-comment-window-state #f)
          (show-auxiliary-widget #f))
        (open-comment-editor))))

(tm-define (kbd-control-return)
  (:require (inside? 'carbon-comment))
  (comment-editor-done))

; (tm-define (make-folded-comment type)
;   (:applicable (not (inside-comment?)))
;   (make-comment 'folded-comment type (list 1) open-comment-editor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open comments editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-permission-handler (comments name type)
  (in? type (list "read")))

(tmfs-title-handler (comments name doc)
  (with u (tmfs-string->url name)
    (string-append (url->system (url-tail u)) " - Comments")))

(define (mirror-comment t . opt-lab)
  (let* ((uid' (if (tm-atomic? (tm-ref t 0))
                   (string-append (tm->string (tm-ref t 0)) "-edit")
                   (create-unique-id)))
         (mid  (tm->string (tm-ref t 1)))
         (typ  (tm->string (tm-ref t 2)))
         (by   (tm->string (tm-ref t 3)))
         (tim  (tm->string (tm-ref t 4)))
         (bod  (tm-ref t :last))
         (lab  (if (null? opt-lab) 'mirror-comment (car opt-lab))))
    `(,lab ,uid' ,mid ,typ ,by ,tim "" ,bod)))

(tmfs-load-handler (comments name)
  (let* ((u (tmfs-string->url name))
         (doc (tree->stree (buffer-get u))))
    (tm-replace doc (cut tm-func? <> 'body 1)
                (lambda (t)
                  (let* ((l (tm-search t comment-context?))
                         (r (map mirror-comment l)))
                    (if (null? r)
                        `(body (document ""))
                        `(body (document ,@r))))))))

(tm-define (open-comments-editor)
  (:applicable (comments-in-buffer))
  (let* ((l (comments-in-buffer))
         (u (current-buffer))
         (cu (string-append "tmfs://comments/" (url->tmfs-string u))))
    (when (not (tree-innermost any-comment-context? #t))
      (list-go-to l :next))
    (when (not (tree-innermost any-comment-context? #t))
      (list-go-to l :previous))
    (when (not (tree-innermost any-comment-context? #t))
      (list-go-to l :first))
    (let* ((t (tree-innermost any-comment-context? #t))
           (id (comment-id t)))
      (when t (tree-select t))
      (load-buffer-in-new-window cu)
      (buffer-set-master cu u)
      (delayed
        (:pause 50)
        (and-let* ((b (buffer-get-body cu))
                   (c (search-comment b id)))
          (with-buffer cu
            (tree-go-to c :start)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pairing cursor positions: comments -> commented
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-comments-editor?)
  (string-starts? (url->string (current-buffer)) "tmfs://comments/"))

(define ((comment-by-id? id) t)
  (and (any-comment-context? t)
       (== (comment-id t) id)))

(define (search-comment t id)
  (with l (tree-search t (comment-by-id? id))
    (and (nnull? l) (car l))))

(define (sync-master-cursor)
  (and-let* ((c (tree-innermost 'mirror-comment))
             (m (buffer-get-master (current-buffer)))
             (b (buffer-get-body m))
             (i (comment-id c))
             (t (search-comment b i))
             (inv? (not (notified-change? 4))))
    (with-buffer m
      (when inv? (tree-select t))
      (tree-go-to t :end)
      (when (and (not (cursor-accessible?)) (not (in-source?)))
        (cursor-show-hidden)))))

(tm-define (mouse-event key x y mods time data)
  (former key x y mods time data)
  (when (and (in-comments-editor?) (!= key "move"))
    (sync-master-cursor)))

(tm-define (keyboard-press key time)
  (former key time)
  (when (in-comments-editor?)
    (sync-master-cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pairing cursor positions: commented -> comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (has-comments-editor?)
  (let* ((u (current-buffer))
         (cu (string-append "tmfs://comments/" (url->tmfs-string u))))
    (and (buffer-exists? cu)
         (in? (string->url cu) (map window->buffer (window-list))))))

(define (sync-comments-cursor)
  (let* ((m (current-buffer))
         (u (string-append "tmfs://comments/" (url->tmfs-string m)))
         (b (buffer-get-body u))
         (t (tree-innermost any-comment-context? #t))
         (inv? (not (notified-change? 4))))
    (with-buffer u
      (if t
          (and-let* ((i (comment-id t))
                     (c (search-comment b i)))
            (when inv? (tree-select (tm-ref c :last)))
            (tree-go-to c :start))
          (selection-cancel)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 新增功能：先编辑，确认后再插入 (仿照 open-comment-editor-aux 修正版)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 辅助函数：从编辑器复杂的树结构中提取用户输入的核心 document
;; 结构通常是: (with ... (document (hide-preamble ...) (document "用户内容")))
(define (extract-editor-body t)
  (cond 
    ;; 如果是 (with ... body)，取最后一个孩子继续剥离
    ((tm-func? t 'with) 
     (extract-editor-body (tm-ref t :last)))
    
    ;; 如果是 (document ...)，检查是否包含 hide-preamble
    ((tm-func? t 'document)
     (if (and (> (tree-arity t) 0) 
              (tm-func? (tm-ref t 0) 'hide-preamble))
         ;; 如果第一个孩子是 hide-preamble，说明这是外层包裹，取最后一个孩子（即真正的编辑器内容）
         (extract-editor-body (tm-ref t :last))
         ;; 否则这就是最内层的 document，直接返回
         t))
    
    ;; 其他情况直接返回
    (else t)))

;; 1. 提交函数
(define (new-comment-done target-buffer target-path type)
  (let* ((editor-buffer (current-buffer)) ;; 获取当前点击按钮时的 aux buffer
         (editor-root (buffer-get-body editor-buffer))
         ;; 【关键】使用提取函数拿到干净的内容，并深拷贝
         (clean-content (tree-copy (extract-editor-body editor-root))))
    
    ;; 关闭窗口
    (set-comment-window-state #f)
    (show-auxiliary-widget #f)
    
    ;; 切换回主文档插入
    (if (buffer-exists? target-buffer)
        (with-buffer target-buffer
          (tree-go-to target-path)
          ;; 插入批注，使用 lambda 返回提取出的干净内容
          (make-comment 'folded-comment 
                        type 
                        (list 1) 
                        (lambda () clean-content)))
        (display "Error: Target buffer lost.\n"))))

;; 2. Widget 定义 (参数传递版)
(tm-widget ((new-comment-aux-widget u packs doc target-buf target-path type) quit)
  (padded
    (resize "480px" "300px"
      (texmacs-input doc `(style (tuple ,@packs)) u))
    ===
    (hlist
      >>
      (explicit-buttons
        ("Insert" (new-comment-done target-buf target-path type))))))

;; 3. 打开编辑器 (完全仿照 open-comment-editor-aux 的环境构建)
(tm-define (open-new-comment-editor-aux target-buf target-path type)
  (and-let* ((b target-buf) ;; 主文档 buffer
             (u (string->url "tmfs://aux/new-comment"))
             (packs (embedded-style-list))
             ;; 获取主文档的 preamble 和初始化环境，保证显示效果一致
             (pre (document-get-preamble (buffer-tree)))
             (inits* (map cdr (cdr (tm->stree (get-all-inits)))))
             (inits (list-filter inits* allow-init?))
             (env (apply append inits))
             ;; 【核心差异】
             ;; 原版这里是 (mirror-comment ...)，我们这里放一个空的 (document "")
             ;; 这样结构就保持了一致：(with ... (document (hide-preamble ...) (document "")))
             (empty-body '(document ""))
             (doc `(with ,@env (document (hide-preamble ,pre) ,empty-body))))
    
    ;; 关联 Master，虽然我们在 done 里显式传了 buffer，但这一步对样式渲染很重要
    (buffer-set-master u b)
    
    ;; 启动 Widget，传入所有必要参数
    (auxiliary-widget (new-comment-aux-widget u packs doc target-buf target-path type)
                      (comment-cancel b)
                      "New Comment" u)
    
    (buffer-focus "tmfs://aux/new-comment" #t)))

;; 4. 入口函数
(tm-define (make-folded-comment type)
  (:applicable (not (inside-comment?)))
  (:interactive #t)
  
  (let ((current-buf (current-buffer))
        (current-path (cursor-path)))
    (set-comment-window-state #t)
    (open-new-comment-editor-aux current-buf current-path type)))

(tm-define (mouse-event key x y mods time data)
  (former key x y mods time data)
  (when (and (has-comments-editor?) (!= key "move"))
    (sync-comments-cursor)))

(tm-define (keyboard-press key time)
  (former key time)
  (when (has-comments-editor?)
    (sync-comments-cursor)))

;; 注册comment编辑器widget类型
(register-auxiliary-widget-type 'comment-editor (list open-comment-editor-aux))
