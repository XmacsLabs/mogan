(texmacs-module (liii ocr))
(import (liii os))
(import (liii base64))

(define temp-dir (os-temp-dir))

(tm-define (get-image t i bool)
  (let* ((cur-t (tree-ref t i)))
    (cond 
      ((not cur-t) #f)
      ((tree-is? cur-t 'image) (get-image-tuple cur-t 0 bool))
      (else (get-image t (+ i 1) bool)))))

(tm-define (get-image-tuple t i bool)
(if bool
  (let* ((cur-t (tree-ref t i)))
    (cond 
      ((not cur-t) #f)
      ((tree-is? cur-t 'tuple) (get-image-name cur-t 0))
      (else (get-image-tuple t (+ i 1)))))
  (let* ((cur-t (tree-ref t i)))
    (cond 
      ((not cur-t) #f)
      ((tree-is? cur-t 'tuple) (get-image-data cur-t 0))
      (else (get-image-tuple t (+ i 1)))))))

(tm-define (get-image-name t i)
  (let* ((cur-t (tree-ref t i)))
    (cond 
      ((not cur-t) #f)
      ((not (string=? (tree->string cur-t) "")) (tree->string cur-t))
      (else (get-image-name t (+ i 1))))))

(tm-define (get-image-data t i)
  (let* ((cur-t (tree-ref t i)))
    (cond 
      ((not cur-t) #f)
      ((tree-is? cur-t 'raw-data) (cdr (tree->stree cur-t)))
      (else (get-image-name t (+ i 1))))))

(tm-define (get-image-extention name)
  (let* ((parts (string-split name #\.)))
    (if (> (length parts) 1)
        (last parts)
        name)))

(tm-define (insert-tips p)
  (go-to p)
  (go-to-next-node)
  (insert `(with "par-mode" "center" (document ,(utf8->cork "很抱歉，目前OCR功能仅为会员用户提供。")))))

; (get-image-extention (get-image t 0 #t)) 获取文件后缀，创建对应临时文件
; (get-image t 0 #f) 获取 raw-data

(tm-define (create-temp-image t p)
  (let* ((extention 
           (get-image-extention (get-image t 0 #t)))
         (temp-name 
           (string-append temp-dir "/temp-" (number->string (current-time)) "." extention))
         (data-list 
           (get-image t 0 #f)))
    (when (and (list? data-list) (not (null? data-list)))
      (let* ((base64-str (car data-list))
             (binary-data (decode-base64 base64-str)))
        (string-save binary-data temp-name)
        (display* "Image has saved to " temp-name "\n"))))
  (insert-tips p))