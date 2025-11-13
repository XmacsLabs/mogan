(texmacs-module (liii account))
(import (liii os))

(tm-define (account-save-token-display token)
  (display "OAuth2 Token: ")
  (display token)
  (newline))

;; 获取缓存文件路径的辅助函数
(define (get-token-cache-file)
  (let* ((cache-dir (url-append (get-tm-cache-path) "account"))
         (cache-file (url-append cache-dir "token.txt")))
    cache-file))

;; 确保缓存目录存在的辅助函数
(define (ensure-cache-dir-exists)
  (let ((cache-dir (url-append (get-tm-cache-path) "account")))
    (if (not (url-exists? cache-dir))
        (system-mkdir cache-dir))))

;; token 保存到缓存文件
(tm-define (account-save-token token)
  (ensure-cache-dir-exists)
  (let ((cache-file (get-token-cache-file)))
    ;; 如果文件存在则删除后重新创建，确保覆盖内容
    (when (url-exists? cache-file)
      (system-remove cache-file))
    ;; 保存token到文件
    (string-save token cache-file)
    ))

;; 读取token缓存文件
(tm-define (account-load-token)
  (let ((cache-file (get-token-cache-file)))
    (if (url-exists? cache-file)
        (string-load cache-file)
        "")))