
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : account.scm
;; DESCRIPTION : login
;; COPYRIGHT   : (C) 2025  Mogan STEM authors
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (liii account))
(import (liii os))

;; 获取当前时间戳（秒）
(define (get-current-time)
  (let* ((now (current-time))
         (seconds (car now))
         (microseconds (cdr now)))
    (+ seconds (/ microseconds 1000000.0))))

;; 获取账户数据目录
(define (get-account-data-dir)
  (url-append (get-texmacs-home-path) "system/account"))

;; 生产
; (tm-define (account-oauth2-config key)
;   (cond
;     ((== key "authorization-url") "http://www.liiistem.cn/oauth2-login.html")
;     ((== key "access-token-url") "http://www.liiistem.cn:8080/oauth2/token")
;     ((== key "client-identifier") "public-client")
;     ((== key "scope") "user+llm")
;     ((== key "port-list") "6029,8087,9256,7438,5173,6391,8642,9901,44118,55055,1895")
;     ((== key "user-info-url") "http://www.liiistem.cn/api/oauthUser/membershipInfo")
;     ((== key "pricing-url") "https://www.liiistem.cn/pricing-fruit.html")
;     ((== key "click-return-liii-url") "https://www.liiistem.cn/?from=login_button")
;     (else "")))

;; 测试
(tm-define (account-oauth2-config key)
  (cond
    ((== key "authorization-url") "http://test-www.liiistem.cn/oauth2-login.html")
    ((== key "access-token-url") "http://test-www.liiistem.cn:8080/oauth2/token")
    ((== key "client-identifier") "public-client")
    ((== key "scope") "user+llm")
    ((== key "port-list") "6029,8087,9256,7438,5173,6391,8642,9901,44118,55055,1895")
    ((== key "user-info-url") "http://test-www.liiistem.cn/api/oauthUser/membershipInfo")
    ((== key "pricing-url") "http://test-www.liiistem.cn/pricing-fruit.html")
    ((== key "click-return-liii-url") "https://www.liiistem.cn/?from=login_button")
    (else "")))

;; 本地
; (tm-define (account-oauth2-config key)
;   (cond
;     ((== key "authorization-url") "http://127.0.0.1:3000/oauth2-login")
;     ((== key "access-token-url") "http://127.0.0.1:8080/oauth2/token")
;     ((== key "client-identifier") "public-client")
;     ((== key "client-secret") "secret")
;     ((== key "scope") "user+llm")
;     ((== key "port-list") "6029,8087,9256,7438,5173,6391,8642,9901,44118,55055,1895")
;     ((== key "user-info-url") "http://127.0.0.1:8080/api/oauthUser/membershipInfo")
;     ((== key "pricing-url") "http://127.0.0.1:3000/pricing-fruit.html")
;     ((== key "click-return-liii-url") "http://127.0.0.1:3000/?from=login_button")
;     (else "")))


(tm-define (account-save-token-display token)
  (display "OAuth2 Token: ")
  (display token)
  (newline))

;; 获取token数据文件路径
(define (get-token-data-file)
  (let* ((data-dir (get-account-data-dir))
         (data-file (url-append data-dir "token.txt")))
    data-file))

;; 确保数据目录存在的辅助函数
(define (ensure-data-dir-exists)
  (let ((data-dir (get-account-data-dir)))
    (if (not (url-exists? data-dir))
        (system-mkdir data-dir))))

;; token 保存到数据文件
(tm-define (account-save-token token)
  (ensure-data-dir-exists)
  (let ((data-file (get-token-data-file)))
    ;; 如果文件存在则删除后重新创建，确保覆盖内容
    (when (url-exists? data-file)
      (system-remove data-file))
    ;; 保存token到文件
    (string-save token data-file)
    ))

;; 读取token数据文件
(tm-define (account-load-token)
  (let ((data-file (get-token-data-file)))
    (if (url-exists? data-file)
        (string-load data-file)
        "")))

;; 获取refresh_token数据文件路径
(define (get-refresh-token-data-file)
  (let* ((data-dir (get-account-data-dir))
         (data-file (url-append data-dir "refresh_token.txt")))
    data-file))

;; refresh_token 保存到数据文件
(tm-define (account-save-refresh-token refresh-token)
  (ensure-data-dir-exists)
  (let ((data-file (get-refresh-token-data-file)))
    ;; 如果文件存在则删除后重新创建，确保覆盖内容
    (when (url-exists? data-file)
      (system-remove data-file))
    ;; 保存refresh_token到文件
    (string-save refresh-token data-file)
    ))

;; 读取refresh_token数据文件
(tm-define (account-load-refresh-token)
  (let ((data-file (get-refresh-token-data-file)))
    (if (url-exists? data-file)
        (string-load data-file)
        "")))

;; 获取token过期时间数据文件路径
(define (get-token-expiry-data-file)
  (let* ((data-dir (get-account-data-dir))
         (data-file (url-append data-dir "token_expiry.txt")))
    data-file))

;; token过期时间保存到数据文件
(tm-define (account-save-token-expiry expiry-time)
  (ensure-data-dir-exists)
  (let ((data-file (get-token-expiry-data-file)))
    ;; 如果文件存在则删除后重新创建，确保覆盖内容
    (when (url-exists? data-file)
      (system-remove data-file))
    ;; 保存过期时间到文件
    (string-save expiry-time data-file)
    ))

;; 读取token过期时间数据文件
(tm-define (account-load-token-expiry)
  (let ((data-file (get-token-expiry-data-file)))
    (if (url-exists? data-file)
        (string-load data-file)
        "")))

;; 清除所有token数据
(tm-define (account-clear-tokens)
  (ensure-data-dir-exists)
  (let ((token-file (get-token-data-file))
        (refresh-token-file (get-refresh-token-data-file))
        (expiry-file (get-token-expiry-data-file)))
    (when (url-exists? token-file)
      (system-remove token-file))
    (when (url-exists? refresh-token-file)
      (system-remove refresh-token-file))
    (when (url-exists? expiry-file)
      (system-remove expiry-file))))