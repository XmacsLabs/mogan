;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : octave.scm
;; DESCRIPTION : prog format for Octave
;; COPYRIGHT   : (C) 2026  Hongli Zha
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DESCRIPTION:
;;;   This module defines the format conversion for Octave files. It specifies
;;;   how to convert between TeXmacs document tree format and Octave source
;;;   code format, allowing users to import and export Octave code.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 定义octave格式模块
(texmacs-module (data octave))

;;------------------------------------------------------------------------------
;; 格式定义
;;

;; 定义Octave格式，指定名称和文件后缀
(define-format octave
  (:name "Octave source code")
  (:suffix "m"))

;;------------------------------------------------------------------------------
;; 转换函数定义
;;

;; 定义从TeXmacs格式转换为Octave格式的函数
(define (texmacs->octave x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

;; 定义从Octave格式转换为TeXmacs格式的函数
(define (octave->texmacs x . opts)
  (code->texmacs x))

;; 定义从Octave代码片段转换为TeXmacs格式的函数
(define (octave-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

;;------------------------------------------------------------------------------
;; 转换器注册
;;

;; 注册TeXmacs文档树到Octave文档的转换器
(converter texmacs-tree octave-document
  (:function texmacs->octave))

;; 注册Octave文档到TeXmacs文档树的转换器
(converter octave-document texmacs-tree
  (:function octave->texmacs))

;; 注册TeXmacs文档树到Octave代码片段的转换器
(converter texmacs-tree octave-snippet
  (:function texmacs->octave))

;; 注册Octave代码片段到TeXmacs文档树的转换器
(converter octave-snippet texmacs-tree
  (:function octave-snippet->texmacs))