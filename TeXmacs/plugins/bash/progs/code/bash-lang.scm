;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bash-lang.scm
;; DESCRIPTION : Bash Language
;; COPYRIGHT   : (C) 2026 Hongli Zha
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code bash-lang)
  (:use (prog default-lang)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bash reserved words / builtins / constants
(tm-define (parser-feature lan key)
  (:require (and (== lan "bash") (== key "keyword")))
  `(,(string->symbol key)
    (extra_chars "_")  ;; 标识符允许下划线

    ;; 常量
    (constant
      "true" "false")

    ;; Bash builtins
    (declare_function
      ":" "." "source"
      "alias" "unalias"
      "bg" "fg" "jobs" "disown" "wait" "kill" "suspend"
      "cd" "pwd" "dirs" "pushd" "popd"
      "echo" "printf"
      "read" "mapfile"
      "test" "[" "[["
      "type" "hash" "help"
      "builtin" "command" "enable"
      "eval" "exec"
      "exit" "logout" "return"
      "export" "readonly" "unset"
      "set" "shift"
      "getopts"
      "declare" "typeset" "local"
      "let"
      "trap" "shopt" "ulimit" "umask"
      "history" "fc"
      "compgen" "complete"
      "times")

    ;; 关键字
    (keyword
      "if" "then" "elif" "else" "fi"
      "for" "in" "until" "while" "do" "done"
      "case" "esac" "select"
      "function" "coproc" "time"
      "{" "}"
      "!" )

    ;; 控制/环境相关
    (keyword_control
      "break" "continue")))

;; Operators / redirections / parameter expansion helpers
(tm-define (parser-feature lan key)
  (:require (and (== lan "bash") (== key "operator")))
  `(,(string->symbol key)

    ;; 基本运算符（算术/比较/逻辑）
    (operator
      "+" "-" "*" "/" "%" "**"
      "++" "--"
      "==" "!=" "<" ">" "<=" ">="
      "!" "~"
      "&" "&&" "|" "||"
      "^"
      "=" "+=" "-=" "*=" "/=" "%=")

    ;; 特殊运算符
    (operator_special
      ";" ";;"
      "|&"
      ">" ">>" "<" "<<"
      "<<<"
      "<>"
      ">|"
      "2>" "2>>" "1>" "1>>"
      "&>" "&>>")

    ;; 开闭符号
    (operator_openclose
      "(" ")" "[" "]" "{" "}")

    ;; 字段/参数相关符号（特殊参数 + 参数展开运算符）
    (operator_field
      "$" "@" "#" "?" "!" "-" "*"         ;; $@ $# $? $! $$ $- $*
      ":-" ":=" ":?" ":+"                ;; ${var:-x} ${var:=x} ...
      "##" "#" "%%" "%"                   ;; ${var##pat} ${var%pat} ...
      )))

;; Numbers
(tm-define (parser-feature lan key)
  (:require (and (== lan "bash") (== key "number")))
  `(,(string->symbol key)
    ;; Bash 算术扩展常见前缀：0x(十六进制), 0(八进制)
    (bool_features "decimal" "prefix_0x" "prefix_0")
    (separator "_")
    (suffix)))

;; Strings
(tm-define (parser-feature lan key)
  (:require (and (== lan "bash") (== key "string")))
  `(,(string->symbol key)
    (bool_features
      "single_quote" "double_quote"
      "multi_byte" "unicode_escape" "hex_escape")
    ;; Bash 常见转义
    (escape_sequences "\\" "\"" "'" "a" "b" "f" "n" "r" "t" "v"
                     "newline" "x" "u" "U" "$" "`" "!")))

;; Comments
(tm-define (parser-feature lan key)
  (:require (and (== lan "bash") (== key "comment")))
  `(,(string->symbol key)
    (inline "#")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences for syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-bash-syntax var val)
  (syntax-read-preferences "bash"))

(define-preferences
  ("syntax:bash:none" "red" notify-bash-syntax)
  ("syntax:bash:comment" "brown" notify-bash-syntax)
  ("syntax:bash:error" "dark red" notify-bash-syntax)
  ("syntax:bash:constant" "#4040c0" notify-bash-syntax)
  ("syntax:bash:constant_number" "#4040c0" notify-bash-syntax)
  ("syntax:bash:constant_string" "dark grey" notify-bash-syntax)
  ("syntax:bash:constant_char" "#333333" notify-bash-syntax)
  ("syntax:bash:declare_function" "#0000c0" notify-bash-syntax)
  ("syntax:bash:declare_type" "#0000c0" notify-bash-syntax)
  ("syntax:bash:operator" "#8b008b" notify-bash-syntax)
  ("syntax:bash:operator_openclose" "#B02020" notify-bash-syntax)
  ("syntax:bash:operator_field" "#B02020" notify-bash-syntax)
  ("syntax:bash:operator_special" "orange" notify-bash-syntax)
  ("syntax:bash:keyword" "#309090" notify-bash-syntax)
  ("syntax:bash:keyword_control" "#309090" notify-bash-syntax))
