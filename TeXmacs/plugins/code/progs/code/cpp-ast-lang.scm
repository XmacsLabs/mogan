;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cpp-ast-lang.scm
;; DESCRIPTION : the C++ Language
;; COPYRIGHT   : (C) 2024  UnbSky
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code cpp-ast-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "cpp-ast") (== key "keytoken")))
  `(,(string->symbol key)
    (constant
      "false" "true" "cout" "cin" "cerr"
      "null" "nullptr" "nullptr_t" "NULL")
    (declare_type "class" "interface" "enum")
    (declare_module "namespace" "using")
    (keyword
      "asm" "auto" "calloc" "class" "concept" "concrete" "const"
      "const_cast" "constant" "constexpr" "default" "delete" "dynamic_cast"
      "enum" "explicit" "export" "extern" "free" "friend" "inline" "malloc"
      "mutable" "new" "operator" "private" "protected" "public" "realloc"
      "register" "reinterpret_cast" "sizeof" "static" "static_cast"
      "struct" "template" "this" "to" "typedef" "typeid" "typename" "union"
      "virtual" "volatile")
    (keyword_conditional
      "break" "continue" "do" "else" "for" "if"
      "while" "goto" "switch" "case")
    (keyword_control
      "throw" "catch" "finally" "return" "try" "yield")
    (string_quate
      "R\"" "\"" "'")
    (preprocess 
    "#define" "#undef" "#include"
     "#if" "#ifdef" "#ifndef" "#else" "#elif" "#endif"
     "#line" "#error" "#pragma")
    (operator
      "+" "-" "/" "*" "%" ;; Arith
      "|" "&" "^" ;; Bit
      "<<" ">>" 
      "++" "--" 
      "==" "!=" "<" ">" "<=" ">=" "&&" "||" "!" "==" ;; Boolean
      "+=" "-=" "/=" "*=" "%=" "|=" "&=" "^=" ;; Assignment
      "=" ":" ";" ",")
    (operator_special "->")
    (operator_decoration "@")
    (operator_field "." "::")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences for syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tm-define (parser-feature lan key)
  (:require (and (== lan "cpp-ast") (== key "white_theme")))
  `(,(string->symbol key)
    (C#000000 "none")
    (C#0000FF "constant")
    (C#0070C0 "declare_type" "declare_module")
    (C#AF00DB "keyword" "keyword_conditional" "keyword_control")
    (C#000000 "operator" "operator_special" "operator_decoration" "operator_field" "operator_openclose")
    (C#001080 "identifier")
    (C#795E26 "number_literal")
    (C#267F99 "comment" "documentation")
    (C#D16969 "punctuation")
    (C#6A0D91 "preprocess")
    (C#D7BA7D "variable" "variable_parameter" "variable_property" "variable_special")
    (C#3A9B8C "type_identifier", "namespace_identifier")
    (C#4077A3 "primitive_type" )
    (C#D32F2F "string_content" "char_content" "string_quate", "raw_string_content")
    (C#BF2C2C "escape_sequence" "system_lib_string")
    (C#5D3A1A "field_identifier")
    (C#800080 "INNER_ERROR")
    (C#006400 "(0" ")0" "[0" "]0" "{0" "}0")
    (C#00008B "(1" ")1" "[1" "]1" "{1" "}1")
    (C#654321 "(2" ")2" "[2" "]2" "{2" "}2")))
