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
  (:require (and (== lan "cpp") (== key "keytoken")))
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
    (string_quote
      "R\"" "\"" "'")
    (preprocessor 
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
      "=" ":" ";" "," "...")
    (operator_special "->")
    (operator_decoration "@")
    (operator_field "." "::")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences for syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tm-define (parser-feature lan key)
  (:require (and (== lan "cpp") (== key "light_theme")))
  `(,(string->symbol key)
    ("#000000" "none")
    ("#0000FF" "constant")
    ("#0070C0" "declare_type" "declare_module")
    ("#AF00DB" "keyword" "keyword_conditional" "keyword_control")
    ("#4B0082" "operator" "operator_special" "operator_decoration" "operator_field" "operator_openclose")
    ("#000000" "identifier")
    ("#3030B0" "number_literal")
    ("#808080" "comment" "documentation")
    ("#D16969" "punctuation")
    ("#004000" "preprocessor")
    ("#D7BA7D" "variable" "variable_parameter" "variable_property" "variable_special")
    ("#3A9B8C" "type_identifier", "namespace_identifier")
    ("#4077A3" "primitive_type" )
    ("#D32F2F" "string_content" "char_content" "string_quote", "raw_string_content")
    ("#BF2C2C" "escape_sequence" "system_lib_string")
    ("#5D3A1A" "field_identifier")
    ("#800080" "INNER_ERROR")
    ("#006400" "(0" ")0" "[0" "]0" "{0" "}0")
    ("#00008B" "(1" ")1" "[1" "]1" "{1" "}1")
    ("#654321" "(2" ")2" "[2" "]2" "{2" "}2")))
