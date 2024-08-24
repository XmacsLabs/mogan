;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scheme-ast-lang.scm
;; DESCRIPTION : the Scheme Language (keywords from tm-model.el)
;; COPYRIGHT   : (C) 2024  UnbSky
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code scheme-ast-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "scheme") (== key "keytoken")))
  `(,(string->symbol key)
      ;;keywords are from tm-model.el
    (nullary-keywords
      "begin" "cond" "else"  
      "values" "define-preferences" "menu-dynamic" "conserve-focus" "attach-macro"  
      "case-lambda" "kbd-map" "kbd-wildcards" "kbd-commands" "kbd-symbols"  
      "define-grammar" "define-regexp-grammar"  
      "drd-rule" "logic-rules" "assume" "texmacs-modes"  
      "user-delayed" "delayed" "on-entry" "on-exit" "widget-delayed"  
      "with-wallet"  
      "association-tile" "bar" "concat" "dense-bar" "dense-tile" "document"  
      "header-bar" "sequence" "short-bar" "short-tile" "minibar"  
      "wrap-selection-any" "wrap-selection-small"  
      "try-modification" "try-correct"  
      "tabs" "icon-tabs" "padded" "centered" "aligned" "bottom-buttons" "scrollable"  
      "hlist" "vlist" "hsplit" "vsplit" "explicit-buttons" "horizontal" "vertical"  
      "$begin" "$cond")
    (nullary-keywords
      ":use" ":inherit"  
      "$tmdoc" "$tmdoc-title"  
      "$para" "$itemize" "$enumerate"  
      "$description" "$description-aligned" "$description-long"  
      "$tm-fragment")
    (unary-keywords
      "let" "let*" "lambda"  
      "with-result" "and-let*" "setup-append-if"  
      "while" "for" "repeat" "when" "unless" "assuming" "mini" "tile"  
      "plugin-configure"  
      "define-preference-names"  
      "with-focus-after"  
      "logic-group" "logic-table" "logic-dispatcher"  
      "with-aux" "with-action" "with-module"  
      "with-cursor" "with-buffer" "with-window" "with-author"  
      "with-server" "with-database" "with-database*"  
      "with-time" "with-time-stamp" "with-limit"  
      "with-encoding" "with-indexing"  
      "with-user" "with-extra-fields"  
      "with-remote-context" "with-identifier-context"  
      "speech-symbols"  
      "user-ask"  
      "tab" "icon-tab" "form" "item" "meti" "refreshable" "division"  
      "$when" "$let" "$let*" "$for" "$refreshable" "$division"  
      "tmfs-load-handler" "tmfs-save-handler"  
      "tmfs-autosave-handler" "tmfs-remove-handler" "tmfs-wrap-handler"  
      "tmfs-date-handler" "tmfs-title-handler" "tmfs-permission-handler"  
      "tmfs-master-handler" "tmfs-format-handler"  
      "push-focus" "pull-focus")
    (unary-definitions
      "define" "define-public" "define-macro" "define-public-macro"  
      "texmacs-module" "provide-public" "define-group"  
      "tm-define" "tm-define-macro" "lazy-body-macro" "tm-property" "request-handler"  
      "tm-menu" "define-menu" "tm-widget" "define-widget" "tm-generate" "tm-tool" "tm-tool*"  
      "tm-build" "tm-build-macro" "tm-build-widget"  
      "menu-bind" "smart-table"  
      "define-table" "extend-table" "define-collection" "extend-collection"  
      "tm-service" "tm-call-back"  
      "define-format" "define-language" "define-graphics")
    (unary-no-highlight
      "format" "interactive"  
      "$describe-item" "$link" "$tmdoc-link"  
      "$folded-documentation" "$unfolded-documentation" "$explain")
    (binary-keywords
      "with" "with-define" "with-global" "and-with" "with-innermost" "receive"  
      "with-environment" "with-environment*" "converter" "with-cache"  
      "speech-map" "speech-map-wildcard"  
      "speech-adjust" "speech-reduce" "speech-collection"  
      "user-confirm" "user-url"  
      "resize" "cached"  
      "$with")
    (binary-no-highlight
      "client-remote-eval" "server-remote-eval")
    (ternary-keywords
      "ahash-with" "canvas-input"  
      "with-remote-get-attributes"  
      "with-remote-get-entry" "with-remote-create-entry"  
      "with-remote-search" "with-remote-search-user"  
      "with-remote-get-user-pseudo" "with-remote-get-user-name"  
      "with-remote-identifier")
    (quaternary-keywords
      "with-remote-get-field")
    (other-keywords
      "for" "if" "inherit" "former"  
      "define-secure-symbols" "map-in-order" "link" "promise"  
      "plugin-input-converters" "use-modules" "export" "import-from" "inherit-modules"  
      "lazy-menu" "lazy-keyboard" "lazy-define" "lazy-define-macro" "lazy-initialize"  
      "lazy-format" "lazy-language" "lazy-input-converter" "lazy-tmfs-handler"  
      "$if")
    (operator "{" "[" "(" ")" "]" "}" "'" ",")
    (string_quote "\"")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "scheme") (== key "brackets")))
  `(,(string->symbol key)
    (3 "(" ")")
    (3 "[" "]")
    (3 "{" "}")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "scheme") (== key "special_symbol")))
  `(,(string->symbol key)
    (tokenize "symbol" "first_symbol")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences for syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tm-define (parser-feature lan key)
  (:require (and (== lan "scheme") (== key "light_theme")))
  `(,(string->symbol key)
    ("#000000" "none")
    ("#458C85" 
      "nullary-keywords" "unary-keywords" "unary-definitions"  
      "binary-keywords" "ternary-keywords" "quaternary-keywords"  
      "other-keywords" "unary-definitions")
    ("#000000" 
      "unary-no-highlight" "binary-no-highlight")
    ("#0000AB" "first_symbol")
    ("#000000" "symbol"  "operator")
    ("#795E26" "number" "boolean")
    ("#267F99" "comment")
    ("#D32F2F" "string_content" "string_quote" "character")
    ("#BF2C2C" "escape_sequence")
    ("#006400" "(0" ")0" "[0" "]0" "{0" "}0")
    ("#00008B" "(1" ")1" "[1" "]1" "{1" "}1")
    ("#654321" "(2" ")2" "[2" "]2" "{2" "}2")))
