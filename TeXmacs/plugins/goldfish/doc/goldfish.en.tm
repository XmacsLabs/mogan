<TeXmacs|2.1.2>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths|goldfish>>

<\body>
  <tmdoc-title|The Goldfish Scheme plugin>

  Goldfish Scheme is based on <hlink|S7|https://ccrma.stanford.edu/software/goldfish/>
  Scheme. To insert a Goldfish Scheme session, just click
  <menu|Insert|Session|S7 Scheme>. Goldfish Scheme session is running in a
  separated process, as a result, infinite loop in Goldfish Scheme session
  will not freeze Mogan while infinite loop in Scheme session will freeze
  Mogan. The Goldfish Scheme session in Mogan STEM Suite is using the
  Goldfish Scheme Community, the Goldfish Scheme session in Liii STEM Suite
  is using the Goldfish Scheme Enterprise.

  Here are snippets which only works in the Goldfish Scheme session:

  <paragraph|Unicode Support>

  The string literal in Goldfish Scheme does not support Unicode. In the
  Goldfish Scheme session, we use the cork encoding as a workaround.

  <\scm-code>
    <block|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<table|<row|<cell|Scheme
    Code>|<cell|Actual Scheme Code>>|<row|<cell|'\<#4E2D\>\<#6587\>>|<cell|'\<less\>#4E2D\<gtr\>\<less\>#6587\<gtr\>>>|<row|<cell|(length
    "\<#4E2D\>\<#6587\>")>|<cell|(length "\<less\>#4E2D\<gtr\>\<less\>#6587\<gtr\>")>>>>>
  </scm-code>

  <\session|goldfish|default>
    <\output>
      Goldfish Scheme (based on S7 Scheme 10.11, 2-July-2024)
    </output>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      '\<#4E2D\>\<#6587\>
    <|unfolded-io>
      <goldfish-result|\<#4E2D\>\<#6587\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      '\<less\>#4E2D\<gtr\>\<less\>#6587\<gtr\>
    <|unfolded-io>
      <goldfish-result|\<#4E2D\>\<#6587\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (length "\<#4E2D\>\<#6587\>")
    <|unfolded-io>
      <goldfish-result|14>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (length "\<less\>#4E2D\<gtr\>\<less\>#6587\<gtr\>")
    <|unfolded-io>
      <goldfish-result|14>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|Special Rules for Rendering>

  For Scheme snippets starting with the markup
  <markup|document>\<#3001\><markup|math>\<#3001\><markup|equation*>\<#3001\><markup|align>\<#3001\><markup|with>\<#3001\><markup|graphics>:

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      `(document (frac "\<#5206\>\<#5B50\>" "\<#5206\>\<#6BCD\>"))
    <|unfolded-io>
      <frac|\<#5206\>\<#5B50\>|\<#5206\>\<#6BCD\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      `(math (frac "1" "2"))
    <|unfolded-io>
      <math|<frac|1|2>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      `(with "color" "red" "Hello")
    <|unfolded-io>
      <with|color|red|Hello>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|Side Effect and Eval Result>

  Eval result is rendered in green background. Side effect is in normal white
  background.

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (begin (display "Hello") (newline) (+ 1 2))
    <|unfolded-io>
      Hello

      <goldfish-result|3>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (for-each (lambda (x) (display x) (newline))

      \ \ (list 1 2 3))
    <|unfolded-io>
      1

      2

      3
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|SRFI>

  SRFI implementation for Goldfish Scheme will be built-in for the Goldfish
  Scheme session. Here is the concise guide to load the SRFIs.

  <\wide-tabular>
    <tformat|<table|<row|<\cell>
      <hlink|R7RS|$TEXMACS_PATH/plugins/goldfish/goldfish/scheme/base.scm>
    </cell>|<\cell>
      R7RS base library
    </cell>|<\cell>
      <scm|(import (scheme base))>
    </cell>>|<row|<\cell>
      <hlink|R7RS|$TEXMACS_PATH/plugins/goldfish/goldfish/scheme/case-lambda.scm>
    </cell>|<\cell>
      R7RS case-lambda
    </cell>|<\cell>
      <scm|(import (scheme case-lambda))>
    </cell>>|<row|<\cell>
      <hlink|R7RS|$TEXMACS_PATH/plugins/goldfish/goldfish/scheme/file.scm>
    </cell>|<\cell>
      R7RS file library
    </cell>|<\cell>
      <scm|(import (scheme file))>
    </cell>>|<row|<\cell>
      <hlink|R7RS|$TEXMACS_PATH/plugins/goldfish/goldfish/scheme/time.scm>
    </cell>|<\cell>
      R7RS time library
    </cell>|<\cell>
      <scm|(import (scheme time))>
    </cell>>|<row|<\cell>
      <hlink|SRFI 1|$TEXMACS_PATH/plugins/goldfish/goldfish/srfi/srfi-1.scm>
    </cell>|<\cell>
      List Library
    </cell>|<\cell>
      <scm|(import (srfi srfi-1))>
    </cell>>|<row|<\cell>
      <hlink|SRFI 8|$TEXMACS_PATH/plugins/goldfish/goldfish/srfi/srfi-8.scm>
    </cell>|<\cell>
      Provide <scm|receive>
    </cell>|<\cell>
      <scm|(import (srfi srfi-8))>
    </cell>>|<row|<\cell>
      <hlink|SRFI 9|$TEXMACS_PATH/plugins/goldfish/goldfish/srfi/srfi-9.scm>
    </cell>|<\cell>
      Provide <scm|define-record-type>
    </cell>|<\cell>
      <scm|(import (srfi srfi-9))>
    </cell>>|<row|<\cell>
      <hlink|SRFI 13|$TEXMACS_PATH/plugins/goldfish/goldfish/srfi/srfi-13.scm>
    </cell>|<\cell>
      String Library
    </cell>|<\cell>
      <scm|(import (srfi srfi-13))>
    </cell>>|<row|<\cell>
      <hlink|SRFI 16|$TEXMACS_PATH/plugins/goldfish/goldfish/srfi/srfi-16.scm>
    </cell>|<\cell>
      Provide <scm|case-lambda>
    </cell>|<\cell>
      <scm|(import (srfi srfi-16))>
    </cell>>|<row|<\cell>
      <hlink|SRFI 39|$TEXMACS_PATH/plugins/goldfish/goldfish/srfi/srfi-39.scm>
    </cell>|<\cell>
      Parameter objects
    </cell>|<\cell>
      <scm|(import (srfi srfi-39))>
    </cell>>|<row|<\cell>
      <hlink|SRFI 78|$TEXMACS_PATH/plugins/goldfish/goldfish/srfi/srfi-78.scm>
    </cell>|<\cell>
      Lightweight testing
    </cell>|<\cell>
      <scm|(import (srfi srfi-78))>
    </cell>>>>
  </wide-tabular>

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (import (srfi srfi-1))
    <|unfolded-io>
      <goldfish-result|(rootlet)>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (reduce + 0 (list 1 2 3))
    <|unfolded-io>
      <goldfish-result|6>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (import (srfi srfi-78))
    <|unfolded-io>
      <goldfish-result|(rootlet)>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (check (+ 1 2) =\<gtr\> 3)
    <|unfolded-io>
      \;

      (+ 1 2) =\<gtr\> 3 ; correct

      <goldfish-result|1>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  Here are code snippets from <slink|https://ccrma.stanford.edu/software/s7/s7.html>.

  <paragraph|help>

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (help 'equal?)
    <|unfolded-io>
      <goldfish-result|"(equal? obj1 obj2) returns #t if obj1 is equal to
      obj2">
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (help 'cdar)
    <|unfolded-io>
      <goldfish-result|"(cdar lst) returns (cdr (car lst)): (cdar '((1 2 3)))
      -\> '(2 3)">
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|equality>

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (eq? () ())
    <|unfolded-io>
      <goldfish-result|#t>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (eq? 2 2.0)
    <|unfolded-io>
      <goldfish-result|#f>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (equal? 2 2.0)
    <|unfolded-io>
      <goldfish-result|#f>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (equivalent? 2 2.0)
    <|unfolded-io>
      <goldfish-result|#t>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (equivalent? .1 1/10)
    <|unfolded-io>
      <goldfish-result|#t>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|multiprecision arithmetic>

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (* 1.0 pi)
    <|unfolded-io>
      <goldfish-result|3.141592653589793>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (*goldfish* 'bignum-precision)
    <|unfolded-io>
      <goldfish-result|128>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (set! (*goldfish* 'bignum-precision) 256)
    <|unfolded-io>
      <goldfish-result|256>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (* 1.0 pi)
    <|unfolded-io>
      <goldfish-result|3.141592653589793>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (bignum "123456789123456789")
    <|unfolded-io>
      <goldfish-result|123456789123456789>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (bignum "1.123123123123123123123123123")
    <|unfolded-io>
      <goldfish-result|1.1231231231231231>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|math functions>

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (exact? 1.0)
    <|unfolded-io>
      <goldfish-result|#f>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (rational? 1.5)
    <|unfolded-io>
      <goldfish-result|#f>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (floor 1.4)
    <|unfolded-io>
      <goldfish-result|1>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (remainder 2.4 1)
    <|unfolded-io>
      <goldfish-result|0.3999999999999999>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (modulo 1.4 1.0)
    <|unfolded-io>
      <goldfish-result|0.3999999999999999>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (lcm 3/4 1/6)
    <|unfolded-io>
      <goldfish-result|3/2>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (log 8 2)
    <|unfolded-io>
      <goldfish-result|3>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (number-\<gtr\>string 0.5 2)
    <|unfolded-io>
      <goldfish-result|"0.1">
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (string-\<gtr\>number "0.1" 2)
    <|unfolded-io>
      <goldfish-result|0.5>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (rationalize 1.5)
    <|unfolded-io>
      <goldfish-result|3/2>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (complex 1/2 0)
    <|unfolded-io>
      <goldfish-result|1/2>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (logbit? 6 1)
    <|unfolded-io>
      <goldfish-result|#t>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (random 0)
    <|unfolded-io>
      <goldfish-result|0>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (random 1.0)
    <|unfolded-io>
      <goldfish-result|0.4550541097362356>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (random 3/4)
    <|unfolded-io>
      <goldfish-result|380585/1487509>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|multidimensional vectors>

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (define v (make-vector '(2 3) 1.0))
    <|unfolded-io>
      <goldfish-result|#2d((1.0 1.0 1.0) (1.0 1.0 1.0))>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (set! (v 0 1) 2.0)
    <|unfolded-io>
      <goldfish-result|2.0>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (v 0 1)
    <|unfolded-io>
      <goldfish-result|2.0>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (vector-length v)
    <|unfolded-io>
      <goldfish-result|6>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|hash-tables>

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (let ((ht (make-hash-table)))

      \ \ (set! (ht "hi") 123)

      \ \ (ht "hi"))
    <|unfolded-io>
      <goldfish-result|123>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (define test-tb (hash-table 'a 1 'b 2))
    <|unfolded-io>
      <goldfish-result|(hash-table 'b 2 'a 1)>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (hash-table? test-tb)
    <|unfolded-io>
      <goldfish-result|#t>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (hash-table-ref test-tb 'a)
    <|unfolded-io>
      <goldfish-result|1>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (hash-table-entries test-tb)
    <|unfolded-io>
      <goldfish-result|2>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|multiple-values>

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (+ (values 1 2 3) 4)
    <|unfolded-io>
      <goldfish-result|10>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (values 1 2 3)
    <|unfolded-io>
      <\errput>
        wrong-number-of-args

        goldfish-print: too many arguments: 2
      </errput>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      ((lambda (a b) (+ a b)) 1 2)
    <|unfolded-io>
      <goldfish-result|3>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      ((lambda (a b) (+ a b)) (values 1 2))
    <|unfolded-io>
      <goldfish-result|3>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|format>

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (object-\<gtr\>string "hiho")
    <|unfolded-io>
      <goldfish-result|""hiho"">
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (format #f "~S" "hiho")
    <|unfolded-io>
      <goldfish-result|""hiho"">
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (format #f "~A ~D ~F" 'hi 123 3.14)
    <|unfolded-io>
      <goldfish-result|"hi 123 3.140000">
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|eval>

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (eval '(+ 1 2))
    <|unfolded-io>
      <goldfish-result|3>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (eval-string "(+ 1 2)")
    <|unfolded-io>
      <goldfish-result|3>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|IO and other OS functions>

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (directory? "/tmp")
    <|unfolded-io>
      <goldfish-result|#t>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (file-exists? "/tmp")
    <|unfolded-io>
      <goldfish-result|#t>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (getenv "HOME")
    <|unfolded-io>
      <goldfish-result|"/home/da">
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|errors>

  <\session|goldfish|default>
    <\output>
      S7 Scheme 10.6 (14-Apr-2023)
    </output>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (abs 1 2)
    <|unfolded-io>
      <\errput>
        wrong-number-of-args

        abs: too many arguments: (abs 1 2)
      </errput>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (catch 'wrong-number-of-args

      \ \ \ \ (lambda () \ \ \ \ ; code protected by the catch

      \ \ \ \ \ \ (abs 1 2))

      \ \ \ \ (lambda args \ \ ; the error handler

      \ \ \ \ \ \ (apply format #t (cadr args))))
    <|unfolded-io>
      abs: too many arguments: (abs 1 2)<goldfish-result|"abs: too many
      arguments: (abs 1 2)">
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (catch 'division-by-zero

      \ \ \ \ (lambda () (/ 1.0 0.0))

      \ \ \ \ (lambda args (string-\<gtr\>number "+inf.0")))
    <|unfolded-io>
      <goldfish-result|+inf.0>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|autoload>

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (load "test.scm")
    <|unfolded-io>
      <\errput>
        io-error

        load: No such file or directory "test.scm"
      </errput>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|define-constant>

  <\session|goldfish|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (define v (immutable! (vector 1 2 3)))
    <|unfolded-io>
      <goldfish-result|#(1 2 3)>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (vector-set! v 0 23)
    <|unfolded-io>
      <\errput>
        immutable-error

        can't vector-set! #(1 2 3) (it is immutable)
      </errput>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (immutable? v)
    <|unfolded-io>
      <goldfish-result|#t>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (define-constant var 32)
    <|unfolded-io>
      <goldfish-result|32>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (set! var 1)
    <|unfolded-io>
      <\errput>
        immutable-error

        can't set! var (it is immutable)
      </errput>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (let ((var 1)) var)
    <|unfolded-io>
      <\errput>
        wrong-type-arg

        let: can't bind an immutable object: ((var 1))
      </errput>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <tmdoc-copyright|2024|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|par-hyphen|normal>
    <associate|preamble|false>
  </collection>
</initial>