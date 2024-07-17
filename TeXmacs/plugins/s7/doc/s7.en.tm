<TeXmacs|2.1.2>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths|s7>>

<\body>
  <tmdoc-title|The S7 Scheme plugin>

  <hlink|S7|https://ccrma.stanford.edu/software/s7/> is a Scheme interpreter.
  To insert a S7 Scheme session, just click <menu|Insert|Session|S7 Scheme>.
  The Scheme session is for interacting with Mogan and the S7 Scheme session
  is for interacting with S7 Scheme. S7 Scheme session is running in a
  separated process, as a result, infinite loop in S7 Scheme session will not
  freeze Mogan while infinite loop in Scheme session will freeze Mogan.

  Here are snippets which only works in the S7 Scheme session:

  <paragraph|unicode support>

  The string literal in S7 Scheme does not support Unicode. In the S7 Scheme
  session, we use the cork encoding as a workaround.

  <\scm-code>
    <block|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<table|<row|<cell|Scheme
    Code>|<cell|Actual Scheme Code>>|<row|<cell|'\<#4E2D\>\<#6587\>>|<cell|'\<less\>#4E2D\<gtr\>\<less\>#6587\<gtr\>>>|<row|<cell|(length
    "\<#4E2D\>\<#6587\>")>|<cell|(length "\<less\>#4E2D\<gtr\>\<less\>#6587\<gtr\>")>>>>>
  </scm-code>

  <\session|s7|default>
    <\output>
      S7 Scheme 10.6 (14-Apr-2023)
    </output>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      '\<#4E2D\>\<#6587\>
    <|unfolded-io>
      <s7-result|\<#4E2D\>\<#6587\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      '\<less\>#4E2D\<gtr\>\<less\>#6587\<gtr\>
    <|unfolded-io>
      <s7-result|\<#4E2D\>\<#6587\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (length "\<#4E2D\>\<#6587\>")
    <|unfolded-io>
      <s7-result|14>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (length "\<less\>#4E2D\<gtr\>\<less\>#6587\<gtr\>")
    <|unfolded-io>
      <s7-result|14>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|special rules for rendering>

  For Scheme snippets starting with the markup
  <markup|document>\<#3001\><markup|math>\<#3001\><markup|equation*>\<#3001\><markup|align>\<#3001\><markup|with>\<#3001\><markup|graphics>:

  <\session|s7|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      `(document (frac "\<#5206\>\<#5B50\>" "\<#5206\>\<#6BCD\>"))
    <|unfolded-io>
      <s7-result|(document (frac "\<#5206\>\<#5B50\>" "\<#5206\>\<#6BCD\>"))>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      `(math (frac "1" "2"))
    <|unfolded-io>
      <s7-result|(math (frac "1" "2"))>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      `(with "color" "red" "Hello")
    <|unfolded-io>
      <s7-result|(with "color" "red" "Hello")>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|SRFI>

  SRFI implementation for S7 Scheme will be built-in for the S7 Scheme
  session. Here is the concise guide to load the SRFIs.

  <\wide-tabular>
    <tformat|<table|<row|<\cell>
      <hlink|R7RS|$TEXMACS_PATH/plugins/s7/scheme/case-lambda.scm>
    </cell>|<\cell>
      case-lambda
    </cell>|<\cell>
      <scm|(import (scheme case-lambda))>
    </cell>>|<row|<\cell>
      <hlink|SRFI 1|$TEXMACS_PATH/plugins/s7/srfi/srfi-1.scm>
    </cell>|<\cell>
      List Library
    </cell>|<\cell>
      <scm|(import (srfi srfi-1))>
    </cell>>|<row|<\cell>
      <hlink|SRFI 8|$TEXMACS_PATH/plugins/s7/srfi/srfi-8.scm>
    </cell>|<\cell>
      receive: Binding to multiple values
    </cell>|<\cell>
      <scm|(import (srfi srfi-8))>
    </cell>>|<row|<\cell>
      <hlink|SRFI 9|$TEXMACS_PATH/plugins/s7/srfi/srfi-9.scm>
    </cell>|<\cell>
      Defining Record Types
    </cell>|<\cell>
      <scm|(import (srfi srfi-9))>
    </cell>>|<row|<\cell>
      <hlink|SRFI 16|$TEXMACS_PATH/plugins/s7/srfi/srfi-16.scm>
    </cell>|<\cell>
      Syntax for procedures of variable arity
    </cell>|<\cell>
      <scm|(import (srfi srfi-16))>
    </cell>>|<row|<\cell>
      <hlink|SRFI 39|$TEXMACS_PATH/plugins/s7/srfi/srfi-39.scm>
    </cell>|<\cell>
      Parameter objects
    </cell>|<\cell>
      <scm|(import (srfi srfi-39))>
    </cell>>|<row|<\cell>
      <hlink|SRFI 78|$TEXMACS_PATH/plugins/s7/srfi/srfi-78.scm>
    </cell>|<\cell>
      Lightweight testing
    </cell>|<\cell>
      <scm|(import (srfi srfi-78))>
    </cell>>>>
  </wide-tabular>

  <\session|s7|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (import (srfi srfi-1))
    <|unfolded-io>
      <s7-result|(rootlet)>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (reduce + 0 (list 1 2 3))
    <|unfolded-io>
      <s7-result|6>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (import (srfi srfi-78))
    <|unfolded-io>
      <s7-result|(rootlet)>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (check (+ 1 2) =\<gtr\> 3)
    <|unfolded-io>
      \;

      (+ 1 2) =\<gtr\> 3 ; correct

      <s7-result|1>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  Here are code snippets from <slink|https://ccrma.stanford.edu/software/s7/s7.html>.

  <paragraph|help>

  <\session|s7|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (help 'equal?)
    <|unfolded-io>
      <s7-result|"(equal? obj1 obj2) returns #t if obj1 is equal to obj2">
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (help 'cdar)
    <|unfolded-io>
      <s7-result|"(cdar lst) returns (cdr (car lst)): (cdar '((1 2 3))) -\>
      '(2 3)">
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|equality>

  <\session|s7|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (eq? () ())
    <|unfolded-io>
      <s7-result|#t>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (eq? 2 2.0)
    <|unfolded-io>
      <s7-result|#f>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (equal? 2 2.0)
    <|unfolded-io>
      <s7-result|#f>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (equivalent? 2 2.0)
    <|unfolded-io>
      <s7-result|#t>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (equivalent? .1 1/10)
    <|unfolded-io>
      <s7-result|#t>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|multiprecision arithmetic>

  <\session|s7|default>
    <\output>
      S7 Scheme 10.6 (14-Apr-2023)
    </output>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      pi
    <|unfolded-io>
      <s7-result|pi>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (*s7* 'bignum-precision)
    <|unfolded-io>
      <s7-result|128>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (set! (*s7* 'bignum-precision) 256)
    <|unfolded-io>
      <s7-result|256>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      pi
    <|unfolded-io>
      <s7-result|pi>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (bignum "123456789123456789")
    <|unfolded-io>
      <s7-result|123456789123456789>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (bignum "1.123123123123123123123123123")
    <|unfolded-io>
      <s7-result|1.1231231231231231>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|math functions>

  <\session|s7|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (exact? 1.0)
    <|unfolded-io>
      <s7-result|#f>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (rational? 1.5)
    <|unfolded-io>
      <s7-result|#f>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (floor 1.4)
    <|unfolded-io>
      <s7-result|1>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (remainder 2.4 1)
    <|unfolded-io>
      <s7-result|0.3999999999999999>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (modulo 1.4 1.0)
    <|unfolded-io>
      <s7-result|0.3999999999999999>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (lcm 3/4 1/6)
    <|unfolded-io>
      <s7-result|3/2>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (log 8 2)
    <|unfolded-io>
      <s7-result|3>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (number-\<gtr\>string 0.5 2)
    <|unfolded-io>
      <s7-result|"0.1">
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (string-\<gtr\>number "0.1" 2)
    <|unfolded-io>
      <s7-result|0.5>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (rationalize 1.5)
    <|unfolded-io>
      <s7-result|3/2>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (complex 1/2 0)
    <|unfolded-io>
      <s7-result|1/2>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (logbit? 6 1)
    <|unfolded-io>
      <s7-result|#t>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (random 0)
    <|unfolded-io>
      <s7-result|0>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (random 1.0)
    <|unfolded-io>
      <s7-result|0.4550541097362356>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (random 3/4)
    <|unfolded-io>
      <s7-result|380585/1487509>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|multidimensional vectors>

  <\session|s7|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (define v (make-vector '(2 3) 1.0))
    <|unfolded-io>
      <s7-result|#2d((1.0 1.0 1.0) (1.0 1.0 1.0))>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (set! (v 0 1) 2.0)
    <|unfolded-io>
      <s7-result|2.0>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (v 0 1)
    <|unfolded-io>
      <s7-result|2.0>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (vector-length v)
    <|unfolded-io>
      <s7-result|6>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|hash-tables>

  <\session|s7|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (let ((ht (make-hash-table)))

      \ \ (set! (ht "hi") 123)

      \ \ (ht "hi"))
    <|unfolded-io>
      <s7-result|123>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (define test-tb (hash-table 'a 1 'b 2))
    <|unfolded-io>
      <s7-result|(hash-table 'b 2 'a 1)>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (hash-table? test-tb)
    <|unfolded-io>
      <s7-result|#t>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (hash-table-ref test-tb 'a)
    <|unfolded-io>
      <s7-result|1>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (hash-table-entries test-tb)
    <|unfolded-io>
      <s7-result|2>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|multiple-values>

  <\session|s7|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (+ (values 1 2 3) 4)
    <|unfolded-io>
      <s7-result|10>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (values 1 2 3)
    <|unfolded-io>
      <\errput>
        wrong-number-of-args

        s7-print: too many arguments: 2
      </errput>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      ((lambda (a b) (+ a b)) 1 2)
    <|unfolded-io>
      <s7-result|3>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      ((lambda (a b) (+ a b)) (values 1 2))
    <|unfolded-io>
      <s7-result|3>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|format>

  <\session|s7|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (object-\<gtr\>string "hiho")
    <|unfolded-io>
      <s7-result|""hiho"">
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (format #f "~S" "hiho")
    <|unfolded-io>
      <s7-result|""hiho"">
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (format #f "~A ~D ~F" 'hi 123 3.14)
    <|unfolded-io>
      <s7-result|"hi 123 3.140000">
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|eval>

  <\session|s7|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (eval '(+ 1 2))
    <|unfolded-io>
      <s7-result|3>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (eval-string "(+ 1 2)")
    <|unfolded-io>
      <s7-result|3>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|IO and other OS functions>

  <\session|s7|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (directory? "/tmp")
    <|unfolded-io>
      <s7-result|#t>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (file-exists? "/tmp")
    <|unfolded-io>
      <s7-result|#t>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (getenv "HOME")
    <|unfolded-io>
      <s7-result|"/home/da">
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|errors>

  <\session|s7|default>
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
      abs: too many arguments: (abs 1 2)<s7-result|"abs: too many arguments:
      (abs 1 2)">
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (catch 'division-by-zero

      \ \ \ \ (lambda () (/ 1.0 0.0))

      \ \ \ \ (lambda args (string-\<gtr\>number "+inf.0")))
    <|unfolded-io>
      <s7-result|+inf.0>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <paragraph|autoload>

  <\session|s7|default>
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

  <\session|s7|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (define v (immutable! (vector 1 2 3)))
    <|unfolded-io>
      <s7-result|#(1 2 3)>
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
      <s7-result|#t>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (define-constant var 32)
    <|unfolded-io>
      <s7-result|32>
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