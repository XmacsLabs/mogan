<TeXmacs|2.1.4>

<style|<tuple|book|no-page-numbers|chinese|doc>>

<\body>
  <\hide-preamble>
    <assign|dlink|<macro|x|<arg|x>>>
  </hide-preamble>

  <part*|[3] xmake\<#96C6\>\<#6210\>><part*|[2] S7\<#96C6\>\<#6210\>>

  <\wide-tabular>
    <tformat|<cwith|1|16|2|2|cell-background|pastel
    green>|<cwith|17|17|2|2|cell-background|pastel green>|<table|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_1
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Initial import of Massimiliano Gubinelli's work on S7 Scheme
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_2
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Deprecate Tiny Scheme integration
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_3
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Deprecate GNU Guile integration
    </cell>>|<row|<\cell>
      Bug
    </cell>|<\cell>
      <dlink|2_4>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: procedure-symbol-name
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_5
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: disable benchmark-menu-expend
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_6
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: fix with-global by mgubi
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_7
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: Fix hang of shortcut editor
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_8
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: activate keyboard in chinese doc
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_9
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: Fix Bibtex related S7 Scheme bugs
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_10
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: implement iota
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_11
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: adopt hex character literal
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_12
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: compute-interactive-args failed for lazy-define, just use-modules
      to fix doc search
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <dlink|2_13>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: upgrade from 9.12 to 10.5
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <dlink|2_14>
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Use lua to generate s7 glue
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <dlink|2_15>
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Optimize generated s7 glue
    </cell>>|<row|<\cell>
      Bug
    </cell>|<\cell>
      <dlink|2_16>
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Use procedures defined in S7 to look up metadata instead of procedures
      defined in Guile.
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_17
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Remove legacy glue generator based on guile
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_18
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: fix unbound variable <scm|custom-keyboard-toolbar>
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      2_19
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7: fix unbound variable <scm|get-keyboard>
    </cell>>>>
  </wide-tabular>

  \;

  <chapter*|[2_19]>

  \<#9519\>\<#8BEF\>\<#4FE1\>\<#606F\>\<#FF1A\>

  <\scm-code>
    ;unbound variable get-keyboard

    ; \ \ \ (cond ((tree? x) (if (tree? y) (== x...

    ; \ \ \ /home/da/git/LiiiSTEM/TeXmacs/progs/check/check-master.scm, line
    24, position: 922

    ; (cond ((tree? x) (if (tree? y) (== x y) (...

    ; ((get-the-keyboard))

    ; (($texmacs-output (list-values 'with "bg-...

    ; (($list ($menu-invisible (get-the-keyboar...
  </scm-code>

  <chapter*|[2_18]>

  \<#9519\>\<#8BEF\>\<#4FE1\>\<#606F\>\<#FF1A\>

  <\scm-code>
    ;unbound variable custom-keyboard-toolbar

    ; \ \ \ ($dynamic$impl w)

    ; \ \ \ /home/da/git/LiiiSTEM/TeXmacs/progs/database/db-menu.scm, line
    16, position: 636

    ; ($dynamic$impl w)

    ; ((custom-keyboard-toolbar))

    ; (($dynamic (custom-keyboard-toolbar)))

    ; (($list ($delayed-when (or (extra-bottom-... ; tools: ()
  </scm-code>

  \<#8FD9\>\<#4E2A\>\<#9519\>\<#8BEF\>\<#6BD4\>\<#8F83\>\<#660E\>\<#663E\>\<#FF0C\>\<#53EA\>\<#8981\>\<#66F4\>\<#65B0\><shell|init-texmacs-s7.scm>\<#5373\>\<#53EF\>\<#3002\><chapter*|[2_4]>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: Yiqi Xu on <hlink|Gitee|https://gitee.com/XmacsLabs/mogan/issues/I6R51R>

    <item>Owner: Darcy Shen

    <item>Tester: Darcy Shen
  </itemize>

  <section|How to reproduce it>

  <\enumerate>
    <item>Launch Mogan via <shell|<code|<code*|/usr/bin/mogan>>> in terminal
    on GNU/Linux

    <item>Click <menu|Format|Whitespace|Stretchable>
  </enumerate>

  \;

  Here is the error message:

  <\scm-code>
    ;procedure-source argument, #f, is boolean but should be a procedure or a
    macro

    ; \ \ \ (procedure-source fun)

    ; \ \ \ /share/Xmacs/progs/kernel/texmacs/tm-dialogue.scm, line 226,
    position: 7448

    ; compute-interactive-args-try-hard: (proce... ; fun: #f

    ; ((with fun-args (build-interactive-args f... ; args: ()

    ; make-menu-entry-button: (protected-call (... ; style: 0
  </scm-code>

  <section|Description>

  <subsection|The root cause>

  It is related to the migration from GNU Guile to S7 Scheme.

  The root cause of this bug is that the result of <scm|(procedure-name
  make-space)> is <scm|#f>.

  Here is the expected result

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (procedure-name make-space)
    <|unfolded-io>
      make-space
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <subsection|How to fix it>

  Based on the fact that, for all glued symbols, it must starts with alpha:

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (filter\ 

      \ (lambda (x) (not x))

      \ (map

      \ \ (lambda (x) (string-alpha? (string-take x 1)))

      \ \ (all-glued-symbols)))
    <|unfolded-io>
      ()
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  That's the reason why I made this fix.

  <subsection|How to test it>

  <\session|scheme|default>
    <\input|Scheme] >
      (regtest-tm-define)
    </input>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <part*|[1] \<#65E0\>\<#7528\>\<#4EE3\>\<#7801\>\<#6E05\>\<#7406\>>

  <\wide-tabular>
    <tformat|<cwith|1|1|1|-1|cell-tborder|0ln>|<cwith|1|1|1|-1|cell-bborder|0ln>|<cwith|2|2|1|-1|cell-tborder|0ln>|<cwith|1|1|1|1|cell-lborder|0ln>|<cwith|1|1|2|2|cell-rborder|0ln>|<cwith|1|1|1|1|cell-halign|l>|<cwith|39|39|1|1|cell-valign|b>|<table|<row|<\cell>
      1_1
    </cell>|<\cell>
      Remove the support of Autotools
    </cell>>|<row|<\cell>
      1_2
    </cell>|<\cell>
      Remove the support of CMake
    </cell>>|<row|<\cell>
      1_3
    </cell>|<\cell>
      Remove the Cocoa integration
    </cell>>|<row|<\cell>
      1_4
    </cell>|<\cell>
      Remove the Widkit integration
    </cell>>|<row|<\cell>
      1_5
    </cell>|<\cell>
      Remove the X11 integration
    </cell>>|<row|<\cell>
      1_6
    </cell>|<\cell>
      Remove the Matlab plugin
    </cell>>|<row|<\cell>
      1_7
    </cell>|<\cell>
      Remove the Mathematica plugin
    </cell>>|<row|<\cell>
      1_8
    </cell>|<\cell>
      Remove the Coq plugin
    </cell>>|<row|<\cell>
      1_9
    </cell>|<\cell>
      Remove the Maple plugin
    </cell>>|<row|<\cell>
      1_10
    </cell>|<\cell>
      Remove the plugins implemented in Python
    </cell>>|<row|<\cell>
      1_11
    </cell>|<\cell>
      Remove packager scripts for un-supported OS\ 
    </cell>>|<row|<\cell>
      1_12
    </cell>|<\cell>
      Remove the lush plugin
    </cell>>|<row|<\cell>
      1_13
    </cell>|<\cell>
      Remove the equation_editor plugin
    </cell>>|<row|<\cell>
      1_14
    </cell>|<\cell>
      Remove the R plugin
    </cell>>|<row|<\cell>
      1_15
    </cell>|<\cell>
      Remove the Axiom plugin
    </cell>>|<row|<\cell>
      1_16
    </cell>|<\cell>
      Remove the Libertine font
    </cell>>|<row|<\cell>
      1_17
    </cell>|<\cell>
      Remove the python-implemented example plugins
    </cell>>|<row|<\cell>
      1_18
    </cell>|<\cell>
      Remove the example texts
    </cell>>|<row|<\cell>
      1_19
    </cell>|<\cell>
      Remove the feynmf plugin
    </cell>>|<row|<\cell>
      1_20
    </cell>|<\cell>
      Remove the Scilab plugin
    </cell>>|<row|<\cell>
      1_21
    </cell>|<\cell>
      Remove the texgraph plugin
    </cell>>|<row|<\cell>
      1_22
    </cell>|<\cell>
      Remove the lisp plugin
    </cell>>|<row|<\cell>
      1_23
    </cell>|<\cell>
      Remove the reduce plugin
    </cell>>|<row|<\cell>
      1_24
    </cell>|<\cell>
      Remove the mupad plugin
    </cell>>|<row|<\cell>
      1_25
    </cell>|<\cell>
      Remove the cococa5 plugin
    </cell>>|<row|<\cell>
      1_26
    </cell>|<\cell>
      Remove the yacas plugin
    </cell>>|<row|<\cell>
      1_27
    </cell>|<\cell>
      Remove mathemagix related plugins
    </cell>>|<row|<\cell>
      1_28
    </cell>|<\cell>
      Remove the mycas plugin
    </cell>>|<row|<\cell>
      1_29
    </cell>|<\cell>
      Remove the qcl plugin
    </cell>>|<row|<\cell>
      1_30
    </cell>|<\cell>
      Remove the macaulay2 plugin
    </cell>>|<row|<\cell>
      1_31
    </cell>|<\cell>
      Remove the gtybalt plugin
    </cell>>|<row|<\cell>
      1_32
    </cell>|<\cell>
      Remove the Axel C++ interface
    </cell>>|<row|<\cell>
      1_33
    </cell>|<\cell>
      Remove the Cairo C++ interface
    </cell>>|<row|<\cell>
      1_34
    </cell>|<\cell>
      Remove the Imlib2 C++ interface
    </cell>>|<row|<\cell>
      1_35
    </cell>|<\cell>
      Remove the Fira fonts
    </cell>>|<row|<\cell>
      1_36
    </cell>|<\cell>
      Remove the Mplayer C++ interface
    </cell>>|<row|<\cell>
      1_37
    </cell>|<\cell>
      Remove docs in plain text for GNU <TeXmacs>
    </cell>>|<row|<\cell>
      1_38
    </cell>|<\cell>
      Remove the texgyre fonts
    </cell>>|<row|<\cell>
      1_39
    </cell>|<\cell>
      Remove the Stix fonts
    </cell>>|<row|<\cell>
      1_40
    </cell>|<\cell>
      Remove the Sqlite3 C++ interface
    </cell>>|<row|<\cell>
      1_41
    </cell>|<\cell>
      Remove <shell|misc/admin>
    </cell>>|<row|<\cell>
      1_42
    </cell>|<\cell>
      Remove the GNU <TeXmacs> packager for Windows
    </cell>>|<row|<\cell>
      1_43
    </cell>|<\cell>
      Remove the shell plugin
    </cell>>|<row|<\cell>
      1_44
    </cell>|<\cell>
      Remove the giac plugin
    </cell>>|<row|<\cell>
      1_45
    </cell>|<\cell>
      clean up for src/Graphics/Mathematics
    </cell>>|<row|<\cell>
      <todo|1_46>
    </cell>|<\cell>
      remove experimental style code
    </cell>>|<row|<\cell>
      1_47
    </cell>|<\cell>
      Remove the Cadabra plugin
    </cell>>|<row|<\cell>
      1_48
    </cell>|<\cell>
      Remove the pari plugin
    </cell>>|<row|<\cell>
      1_49
    </cell>|<\cell>
      Remove the fricas plugin
    </cell>>|<row|<\cell>
      1_50
    </cell>|<\cell>
      Remove the style package for doxygen support
    </cell>>>>
  </wide-tabular>
</body>

<\initial>
  <\collection>
    <associate|page-screen-margin|false>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|?|?>>
    <associate|auto-10|<tuple|3.1|?>>
    <associate|auto-11|<tuple|3.2|?>>
    <associate|auto-12|<tuple|3.3|?>>
    <associate|auto-13|<tuple|3.3|?>>
    <associate|auto-2|<tuple|?|?>>
    <associate|auto-3|<tuple|?|?>>
    <associate|auto-4|<tuple|?|?>>
    <associate|auto-5|<tuple|?|?>>
    <associate|auto-6|<tuple|1|?>>
    <associate|auto-7|<tuple|2|?>>
    <associate|auto-8|<tuple|2|?>>
    <associate|auto-9|<tuple|3|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|ss>|\<#683C\>\<#5F0F\>>|<with|font-family|<quote|ss>|\<#7A7A\>\<#683C\>>|<with|font-family|<quote|ss>|\<#53EF\>\<#62C9\>\<#4F38\>>>|<pageref|auto-5>>
    </associate>
    <\associate|toc>
      <vspace*|2fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|font-size|<quote|1.19>|[3]
      xmake\<#96C6\>\<#6210\>> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|1fn>

      <vspace*|2fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|font-size|<quote|1.19>|[2]
      S7\<#96C6\>\<#6210\>> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|1fn>

      1<space|2spc>Bug Metadata <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3>

      2<space|2spc>How to reproduce it <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4>

      3<space|2spc>Description <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-6>

      <with|par-left|<quote|1tab>|3.1<space|2spc>The root cause
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-7>>

      <with|par-left|<quote|1tab>|3.2<space|2spc>How to fix it
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-8>>

      <with|par-left|<quote|1tab>|3.3<space|2spc>How to test it
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-9>>

      <vspace*|2fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|font-size|<quote|1.19>|[1]
      \<#65E0\>\<#7528\>\<#4EE3\>\<#7801\>\<#6E05\>\<#7406\>>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-10><vspace|1fn>
    </associate>
  </collection>
</auxiliary>