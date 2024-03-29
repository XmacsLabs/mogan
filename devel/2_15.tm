<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|devel>>

<\body>
  <tmdoc-title|Optimize generated s7 glue>

  <section|Feature Metadata>

  <\description>
    <item*|Owner>jingkaimori
  </description>

  <section|Description>

  <subsection|What>

  <subsection|Why>

  Currently scheme binding(glue) is registerd by <cpp|s7_define>. S7 would
  unbox <cpp|s7_pointer> when executing these glue. If glue is registered by
  <verbatim|s7_set_x_x_function>, s7 can skip unnecessary unbox to improve
  performance.

  <\cpp-code>
    <\code>
      static s7_int plus_one(s7_int x) {return(x + 1);}

      \;

      s7_set_i_i_function(sc, s7_name_to_value(sc, "plus1"), plus_one);

      \;

      // equivent define, but slower:

      <\code>
        static s7_pointer g_plus_one(s7_scheme *sc, s7_pointer args)\ 

        {

        \ \ return(s7_make_integer(sc, s7_integer(s7_car(args)) + 1));

        }
      </code>

      <code|s7_define(s7, "plus1", g_plus_one, 1, 0, false, "[missing
      doc]"))>
    </code>
  </cpp-code>

  <subsection|How>

  <tmdoc-copyright|2023|jingkaimori>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>