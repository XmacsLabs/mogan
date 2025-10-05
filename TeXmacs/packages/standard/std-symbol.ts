<TeXmacs|2.1.4>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-symbol|1.0>

    <\src-purpose>
      Some additional symbols for text mode. This file should become obsolete
      when better support for Unicode will be implemented.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|cent|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tc|�>|<with|mode|text|font|tc|�>>>>>

  <assign|currency|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tc|�>|<with|mode|text|font|tc|�>>>>>

  <assign|yen|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tc|�>|<with|mode|text|font|tc|�>>>>>

  <assign|copyright|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tcx|�>|<with|mode|text|font|tcx|�>>>>>

  <assign|copyleft|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tcx|�>|<with|mode|text|font|tcx|�>>>>>

  <assign|registered|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tcx|�>|<with|mode|text|font|tcx|�>>>>>

  <assign|degreesign|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tc|�>|<with|mode|text|font|tc|�>>>>>

  <assign|twosuperior|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tc|�>|<with|mode|text|font|tc|�>>>>>

  <assign|threesuperior|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tc|�>|<with|mode|text|font|tc|�>>>>>

  <assign|onesuperior|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tc|�>|<with|mode|text|font|tc|�>>>>>

  <assign|mu|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tcx|�>|<with|mode|text|font|tcx|�>>>>>

  <assign|paragraphsign|<active*|<macro|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tc|�>|<with|mode|text|font|tc|�>>>>>

  <assign|onequarter|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tc|�>|<with|mode|text|font|tc|�>>>>>

  <assign|onehalf|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tc|�>|<with|mode|text|font|tc|�>>>>>

  <assign|threequarters|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tc|�>|<with|mode|text|font|tc|�>>>>>

  <assign|euro|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tcx|�>|<with|mode|text|font|tcx|�>>>>>

  <assign|trademark|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|mode|text|font|tcx|�>|<with|mode|text|font|tcx|�>>>>>

  <assign|emdash|<macro|<active*|<if|<equal|<value|mode>|math>|<with|mode|text|font|roman|\V>|<with|font|roman|\V>>>>>

  <assign|masculine|<active*|<rsup|<wide*|o|\<wide-bar\>>>>>

  <assign|ordfeminine|<active*|<rsup|<wide*|a|\<wide-bar\>>>>>

  <assign|varmasculine|<active*|<rsup|o>>>

  <assign|varordfeminine|<active*|<rsup|a>>>

  <assign|textless|\<less\>>

  <assign|textgreater|\<gtr\>>

  <assign|textbar|\|>

  <assign|textasciitilde|~>

  <assign|iint|<macro|<big|iint>>>

  <assign|iiint|<macro|<big|iiint>>>

  <assign|oint|<macro|<big|oint>>>

  <assign|oiint|<macro|<big|oiint>>>

  <assign|idotsint|<macro|<big|idotsint>>>

  <assign|nbsp|<macro| <no-break><specific|screen|<resize|<move|<with|color|#A0A0FF|->|-0.3em|>|0em||0em|>>>>

  <assign|nbhyph|<macro|-<no-break>>>

  <assign|boxed|<macro|x|<ornament|<arg|x>>>>

  <assign|textquotesingle|'>

  <assign|mathscr|<macro|body|<with|font|cal*|<arg|body>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>
