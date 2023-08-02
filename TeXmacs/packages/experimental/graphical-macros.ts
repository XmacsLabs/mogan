<TeXmacs|1.99.2>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|graphical-macros|1.0>

    <\src-purpose>
      Extra macros for graphics.
    </src-purpose>

    <src-copyright|2012|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(graphics graphics-markup)>

  <\active*>
    <\src-comment>
      Basic macros.
    </src-comment>
  </active*>

  <assign|rectangle|<macro|p1|p2|<extern|rectangle|<arg|p1>|<arg|p2>>>>

  <assign|circle|<macro|p1|p2|<extern|circle|<arg|p1>|<arg|p2>>>>

  <\active*>
    <\src-comment>
      Electrical.
    </src-comment>
  </active*>

  <assign|condensator|<macro|p1|p2|p3|<extern|condensator|<arg|p1>|<arg|p2>|<arg|p3>>>>

  <assign|diode|<macro|p1|p2|p3|<extern|diode|<arg|p1>|<arg|p2>|<arg|p3>>>>

  <assign|battery|<macro|p1|p2|p3|<extern|battery|<arg|p1>|<arg|p2>|<arg|p3>>>>

  <assign|resistor|<macro|p1|p2|p3|<extern|resistor|<arg|p1>|<arg|p2>|<arg|p3>>>>

  <assign|three-points-circle|<macro|p1|p2|p3|<extern|three-points-circle|<arg|p1>|<arg|p2>|<arg|p3>>>>

  <\active*>
    <\src-comment>
      Testing.
    </src-comment>
  </active*>

  <assign|triangle-with-text|<macro|p1|p2|p3|t|<extern|triangle-with-text|<arg|p1>|<arg|p2>|<arg|p3>|<quote-arg|t>>>>

  <drd-props|triangle-with-text|arity|4|accessible|3>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>