<TeXmacs|1.99.8>

<style|<tuple|source|std-pattern>>

<\body>
  <active*|<\src-title>
    <src-package-dtd|indent-paragraphs|1.0|indent-paragraphs|1.0>

    <\src-purpose>
      Use first indentation to mark new paragraphs
    </src-purpose>

    <src-copyright|1998--2018|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <if|<equal|<value|language>|chinese>|
    <assign|par-first|2fn>
  |
    <assign|par-first|<value|indent-par-first>>
    <assign|par-par-sep|0.5fns>
  >
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>