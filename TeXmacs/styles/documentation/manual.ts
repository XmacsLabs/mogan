<TeXmacs|2.1.2>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|manual|1.0>

      <\src-purpose>
        The old manual style.
      </src-purpose>

      <\src-copyright|1998--2004>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|book>

  \;

  <assign|par-hyphen|professional>

  \;

  <assign|scheme|<with|font-shape|small-caps|Scheme>>

  <assign|pari|<with|font-shape|small-caps|Pari>>

  <assign|tmat|@>

  <assign|tmunsc|<with|font-family|tt|_>>

  \;

  <assign|tmdef|<macro|concept|<with|font-shape|italic|<concept>>>>

  <assign|tmref|<macro|concept|index|extra|<arg|concept>>>

  <assign|key|<macro|which|<block*|<tformat|<table|<row|<cell|<arg|which>>>>>>>>

  <assign|menu|<macro|name|<with|font-family|ss|<arg|name>>>>

  <assign|submenu|<macro|name|sub|<with|font-family|ss|<style-with|src-compact|none|<arg|name><with|mode|math|\<rightarrow\>><arg|sub>>>>>

  <assign|subsubmenu|<macro|name|sub|subsub|<with|font-family|ss|<style-with|src-compact|none|<arg|name><with|mode|math|\<rightarrow\>><arg|sub><with|mode|math|\<rightarrow\>><arg|subsub>>>>>

  \;

  <assign|verbatim|<macro|body|<style-with|src-compact|none|<surround|<vspace*|0.5fn><no-page-break*>|<vspace|0.5fn><no-indent*>|<with|font-family|tt|language|verbatim|<arg|body>>>>>>

  <assign|big-figure|<macro|fig|cap|<style-with|src-compact|none|<surround|<style-with|src-compact|none|<vspace*|1fn><no-indent><assign|figure-nr|<plus|<figure-nr>|1>><set-binding|<prefix><figure-nr>>>|<style-with|src-compact|none|<vspace|1fn><no-indent*>>|<tabular*|<tformat|<twith|table-width|1par>|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<table|<row|<cell|<arg|fig>>>|<row|<cell|>>|<row|<\cell>
    <with|font-size|0.84|<surround|<with|font-series|bold|<localize|Figure>
    <figure-nr>. >||<arg|cap>>>
  </cell>>>>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>