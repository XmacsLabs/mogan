<TeXmacs|2.1.2>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|physics|1.0.0>

    <\src-purpose>
      This package contains macros for physics
    </src-purpose>

    <src-copyright|2024|Darcy Shen>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(contrib physics physics-drd)>

  <assign|bra|<macro|x|<around*|\<langle\>|<arg|x>|\|>>>

  <assign|bra*|<macro|x|\<langle\><arg|x>\|>>

  <assign|ket|<macro|x|<around*|\||<arg|x>|\<rangle\>>>>

  <assign|ket*|<macro|x|\|<arg|x>\<rangle\>>>
</body>

<initial|<\collection>
</collection>>
