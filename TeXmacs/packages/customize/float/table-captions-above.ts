<TeXmacs|2.1.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|table-captions-above|1.0>

    <\src-purpose>
      Place captions above tables
    </src-purpose>

    <src-copyright|2014|Miguel de Benito Delgado>

    <src-copyright|2019|Darcy Shen>

    <src-copyright|2022|Joris van der Hoeven>

    <src-copyright|2025|Yuki Lu>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license v3|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you don't have this file, then write to the Free
      Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
      02110-1301, USA.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Save original environments.
    </src-comment>
  </active*>

  <assign|render-small-table-captions-below|<value|render-small-table>>

  <assign|render-big-table-captions-below|<value|render-big-table>>

  <\active*>
    <\src-comment>
      New rendering environments.
    </src-comment>
  </active*>

  <assign|render-small-table-captions-above|<macro|type|name|fig|cap|<tabular*|<tformat|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<twith|table-valign|B>|<cwith|3|3|1|1|cell-hyphen|t>|<twith|table-width|1par>|<twith|table-hmode|min>|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|3|3|1|1|cell-lsep|0spc>|<cwith|3|3|1|1|cell-rsep|0spc>|<table|<row|<\cell>
    <render-caption|<arg|type>|<arg|name>|<arg|cap>>
  </cell>>|<row|<cell|>>|<\row>
    <resize|<arg|fig>|<minus|1l|2fn>||<plus|1r|2fn>|>
  </row>>>>>>

  <assign|render-big-table-captions-above|<\macro|type|name|fig|cap>
    <padded-normal|1fn|1fn|<tabular*|<tformat|<twith|table-width|<value|figure-width>>|<cwith|1|-1|1|-1|cell-lsep|<value|figure-left-padding>>|<cwith|1|-1|1|-1|cell-rsep|<value|figure-right-padding>>|<cwith|2|2|1|1|cell-height|<value|figure-caption-sep>>|<cwith|3|3|1|1|cell-lsep|<value|figure-left-padding>>|<cwith|3|3|1|1|cell-rsep|<value|figure-right-padding>>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-lsep|<value|figure-left-padding>>|<cwith|1|1|1|1|cell-rsep|<value|figure-right-padding>>|<cwith|1|1|1|1|cell-lsep|<value|caption-left-padding>>|<cwith|1|1|1|1|cell-rsep|<value|caption-right-padding>>|<table|<\row>
      <render-caption|<arg|type>|<arg|name>|<arg|cap>>
    </row>|<row|<cell|>>|<row|<cell|<arg|fig>>>>>>>
  </macro>>

  \;

  <assign|render-small-table|<value|render-small-table-captions-above>>

  <assign|render-big-table|<value|render-big-table-captions-above>>

  <\active*>
    <\src-comment>
      Extra hooks for controlling whether we want captions above or below for
      specific environments.
    </src-comment>
  </active*>

  <assign|table-captions-above|true>

  <assign|small-table-default-caps|<value|small-table>>

  <assign|big-table-default-caps|<value|big-table>>

  <assign|small-table|<macro|fig|cap|<with|render-small-table|<if|<equal|<value|table-captions-above>|true>|<value|render-small-table-captions-above>|<value|render-small-table-captions-below>>|<small-table-default-caps|<arg|fig>|<arg|cap>>>>>

  <assign|big-table|<\macro|fig|cap>
    <\with|render-big-table|<if|<equal|<value|table-captions-above>|true>|<value|render-big-table-captions-above>|<value|render-big-table-captions-below>>>
      <big-table-default-caps|<arg|fig>|<arg|cap>>
    </with>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>