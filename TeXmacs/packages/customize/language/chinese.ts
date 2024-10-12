<TeXmacs|2.1.2>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|chinese|1.0>

    <\src-purpose>
      A style package for chinese language support
    </src-purpose>

    <\src-copyright|2013>
      Joris van der Hoeven
    </src-copyright>

    <\src-copyright|2020-2021>
      Darcy Shen
    </src-copyright>

    <\src-copyright|2023>
      Jingkaimori
    </src-copyright>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|language|chinese>

  <assign|font|sys-chinese>

  <use-module|(lang chinese-kbd)>

  <if|<greater|<value|par-first>|0fn>|<assign|par-first|2fn>>

  <assign|par-spacing|kaiming>

  <assign|render-small-figure|<macro|type|name|fig|cap|<if|<equal|<arg|type>|table>|<tabular*|<tformat|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<twith|table-valign|B>|<cwith|3|3|1|1|cell-hyphen|t>|<twith|table-width|1par>|<twith|table-hmode|min>|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|3|3|1|1|cell-lsep|0spc>|<cwith|3|3|1|1|cell-rsep|0spc>|<table|<row|<\cell>
    <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
      <arg|cap>
    </surround>>
  </cell>>|<row|<cell|>>|<\row>
    <resize|<arg|fig>|<minus|1l|2fn>||<plus|1r|2fn>|>
  </row>>>>|<tabular*|<tformat|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<twith|table-valign|B>|<cwith|3|3|1|1|cell-hyphen|t>|<twith|table-width|1par>|<twith|table-hmode|min>|<table|<row|<cell|<resize|<arg|fig>|<minus|1l|2fn>||<plus|1r|2fn>|>>>|<row|<cell|>>|<row|<\cell>
    <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
      <\html-div-class|float>
        <arg|cap>
      </html-div-class>
    </surround>>
  </cell>>>>>>>>

  <assign|render-big-figure|<\macro|type|name|fig|cap>
    <padded-normal|1fn|1fn|<if|<equal|<arg|type>|table>|<tabular*|<tformat|<twith|table-width|<value|figure-width>>|<cwith|1|-1|1|-1|cell-lsep|<value|figure-left-padding>>|<cwith|1|-1|1|-1|cell-rsep|<value|figure-right-padding>>|<cwith|2|2|1|1|cell-height|<value|figure-caption-sep>>|<cwith|3|3|1|1|cell-lsep|<value|figure-left-padding>>|<cwith|3|3|1|1|cell-rsep|<value|figure-right-padding>>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-lsep|<value|figure-left-padding>>|<cwith|1|1|1|1|cell-rsep|<value|figure-right-padding>>|<cwith|1|1|1|1|cell-lsep|<value|caption-left-padding>>|<cwith|1|1|1|1|cell-rsep|<value|caption-right-padding>>|<table|<\row>
      <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
        <arg|cap>
      </surround>>
    </row>|<row|<cell|>>|<row|<cell|<arg|fig>>>>>>|<tabular*|<tformat|<twith|table-width|<value|figure-width>>|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|<value|figure-left-padding>>|<cwith|1|-1|1|-1|cell-rsep|<value|figure-right-padding>>|<cwith|2|2|1|1|cell-height|<value|figure-caption-sep>>|<cwith|3|3|1|1|cell-lsep|<value|caption-left-padding>>|<cwith|3|3|1|1|cell-rsep|<value|caption-right-padding>>|<table|<row|<cell|<arg|fig>>>|<row|<cell|>>|<row|<\cell>
      <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
        <\html-div-class|float>
          <arg|cap>
        </html-div-class>
      </surround>>
    </cell>>>>>>>
  </macro>>

  <\active*>
    <\src-comment>
      Document in Chinese should indent every paragraph, though some
      mathematics documents in Latin align the first paragraph in section to
      the title of section.
    </src-comment>
  </active*>

  <assign|sectional-normal|<macro|name|<wide-normal|<arg|name><no-page-break>>>>

  <assign|sectional-centered|<macro|name|<wide-centered|<arg|name><no-page-break>>>>

  <\active*>
    <\src-comment>
      Customize doc-data macro
    </src-comment>
  </active*>

  <assign|by-text|>

  <\active*>
    <\src-comment>
      For Algorithm and Pseudo code blocks, do not translate the underlying
      keywords when documentation language is Chinese.

      \;

      Affected:

      Insert -\<gtr\> Program<compound|math|><compound|math|> -\<gtr\>
      Algorithm

      Insert -\<gtr\> Program -\<gtr\> Pseudo code
    </src-comment>
  </active*>

  <assign|render-keyword|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|dfn|<macro|body|<with|font-series|bold|math-font-series|bold|<arg|body>>>>

  <assign|cite*|<macro|body|<arg|body>>>

  <assign|render-theorem|<\macro|which|body>
    <render-enunciation|<theorem-name|<arg|which><theorem-sep>>|<arg|body>>
  </macro>>

  \;

  <assign|chapter-numbered-title|<macro|title|<style-with|src-compact|none|<chapter-title|\<#7B2C\>
  <the-chapter> <chapter-text> <vspace|1fn><new-line><arg|title>>>>>

  \;

  <assign|part-numbered-title|<macro|name|<part-title-sub|<htab|0fn>\<#7B2C\>
  <the-part> <part-text><htab|0fn><vspace|0.1pag><new-line><htab|0fn><arg|name><htab|0fn>>>>
</body>

<initial|<\collection>
</collection>>