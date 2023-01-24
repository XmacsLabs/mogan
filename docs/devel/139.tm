<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|comment>>

<\body>
  <tmdoc-title|Auto small spaces between Chinese text and other structure>

  <section|Feature metadata>

  <\itemize>
    <item>Reporter: Jade on <hlink|Gitee|https://gitee.com/XmacsLabs/mogan/issues/I6B6AH>

    <item>Owner: Darcy Shen
  </itemize>

  <section|Description>

  中文和拉丁字母ABC之间的间隔太小，加空格 ABC 又太大。

  中文和数学公式<math|y=f<around*|(|x|)>>之间的间隔也太小，加空格
  <math|y=f<around*|(|x|)>> 太大。

  中文与其他行内结构，比如<cpp|int
  main()>之间的问题类似。

  \;

  <unfolded-comment|+Hh8Wq0T1p7qzE1Q|+Hh8Wq0T1p7qzE1R|comment|Jade|1674568692||<strong|用0.6spc的效果感觉比较舒服（比<LaTeX>好）>>

  中文和拉丁字母<space|0.6spc>ABC<space|0.6spc>之间的间隔

  中文和数学公式<space|0.6spc><math|y=f<around*|(|x|)>><space|0.6spc>之间的间隔

  中文和其他行内结构<space|0.6spc><cpp|int
  main()><space|0.6spc>之间的间隔

  <section|Detailed Description>

  <subsection|Line Break with different hard-coded spaces>

  <tabular|<tformat|<cwith|2|11|5|5|cell-hyphen|t>|<cwith|1|11|5|5|cell-width|60pt>|<cwith|1|11|5|5|cell-hmode|exact>|<cwith|2|11|6|6|cell-hyphen|t>|<cwith|1|-1|6|6|cell-width|65pt>|<cwith|1|-1|6|6|cell-hmode|exact>|<cwith|2|11|3|3|cell-hyphen|t>|<cwith|1|11|3|3|cell-width|50pt>|<cwith|1|11|3|3|cell-hmode|exact>|<cwith|2|11|4|4|cell-hyphen|t>|<cwith|1|-1|4|4|cell-width|55pt>|<cwith|1|-1|4|4|cell-hmode|exact>|<table|<row|<cell|>|<cell|no
  wrap>|<cell|50pt>|<cell|55pt>|<cell|60pt>|<cell|65pt>>|<row|<cell|0.1spc>|<cell|中文和<space|0.1spc>ABC<space|0.1spc>的间隔>|<\cell>
    中文和<space|0.1spc>ABC<space|0.1spc>的间隔
  </cell>|<\cell>
    中文和<space|0.1spc>ABC<space|0.1spc>的间隔
  </cell>|<\cell>
    中文和<space|0.1spc>ABC<space|0.1spc>的间隔
  </cell>|<\cell>
    中文和<space|0.1spc>ABC<space|0.1spc>的间隔
  </cell>>|<row|<cell|0.2spc>|<cell|中文和<space|0.2spc>ABC<space|0.2spc>的间隔>|<\cell>
    中文和<space|0.2spc>ABC<space|0.2spc>的间隔
  </cell>|<\cell>
    中文和<space|0.2spc>ABC<space|0.2spc>的间隔
  </cell>|<\cell>
    中文和<space|0.2spc>ABC<space|0.2spc>的间隔
  </cell>|<\cell>
    中文和<space|0.2spc>ABC<space|0.2spc>的间隔
  </cell>>|<row|<cell|0.3spc>|<cell|中文和<space|0.3spc>ABC<space|0.3spc>的间隔>|<\cell>
    中文和<space|0.3spc>ABC<space|0.3spc>的间隔
  </cell>|<\cell>
    中文和<space|0.3spc>ABC<space|0.3spc>的间隔
  </cell>|<\cell>
    中文和<space|0.3spc>ABC<space|0.3spc>的间隔
  </cell>|<\cell>
    中文和<space|0.3spc>ABC<space|0.3spc>的间隔
  </cell>>|<row|<cell|0.4spc>|<cell|中文和<space|0.4spc>ABC<space|0.4spc>的间隔>|<\cell>
    中文和<space|0.4spc>ABC<space|0.4spc>的间隔
  </cell>|<\cell>
    中文和<space|0.4spc>ABC<space|0.4spc>的间隔
  </cell>|<\cell>
    中文和<space|0.4spc>ABC<space|0.4spc>的间隔
  </cell>|<\cell>
    中文和<space|0.4spc>ABC<space|0.4spc>的间隔
  </cell>>|<row|<cell|0.5spc>|<cell|中文和<space|0.5spc>ABC<space|0.5spc>的间隔>|<\cell>
    中文和<space|0.5spc>ABC<space|0.5spc>的间隔
  </cell>|<\cell>
    中文和<space|0.5spc>ABC<space|0.5spc>的间隔
  </cell>|<\cell>
    中文和<space|0.5spc>ABC<space|0.5spc>的间隔
  </cell>|<\cell>
    中文和<space|0.5spc>ABC<space|0.5spc>的间隔
  </cell>>|<row|<cell|0.6spc>|<cell|中文和<space|0.6spc>ABC<space|0.6spc>的间隔>|<\cell>
    中文和<space|0.6spc>ABC<space|0.6spc>的间隔
  </cell>|<\cell>
    中文和<space|0.6spc>ABC<space|0.6spc>的间隔
  </cell>|<\cell>
    中文和<space|0.6spc>ABC<space|0.6spc>的间隔
  </cell>|<\cell>
    中文和<space|0.6spc>ABC<space|0.6spc>的间隔
  </cell>>|<row|<cell|0.7spc>|<cell|中文和<space|0.7spc>ABC<space|0.7spc>的间隔>|<\cell>
    中文和<space|0.7spc>ABC<space|0.7spc>的间隔
  </cell>|<\cell>
    中文和<space|0.7spc>ABC<space|0.7spc>的间隔
  </cell>|<\cell>
    中文和<space|0.7spc>ABC<space|0.7spc>的间隔
  </cell>|<\cell>
    中文和<space|0.7spc>ABC<space|0.7spc>的间隔
  </cell>>|<row|<cell|0.8spc>|<cell|中文和<space|0.8spc>ABC<space|0.8spc>的间隔>|<\cell>
    中文和<space|0.8spc>ABC<space|0.8spc>的间隔
  </cell>|<\cell>
    中文和<space|0.8spc>ABC<space|0.8spc>的间隔
  </cell>|<\cell>
    中文和<space|0.8spc>ABC<space|0.8spc>的间隔
  </cell>|<\cell>
    中文和<space|0.8spc>ABC<space|0.8spc>的间隔
  </cell>>|<row|<cell|0.9spc>|<cell|中文和<space|0.9spc>ABC<space|0.9spc>的间隔>|<\cell>
    中文和<space|0.9spc>ABC<space|0.9spc>的间隔
  </cell>|<\cell>
    中文和<space|0.9spc>ABC<space|0.9spc>的间隔
  </cell>|<\cell>
    中文和<space|0.9spc>ABC<space|0.9spc>的间隔
  </cell>|<\cell>
    中文和<space|0.9spc>ABC<space|0.9spc>的间隔
  </cell>>|<row|<cell|1spc>|<cell|中文和 ABC 的间隔>|<\cell>
    中文和 ABC 的间隔
  </cell>|<\cell>
    中文和 ABC 的间隔
  </cell>|<\cell>
    中文和 ABC 的间隔
  </cell>|<\cell>
    中文和 ABC 的间隔
  </cell>>>>>

  <subsection|Same hard-coded spaces and different font size>

  <tabular|<tformat|<cwith|2|5|3|3|cell-hyphen|t>|<cwith|1|5|3|3|cell-width|50pt>|<cwith|1|5|3|3|cell-hmode|exact>|<cwith|2|5|4|4|cell-hyphen|t>|<cwith|1|-1|4|4|cell-width|55pt>|<cwith|1|-1|4|4|cell-hmode|exact>|<cwith|2|4|3|3|cell-hyphen|t>|<cwith|1|4|3|3|cell-width|50pt>|<cwith|1|4|3|3|cell-hmode|exact>|<cwith|2|4|2|2|cell-hyphen|t>|<cwith|1|-1|2|2|cell-width|80pt>|<cwith|1|-1|2|2|cell-hmode|exact>|<cwith|2|2|2|-1|font|Noto
  CJK SC>|<cwith|2|2|4|4|font-base-size|8>|<cwith|3|3|2|-1|font|Noto CJK
  SC>|<cwith|4|4|2|-1|font|Noto CJK SC>|<cwith|4|4|4|4|font-base-size|12>|<table|<row|<cell|>|<cell|0.5spc>|<cell|0.5spc>|<cell|1spc>>|<row|<cell|8pt>|<\cell>
    <with|font|Noto CJK SC|font-base-size|8|中文和<space|0.5spc>ABC<space|0.5spc>的间隔>
  </cell>|<\cell>
    <with|font|Noto CJK SC|font-base-size|8|中文和<space|0.5spc>ABC<space|0.5spc>的间隔>
  </cell>|<\cell>
    中文和 ABC 的间隔
  </cell>>|<row|<cell|10pt>|<\cell>
    <with|font|Noto CJK SC|中文和<space|0.5spc>ABC<space|0.5spc>的间隔>
  </cell>|<\cell>
    <with|font|Noto CJK SC|中文和<space|0.5spc>ABC<space|0.5spc>的间隔>
  </cell>|<\cell>
    中文和 ABC 的间隔
  </cell>>|<row|<cell|12pt>|<\cell>
    <with|font|Noto CJK SC|font-base-size|12|中文和<space|0.5spc>ABC<space|0.5spc>的间隔>
  </cell>|<\cell>
    <with|font|Noto CJK SC|font-base-size|12|中文和<space|0.5spc>ABC<space|0.5spc>的间隔>
  </cell>|<\cell>
    中文和 ABC 的间隔
  </cell>>>>>

  <subsection|Why <verbatim|0.5spc> is a good value?>

  For CJK fonts, the <math|font width> is around <math|0.7\<times\>font
  height>. And<\footnote>
    See <slink|https://www.w3.org/International/clreq/#mixed_text_composition_in_horizontal_writing_mode>
  </footnote>:

  <\quote-env>
    In horizontal writing mode, the basic approach uses proportional fonts to
    represent Western text and uses proportional or monospace fonts for
    <hlink|European numerals|https://www.w3.org/International/clreq/#term.european-numerals>.
    In principle, there is tracking or spacing between an adjacent Han
    character and a Western character of up to 1/4<nbsp>em, except at the
    line start or end.
  </quote-env>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (* 0.25 (* 0.7 (length-decode "10pt")))
    <|unfolded-io>
      3719.2750000000005
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (length-decode "0.5spc")
    <|unfolded-io>
      3554
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <tmdoc-copyright|2023|Jade|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
    <associate|preamble|false>
  </collection>
</initial>