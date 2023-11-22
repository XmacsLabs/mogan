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

  \<#4E2D\>\<#6587\>\<#548C\>\<#62C9\>\<#4E01\>\<#5B57\>\<#6BCD\>ABC\<#4E4B\>\<#95F4\>\<#7684\>\<#95F4\>\<#9694\>\<#592A\>\<#5C0F\>\<#FF0C\>\<#52A0\>\<#7A7A\>\<#683C\>
  ABC \<#53C8\>\<#592A\>\<#5927\>\<#3002\>

  \<#4E2D\>\<#6587\>\<#548C\>\<#6570\>\<#5B66\>\<#516C\>\<#5F0F\><math|y=f<around*|(|x|)>>\<#4E4B\>\<#95F4\>\<#7684\>\<#95F4\>\<#9694\>\<#4E5F\>\<#592A\>\<#5C0F\>\<#FF0C\>\<#52A0\>\<#7A7A\>\<#683C\>
  <math|y=f<around*|(|x|)>> \<#592A\>\<#5927\>\<#3002\>

  \<#4E2D\>\<#6587\>\<#4E0E\>\<#5176\>\<#4ED6\>\<#884C\>\<#5185\>\<#7ED3\>\<#6784\>\<#FF0C\>\<#6BD4\>\<#5982\><cpp|int
  main()>\<#4E4B\>\<#95F4\>\<#7684\>\<#95EE\>\<#9898\>\<#7C7B\>\<#4F3C\>\<#3002\>

  \;

  <unfolded-comment|+Hh8Wq0T1p7qzE1Q|+Hh8Wq0T1p7qzE1R|comment|Jade|1674568692||<strong|\<#7528\>0.6spc\<#7684\>\<#6548\>\<#679C\>\<#611F\>\<#89C9\>\<#6BD4\>\<#8F83\>\<#8212\>\<#670D\>\<#FF08\>\<#6BD4\><LaTeX>\<#597D\>\<#FF09\>>>

  \<#4E2D\>\<#6587\>\<#548C\>\<#62C9\>\<#4E01\>\<#5B57\>\<#6BCD\><space|0.6spc>ABC<space|0.6spc>\<#4E4B\>\<#95F4\>\<#7684\>\<#95F4\>\<#9694\>

  \<#4E2D\>\<#6587\>\<#548C\>\<#6570\>\<#5B66\>\<#516C\>\<#5F0F\><space|0.6spc><math|y=f<around*|(|x|)>><space|0.6spc>\<#4E4B\>\<#95F4\>\<#7684\>\<#95F4\>\<#9694\>

  \<#4E2D\>\<#6587\>\<#548C\>\<#5176\>\<#4ED6\>\<#884C\>\<#5185\>\<#7ED3\>\<#6784\><space|0.6spc><cpp|int
  main()><space|0.6spc>\<#4E4B\>\<#95F4\>\<#7684\>\<#95F4\>\<#9694\>

  <section|Detailed Description>

  <subsection|Line Break with different hard-coded spaces>

  <tabular|<tformat|<cwith|2|11|5|5|cell-hyphen|t>|<cwith|1|11|5|5|cell-width|60pt>|<cwith|1|11|5|5|cell-hmode|exact>|<cwith|2|11|6|6|cell-hyphen|t>|<cwith|1|-1|6|6|cell-width|65pt>|<cwith|1|-1|6|6|cell-hmode|exact>|<cwith|2|11|3|3|cell-hyphen|t>|<cwith|1|11|3|3|cell-width|50pt>|<cwith|1|11|3|3|cell-hmode|exact>|<cwith|2|11|4|4|cell-hyphen|t>|<cwith|1|-1|4|4|cell-width|55pt>|<cwith|1|-1|4|4|cell-hmode|exact>|<table|<row|<cell|>|<cell|no
  wrap>|<cell|50pt>|<cell|55pt>|<cell|60pt>|<cell|65pt>>|<row|<cell|0.1spc>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.1spc>ABC<space|0.1spc>\<#7684\>\<#95F4\>\<#9694\>>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.1spc>ABC<space|0.1spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.1spc>ABC<space|0.1spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.1spc>ABC<space|0.1spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.1spc>ABC<space|0.1spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>>|<row|<cell|0.2spc>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.2spc>ABC<space|0.2spc>\<#7684\>\<#95F4\>\<#9694\>>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.2spc>ABC<space|0.2spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.2spc>ABC<space|0.2spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.2spc>ABC<space|0.2spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.2spc>ABC<space|0.2spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>>|<row|<cell|0.3spc>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.3spc>ABC<space|0.3spc>\<#7684\>\<#95F4\>\<#9694\>>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.3spc>ABC<space|0.3spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.3spc>ABC<space|0.3spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.3spc>ABC<space|0.3spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.3spc>ABC<space|0.3spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>>|<row|<cell|0.4spc>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.4spc>ABC<space|0.4spc>\<#7684\>\<#95F4\>\<#9694\>>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.4spc>ABC<space|0.4spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.4spc>ABC<space|0.4spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.4spc>ABC<space|0.4spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.4spc>ABC<space|0.4spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>>|<row|<cell|0.5spc>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc>ABC<space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.5spc>ABC<space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.5spc>ABC<space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.5spc>ABC<space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.5spc>ABC<space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>>|<row|<cell|0.6spc>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.6spc>ABC<space|0.6spc>\<#7684\>\<#95F4\>\<#9694\>>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.6spc>ABC<space|0.6spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.6spc>ABC<space|0.6spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.6spc>ABC<space|0.6spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.6spc>ABC<space|0.6spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>>|<row|<cell|0.7spc>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.7spc>ABC<space|0.7spc>\<#7684\>\<#95F4\>\<#9694\>>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.7spc>ABC<space|0.7spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.7spc>ABC<space|0.7spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.7spc>ABC<space|0.7spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.7spc>ABC<space|0.7spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>>|<row|<cell|0.8spc>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.8spc>ABC<space|0.8spc>\<#7684\>\<#95F4\>\<#9694\>>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.8spc>ABC<space|0.8spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.8spc>ABC<space|0.8spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.8spc>ABC<space|0.8spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.8spc>ABC<space|0.8spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>>|<row|<cell|0.9spc>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.9spc>ABC<space|0.9spc>\<#7684\>\<#95F4\>\<#9694\>>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.9spc>ABC<space|0.9spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.9spc>ABC<space|0.9spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.9spc>ABC<space|0.9spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\><space|0.9spc>ABC<space|0.9spc>\<#7684\>\<#95F4\>\<#9694\>
  </cell>>|<row|<cell|1spc>|<cell|\<#4E2D\>\<#6587\>\<#548C\> ABC
  \<#7684\>\<#95F4\>\<#9694\>>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\> ABC \<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\> ABC \<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\> ABC \<#7684\>\<#95F4\>\<#9694\>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\> ABC \<#7684\>\<#95F4\>\<#9694\>
  </cell>>>>>

  <subsection|Same hard-coded spaces and different font size>

  <tabular|<tformat|<cwith|2|5|3|3|cell-hyphen|t>|<cwith|1|5|3|3|cell-width|50pt>|<cwith|1|5|3|3|cell-hmode|exact>|<cwith|2|5|4|4|cell-hyphen|t>|<cwith|1|-1|4|4|cell-width|55pt>|<cwith|1|-1|4|4|cell-hmode|exact>|<cwith|2|4|3|3|cell-hyphen|t>|<cwith|1|4|3|3|cell-width|50pt>|<cwith|1|4|3|3|cell-hmode|exact>|<cwith|2|4|2|2|cell-hyphen|t>|<cwith|1|-1|2|2|cell-width|80pt>|<cwith|1|-1|2|2|cell-hmode|exact>|<cwith|2|2|2|-1|font|Noto
  CJK SC>|<cwith|2|2|4|4|font-base-size|8>|<cwith|3|3|2|-1|font|Noto CJK
  SC>|<cwith|4|4|2|-1|font|Noto CJK SC>|<cwith|4|4|4|4|font-base-size|12>|<table|<row|<cell|>|<cell|0.5spc>|<cell|0.5spc>|<cell|1spc>>|<row|<cell|8pt>|<\cell>
    <with|font|Noto CJK SC|font-base-size|8|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc>ABC<space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>
  </cell>|<\cell>
    <with|font|Noto CJK SC|font-base-size|8|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc>ABC<space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\> ABC \<#7684\>\<#95F4\>\<#9694\>
  </cell>>|<row|<cell|10pt>|<\cell>
    <with|font|Noto CJK SC|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc>ABC<space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>
  </cell>|<\cell>
    <with|font|Noto CJK SC|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc>ABC<space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\> ABC \<#7684\>\<#95F4\>\<#9694\>
  </cell>>|<row|<cell|12pt>|<\cell>
    <with|font|Noto CJK SC|font-base-size|12|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc>ABC<space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>
  </cell>|<\cell>
    <with|font|Noto CJK SC|font-base-size|12|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc>ABC<space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>
  </cell>|<\cell>
    \<#4E2D\>\<#6587\>\<#548C\> ABC \<#7684\>\<#95F4\>\<#9694\>
  </cell>>>>>

  <unfolded-comment|+2Q749q3u2JHiP5rq|+2Q749q3u2JHiP5rr|comment|Darcy
  Shen|1674782608||The width of space between Chinese text and upcased ABC
  \ should be less than 0.5spc.>

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

  <subsection|Lower-cased alphabet, number and inline math>

  <unfolded-comment|+2Q749q3u2JHiP5rs|+2Q749q3u2JHiP5rt|comment|Jade|1674784722||0.2spc
  seems to be a better value.>

  <unfolded-comment|+2Q749q3u2JHiP5ru|+2Q749q3u2JHiP5rv|comment|Yiqi
  Xu|1674784753||0.2spc seems to be a better value.>

  <paragraph|Normal Font>

  <with|font-base-size|15|<tabular|<tformat|<cwith|1|1|7|7|cell-valign|b>|<twith|table-width|1par>|<twith|table-hmode|exact>|<table|<row|<cell|0.2spc>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.2spc>ABC<space|0.2spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|0.4spc>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.4spc>ABC<space|0.4spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>ABC\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>ABC\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.2spc>abc<space|0.2spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.4spc>abc<space|0.4spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>abc\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>abc\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.2spc>123<space|0.2spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.4spc>123<space|0.4spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>123\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>123\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|0.3spc>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.3spc>ABC<space|0.3spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|0.5spc>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc>ABC<space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>ABC\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>ABC\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.3spc>abc<space|0.3spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc>abc<space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>abc\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>abc\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.3spc>123<space|0.3spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc>123<space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>123\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>123\<#7684\>\<#95F4\>\<#9694\>>>>>>>

  <paragraph|With the <markup|strong> content tag>

  \;

  <with|font-base-size|15|<tabular|<tformat|<cwith|1|1|7|7|cell-valign|b>|<twith|table-width|1par>|<twith|table-hmode|exact>|<table|<row|<cell|0.2spc>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.2spc><strong|ABC><space|0.2spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|0.4spc>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.4spc><strong|ABC><space|0.4spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><strong|ABC>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><strong|ABC>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>ABC\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>ABC\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.2spc><strong|abc><space|0.2spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.4spc><strong|abc><space|0.4spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><strong|abc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><strong|abc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>abc\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>abc\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|0.3spc>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.3spc><strong|ABC><space|0.3spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|0.5spc>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc><strong|ABC><space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><strong|ABC>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><strong|ABC>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>ABC\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>ABC\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.3spc><strong|abc><space|0.3spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc><strong|abc><space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><strong|abc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><strong|abc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>abc\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>abc\<#7684\>\<#95F4\>\<#9694\>>>>>>>

  <paragraph|With the <markup|em> content tag>

  <with|font-base-size|15|<tabular|<tformat|<cwith|1|1|7|7|cell-valign|b>|<twith|table-width|1par>|<twith|table-hmode|exact>|<table|<row|<cell|0.2spc>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.2spc><em|ABC><space|0.2spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|0.4spc>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.4spc><em|ABC><space|0.4spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><em|ABC>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><em|ABC>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>ABC\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>ABC\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.2spc><em|yef><space|0.2spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.4spc><em|yef><space|0.4spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><em|yef>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><em|yef>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>yef\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>yef\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|0.3spc>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.3spc><em|ABC><space|0.3spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|0.5spc>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc><em|ABC><space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><em|ABC>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><em|ABC>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>ABC\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>ABC\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.3spc><em|yef><space|0.3spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc><em|yef><space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><em|yef>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><em|yef>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>yef\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\>yef\<#7684\>\<#95F4\>\<#9694\>>>>>>>

  <paragraph|With math mode>

  <with|font-base-size|15|<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<table|<row|<cell|0.2spc>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.2spc><math|\<Pi\>><space|0.2spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|0.4spc>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.4spc><math|\<Pi\>><space|0.4spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><math|\<Pi\>>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><math|\<Pi\>>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.2spc><math|\<beta\>><space|0.2spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.4spc><math|\<beta\>><space|0.4spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><math|\<beta\>>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><math|\<beta\>>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|0.3spc>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.3spc><math|\<Pi\>><space|0.3spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|0.5spc>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc><math|\<Pi\>><space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><math|\<Pi\>>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><math|\<Pi\>>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.3spc><math|\<beta\>><space|0.3spc>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><space|0.5spc><math|\<beta\>><space|0.5spc>\<#7684\>\<#95F4\>\<#9694\>>>|<row|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><math|\<beta\>>\<#7684\>\<#95F4\>\<#9694\>>|<cell|>|<cell|>|<cell|>|<cell|\<#4E2D\>\<#6587\>\<#548C\><math|\<beta\>>\<#7684\>\<#95F4\>\<#9694\>>>>>>>

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
    <associate|font-base-size|10>
    <associate|page-medium|papyrus>
    <associate|preamble|false>
  </collection>
</initial>