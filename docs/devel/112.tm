<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese>>

<\body>
  <tmdoc-title|Insufficient punctuation rules for CJK>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: Rui Zhang

    <item>Mogan Editor Version: v1.1.1
  </itemize>

  <section|Description>

  <with|language|chinese|<tabular|<tformat|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|1|1|cell-width|60pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|3|3|cell-width|60pt>|<cwith|1|1|3|3|cell-hmode|exact>|<cwith|1|1|3|3|cell-hyphen|t>|<cwith|1|1|2|2|cell-width|55pt>|<cwith|1|1|2|2|cell-hmode|exact>|<table|<row|<\cell>
    我能吞下（玻璃）而不伤身体。
  </cell>|<\cell>
    我能吞下“玻璃”而不伤身体。
  </cell>|<\cell>
    我能吞下【玻璃】而不伤身体。
  </cell>>>>>>

  \;

  Problems:

  <\itemize>
    <item>Punctuations like \P, （ and 【 should not be allowed at the end
    of a line.

    <item>Punctuations like \Q, ） and 】 should not be allowed at the
    beginning of a line.
  </itemize>

  <section|Prohibition Rules for Line Start and Line End>

  <\quote-env>
    Pause or stop punctuation marks (secondary commas, commas, semicolons,
    colons, periods, exclamation marks, and question marks), closing
    quotation marks, closing parentheses, closing angle brackets, connector
    marks, and interpuncts should not appear at the line start. Opening
    quotation marks, opening parentheses, and opening angle brackets should
    not appear at the line end. This is the most recommended
    method.<\footnote>
      See <slink|https://www.w3.org/International/clreq/#prohibition_rules_for_line_start_end>
    </footnote>
  </quote-env>

  <paragraph|For line start>

  <tabular*|<tformat|<table|<row|<cell|顿号>|<cell|逗号>|<cell|句号>|<cell|冒号>|<cell|分号>|<cell|叹号>|<cell|问号>>|<row|<cell|、>|<cell|，>|<cell|。>|<cell|：>|<cell|；>|<cell|！>|<cell|？>>>>>

  <tabular|<tformat|<twith|table-halign|l>|<cwith|1|-1|1|-1|cell-halign|c>|<table|<row|<cell|连接号>|<cell|间隔号>|<cell|分隔号>>|<row|<cell|<subtable|<tformat|<table|<row|<cell|\V>|<cell|->|<cell|～>>>>>>|<cell|\<centerdot\>>|<cell|/>>>>>

  <tabular|<tformat|<cwith|2|2|1|1|cell-halign|c>|<cwith|1|1|2|2|cell-halign|r>|<cwith|1|1|3|3|cell-halign|r>|<cwith|1|1|1|-1|cell-tborder|0ln>|<cwith|1|1|1|-1|cell-bborder|1ln>|<cwith|2|2|1|-1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-lborder|0ln>|<cwith|1|1|3|3|cell-rborder|0ln>|<cwith|1|1|2|2|cell-tborder|0ln>|<cwith|2|2|2|2|cell-bborder|0ln>|<cwith|1|-1|2|2|cell-lborder|1ln>|<cwith|1|-1|1|1|cell-rborder|1ln>|<cwith|1|-1|2|2|cell-rborder|1ln>|<cwith|1|-1|3|3|cell-lborder|1ln>|<table|<row|<cell|结束引号>|<cell|结束括号>|<cell|结束书名号>>|<row|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-halign|c>|<table|<row|<cell|”>|<cell|'>|<cell|』>|<cell|」>>>>>>|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-halign|c>|<table|<row|<cell|）>|<cell|】>>>>>>|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-halign|c>|<table|<row|<cell|》>|<cell|〉>>>>>>>>>>

  \;

  <paragraph|For line end>

  <tabular|<tformat|<cwith|2|2|1|1|cell-halign|c>|<cwith|1|1|2|2|cell-halign|r>|<cwith|1|1|3|3|cell-halign|r>|<cwith|1|1|1|-1|cell-tborder|0ln>|<cwith|1|1|1|-1|cell-bborder|1ln>|<cwith|2|2|1|-1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-lborder|0ln>|<cwith|1|1|3|3|cell-rborder|0ln>|<cwith|1|1|2|2|cell-tborder|0ln>|<cwith|1|1|2|2|cell-bborder|1ln>|<cwith|2|2|2|2|cell-tborder|1ln>|<cwith|1|1|2|2|cell-lborder|1ln>|<cwith|1|1|1|1|cell-rborder|1ln>|<cwith|1|1|2|2|cell-rborder|1ln>|<cwith|1|1|3|3|cell-lborder|1ln>|<table|<row|<cell|开始引号>|<cell|开始括号>|<cell|开始书名号>>|<row|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-halign|c>|<table|<row|<cell|“>|<cell|`>|<cell|『>|<cell|「>>>>>>|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-halign|c>|<cwith|1|1|1|1|cell-tborder|0ln>|<cwith|1|1|1|1|cell-bborder|0ln>|<cwith|1|1|1|1|cell-lborder|1ln>|<cwith|1|1|2|2|cell-tborder|0ln>|<cwith|1|1|2|2|cell-bborder|0ln>|<cwith|1|1|2|2|cell-lborder|0ln>|<cwith|1|1|1|1|cell-rborder|0ln>|<cwith|1|1|2|2|cell-rborder|1ln>|<table|<row|<cell|(>|<cell|【>>>>>>|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-halign|c>|<table|<row|<cell|《>|<cell|〈>>>>>>>>>>

  <section|More cases for the bug>

  The green part is ok.

  <paragraph|For line start>

  \| could be at the line start.

  <with|language|chinese|<tabular|<tformat|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-background|pastel
  green>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|3|3|cell-hyphen|t>|<cwith|1|1|4|4|cell-hyphen|t>|<cwith|1|1|4|4|cell-width|50pt>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|3|3|cell-width|50pt>|<cwith|1|1|3|3|cell-hmode|exact>|<cwith|1|1|5|5|cell-hyphen|t>|<cwith|1|1|6|6|cell-hyphen|t>|<cwith|1|1|6|6|cell-width|50pt>|<cwith|1|1|6|6|cell-hmode|exact>|<cwith|1|1|5|5|cell-width|50pt>|<cwith|1|1|5|5|cell-hmode|exact>|<cwith|1|1|7|7|cell-hyphen|t>|<cwith|1|1|7|7|cell-width|50pt>|<cwith|1|1|7|7|cell-hmode|exact>|<cwith|1|1|2|7|cell-background|pastel
  green>|<cwith|1|1|2|2|cell-width|50pt>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|1|1|cell-width|50pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-background|pastel
  green>|<table|<row|<\cell>
    我能吞下\|玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下,玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下.玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下:玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下;玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下!玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下?玻璃而不伤身体。
  </cell>>>>>>

  \;

  <with|language|chinese|<tabular|<tformat|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-width|50pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-background|pastel
  green>|<cwith|1|1|1|1|cell-width|60pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-width|50pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|3|3|cell-hyphen|t>|<cwith|1|1|3|3|cell-width|50pt>|<cwith|1|1|3|3|cell-hmode|exact>|<cwith|1|1|2|2|cell-width|50pt>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|4|4|cell-hyphen|t>|<cwith|1|1|5|5|cell-hyphen|t>|<cwith|1|1|5|5|cell-width|50pt>|<cwith|1|1|5|5|cell-hmode|exact>|<cwith|1|1|4|4|cell-width|50pt>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|6|6|cell-hyphen|t>|<cwith|1|1|6|6|cell-width|50pt>|<cwith|1|1|6|6|cell-hmode|exact>|<cwith|1|1|1|6|cell-background|pastel
  green>|<cwith|1|1|7|7|cell-hyphen|t>|<cwith|1|1|7|7|cell-width|50pt>|<cwith|1|1|7|7|cell-hmode|exact>|<cwith|1|1|7|7|cell-background|pastel
  green>|<table|<row|<\cell>
    我能吞下，玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下。玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下：玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下；玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下！玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下？玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下、玻璃而不伤身体。
  </cell>>>>>>

  \;

  <tabular|<tformat|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-width|50pt>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|1|1|cell-width|50pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|3|3|cell-hyphen|t>|<cwith|1|1|3|3|cell-width|50pt>|<cwith|1|1|3|3|cell-hmode|exact>|<cwith|1|1|4|4|cell-hyphen|t>|<cwith|1|1|5|5|cell-hyphen|t>|<cwith|1|1|5|5|cell-width|50pt>|<cwith|1|1|5|5|cell-hmode|exact>|<cwith|1|1|4|4|cell-width|50pt>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|3|3|cell-background|pastel
  green>|<table|<row|<\cell>
    我能吞下/玻璃而不伤身体。
  </cell>|<\cell>
    列奥纳多\<centerdot\>达\<centerdot\>芬奇
  </cell>|<\cell>
    我能吞下～玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下-玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下\V玻璃而不伤身体。
  </cell>>>>>

  \;

  <tabular|<tformat|<cwith|1|1|2|2|cell-width|60pt>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-width|50pt>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|1|1|cell-width|50pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|4|4|cell-width|60pt>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|4|4|cell-hyphen|t>|<cwith|1|1|3|3|cell-hyphen|t>|<cwith|1|1|4|4|cell-hyphen|t>|<cwith|1|1|4|4|cell-width|50pt>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|3|3|cell-width|50pt>|<cwith|1|1|3|3|cell-hmode|exact>|<cwith|1|1|6|6|cell-width|60pt>|<cwith|1|1|6|6|cell-hmode|exact>|<cwith|1|1|6|6|cell-hyphen|t>|<cwith|1|1|5|5|cell-hyphen|t>|<cwith|1|1|6|6|cell-hyphen|t>|<cwith|1|1|6|6|cell-width|50pt>|<cwith|1|1|6|6|cell-hmode|exact>|<cwith|1|1|5|5|cell-width|50pt>|<cwith|1|1|5|5|cell-hmode|exact>|<cwith|1|1|8|8|cell-width|60pt>|<cwith|1|1|8|8|cell-hmode|exact>|<cwith|1|1|8|8|cell-hyphen|t>|<cwith|1|1|7|7|cell-hyphen|t>|<cwith|1|1|8|8|cell-hyphen|t>|<cwith|1|1|8|8|cell-width|50pt>|<cwith|1|1|8|8|cell-hmode|exact>|<cwith|1|1|7|7|cell-width|50pt>|<cwith|1|1|7|7|cell-hmode|exact>|<cwith|1|1|3|8|cell-background|pastel
  green>|<table|<row|<\cell>
    我能吞下”玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下'玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下』玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下」玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下）玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下】玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下》玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下〉玻璃而不伤身体。
  </cell>>>>>

  <paragraph|For line end>

  <tabular|<tformat|<cwith|1|1|2|2|cell-width|60pt>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-width|50pt>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|1|1|cell-width|50pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|4|4|cell-width|60pt>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|4|4|cell-hyphen|t>|<cwith|1|1|3|3|cell-hyphen|t>|<cwith|1|1|4|4|cell-hyphen|t>|<cwith|1|1|4|4|cell-width|50pt>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|3|3|cell-width|50pt>|<cwith|1|1|3|3|cell-hmode|exact>|<cwith|1|1|6|6|cell-width|60pt>|<cwith|1|1|6|6|cell-hmode|exact>|<cwith|1|1|6|6|cell-hyphen|t>|<cwith|1|1|5|5|cell-hyphen|t>|<cwith|1|1|6|6|cell-hyphen|t>|<cwith|1|1|6|6|cell-width|50pt>|<cwith|1|1|6|6|cell-hmode|exact>|<cwith|1|1|5|5|cell-width|50pt>|<cwith|1|1|5|5|cell-hmode|exact>|<cwith|1|1|8|8|cell-width|60pt>|<cwith|1|1|8|8|cell-hmode|exact>|<cwith|1|1|8|8|cell-hyphen|t>|<cwith|1|1|7|7|cell-hyphen|t>|<cwith|1|1|8|8|cell-hyphen|t>|<cwith|1|1|8|8|cell-width|50pt>|<cwith|1|1|8|8|cell-hmode|exact>|<cwith|1|1|7|7|cell-width|50pt>|<cwith|1|1|7|7|cell-hmode|exact>|<table|<row|<\cell>
    我能吞下“”玻璃而不伤身体。
  </cell>|<\cell>
    我能吞下` '玻璃而不伤身体。
  </cell>|<\cell>
    我能吞『下玻璃而不伤身体。
  </cell>|<\cell>
    我能吞「下玻璃而不伤身体。
  </cell>|<\cell>
    我能吞（（下玻璃而不伤身体。
  </cell>|<\cell>
    我能吞【【下玻璃而不伤身体。
  </cell>|<\cell>
    我能吞《下玻璃而不伤身体。
  </cell>|<\cell>
    我能吞〈下玻璃而不伤身体。
  </cell>>>>>

  <tmdoc-copyright|2022|Rui Zhang|Darcy Shen>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>