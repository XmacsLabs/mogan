<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese>>

<\body>
  <tmdoc-title|Insufficient punctuation rules for CJK>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: Rui Zhang

    <item>Mogan Editor Version: v1.1.1

    <item>Issue: <slink|https://github.com/XmacsLabs/mogan/issues/397>
  </itemize>

  <section|Description>

  <with|language|chinese|<tabular|<tformat|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|1|1|cell-width|60pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|3|3|cell-width|60pt>|<cwith|1|1|3|3|cell-hmode|exact>|<cwith|1|1|3|3|cell-hyphen|t>|<cwith|1|1|2|2|cell-width|55pt>|<cwith|1|1|2|2|cell-hmode|exact>|<table|<row|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#FF08\>\<#73BB\>\<#7483\>\<#FF09\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#201C\>\<#73BB\>\<#7483\>\<#201D\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#3010\>\<#73BB\>\<#7483\>\<#3011\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>>>>>>

  \;

  Problems:

  <\itemize>
    <item>Punctuations like \P, \<#FF08\> and \<#3010\> should not be allowed
    at the end of a line.

    <item>Punctuations like \Q, \<#FF09\> and \<#3011\> should not be allowed
    at the beginning of a line.
  </itemize>

  <subsection|Line break is prohibited between CJK if either is enclosed in
  an environment><\footnote>
    See <slink|https://github.com/XmacsLabs/mogan/issues/398>
  </footnote>

  <tabular|<tformat|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|1|1|cell-width|70pt>|<cwith|2|2|1|1|cell-hmode|exact>|<cwith|2|2|2|2|cell-hyphen|t>|<cwith|1|-1|2|2|cell-width|75pt>|<cwith|1|-1|2|2|cell-hmode|exact>|<cwith|2|2|3|3|cell-hyphen|t>|<cwith|1|-1|3|3|cell-width|80pt>|<cwith|1|-1|3|3|cell-hmode|exact>|<table|<row|<cell|70pt>|<cell|75pt>|<cell|80pt>>|<row|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>

    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#73BB\>\<#7483\>\<#800C\><with|font-series|bold|\<#4E0D\>\<#4F24\>>\<#8EAB\>\<#4F53\>\<#3002\>

    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#73BB\>\<#7483\>\<#800C\><line-break><with|font-series|bold|\<#4E0D\>\<#4F24\>>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>

    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#73BB\>\<#7483\>\<#800C\><with|font-series|bold|\<#4E0D\>\<#4F24\>>\<#8EAB\>\<#4F53\>\<#3002\>

    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#73BB\>\<#7483\>\<#800C\><line-break><with|font-series|bold|\<#4E0D\>\<#4F24\>>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>

    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#73BB\>\<#7483\>\<#800C\><with|font-series|bold|\<#4E0D\>\<#4F24\>>\<#8EAB\>\<#4F53\>\<#3002\>

    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#73BB\>\<#7483\>\<#800C\><line-break><with|font-series|bold|\<#4E0D\>\<#4F24\>>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>>>>>

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

  <tabular*|<tformat|<table|<row|<cell|\<#987F\>\<#53F7\>>|<cell|\<#9017\>\<#53F7\>>|<cell|\<#53E5\>\<#53F7\>>|<cell|\<#5192\>\<#53F7\>>|<cell|\<#5206\>\<#53F7\>>|<cell|\<#53F9\>\<#53F7\>>|<cell|\<#95EE\>\<#53F7\>>>|<row|<cell|\<#3001\>>|<cell|\<#FF0C\>>|<cell|\<#3002\>>|<cell|\<#FF1A\>>|<cell|\<#FF1B\>>|<cell|\<#FF01\>>|<cell|\<#FF1F\>>>>>>

  <tabular|<tformat|<twith|table-halign|l>|<cwith|1|-1|1|-1|cell-halign|c>|<table|<row|<cell|\<#8FDE\>\<#63A5\>\<#53F7\>>|<cell|\<#95F4\>\<#9694\>\<#53F7\>>|<cell|\<#5206\>\<#9694\>\<#53F7\>>>|<row|<cell|<subtable|<tformat|<table|<row|<cell|\V>|<cell|->|<cell|\<#FF5E\>>>>>>>|<cell|\<centerdot\>>|<cell|/>>>>>

  <tabular|<tformat|<cwith|2|2|1|1|cell-halign|c>|<cwith|1|1|2|2|cell-halign|r>|<cwith|1|1|3|3|cell-halign|r>|<cwith|1|1|1|-1|cell-tborder|0ln>|<cwith|1|1|1|-1|cell-bborder|1ln>|<cwith|2|2|1|-1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-lborder|0ln>|<cwith|1|1|3|3|cell-rborder|0ln>|<cwith|1|1|2|2|cell-tborder|0ln>|<cwith|2|2|2|2|cell-bborder|0ln>|<cwith|1|-1|2|2|cell-lborder|1ln>|<cwith|1|-1|1|1|cell-rborder|1ln>|<cwith|1|-1|2|2|cell-rborder|1ln>|<cwith|1|-1|3|3|cell-lborder|1ln>|<table|<row|<cell|\<#7ED3\>\<#675F\>\<#5F15\>\<#53F7\>>|<cell|\<#7ED3\>\<#675F\>\<#62EC\>\<#53F7\>>|<cell|\<#7ED3\>\<#675F\>\<#4E66\>\<#540D\>\<#53F7\>>>|<row|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-halign|c>|<table|<row|<cell|\<#201D\>>|<cell|'>|<cell|\<#300F\>>|<cell|\<#300D\>>>>>>>|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-halign|c>|<table|<row|<cell|\<#FF09\>>|<cell|\<#3011\>>>>>>>|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-halign|c>|<table|<row|<cell|\<#300B\>>|<cell|\<#3009\>>>>>>>>>>>

  \;

  <paragraph|For line end>

  <tabular|<tformat|<cwith|2|2|1|1|cell-halign|c>|<cwith|1|1|2|2|cell-halign|r>|<cwith|1|1|3|3|cell-halign|r>|<cwith|1|1|1|-1|cell-tborder|0ln>|<cwith|1|1|1|-1|cell-bborder|1ln>|<cwith|2|2|1|-1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-lborder|0ln>|<cwith|1|1|3|3|cell-rborder|0ln>|<cwith|1|1|2|2|cell-tborder|0ln>|<cwith|1|1|2|2|cell-bborder|1ln>|<cwith|2|2|2|2|cell-tborder|1ln>|<cwith|1|1|2|2|cell-lborder|1ln>|<cwith|1|1|1|1|cell-rborder|1ln>|<cwith|1|1|2|2|cell-rborder|1ln>|<cwith|1|1|3|3|cell-lborder|1ln>|<table|<row|<cell|\<#5F00\>\<#59CB\>\<#5F15\>\<#53F7\>>|<cell|\<#5F00\>\<#59CB\>\<#62EC\>\<#53F7\>>|<cell|\<#5F00\>\<#59CB\>\<#4E66\>\<#540D\>\<#53F7\>>>|<row|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-halign|c>|<table|<row|<cell|\<#201C\>>|<cell|`>|<cell|\<#300E\>>|<cell|\<#300C\>>>>>>>|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-halign|c>|<cwith|1|1|1|1|cell-tborder|0ln>|<cwith|1|1|1|1|cell-bborder|0ln>|<cwith|1|1|1|1|cell-lborder|1ln>|<cwith|1|1|2|2|cell-tborder|0ln>|<cwith|1|1|2|2|cell-bborder|0ln>|<cwith|1|1|2|2|cell-lborder|0ln>|<cwith|1|1|1|1|cell-rborder|0ln>|<cwith|1|1|2|2|cell-rborder|1ln>|<table|<row|<cell|(>|<cell|\<#3010\>>>>>>>|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-halign|c>|<table|<row|<cell|\<#300A\>>|<cell|\<#3008\>>>>>>>>>>>

  <section|More cases for the bug>

  <\note>
    The green part is ok.
  </note>

  <\note>
    It is reproducible on GNU Linux with Noto as the default Chinese font
    using Mogan v1.1.2 beta1.
  </note>

  <paragraph|For line start>

  \| could be at the line start.

  <with|language|chinese|<tabular|<tformat|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-background|pastel
  green>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|3|3|cell-hyphen|t>|<cwith|1|1|4|4|cell-hyphen|t>|<cwith|1|1|4|4|cell-width|50pt>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|3|3|cell-width|50pt>|<cwith|1|1|3|3|cell-hmode|exact>|<cwith|1|1|5|5|cell-hyphen|t>|<cwith|1|1|6|6|cell-hyphen|t>|<cwith|1|1|6|6|cell-width|50pt>|<cwith|1|1|6|6|cell-hmode|exact>|<cwith|1|1|5|5|cell-width|50pt>|<cwith|1|1|5|5|cell-hmode|exact>|<cwith|1|1|7|7|cell-hyphen|t>|<cwith|1|1|7|7|cell-width|50pt>|<cwith|1|1|7|7|cell-hmode|exact>|<cwith|1|1|2|7|cell-background|pastel
  green>|<cwith|1|1|2|2|cell-width|50pt>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|1|1|cell-width|50pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-background|pastel
  green>|<table|<row|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\|\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>,\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>.\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>:\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>;\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>!\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>?\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>>>>>>

  \;

  <with|language|chinese|<tabular|<tformat|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-width|50pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-background|pastel
  green>|<cwith|1|1|1|1|cell-width|60pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-width|50pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|3|3|cell-hyphen|t>|<cwith|1|1|3|3|cell-width|50pt>|<cwith|1|1|3|3|cell-hmode|exact>|<cwith|1|1|2|2|cell-width|50pt>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|4|4|cell-hyphen|t>|<cwith|1|1|5|5|cell-hyphen|t>|<cwith|1|1|5|5|cell-width|50pt>|<cwith|1|1|5|5|cell-hmode|exact>|<cwith|1|1|4|4|cell-width|50pt>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|6|6|cell-hyphen|t>|<cwith|1|1|6|6|cell-width|50pt>|<cwith|1|1|6|6|cell-hmode|exact>|<cwith|1|1|1|6|cell-background|pastel
  green>|<cwith|1|1|7|7|cell-hyphen|t>|<cwith|1|1|7|7|cell-width|50pt>|<cwith|1|1|7|7|cell-hmode|exact>|<cwith|1|1|7|7|cell-background|pastel
  green>|<table|<row|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#FF0C\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#3002\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#FF1A\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#FF1B\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#FF01\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#FF1F\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#3001\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>>>>>>

  \;

  <tabular|<tformat|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-width|50pt>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|1|1|cell-width|50pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|3|3|cell-hyphen|t>|<cwith|1|1|3|3|cell-width|50pt>|<cwith|1|1|3|3|cell-hmode|exact>|<cwith|1|1|4|4|cell-hyphen|t>|<cwith|1|1|5|5|cell-hyphen|t>|<cwith|1|1|5|5|cell-width|50pt>|<cwith|1|1|5|5|cell-hmode|exact>|<cwith|1|1|4|4|cell-width|50pt>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|3|3|cell-background|pastel
  green>|<table|<row|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>/\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#5217\>\<#5965\>\<#7EB3\>\<#591A\>\<centerdot\>\<#8FBE\>\<centerdot\>\<#82AC\>\<#5947\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#FF5E\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>-\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\V\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>>>>>

  \;

  <tabular|<tformat|<cwith|1|1|2|2|cell-width|60pt>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-width|50pt>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|1|1|cell-width|50pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|4|4|cell-width|60pt>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|4|4|cell-hyphen|t>|<cwith|1|1|3|3|cell-hyphen|t>|<cwith|1|1|4|4|cell-hyphen|t>|<cwith|1|1|4|4|cell-width|50pt>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|3|3|cell-width|50pt>|<cwith|1|1|3|3|cell-hmode|exact>|<cwith|1|1|6|6|cell-width|60pt>|<cwith|1|1|6|6|cell-hmode|exact>|<cwith|1|1|6|6|cell-hyphen|t>|<cwith|1|1|5|5|cell-hyphen|t>|<cwith|1|1|6|6|cell-hyphen|t>|<cwith|1|1|6|6|cell-width|50pt>|<cwith|1|1|6|6|cell-hmode|exact>|<cwith|1|1|5|5|cell-width|50pt>|<cwith|1|1|5|5|cell-hmode|exact>|<cwith|1|1|8|8|cell-width|60pt>|<cwith|1|1|8|8|cell-hmode|exact>|<cwith|1|1|8|8|cell-hyphen|t>|<cwith|1|1|7|7|cell-hyphen|t>|<cwith|1|1|8|8|cell-hyphen|t>|<cwith|1|1|8|8|cell-width|50pt>|<cwith|1|1|8|8|cell-hmode|exact>|<cwith|1|1|7|7|cell-width|50pt>|<cwith|1|1|7|7|cell-hmode|exact>|<cwith|1|1|3|8|cell-background|pastel
  green>|<table|<row|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#73BB\>\<#201D\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>'\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#300F\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#300D\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#FF09\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#3011\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#300B\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#3009\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>>>>>

  <paragraph|For line end>

  <tabular|<tformat|<cwith|1|1|2|2|cell-width|60pt>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-width|50pt>|<cwith|1|1|2|2|cell-hmode|exact>|<cwith|1|1|1|1|cell-width|50pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|4|4|cell-width|60pt>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|4|4|cell-hyphen|t>|<cwith|1|1|3|3|cell-hyphen|t>|<cwith|1|1|4|4|cell-hyphen|t>|<cwith|1|1|4|4|cell-width|50pt>|<cwith|1|1|4|4|cell-hmode|exact>|<cwith|1|1|3|3|cell-width|50pt>|<cwith|1|1|3|3|cell-hmode|exact>|<cwith|1|1|6|6|cell-width|60pt>|<cwith|1|1|6|6|cell-hmode|exact>|<cwith|1|1|6|6|cell-hyphen|t>|<cwith|1|1|5|5|cell-hyphen|t>|<cwith|1|1|6|6|cell-hyphen|t>|<cwith|1|1|6|6|cell-width|50pt>|<cwith|1|1|6|6|cell-hmode|exact>|<cwith|1|1|5|5|cell-width|50pt>|<cwith|1|1|5|5|cell-hmode|exact>|<cwith|1|1|8|8|cell-width|60pt>|<cwith|1|1|8|8|cell-hmode|exact>|<cwith|1|1|8|8|cell-hyphen|t>|<cwith|1|1|7|7|cell-hyphen|t>|<cwith|1|1|8|8|cell-hyphen|t>|<cwith|1|1|8|8|cell-width|50pt>|<cwith|1|1|8|8|cell-hmode|exact>|<cwith|1|1|7|7|cell-width|50pt>|<cwith|1|1|7|7|cell-hmode|exact>|<table|<row|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#201C\>\<#73BB\>\<#7483\>\<#201D\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>`
    '\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#300E\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#300C\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#FF08\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#3010\>\<#4E0B\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#300A\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#3008\>\<#73BB\>\<#7483\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>>>>>

  <tmdoc-copyright|2022|Rui Zhang|Darcy Shen>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>