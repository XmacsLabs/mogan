<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|The width of the subtable and joined cell>

  <paragraph|Bug Metadata>

  <\itemize>
    <item>Reporter: Darcy Shen

    <item>Mogan Editer version: v1.1.1
  </itemize>

  <paragraph|Description>

  In the following table, the width of the subtable should be the same as the
  joined cell 1,3.

  <block|<tformat|<cwith|3|3|1|1|cell-row-span|1>|<cwith|3|3|1|1|cell-col-span|2>|<table|<row|<cell|cell
  1,1>|<cell|cell 1,2 long description >>|<row|<cell|cell 1,2>|<cell|cell 2,2
  >>|<row|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-tborder|0ln>|<cwith|1|1|1|-1|cell-bborder|0ln>|<cwith|1|1|1|-1|cell-lborder|1ln>|<cwith|1|1|1|-1|cell-rborder|1ln>|<table|<row|<cell|subtable
  cell 1,1>|<cell|st cell 1,2>>>>>>|<cell|>>>>>

  \;

  <paragraph|Case Study 1>

  <\itemize>
    <item>set the width of table to 500 px
  </itemize>

  1-1:

  <block|<tformat|<cwith|3|3|1|1|cell-row-span|1>|<cwith|3|3|1|1|cell-col-span|2>|<twith|table-width|500px>|<twith|table-hmode|exact>|<table|<row|<cell|cell
  1,1>|<cell|cell 1,2 long description >>|<row|<cell|cell 1,2>|<cell|cell 2,2
  >>|<row|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-tborder|0ln>|<cwith|1|1|1|-1|cell-bborder|0ln>|<cwith|1|1|1|-1|cell-lborder|1ln>|<cwith|1|1|1|-1|cell-rborder|1ln>|<table|<row|<cell|subtable
  cell 1,1>|<cell|st cell 1,2>>>>>>|<cell|>>>>>

  \;

  1-2: setting the width of the subtable to 500px

  <block|<tformat|<cwith|3|3|1|1|cell-row-span|1>|<cwith|3|3|1|1|cell-col-span|2>|<cwith|1|-1|1|-1|cell-width|>|<cwith|1|-1|1|-1|cell-hmode|auto>|<twith|table-width|500px>|<twith|table-hmode|exact>|<table|<row|<cell|cell
  1,1>|<cell|cell 1,2 long description >>|<row|<cell|cell 1,2>|<cell|cell 2,2
  >>|<row|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-tborder|0ln>|<cwith|1|1|1|-1|cell-bborder|0ln>|<cwith|1|1|1|-1|cell-lborder|1ln>|<cwith|1|1|1|-1|cell-rborder|1ln>|<twith|table-width|500px>|<twith|table-hmode|exact>|<table|<row|<cell|subtable
  cell 1,1>|<cell|st cell 1,2>>>>>>|<cell|>>>>>

  Typesetting result:

  <\itemize>
    <item>the width of row 1 is not correct

    <item>the width of the subtable is not correct
  </itemize>

  It seems that although we have joined cell 1,3 and cell 2,3 and created the
  subtable in the joined cell, it seems that the subtable is created in the
  original cell 1,3.

  <paragraph|Case Study 2>

  <\itemize>
    <item>Remove the subtable
  </itemize>

  2-1: setting the width of the joined cell to 500px

  <block|<tformat|<cwith|3|3|1|1|cell-row-span|1>|<cwith|3|3|1|1|cell-col-span|2>|<cwith|3|3|1|1|cell-width|500px>|<cwith|3|3|1|1|cell-hmode|exact>|<table|<row|<cell|cell
  1,1>|<cell|cell 1,2 long description >>|<row|<cell|cell 1,2>|<cell|cell 2,2
  >>|<row|<cell|the joined cell>|<cell|>>>>>

  \;

  2-2: setting the width of the whole table to 500px

  <block|<tformat|<cwith|3|3|1|1|cell-row-span|1>|<cwith|3|3|1|1|cell-col-span|2>|<cwith|3|3|1|1|cell-width|>|<cwith|3|3|1|1|cell-hmode|auto>|<twith|table-width|500px>|<twith|table-hmode|exact>|<table|<row|<cell|cell
  1,1>|<cell|cell 1,2 long description >>|<row|<cell|cell 1,2>|<cell|cell 2,2
  >>|<row|<cell|the joined cell>|<cell|>>>>>

  Typesetting result:

  Setting the width of the joined cell is buggy.

  <\tmdoc-copyright|2023>
    Darcy Shen
  </tmdoc-copyright>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>