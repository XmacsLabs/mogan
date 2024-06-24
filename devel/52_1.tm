<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|The width of the subtable and joined cell>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: Darcy Shen

    <item>Mogan Editer version: v1.2

    <item>Gitee issue: <hlink|\<#5B50\>\<#8868\>\<#683C\>\<#5BBD\>\<#5EA6\>\<#8BBE\>\<#7F6E\>\<#65E0\>\<#6548\>|>
  </itemize>

  <section|Description>

  This is an issue fix from version 1.1 to 1.2, reported by Darcy Shen and
  migrated by Paradisuman.<space|1em>

  In order to adjust the separator position between cell (3,1) and cell
  (3,2), we joined the cell (3,1) and cell (3,2), and insert a subtable
  there.

  <\big-table|<block|<tformat|<cwith|3|3|1|1|cell-row-span|1>|<cwith|3|3|1|1|cell-col-span|2>|<table|<row|<cell|cell
  1,1>|<cell|cell 1,2 long description >>|<row|<cell|cell 2,1>|<cell|cell
  2,2>>|<row|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-tborder|0ln>|<cwith|1|1|1|-1|cell-bborder|0ln>|<cwith|1|1|1|-1|cell-lborder|1ln>|<cwith|1|1|1|-1|cell-rborder|1ln>|<table|<row|<cell|subtable
  cell 1,1>|<cell|st cell 1,2>>>>>>|<cell|>>>>>>
    the width of the subtable should be the same as the joined cell 1,3.
  </big-table>

  <subsection|Case Study 1: the width of the subtable>

  Because of the bug described in Table 1, let us tune the width of the table
  or the subtable.

  <\big-table|<block|<tformat|<cwith|3|3|1|1|cell-row-span|1>|<cwith|3|3|1|1|cell-col-span|2>|<twith|table-width|800px>|<twith|table-hmode|exact>|<table|<row|<cell|cell
  1,1>|<cell|cell 1,2 long description >>|<row|<cell|cell 2,1>|<cell|cell
  2,2>>|<row|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-tborder|0ln>|<cwith|1|1|1|-1|cell-bborder|0ln>|<cwith|1|1|1|-1|cell-lborder|1ln>|<cwith|1|1|1|-1|cell-rborder|1ln>|<table|<row|<cell|subtable
  cell 1,1>|<cell|st cell 1,2>>>>>>|<cell|>>>>>>
    set the width of Table 1 to 800px.
  </big-table>

  <\big-table|<block|<tformat|<cwith|3|3|1|1|cell-row-span|1>|<cwith|3|3|1|1|cell-col-span|2>|<cwith|1|-1|1|-1|cell-width|>|<cwith|1|-1|1|-1|cell-hmode|auto>|<twith|table-hmode|auto>|<table|<row|<cell|cell
  1,1>|<cell|cell 1,2 long description >>|<row|<cell|cell 2,1>|<cell|cell 2,2
  >>|<row|<cell|<subtable|<tformat|<cwith|1|1|1|-1|cell-tborder|0ln>|<cwith|1|1|1|-1|cell-bborder|0ln>|<cwith|1|1|1|-1|cell-lborder|1ln>|<cwith|1|1|1|-1|cell-rborder|1ln>|<twith|table-width|800px>|<twith|table-hmode|exact>|<table|<row|<cell|subtable
  cell 1,1>|<cell|st cell 1,2>>>>>>|<cell|>>>>>>
    set the width of the subtable of Table 1 to 800px
  </big-table>

  Comparing the Table 2 and Table 3 (in Mogan v1.1.1):

  <\itemize>
    <item><math|width<around*|(|column 1 of Table
    2|)>\<neq\>width<around*|(|column 1 of Table 3|)>>

    <item><math|width(subtable of Table 2)\<neq\>width<around*|(|subtable of
    Table 3|)>>
  </itemize>

  <subsection|Case study 2: the width of the joined cell>

  <\big-table|<block|<tformat|<cwith|3|3|1|1|cell-row-span|1>|<cwith|3|3|1|1|cell-col-span|2>|<table|<row|<cell|cell
  1,1>|<cell|cell 1,2 long description >>|<row|<cell|cell 2,1>|<cell|cell 2,2
  >>|<row|<cell|the joined cell>|<cell|>>>>>>
    remove the subtable in Table 1
  </big-table>

  <\big-table|<block|<tformat|<cwith|3|3|1|1|cell-row-span|1>|<cwith|3|3|1|1|cell-col-span|2>|<cwith|3|3|1|1|cell-width|800px>|<cwith|3|3|1|1|cell-hmode|exact>|<table|<row|<cell|cell
  1,1>|<cell|cell 1,2 long description >>|<row|<cell|cell 2,1>|<cell|cell 2,2
  >>|<row|<cell|the joined cell>|<cell|>>>>>>
    set the width of the joined cell of Table 4 to 800px
  </big-table>

  <\big-table|<block|<tformat|<cwith|3|3|1|1|cell-row-span|1>|<cwith|3|3|1|1|cell-col-span|2>|<cwith|3|3|1|1|cell-width|>|<cwith|3|3|1|1|cell-hmode|auto>|<twith|table-width|800px>|<twith|table-hmode|exact>|<table|<row|<cell|cell
  1,1>|<cell|cell 1,2 long description >>|<row|<cell|cell 2,1>|<cell|cell
  2,2>>|<row|<cell|the joined cell>|<cell|>>>>>>
    set the width of the whole table of Table 4 to 800px
  </big-table>

  Comparing Table 5 and Table:

  <\itemize>
    <item><math|width<around*|(|column 1 of Table
    5|)>\<neq\>width<around*|(|column 1 of Table 6|)>>
  </itemize>

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