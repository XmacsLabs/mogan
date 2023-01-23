<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese>>

<\body>
  <tmdoc-title|Line break is prohibited between CJK if either is enclosed in
  an environment>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: Rui Zhang

    <item>Mogan Editor Version: v1.1.1

    <item>Issue: <slink|https://github.com/XmacsLabs/mogan/issues/398>
  </itemize>

  <section|Description>

  <subsection|Using the bold font>

  <tabular|<tformat|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|1|1|cell-width|70pt>|<cwith|2|2|1|1|cell-hmode|exact>|<cwith|2|2|2|2|cell-hyphen|t>|<cwith|1|-1|2|2|cell-width|75pt>|<cwith|1|-1|2|2|cell-hmode|exact>|<cwith|2|2|3|3|cell-hyphen|t>|<cwith|1|-1|3|3|cell-width|80pt>|<cwith|1|-1|3|3|cell-hmode|exact>|<table|<row|<cell|70pt>|<cell|75pt>|<cell|80pt>>|<row|<\cell>
    我能吞下玻璃而不伤身体。

    我能吞下玻璃而<with|font-series|bold|不伤>身体。

    我能吞下玻璃而<line-break><with|font-series|bold|不伤>身体。
  </cell>|<\cell>
    我能吞下玻璃而不伤身体。

    我能吞下玻璃而<with|font-series|bold|不伤>身体。

    我能吞下玻璃而<line-break><with|font-series|bold|不伤>身体。
  </cell>|<\cell>
    我能吞下玻璃而不伤身体。

    我能吞下玻璃而<with|font-series|bold|不伤>身体。

    我能吞下玻璃而<line-break><with|font-series|bold|不伤>身体。
  </cell>>>>>

  <subsection|Using the strong content tag>

  <tabular|<tformat|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|1|1|cell-width|70pt>|<cwith|2|2|1|1|cell-hmode|exact>|<cwith|2|2|2|2|cell-hyphen|t>|<cwith|1|-1|2|2|cell-width|75pt>|<cwith|1|-1|2|2|cell-hmode|exact>|<cwith|2|2|3|3|cell-hyphen|t>|<cwith|1|-1|3|3|cell-width|80pt>|<cwith|1|-1|3|3|cell-hmode|exact>|<table|<row|<cell|70pt>|<cell|75pt>|<cell|80pt>>|<row|<\cell>
    我能吞下玻璃而不伤身体。

    我能吞下玻璃而<strong|不伤>身体。
  </cell>|<\cell>
    我能吞下玻璃而不伤身体。

    我能吞下玻璃而<strong|不伤>身体。
  </cell>|<\cell>
    我能吞下玻璃而不伤身体。

    我能吞下玻璃而<strong|不伤>身体。
  </cell>>>>>

  <subsection|Using the blue font color>

  <tabular|<tformat|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|1|1|cell-width|70pt>|<cwith|2|2|1|1|cell-hmode|exact>|<cwith|2|2|2|2|cell-hyphen|t>|<cwith|1|-1|2|2|cell-width|75pt>|<cwith|1|-1|2|2|cell-hmode|exact>|<cwith|2|2|3|3|cell-hyphen|t>|<cwith|1|-1|3|3|cell-width|80pt>|<cwith|1|-1|3|3|cell-hmode|exact>|<table|<row|<cell|70pt>|<cell|75pt>|<cell|80pt>>|<row|<\cell>
    我能吞下玻璃而不伤身体。

    我能吞下玻璃而<with|color|blue|不伤>身体。
  </cell>|<\cell>
    我能吞下玻璃而不伤身体。

    我能吞下玻璃而<with|color|blue|不伤>身体。
  </cell>|<\cell>
    我能吞下玻璃而不伤身体。

    我能吞下玻璃而<with|color|blue|不伤>身体。
  </cell>>>>>

  <tmdoc-copyright|2022|Rui Zhang|Darcy Shen>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>