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

  <tabular|<tformat|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|1|1|cell-width|70pt>|<cwith|2|2|1|1|cell-hmode|exact>|<cwith|2|2|2|2|cell-hyphen|t>|<cwith|2|2|3|3|cell-hyphen|t>|<cwith|1|-1|2|2|cell-width|70pt>|<cwith|1|-1|2|2|cell-hmode|exact>|<cwith|1|-1|3|3|cell-width|70pt>|<cwith|1|-1|3|3|cell-hmode|exact>|<cwith|2|2|4|4|cell-hyphen|t>|<cwith|2|2|4|4|cell-width|70pt>|<cwith|2|2|4|4|cell-hmode|exact>|<table|<row|<cell|baseline>|<cell|bold>|<cell|strong>|<cell|blue>>|<row|<\cell>
    我能吞下玻璃而不伤身体。

    \;
  </cell>|<\cell>
    我能吞下玻璃<strong|而不>伤身体。

    我能吞下玻<strong|璃而>不伤身体。

    我能吞下玻璃而<strong|不伤>身体。
  </cell>|<\cell>
    我能吞下玻璃<strong|而不>伤身体。

    我能吞下玻<strong|璃而>不伤身体。

    我能吞下玻璃而<strong|不伤>身体。
  </cell>|<\cell>
    我能吞下玻璃<with|color|blue|而不>伤身体。

    我能吞下玻<with|color|blue|璃而>不伤身体。

    我能吞下玻璃而<with|color|blue|不伤>身体。
  </cell>>>>>

  <subsection|In the british language>

  <with|language|british|<tabular|<tformat|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|2|2|cell-hyphen|t>|<cwith|2|2|3|3|cell-hyphen|t>|<cwith|1|-1|1|1|cell-width|70pt>|<cwith|1|-1|1|1|cell-hmode|exact>|<cwith|1|-1|2|2|cell-width|70pt>|<cwith|1|-1|2|2|cell-hmode|exact>|<cwith|1|-1|3|3|cell-width|70pt>|<cwith|1|-1|3|3|cell-hmode|exact>|<cwith|2|2|4|4|cell-hyphen|t>|<cwith|1|-1|4|4|cell-width|70pt>|<cwith|1|-1|4|4|cell-hmode|exact>|<table|<row|<cell|baseline>|<cell|>|<cell|>|<cell|>>|<row|<\cell>
    The quick brown fox jumps over the lazy dog.

    \;
  </cell>|<\cell>
    The quick brown <strong|fox jumps> over the lazy dog.
  </cell>|<\cell>
    The quick <strong|brown fox> jumps over the lazy dog.
  </cell>|<\cell>
    The quick brown fox <strong|jumps over> the lazy dog.
  </cell>>>>>>

  <tmdoc-copyright|2022|Rui Zhang|Darcy Shen>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>