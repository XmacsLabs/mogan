<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese>>

<\body>
  <tmdoc-title|Inconsistent CJK font size>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: Rui Zhang

    <item>Issue: <slink|https://github.com/XmacsLabs/mogan/issues/396>
  </itemize>

  <section|Reproducer>

  Use Mogan Editor v1.1.1 to reproduce the bug:

  <\itemize-dot>
    <item><inactive|<with|font|simsun|<text-dots>>> produces the correct
    size:

    <underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>>
    10pt each.

    <with|font|SimSun|我能吞下玻璃而不伤身体。>I can eat glass,
    it does not hurt me.

    <item>Using <samp|Roman> produces smaller font:

    我能吞下玻璃而不伤身体。I can eat glass, it does not hurt me.

    <item>Using <samp|sys-chinese> produces slightly smaller font:

    <underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>><underline|<space|10pt>>
    10pt each.

    <\with|font|sys-chinese>
      我能吞下玻璃而不伤身体。I can eat glass, it does not hurt
      me.
    </with>
  </itemize-dot>

  <section|Drawbacks of incorrect size>

  <\itemize>
    <item>Inconistent output compared to other software.

    <item>Chinese paragraphs usually use a 2-character (20pt in this case)
    first indent; it cannot be set precisely if the size is incorrect.
  </itemize>

  <section|Goal>

  <\itemize>
    <item>Same output size no matter specifying the Chinese font manually,
    using <code*|Roman> and font substitution, or using <code*|sys-chinese>.

    <item>Allow a precise 2-character first indent.
  </itemize>

  <tmdoc-copyright|2022|Rui Zhang>
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
  </collection>
</initial>