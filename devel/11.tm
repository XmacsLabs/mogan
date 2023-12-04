<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|Font tuning>

  Font handling is the major problem for performance concerns.

  <section|Tasks>

  <\wide-tabular>
    <tformat|<table|<row|<\cell>
      Chore
    </cell>|<\cell>
      11_1
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Only reserve fandol as default Chinese font on Linux
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      11_2
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Only check AppleGothic on macOS for Korean
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <dlink|11_3>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Remove the experimental option: Advanced font customization
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <dlink|11_4>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Set Noto CJK SC as default Chinese font on Linux
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      11_5
    </cell>|<\cell>
      woutersj
    </cell>|<\cell>
      Use fontconfig on Linux to detect font paths
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      11_6
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Detect font paths properly on Debian
    </cell>>|<row|<\cell>
      Bug
    </cell>|<\cell>
      <dlink|11_7>
    </cell>|<\cell>
      <value|da>
    </cell>|<\cell>
      Failed to render circled numbers
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      11_8
    </cell>|<\cell>
      <value|da>
    </cell>|<\cell>
      Faster tt_find_name via provided font_cache.scm
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      11_9
    </cell>|<\cell>
      <value|da>
    </cell>|<\cell>
      Support bbold font using glyph index
    </cell>>|<row|<\cell>
      Bug
    </cell>|<\cell>
      11_10
    </cell>|<\cell>
      <value|da>
    </cell>|<\cell>
      Fix rendering of letters in Unicode Latin-1 Supplement
    </cell>>|<row|<\cell>
      Bug
    </cell>|<\cell>
      11_11
    </cell>|<\cell>
      <value|da>
    </cell>|<\cell>
      Enable ligature when document language is Chinese
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      11_12
    </cell>|<\cell>
      <value|da>
    </cell>|<\cell>
      Fix typo in initialize_virtual
    </cell>>>>
  </wide-tabular>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>