<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <\hide-preamble>
    \;
  </hide-preamble>

  <tmdoc-title|Mogan Research 1.2.1>

  <section|New Projects>

  <tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<table|<row|<cell|<dlink|33>>|<cell|>|<cell|Spell
  checking as a plugin>>|<row|<cell|<dlink|38>>|<cell|Darcy>|<cell|Image
  formats as plugins>>|<row|<cell|<dlink|44>>|<cell|>|<cell|Versioning as
  plugin>>>>>

  <section|Unfinished Projects from <dlink|release_1.2.0>>

  <tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|6|6|1|1|cell-valign|c>|<cwith|18|18|1|1|cell-valign|b>|<cwith|15|15|1|1|cell-background|pastel
  red>|<cwith|14|14|1|1|cell-background|>|<table|<row|<cell|<dlink|66>>|<cell|<value|da>>|<cell|TMU
  format>>|<row|<cell|<dlink|61>>|<cell|Darcy>|<cell|Plugin
  Center>>|<row|<cell|<dlink|59>>|<cell|Darcy>|<cell|Tuning Keyboard
  Shortcuts>>|<row|<cell|<dlink|57>>|<cell|Pluto>|<cell|Slim
  libmogan>>|<row|<cell|<dlink|54>>|<cell|Pluto>|<cell|Community for Users
  and Developers>>|<row|<cell|<dlink|52>>|<cell|>|<cell|Improvements on
  Table>>|<row|<cell|<dlink|50>>|<cell|Oyyko>|<cell|Mogan Draw app & WASM
  (OSPP)>>|<row|<cell|<dlink|46>>|<cell|jingkaimori>|<cell|Handle indentation
  of first paragraph of section>>|<row|<cell|<dlink|45>>|<cell|jingkaimori>|<cell|Complete
  the new mechanism of glue code generation>>|<row|<cell|<dlink|43>>|<cell|Darcy>|<cell|<LaTeX>
  converter as a plugin>>|<row|<cell|<dlink|42>>|<cell|Pluto>|<cell|HTML
  converter as a plugin>>|<row|<cell|<dlink|41>>|<cell|<value|da>>|<cell|Use
  libgs instead of the gs binary>>|<row|<cell|<dlink|27>>|<cell|Oyyko>|<cell|Mogan
  Draw optimization (long term)>>|<row|<cell|<dlink|24>>|<cell|Pluto>|<cell|HTML
  import and export>>|<row|<cell|<dlink|23>>|<cell|Oyyko>|<cell|Mogan Draw
  optimization \ (OSPP)>>|<row|<cell|<dlink|22>>|<cell|Darcy>|<cell|Chinese
  Dictionary>>|<row|<cell|<dlink|19>>|<cell|Darcy>|<cell|Natural language
  related issues>>|<row|<cell|<dlink|5>>|<cell|Darcy>|<cell|Documentation in
  markdown format for users and developers>>>>>

  <section|<TeXmacs> v2.1.2 port>

  <\wide-tabular>
    <tformat|<cwith|2|2|2|2|cell-background|<pattern|C:\\Users\\jingkaimori\\Documents\\Source\\mogan\\build\\packages\\app.mogan\\data\\misc\\patterns\\vintage\\granite-medium.png||>>|<cwith|3|3|2|2|cell-background|pastel
    red>|<cwith|5|6|2|2|cell-background|pastel
    red>|<cwith|4|4|2|2|cell-background|#f0f0f0>|<cwith|7|7|2|2|cell-background|#f0f0f0>|<cwith|9|9|2|2|cell-background|pastel
    green>|<cwith|10|11|2|2|cell-background|pastel
    red>|<cwith|8|8|2|2|cell-background|pastel
    red>|<cwith|10|10|2|2|cell-background|pastel
    red>|<cwith|12|12|2|2|cell-background|pastel
    green>|<cwith|14|14|2|2|cell-background|#f0f0f0>|<cwith|15|15|2|2|cell-background|#f0f0f0>|<cwith|17|17|2|2|cell-background|#f0f0f0>|<cwith|16|16|2|2|cell-background|pastel
    red>|<cwith|18|18|2|2|cell-background|pastel red>|<table|<row|<\cell>
      SVN revision
    </cell>|<\cell>
      Status
    </cell>|<\cell>
      related task
    </cell>|<\cell>
      commit message
    </cell>>|<row|<\cell>
      <svn|14111>
    </cell>|<\cell>
      <with|color|white|base>
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      fix
    </cell>>|<row|<\cell>
      <\svn>
        14112
      </svn>
    </cell>|<\cell>
      todo
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      Tweaking for Carlito font
    </cell>>|<row|<\cell>
      <svn|14113>
    </cell>|<\cell>
      discarded
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      Merge branch 'qt5nopdf'
    </cell>>|<row|<\cell>
      <svn|14114>
    </cell>|<\cell>
      todo
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      Correction
    </cell>>|<row|<\cell>
      <svn|14115>
    </cell>|<\cell>
      todo
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      fixes for apidoc, including bug <savannah-bug|61989>
    </cell>>|<row|<\cell>
      <svn|14116>
    </cell>|<\cell>
      discarded
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      fixes for images, notably tiff
    </cell>>|<row|<\cell>
      <svn|14117>
    </cell>|<\cell>
      todo
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      Extra keyword
    </cell>>|<row|<\cell>
      <svn|14118>
    </cell>|<\cell>
      done
    </cell>|<\cell>
      <dlink|64_1><dlink|9_2>
    </cell>|<\cell>
      fix for #<savannah-bug|64258> and for de-embedding of images with
      unicode filename
    </cell>>|<row|<\cell>
      <svn|14119>
    </cell>|<\cell>
      todo
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      Allow user redefinition of the 'tit macro with different arity
    </cell>>|<row|<\cell>
      <svn|14120>
    </cell>|<\cell>
      todo
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      New Theme
    </cell>>|<row|<\cell>
      <svn|14121>
    </cell>|<\cell>
      done
    </cell>|<\cell>
      <dlink|63_1>
    </cell>|<\cell>
      Fix bug #<savannah-bug|64429>, definition of toc-5 seems wrong
    </cell>>|<row|<\cell>
      <text-dots>
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      \;
    </cell>>|<row|<\cell>
      <svn|14302>
    </cell>|<\cell>
      discarded
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      Fix bug #<savannah-bug|64880>: Crash when converting a big number to
      roman
    </cell>>|<row|<\cell>
      <svn|14303>
    </cell>|<\cell>
      discarded
    </cell>|<\cell>
      <dlink|63_2>
    </cell>|<\cell>
      Revert erroneous patch (disappearing references at every second
      refresh)
    </cell>>|<row|<\cell>
      <svn|14304>
    </cell>|<\cell>
      todo
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      Nicer microtypography for sans serif math capitals
    </cell>>|<row|<\cell>
      <svn|14305>
    </cell>|<\cell>
      discarded
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      DOC_zh: revert tmml doc to tm doc
    </cell>>|<row|<\cell>
      <svn|14306>
    </cell>|<\cell>
      todo
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      Doc_zh: fix shortcuts in main/editing/man-structured-move
    </cell>>>>
  </wide-tabular>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>