<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <\hide-preamble>
    \;
  </hide-preamble>

  <tmdoc-title|Mogan Editor 1.2.0>

  <section|Mogan Editor v1.2.0 beta>

  <tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|33|33|1|1|cell-background|pastel
  red>|<cwith|32|32|1|1|cell-background|pastel
  red>|<cwith|28|28|1|1|cell-background|pastel
  red>|<cwith|39|39|3|3|cell-valign|b>|<cwith|40|40|1|1|cell-valign|b>|<cwith|27|27|1|1|cell-background|pastel
  green>|<cwith|30|30|1|1|cell-background|pastel
  green>|<cwith|26|26|1|1|cell-background|pastel
  green>|<cwith|6|6|2|2|cell-valign|c>|<cwith|5|5|1|1|cell-valign|c>|<cwith|21|21|1|1|cell-background|pastel
  green>|<cwith|4|4|1|1|cell-background|pastel
  green>|<cwith|25|25|1|1|cell-background|pastel
  green>|<table|<row|<cell|<dlink|56>>|<cell|Darcy>|<cell|Tuning cache to
  avoid corruption>>|<row|<cell|<dlink|55>>|<cell|paradisum>|<cell|tmdb
  onboarding>>|<row|<cell|<dlink|54>>|<cell|Pluto>|<cell|Community for Users
  and Developers>>|<row|<cell|<dlink|53>>|<cell|Darcy>|<cell|Lolly
  1.2.0~1.2.6>>|<row|<cell|<dlink|52>>|<cell|>|<cell|Improvements on
  Table>>|<row|<cell|<dlink|51>>|<cell|Darcy>|<cell|Scheme
  Onboarding>>|<row|<cell|<dlink|50>>|<cell|Oyyko>|<cell|Mogan Draw app &
  WASM (OSPP)>>|<row|<cell|<dlink|49>>|<cell|jingkaimori>|<cell|Fine tuning
  of xmake configuration>>|<row|<cell|<dlink|48>>|<cell|Darcy>|<cell|Mogan
  Code on wasm>>|<row|<cell|<dlink|47>>|<cell|jingkaimori>|<cell|Fix
  mechanism of link navigating>>|<row|<cell|<dlink|46>>|<cell|jingkaimori>|<cell|Handle
  indentation of first paragraph of section>>|<row|<cell|<dlink|45>>|<cell|>|<cell|Complete
  the new mechanism of glue code generation>>|<row|<cell|<dlink|44>>|<cell|>|<cell|Versioning
  as plugin>>|<row|<cell|<dlink|43>>|<cell|Darcy>|<cell|LaTeX converters as
  plugin>>|<row|<cell|<dlink|42>>|<cell|Pluto>|<cell|HTML converters as
  plugin>>|<row|<cell|<dlink|41>>|<cell|>|<cell|Use libgs instead of the gs
  binary>>|<row|<cell|<dlink|40>>|<cell|>|<cell|Use XDG_DATA_PATH on
  Linux>>|<row|<cell|<dlink|39>>|<cell|>|<cell|Re-organize the Help
  menu>>|<row|<cell|<dlink|38>>|<cell|Darcy>|<cell|Image formats as
  plugins>>|<row|<cell|<dlink|37>>|<cell|Darcy>|<cell|CICD for the Windows
  platform>>|<row|<cell|<dlink|36>>|<cell|Darcy, jingkaimori>|<cell|Build on
  Windows platform>>|<row|<cell|<dlink|35>>|<cell|>|<cell|Use the Breeze
  icon>>|<row|<cell|<dlink|34>>|<cell|Oyyko>|<cell|CICD for the website and
  mogan wasm>>|<row|<cell|<dlink|33>>|<cell|>|<cell|Spell checking as a
  plugin>>|<row|<cell|<dlink|32>>|<cell|Darcy>|<cell|Build and Test on
  wasm>>|<row|<cell|<dlink|31>>|<cell|Pluto>|<cell|Lolly
  1.1.0>>|<row|<cell|<dlink|30>>|<cell|Darcy>|<cell|Lolly
  1.0.x>>|<row|<cell|<dlink|29>>|<cell|tangdouer>|<cell|OSPP PDF
  Project>>|<row|<cell|<dlink|27>>|<cell|>|<cell|Mogan Draw optimization
  (long term)>>|<row|<cell|<dlink|26>>|<cell|tangdouer>|<cell|OSPP PDF
  Project Onboarding>>|<row|<cell|<dlink|25>>|<cell|Pluto>|<cell|Add CI on
  Gitee>>|<row|<cell|<dlink|24>>|<cell|Pluto>|<cell|HTML import and
  export>>|<row|<cell|<dlink|23>>|<cell|Oyyko>|<cell|Mogan Draw optimization
  \ (OSPP)>>|<row|<cell|<dlink|22>>|<cell|Darcy>|<cell|Chinese
  Dictionary>>|<row|<cell|<dlink|19>>|<cell|Darcy>|<cell|Natural language
  related issues>>|<row|<cell|<dlink|18>>|<cell|jinkaimori>|<cell|Embedded
  documentation issues>>|<row|<cell|<dlink|13>>|<cell|Darcy>|<cell|Libcurl
  integration>>|<row|<cell|<dlink|11>>|<cell|Darcy>|<cell|Font
  tuning>>|<row|<cell|<dlink|7>>|<cell|Darcy>|<cell|Stability
  Issues>>|<row|<cell|<dlink|5>>|<cell|Darcy>|<cell|Documentation in markdown
  format for users and developers>>>>>

  <section|Mogan Editor 1.2.0 alpha>

  <\wide-tabular>
    <tformat|<cwith|16|16|1|1|cell-background|pastel
    green>|<cwith|9|9|1|1|cell-background|pastel
    green>|<cwith|8|8|1|1|cell-background|pastel
    green>|<cwith|7|7|1|1|cell-background|pastel
    green>|<cwith|6|6|1|1|cell-background|pastel
    green>|<cwith|3|3|1|1|cell-background|pastel
    green>|<cwith|1|1|1|1|cell-background|pastel
    green>|<cwith|13|13|1|1|cell-background|pastel
    green>|<cwith|10|10|1|1|cell-background|pastel
    green>|<cwith|2|2|1|1|cell-background|pastel
    green>|<cwith|12|12|1|1|cell-background|pastel
    green>|<cwith|4|4|1|1|cell-background|pastel
    green>|<cwith|5|5|1|1|cell-background|pastel
    green>|<cwith|11|11|1|1|cell-background|pastel
    green>|<cwith|15|15|1|1|cell-background|pastel
    green>|<cwith|14|14|1|1|cell-background|pastel green>|<table|<row|<\cell>
      <dlink|28>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Use Lolly and Remove Kernel L1
    </cell>>|<row|<\cell>
      <dlink|21>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Deprecate getenv in Scheme
    </cell>>|<row|<\cell>
      <dlink|20>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Editor related issues
    </cell>>|<row|<\cell>
      <dlink|17>
    </cell>|<\cell>
      jinkaimori
    </cell>|<\cell>
      Character count for CJK
    </cell>>|<row|<\cell>
      <dlink|16>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Code formatting via clang-format
    </cell>>|<row|<\cell>
      <dlink|15>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      L3 Kernel
    </cell>>|<row|<\cell>
      <dlink|14>
    </cell>|<\cell>
      OSPP Students
    </cell>|<\cell>
      Unit Tests on L1 Kernel for OSPP Students
    </cell>>|<row|<\cell>
      <dlink|12>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      BibTeX improvements
    </cell>>|<row|<\cell>
      <dlink|10>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Tuning default settings
    </cell>>|<row|<\cell>
      <dlink|9>
    </cell>|<\cell>
      jinkaimori
    </cell>|<\cell>
      Fix corrupted field generated by export
    </cell>>|<row|<\cell>
      <dlink|8>
    </cell>|<\cell>
      jinkaimori
    </cell>|<\cell>
      xmake integration for packaging on Ubuntu/Windows/macOS
    </cell>>|<row|<\cell>
      <dlink|6>
    </cell>|<\cell>
      jinkaimori
    </cell>|<\cell>
      Avoid retrieving large pictures from texmacs website
    </cell>>|<row|<\cell>
      <dlink|4>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Deprecate Qt 4.x support and add Qt 6.x support on Linux
    </cell>>|<row|<\cell>
      <dlink|3>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      xmake integration on Ubuntu
    </cell>>|<row|<\cell>
      <dlink|2>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      S7 Scheme Integration
    </cell>>|<row|<\cell>
      <dlink|1>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Cleaning for Mogan Editor 1.2.x
    </cell>>>>
  </wide-tabular>

  \;
</body>

<initial|<\collection>
</collection>>