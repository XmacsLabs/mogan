<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|CI integration for wasm platform>

  <section|Feature metadata>

  <\itemize>
    <item>Issue: <slink|https://gitee.com/XmacsLabs/mogan/issues/I64H5G>

    <item>Reporter: Darcy Shen

    <item>Owner: jingkaimori
  </itemize>

  <section|Description>

  This Pull request adjust xmake.lua and add ci for wasm.\ 

  To build with wasm, Mogan on wasm requires qt 5 and emsdk. Instruction to
  install emsdk and Qt can be found <hlink|here|https://doc.qt.io/qt-5/wasm.html>.

  <section|How to test>

  <\big-table|<tabular|<tformat|<table|<row|<cell|Tester>|<cell|Platform>|<cell|Version>|<cell|How>>|<row|<cell|jingkaimori>|<cell|wasm>|<cell|dev>|<cell|launch
  github ci>>|<row|<cell|>|<cell|GNU/Linux>|<cell|>|<cell|>>|<row|<cell|>|<cell|macOS>|<cell|>|<cell|>>|<row|<cell|>|<cell|Windows>|<cell|>|<cell|>>>>>>
    Check table for testing
  </big-table>

  <section|Known issues>

  Developers of Qt6 is refactoring <verbatim|qtloader.js>, so running with
  qt6 failed.

  xmake will treat qt test as normal wasm webpage, hence missing
  corresponding glue files.

  <tmdoc-copyright|2023|jingkaimori>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>