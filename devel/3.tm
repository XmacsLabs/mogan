<TeXmacs|2.1.2>

<style|<tuple|tmdoc|devel>>

<\body>
  <tmdoc-title|xmake Integration on Ubuntu>

  For Mogan Editor 1.2.x, we will use xmake as the build tool. This is the
  third project. It depends on the initial task of the second project (S7
  Scheme integration). In this project, we will complete the initial xmake
  support on Ubuntu from building to packaging.

  <section|Tasks>

  <\wide-tabular>
    <tformat|<table|<row|<\cell>
      Chore
    </cell>|<\cell>
      3_1
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Remove the included C++ source code of PDF Hummus
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <dlink|3_2>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Import xmake build definition before the kernel decouple from branch
      1.1
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      3_3
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Github Action to build and test on Ubuntu
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      3_4
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      setup Github Action cache on Ubuntu
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      3_5
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      script to package on Debian
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <dlink|3_6>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Decouple the kernel part of GNU <TeXmacs>
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <dlink|3_7>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Scripts to run all C++ tests and run only the target test
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      3_8
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Build definition for the shell plugin
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      3_9
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Enable Iconv
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      3_10
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Improve config.h.xmake
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <dlink|3_11>
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      pin dependencies for reproducible build
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      3_12
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      use packages provided by apt on Debian and Ubuntu
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      3_13
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      use packages provided by apt on UOS
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      3_14
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Github Action to package on Ubuntu
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      3_15
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Support mingw/msys building process
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      3_16
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Github Action to build and test on msys environment
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      3_17
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Github Action to package on msys environment
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      3_18
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Support wasm building process
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      3_19
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Github Action to build for wasm
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <todo|3_20>
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      Github Action to test for wasm
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      <todo|3_21>
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      Github Action to deploy for wasm
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      3_22
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Adjust version information of PDF Hummus
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      3_23
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Remove the included C++ source code of nowide
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      3_24
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Config and use doxygen to generate doxyfile
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      3_25
    </cell>|<\cell>
      jingkaimori
    </cell>|<\cell>
      Adjust <verbatim|url_test.cpp> for mingw build
    </cell>>>>
  </wide-tabular>

  <tmdoc-copyright|2023|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>