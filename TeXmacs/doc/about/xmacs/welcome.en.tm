<TeXmacs|2.1.1>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths|british>>

<\body>
  <\tmdoc-underline>
    <tmdoc-title-font|Welcome to Mogan Editor>
  </tmdoc-underline>

  \;

  Mogan Editor is a fork of GNU <TeXmacs>. Its major differences with
  <TeXmacs> are

  <\itemize>
    <item>Switch from Qt 4.x to Qt 5.x

    <item>Switch from GNU Guile 1.8 to S7
  </itemize>

  Mogan v1.x is based on GNU <TeXmacs> 2.1.1 with the same functionalities.
  Mogan v1.0.x focuses on bug fixes, usabilities and performance.

  <section|Feature Overview>

  <\description>
    <item*|Default Configuration>

    <\itemize>
      <item><menu|Edit|Preferences|General|Look and feel> switches from
      <name|macOS> or <name|Windows> to <name|Emacs>

      <item><menu|Edit|Preferences|General|Interactive questions> switches
      from <menu|On the footer> to <menu|In popup windows>

      <item><menu|Edit|Preferences|General|Buffer management> switches from
      <menu|Documents in separate windows> to <menu|Multiple documents share
      window>

      <item><menu|Edit|Preferences|Other|Document updates run> switches from
      Once to Tree times
    </itemize>

    <item*|Keyboard>

    The default key of Meta on Windows is Win and most key combo with Win is
    conflicted with the system default one. Mogan switches the Meta key from
    Win to Alt to avoid conflicts with system key combo.

    <\itemize-dot>
      <item>Shortcut to insert \<#2460\>\<#2461\>\<#2462\><text-dots>\<#2469\>
      for Chinese documents, eg. <key|1 @> to insert \<#2460\> (v1.0.1)
    </itemize-dot>

    <item*|New Plugins>

    <\itemize>
      <item><hlink|quiver|https://q.uiver.app/> plugin helps you draw
      commutative diagrams in a modern way

      <item>pyenv plugin helps you manage the virtual python environment for
      Python Session
    </itemize>

    <item*|Demo Documents>

    <\itemize>
      <item><hlink|\<#300A\>\<#5A01\>\<#5C3C\>\<#65AF\>\<#300B\>|$TEXMACS_PATH/doc/about/xmacs/\<#5A01\>\<#5C3C\>\<#65AF\>.tm>

      <item><hlink|\<#300A\>\<#5341\>\<#4E8C\>\<#6708\>\<#5341\>\<#4E5D\>\<#591C\>\<#300B\>|$TEXMACS_PATH/doc/about/xmacs/\<#5341\>\<#4E8C\>\<#6708\>\<#5341\>\<#4E5D\>\<#591C\>.tm>

      <item><hlink|\<#300A\>\<#8BD7\>\<#516B\>\<#7AE0\>\<#300B\>|$TEXMACS_PATH/doc/about/xmacs/\<#8BD7\>\<#516B\>\<#7AE0\>.tm>

      <item>2019\<#5E74\>\<#5168\>\<#56FD\>\<#9AD8\>\<#4E2D\>\<#6570\>\<#5B66\>\<#8054\>\<#5408\>\<#7ADE\>\<#8D5B\>\<#4E00\>\<#8BD5\>\<#8BD5\>\<#9898\>A\<#5377\>
      \<#FF08\><hlink|\<#5185\>\<#7F6E\>|$TEXMACS_PATH/doc/about/xmacs/2019\<#5E74\>\<#5168\>\<#56FD\>\<#9AD8\>\<#4E2D\>\<#6570\>\<#5B66\>\<#8054\>\<#5408\>\<#7ADE\>\<#8D5B\>\<#4E00\>\<#8BD5\>\<#8BD5\>\<#9898\>A\<#5377\>.tm>/<hlink|\<#6700\>\<#65B0\>|https://gitee.com/XmacsLabs/mogan/raw/main/TeXmacs/doc/about/xmacs/2019\<#5E74\>\<#5168\>\<#56FD\>\<#9AD8\>\<#4E2D\>\<#6570\>\<#5B66\>\<#8054\>\<#5408\>\<#7ADE\>\<#8D5B\>\<#4E00\>\<#8BD5\>\<#8BD5\>\<#9898\>A\<#5377\>.tm>\<#FF09\>

      <item>2021\<#5E74\>\<#9AD8\>\<#8003\>\<#5168\>\<#56FD\>\<#7532\>\<#5377\>\<#6570\>\<#5B66\>\<#6587\>\<#79D1\>\<#8BD5\>\<#9898\>\<#FF08\><hlink|\<#5185\>\<#7F6E\>|$TEXMACS_PATH/doc/about/xmacs/2021\<#5E74\>\<#5168\>\<#56FD\>\<#9AD8\>\<#8003\>\<#7532\>\<#5377\>\<#6570\>\<#5B66\>\<#6587\>\<#79D1\>\<#8BD5\>\<#9898\>.tm>/<hlink|\<#6700\>\<#65B0\>|https://gitee.com/XmacsLabs/mogan/raw/main/TeXmacs/doc/about/xmacs/2021\<#5E74\>\<#5168\>\<#56FD\>\<#9AD8\>\<#8003\>\<#7532\>\<#5377\>\<#6570\>\<#5B66\>\<#6587\>\<#79D1\>\<#8BD5\>\<#9898\>.tm>\<#FF09\>

      <item>2021\<#5E74\>\<#9AD8\>\<#8003\>\<#5168\>\<#56FD\>\<#4E59\>\<#5377\>\<#6570\>\<#5B66\>\<#6587\>\<#79D1\>\<#8BD5\>\<#9898\>\<#FF08\><hlink|\<#5185\>\<#7F6E\>|$TEXMACS_PATH/doc/about/xmacs/2021\<#5E74\>\<#5168\>\<#56FD\>\<#9AD8\>\<#8003\>\<#4E59\>\<#5377\>\<#6570\>\<#5B66\>\<#6587\>\<#79D1\>\<#8BD5\>\<#9898\>.tm>/<hlink|\<#6700\>\<#65B0\>|https://gitee.com/XmacsLabs/mogan/raw/main/TeXmacs/doc/about/xmacs/2021\<#5E74\>\<#5168\>\<#56FD\>\<#9AD8\>\<#8003\>\<#4E59\>\<#5377\>\<#6570\>\<#5B66\>\<#6587\>\<#79D1\>\<#8BD5\>\<#9898\>.tm>\<#FF09\>

      <item>2021\<#5E74\>\<#9AD8\>\<#8003\>\<#5168\>\<#56FD\>\<#7532\>\<#5377\>\<#6570\>\<#5B66\>\<#7406\>\<#79D1\>\<#8BD5\>\<#9898\>\<#FF08\><hlink|\<#5185\>\<#7F6E\>|$TEXMACS_PATH/doc/about/xmacs/2021\<#5E74\>\<#5168\>\<#56FD\>\<#9AD8\>\<#8003\>\<#7532\>\<#5377\>\<#6570\>\<#5B66\>\<#7406\>\<#79D1\>\<#8BD5\>\<#9898\>.tm>/<hlink|\<#6700\>\<#65B0\>|https://gitee.com/XmacsLabs/mogan/raw/main/TeXmacs/doc/about/xmacs/2021\<#5E74\>\<#5168\>\<#56FD\>\<#9AD8\>\<#8003\>\<#7532\>\<#5377\>\<#6570\>\<#5B66\>\<#7406\>\<#79D1\>\<#8BD5\>\<#9898\>.tm>\<#FF09\>

      <item>2021\<#5E74\>\<#9AD8\>\<#8003\>\<#5168\>\<#56FD\>\<#4E59\>\<#5377\>\<#6570\>\<#5B66\>\<#7406\>\<#79D1\>\<#8BD5\>\<#9898\>\<#FF08\><hlink|\<#5185\>\<#7F6E\>|$TEXMACS_PATH/doc/about/xmacs/2021\<#5E74\>\<#5168\>\<#56FD\>\<#9AD8\>\<#8003\>\<#4E59\>\<#5377\>\<#6570\>\<#5B66\>\<#7406\>\<#79D1\>\<#8BD5\>\<#9898\>.tm>/<hlink|\<#6700\>\<#65B0\>|https://gitee.com/XmacsLabs/mogan/raw/main/TeXmacs/doc/about/xmacs/2021\<#5E74\>\<#5168\>\<#56FD\>\<#9AD8\>\<#8003\>\<#4E59\>\<#5377\>\<#6570\>\<#5B66\>\<#7406\>\<#79D1\>\<#8BD5\>\<#9898\>.tm>\<#FF09\>

      <item>2021\<#5E74\>\<#5168\>\<#56FD\>\<#65B0\>\<#9AD8\>\<#8003\>\<#2160\>\<#5377\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>\<#FF08\><hlink|\<#5185\>\<#7F6E\>|$TEXMACS_PATH/doc/about/xmacs/2021\<#5E74\>\<#5168\>\<#56FD\>\<#65B0\>\<#9AD8\>\<#8003\>\<#2160\>\<#5377\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>.tm>/<hlink|\<#6700\>\<#65B0\>|https://gitee.com/XmacsLabs/mogan/raw/main/TeXmacs/doc/about/xmacs/2021\<#5E74\>\<#5168\>\<#56FD\>\<#65B0\>\<#9AD8\>\<#8003\>\<#2160\>\<#5377\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>.tm>\<#FF09\>

      <item>2021\<#5E74\>\<#5168\>\<#56FD\>\<#65B0\>\<#9AD8\>\<#8003\>II\<#5377\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>\<#FF08\><hlink|\<#5185\>\<#7F6E\>|$TEXMACS_PATH/doc/about/xmacs/2021\<#5E74\>\<#5168\>\<#56FD\>\<#65B0\>\<#9AD8\>\<#8003\>II\<#5377\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>.tm>/<hlink|\<#6700\>\<#65B0\>|https://gitee.com/XmacsLabs/mogan/raw/main/TeXmacs/doc/about/xmacs/2021\<#5E74\>\<#5168\>\<#56FD\>\<#65B0\>\<#9AD8\>\<#8003\>II\<#5377\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>.tm>\<#FF09\>

      <item>2021\<#5E74\>\<#5317\>\<#4EAC\>\<#5E02\>\<#9AD8\>\<#8003\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>\<#FF08\><hlink|\<#5185\>\<#7F6E\>|$TEXMACS_PATH/doc/about/xmacs/2021\<#5E74\>\<#5317\>\<#4EAC\>\<#5E02\>\<#9AD8\>\<#8003\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>.tm>/<hlink|\<#6700\>\<#65B0\>|https://gitee.com/XmacsLabs/mogan/raw/main/TeXmacs/doc/about/xmacs/2021\<#5E74\>\<#5317\>\<#4EAC\>\<#5E02\>\<#9AD8\>\<#8003\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>.tm>\<#FF09\>

      <item>2021\<#5E74\>\<#4E0A\>\<#6D77\>\<#5E02\>\<#9AD8\>\<#8003\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>\<#FF08\><hlink|\<#5185\>\<#7F6E\>|$TEXMACS_PATH/doc/about/xmacs/2021\<#5E74\>\<#4E0A\>\<#6D77\>\<#5E02\>\<#9AD8\>\<#8003\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>.tm>/<hlink|\<#6700\>\<#65B0\>|https://gitee.com/XmacsLabs/mogan/raw/main/TeXmacs/doc/about/xmacs/2021\<#5E74\>\<#4E0A\>\<#6D77\>\<#5E02\>\<#9AD8\>\<#8003\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>.tm>\<#FF09\>

      <item>2021\<#5E74\>\<#5929\>\<#6D25\>\<#5E02\>\<#9AD8\>\<#8003\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>\<#FF08\><hlink|\<#5185\>\<#7F6E\>|$TEXMACS_PATH/doc/about/xmacs/2021\<#5E74\>\<#5929\>\<#6D25\>\<#5E02\>\<#9AD8\>\<#8003\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>.tm>/<hlink|\<#6700\>\<#65B0\>|https://gitee.com/XmacsLabs/mogan/raw/main/TeXmacs/doc/about/xmacs/2021\<#5E74\>\<#5929\>\<#6D25\>\<#5E02\>\<#9AD8\>\<#8003\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>.tm>\<#FF09\>

      <item>2021\<#5E74\>\<#6D59\>\<#6C5F\>\<#7701\>\<#9AD8\>\<#8003\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>\<#FF08\><hlink|\<#5185\>\<#7F6E\>|$TEXMACS_PATH/doc/about/xmacs/2021\<#5E74\>\<#6D59\>\<#6C5F\>\<#7701\>\<#9AD8\>\<#8003\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>.tm>/<hlink|\<#6700\>\<#65B0\>|https://gitee.com/XmacsLabs/mogan/raw/main/TeXmacs/doc/about/xmacs/2021\<#5E74\>\<#6D59\>\<#6C5F\>\<#7701\>\<#9AD8\>\<#8003\>\<#6570\>\<#5B66\>\<#8BD5\>\<#9898\>.tm>\<#FF09\>
    </itemize>
  </description>

  <section|Contact us>

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|cell-background|#ffa>|<cwith|2|-1|1|-1|cell-halign|c>|<table|<row|<cell|What>|<cell|Links>>|<row|<cell|Discussion>|<cell|<hlink|Github
  Discussion|https://github.com/XmacsLabs/mogan/discussions>>>|<row|<cell|Report
  Bugs>|<cell|<hlink|Github Issues|https://github.com/XmacsLabs/mogan/issues>>>>>>>
    Contact us
  </big-table>

  <tmdoc-copyright|2020\U2022|<name|Xmacs> Labs>
</body>

<initial|<\collection>
</collection>>