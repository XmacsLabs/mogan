<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|devel>>

<\body>
  <\hide-preamble>
    \;
  </hide-preamble>

  <\tmdoc-title>
    Rework on keyPressEvent and

    keyReleaseEvent
  </tmdoc-title>

  <section|Feature Metadata>

  <\itemize>
    <item>Reporter: None

    <item>Owner: <value|da>

    <item>Tester: <value|da>
  </itemize>

  <section|What>

  <section|Why>

  <section|How>

  <subsection|How to test>

  <subsubsection|Single key>

  <paragraph|macOS>

  <\verbatim-code>
    \<#2018\>1234567890-=

    qwertyuiop[]\\

    asdfghjkl;'

    zxcvbnm,./
  </verbatim-code>

  <paragraph|UOS>

  <\verbatim-code>
    \<#2018\>1234567890-=

    qwertyuiop[]\\

    asdfghjkl;'

    zxcvbnm,./
  </verbatim-code>

  <paragraph|Windows>

  <\verbatim-code>
    \<#2018\>1234567890-=

    qwertyuiop[]\\

    asdfghjkl;'

    zxcvbnm,./
  </verbatim-code>

  <subsubsection|Modifier: Shift>

  <paragraph|macOS>

  <\verbatim-code>
    ~!@#$^&*()_+

    QWERTYUIOP{}\|

    ASDFGHJKL:"

    ZXCVBNM\<less\>\<gtr\>?
  </verbatim-code>

  <paragraph|UOS>

  <\verbatim-code>
    ~!@#$%^&*()_+

    QWERTYUIOP{}\|

    ASDFGHJKL:"

    ZXCVBNM\<less\>\<gtr\>?
  </verbatim-code>

  <paragraph|Windows>

  <\verbatim-code>
    ~!@#$%^&*()_+

    QWERTYUIOP{}\|

    ASDFGHJKL:"

    ZXCVBNM\<less\>\<gtr\>?
  </verbatim-code>

  <subsubsection|Single key or with Shift modifer under IM>

  <paragraph|macOS>

  <\verbatim-code>
    \<cdot\>1234567890-=\<#3010\>\<#3011\>\<#3001\>\<#FF1B\>`\<#FF0C\>\<#3002\>/

    \<#FF5E\>\<#FF01\>@#\<#A5\>%\<ldots\>\<ldots\>&*\<#FF08\>\<#FF09\>\V\V+\<#300C\>\<#300D\>\<#FF5C\>\<#FF1A\>\P\Q\<#300A\>\<#300B\>\<#FF1F\>
  </verbatim-code>

  <paragraph|UOS>

  <\verbatim-code>
    \<cdot\>1234567890-=\<#3010\>\<#3011\>\<#3001\>\<#FF1B\>\<#FF0C\>\<#3002\>\<#3001\>

    \<#FF5E\>\<#FF01\>@#\<#FFE5\>%\<ldots\>\<ldots\>&*\<#FF08\>\<#FF09\>--+{}\|\<#FF1A\>\P\Q\<#300A\>\<#300B\>\<#FF1F\>
  </verbatim-code>

  <paragraph|Windows>

  <\verbatim-code>
    \<cdot\>1234567890-=\<#3010\>\<#3011\>\<#3001\>\<#FF1B\>'\<#FF0C\>\<#3002\>/

    ~\<#FF01\>@#\<#FFE5\>%\<ldots\>\<ldots\>&*\<#FF08\>\<#FF09\>+{}\|\<#FF1A\>\P\Q\<#300A\>\<#300B\>\<#FF1F\>
  </verbatim-code>

  <subsubsection|Special input via IM>

  <\verbatim-code>
    ^_^ (shift+6 under IM)

    hello (hello under IM)
  </verbatim-code>

  <subsubsection|Shortcuts: <key|A->>

  <key|A-1>\<#FF0C\><key|A-2>\<#FF0C\><key|A-3>\<#FF0C\><key|A-4>\<#FF0C\><key|A-5>

  <key|A-a>\<#FF0C\><key|A-d>\<#FF0C\><key|A-v>\<#FF0C\><key|A-p>\<#FF0C\><key|A-q>\<#FF0C\><key|A-s>

  <key|A-$>\<#FF0C\><key|A-&>

  <subsubsection|Shortcuts: <key|C->>

  <key|C-!>\<#FF0C\><key|C-?>\<#FF0C\><key|C-#>

  <subsubsection|macOS Shortcuts: <key|M->>

  <key|M-0>\<#FF0C\><key|M-8>\<#FF0C\><key|M-9>

  <key|M- ->\<#FF0C\><key|M-+>

  <subsubsection|macOS Shortcuts: <key|M-C->>

  <key|M-C-p>\<#FF0C\><key|M-C-s>

  <subsubsection|Structured Insert/Delete>

  <key|C-delete>\<#FF0C\><key|C-backspace>\<#FF0C\><key|A-delete>\<#FF0C\><key|A-backspace>\<#FF0C\><key|A-left>\<#FF0C\><key|A-right>\<#FF0C\><key|A-up>\<#FF0C\><key|A-down>

  <tree|root|left|right>

  <key|C-left>,<key|C-right>

  This is a paragraph.

  <\tmdoc-copyright|2023>
    <value|da>
  </tmdoc-copyright>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
    <associate|preamble|false>
  </collection>
</initial>