<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|devel>>

<\body>
  <tmdoc-title|After entering the function of moving, the cursor should
  become a small hand.>

  This is a <strong|Feature> of <with|font-series|bold|project 23>.\ 

  <section|Feature metadata>

  <\description>
    <item*|Owner>Oyyko

    <item*|Gitee issue><href|https://gitee.com/XmacsLabs/mogan/issues/I5XGM7>

    <item*|Github issue>NA
  </description>

  <section|Description>

  <subsection|Feature Description>

  After entering the function of moving in the graphics mode, the mouse
  cursor should become a small hand. <image|<tuple|<#89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF610000000970485973000013FE000013FE01079439840000007549444154388DA590510EC0200843A9D9FDAFDC7DCC2684090ED7C40FD1B62F98D5E23CA9B030684EF2B902483DC39B49DA34BD5AB3B7113F36C41800A10ABD328BE688C01728804A5D51544497E5DBDEB67B826D53D42CC330B7BC6E8827C017FC2AA025E1C780238A9583D51E7C7B24F84572AC1BB0103E0A7D06BA330000000049454E44AE426082>|png>|16pt|16pt||>
  and <image|<tuple|<#89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF610000000970485973000013FE000013FE01079439840000005D49444154388DDD50410AC0200C6B86FFFF72769848A7698B9EC60242149B2631FB25D84FC607300F93CF1F0016713F7B853648C9DD9D4A006E4329AC04968C15DA5B7C7B3EEE20438FB99458E69762E22D8DE2B7CF0E8E9C1C75F02DDC9ED8260DE8625F0D0000000049454E44AE426082>|png>|16pt|16pt||>
  are the OpenHand and CloseHand icon. They are
  <cpp|<hlink|Qt::OpenHandCursor|https://doc.qt.io/qt-5.15/qt.html#CursorShape-enum>>
  and <cpp|<hlink|Qt::ClosedHandCursor|https://doc.qt.io/qt-5.15/qt.html#CursorShape-enum>>
  in Qt. The planned working method is to change to Closed Hand when the
  mouse is pressed, and to Open Hand when the mouse is released.

  <subsection|Planned Design>

  <\enumerate-numeric>
    <item>The user clicks the mouse \<rightarrow\> mouseEvent Qt
    \<rightarrow\> eventQueue \<rightarrow\> process_mouse \<rightarrow\>
    mouse-event (scm)\ 

    Add dummy code for changing cursor display on mouse-xyz-event.
    <scm|(display "Change mouse style")>.

    <item>The dummy code will eventually call the dummy cpp code. <cpp|cout
    \<less\>\<less\> "change mouse style">.\ 

    <item>change mouse style C++ code.
  </enumerate-numeric>

  About changing the mouse style in C++ code:

  <\enumerate-numeric>
    <item>add mouse-style related interfaces in the widget.

    <item>Implement related interfaces in Qt.
  </enumerate-numeric>

  One of the goals is to implement an interface to change the mouse style in
  the source code, which can be used by future developers.

  <section|Related Code>

  <href|https://doc.qt.io/qt-5.15/qcursor.html>

  <href|<hlink|https://github.com/XmacsLabs/mogan/blob/1199d4857c94d7cbc1b5e32da8256badd624b88e/src/Plugins/Qt/qt_tm_widget.cpp#L46|https://github.com/XmacsLabs/mogan/blob/1199d4857c94d7cbc1b5e32da8256badd624b88e/src/Plugins/Qt/qt_tm_widget.cpp#L46>>

  <href|<hlink|https://github.com/XmacsLabs/mogan/blob/a3f4537c272bc110825b3d183ee05368c003f152/src/Plugins/Qt/QTMWidget.cpp#L287|https://github.com/XmacsLabs/mogan/blob/a3f4537c272bc110825b3d183ee05368c003f152/src/Plugins/Qt/QTMWidget.cpp#L287>>

  <href|<hlink|https://github.com/XmacsLabs/mogan/blob/bff6e75a6ebe2df744e9eed48eb85092b2094040/src/Plugins/Qt/qt_gui.cpp#L718|https://github.com/XmacsLabs/mogan/blob/bff6e75a6ebe2df744e9eed48eb85092b2094040/src/Plugins/Qt/qt_gui.cpp#L718>>

  <href|<hlink|https://github.com/XmacsLabs/mogan/blob/bff6e75a6ebe2df744e9eed48eb85092b2094040/src/Plugins/Qt/qt_gui.cpp#L803|https://github.com/XmacsLabs/mogan/blob/bff6e75a6ebe2df744e9eed48eb85092b2094040/src/Plugins/Qt/qt_gui.cpp#L803>>

  <href|<hlink|https://github.com/XmacsLabs/mogan/blob/015fb58dd983e560157fc884e1b0dff9cff67a63/src/Graphics/Gui/window.hpp#L65|https://github.com/XmacsLabs/mogan/blob/015fb58dd983e560157fc884e1b0dff9cff67a63/src/Graphics/Gui/window.hpp#L65>>

  \;

  <href|>

  \;

  \;

  <tmdoc-copyright|2023|Oyyko>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
  </collection>
</initial>