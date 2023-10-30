<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|devel>>

<\body>
  <tmdoc-title|Wrong image size>

  <section|Bug metadata>

  <\description>
    <item*|Owner><value|da>

    <item*|Reporter><value|da><strong|>
  </description>

  <section|How to reproduce it>

  Here is the image from web:

  https://gitee.com/XmacsLabs/mogan/raw/v1.1.6/TeXmacs/misc/images/texmacs-256.png

  Put the web image<strong|> in a <markup|image> tag:

  <image|https://gitee.com/XmacsLabs/mogan/raw/v1.1.6/TeXmacs/misc/images/texmacs-256.png||||>

  Put the included image here:

  <image|../TeXmacs/misc/images/texmacs-256.png|256pt|256pt||><compound|markup|>

  Secondly, the image size of the web image should be 192pt. Both the
  included local image or the web image are using the wrong image size.

  Here is the expected image size:

  <image|../TeXmacs/misc/images/texmacs-256.png|192pt|192pt||>

  \;

  <tmdoc-copyright|2023|Darcy>

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