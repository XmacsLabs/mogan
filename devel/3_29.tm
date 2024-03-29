<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese>>

<\body>
  <tmdoc-title|Separate package cache from xmake cache>

  <section|Feature metadata>

  <\description>
    <item*|Owner>jingkaimori
  </description>

  <section|Description>

  <subsection|What>

  This feature extract external package requirements from xmake.lua to a
  separate file, which can be used as independent key of package cache.

  <subsection|Why>

  If key of build cache and package cache are different, it will be possible
  to reuse package cache with modified build configuration.

  <subsection|How>

  All configuration of package must be put in
  <verbatim|misc/xmake/packages.lua>, and instruction of global configuration
  such as <verbatim|add_requires()> must be placed in function
  <verbatim|add_mogan_packages>. This file should be included and the
  function should be invoked so that configurations can take effect.

  <tmdoc-copyright|2023|jingkaimori>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>.
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>