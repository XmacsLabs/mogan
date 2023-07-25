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

  \;
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>