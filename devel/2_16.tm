<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese|devel>>

<\body>
  <tmdoc-title|Use procedures defined in S7 to look up metadata instead of
  procedures defined in Guile.>

  <section|Bug Metadata>

  <\description>
    <item*|Reporter> jingkaimori

    <item*|Github issue> <hlink|732|https://github.com/XmacsLabs/mogan/issues/732>
  </description>

  <section|How to reproduce it>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (help min)
    <|unfolded-io>
      <errput|unbound variable procedure-documentation in
      (procedure-documentation about)>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (procedure-sources car)
    <|unfolded-io>
      (())
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (procedure-sources detect-remote-plugins)
    <|unfolded-io>
      ((lambda (where) (load-remote-plugins) (ahash-set! remote-plugins-table
      where (get-remote-plugin-info where)) (update-remote-tables)
      (save-remote-plugins)))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (set-trace-level! car)
    <|unfolded-io>
      <errput|unbound variable procedure-property in (procedure-property car
      'trace-wrapped)>
    </unfolded-io>

    <\input|Scheme] >
      (texmacs-mode-mode in-prog?) ;this will hang the whole editor
    </input>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <section|Description>

  This bug is similar to <dlink|2_4>.

  Procedures need to be examined and replaced are listed below, according to
  <hlink|Guile manual|https://www.gnu.org/software/guile/manual/guile.html#index-procedure_002ddocumentation>:

  <\itemize>
    <item><scm|procedure-source>, which has the same usage as S7

    <item><scm|procedure-documentation>

    <item><scm|procedure-name> (handled in <dlink|2_4> partly)

    <item><verbatim|procedure-property>, which affects behavior of
    <scm|set-trace-level!>.
  </itemize>

  <tmdoc-copyright|2023|jingkaimori>

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