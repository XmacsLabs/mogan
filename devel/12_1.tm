<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|BibTeX import: encoding of tag name>

  <section|Bug Metadata>

  <\itemize>
    <item>Reporter: Darcy Shen on <hlink|Gitee|https://gitee.com/XmacsLabs/mogan/issues/I5LYI3>

    <item>Owner: Darcy Shen

    <item>Tester: Darcy Shen
  </itemize>

  <section|How to test it>

  Before, the book name messed up.

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (parse-bib (string-load "$TEXMACS_PATH/tests/12_1.bib"))
    <|unfolded-io>
      <\text>
        <\bib-entry|book|\<#6570\>\<#7406\>\<#903B\>\<#8F91\>2010\<#6C6A\>\<#82B3\>\<#5EAD\>>
          <bib-field|title|\<#6570\>\<#7406\>\<#903B\>\<#8F91\>.\<#7B2C\>2\<#7248\>>

          <bib-field|author|<bib-names|<bib-name|||\<#534E\>\<#7F57\>\<#5E9A\>|>>>

          <bib-field|publisher|\<#4E2D\>\<#56FD\>\<#79D1\>\<#5B66\>\<#6280\>\<#672F\>\<#5927\>\<#5B66\>\<#51FA\>\<#7248\>\<#793E\>>

          <bib-field|year|2011>
        </bib-entry>
      </text>
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

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
    <associate|page-medium|papyrus>
  </collection>
</initial>