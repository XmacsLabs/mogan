<TeXmacs|2.1.2>

<style|<tuple|tmdoc|chinese>>

<\body>
  <\tmdoc-title>
    S7: upgrade from 9.12 to 10.5
  </tmdoc-title>

  <section|Feature Metadata>

  <\itemize>
    <item>Owner: jingkaimori

    <item>Tester:
  </itemize>

  <section|Description>

  I, the owner of this story, would like to upgrade S7 to current version
  before develop S7 unicode support.

  S7 is upgraded from 9.12 to 10.5, with several performace improves and
  changes of several api:

  <\itemize>
    <item><scm|nan>, <verbatim|nan-payload>,
    <verbatim|+nan.\<less\>int\<gtr\>>.

    <item><verbatim|s7_let_field*> synonyms: <verbatim|s7_starlet*>.

    <item><verbatim|s7_number_to_real_with_location>,
    \ <verbatim|s7_wrong_type_error>. \ <verbatim|s7_make_string_wrapper_with_length>.
    <verbatim|s7_make_semipermanent_string>.

    <item><verbatim|s7_is_multiple_value>.

    <item>removed <verbatim|s7_apply_*>.

    <item><verbatim|s7_eval_with_location>.

    <item>moved <verbatim|s7_p_p_t> and friends into <verbatim|s7.h>.

    <item><verbatim|s7_make_byte_vector>, <verbatim|s7_is_byte_vector>,
    <verbatim|s7_byte_vector_ref\|set\|elements>.

    <item><verbatim|s7_output_string> (like <verbatim|s7_get_output_string>,
    but returns an s7 string).

    <item><verbatim|s7_is_random_state>, <verbatim|s7_make_normal_vector>.
    <verbatim|s7_array_to_list>.
  </itemize>

  these affected api is not used by texmacs, thus there is no concern of
  breaking compability.

  <tmdoc-copyright|2022|Jingkaimori>

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
    <associate|page-medium|paper>
  </collection>
</initial>