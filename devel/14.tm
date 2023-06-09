<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|Unit Tests for OSPP Students in 2023>

  For analyze_test and tree_test, there are existing unit tests. Here are the
  command line to build and test:

  <\shell-code>
    bin/test_only tree_test

    bin/test_only analyze_test
  </shell-code>

  <cpp|tree> and <cpp|rectangle> are two important C++ class for the Mogan
  Draw OSPP project. The functions in <shell|analyze.cpp> are widely used for
  string manipulation.

  <section|Tasks>

  <\wide-tabular>
    <tformat|<table|<row|<\cell>
      Chore
    </cell>|<\cell>
      14_1
    </cell>|<\cell>
      pokyux, \<#5C18\>
    </cell>|<\cell>
      several (\<gtr\>3) unit tests for <cpp|tree>
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      14_2
    </cell>|<\cell>
      Oyyko
    </cell>|<\cell>
      several (\<gtr\>3) unit tests for <cpp|rectangle>
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      14_3
    </cell>|<\cell>
      tangdouer
    </cell>|<\cell>
      several (\<gtr\>3) unit tests for <cpp|analyze>
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      14_4
    </cell>|<\cell>
      \<#53F6\>\<#7530\>\<#946B\>
    </cell>|<\cell>
      several (\<gtr\>3) unit tests for <cpp|point>
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      14_5
    </cell>|<\cell>
      \<#53F6\>\<#7530\>\<#946B\>
    </cell>|<\cell>
      several (\<gtr\>3) unit tests for <cpp|curve>
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      14_6
    </cell>|<\cell>
      xcz_physics
    </cell>|<\cell>
      several (\<gtr\>3) unit tests for <cpp|color>
    </cell>>>>
  </wide-tabular>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>