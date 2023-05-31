<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|Unit Tests on L1 Kernel for OSPP Students>

  For analyze_test and tree_test, there are existing unit tests. Here are the
  command line to build and test:

  <\shell-code>
    bin/test_only tree_test

    bin/test_only analyze_test
  </shell-code>

  For the <cpp|rectangle> class, Edmond needs to create a new unit test.

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
      pokyux
    </cell>|<\cell>
      several (\<gtr\>3) unit tests for <cpp|tree>
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      14_2
    </cell>|<\cell>
      \;
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
      \;
    </cell>|<\cell>
      several (\<gtr\>3) unit tests for <cpp|point>
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      14_5
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      several (\<gtr\>3) unit tests for <cpp|curve>
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      14_6
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      several (\<gtr\>3) unit tests for <cpp|color>
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      14_7
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      several (\<gtr\>3) unit tests for <cpp|grid>
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      14_8
    </cell>|<\cell>
      \;
    </cell>|<\cell>
      several (\<gtr\>3) unit tests for <cpp|equations>
    </cell>>>>
  </wide-tabular>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>