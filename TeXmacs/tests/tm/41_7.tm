<TeXmacs|2.1.2>

<style|<tuple|generic|no-page-numbers|chinese>>

<\body>
  <slink|$TEXMACS_PATH/plugins/binary/progs/binary/gs.scm>

  \;

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (gs-ps-image-size (system-\<gtr\>url
      "$TEXMACS_PATH/tests/eps/41_7.eps"))
    <|unfolded-io>
      (79 58 708 582)
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (gs-ps-to-pdf (system-\<gtr\>url "$TEXMACS_PATH/tests/eps/41_7.eps")
      (system-\<gtr\>url "/tmp/41_7.pdf"))
    <|unfolded-io>
      "Error: /undefined in \\"

      Operand stack:

      Execution stack: \ \ %interp_exit \ \ .runexec2 \ \ --nostringval--
      \ \ --nostringval-- \ \ --nostringval-- \ \ 2 \ \ %stopped_push
      \ \ --nostringval-- \ \ --nostringval-- \ \ --nostringval-- \ \ false
      \ \ 1 \ \ %stopped_push \ \ .runexec2 \ \ --nostringval--
      \ \ --nostringval-- \ \ --nostringval-- \ \ 2 \ \ %stopped_push
      \ \ --nostringval--

      Dictionary stack: \ \ --dict:747/1123(ro)(G)-- \ \ --dict:0/20(G)--
      \ \ --dict:85/200(L)--

      Current allocation mode is local"
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (check-stdout "echo \\"abc\\"")
    <|unfolded-io>
      "\\"abc\\""
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  Before the big ps image:

  <image|../eps/41_7.eps|380pt|316pt||>

  After the big ps image.
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
    <associate|page-screen-margin|false>
  </collection>
</initial>