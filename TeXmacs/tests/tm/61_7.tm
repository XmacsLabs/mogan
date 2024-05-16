<TeXmacs|2.1.2>

<style|<tuple|generic|no-page-numbers|chinese|s7>>

<\body>
  <\session|s7|default>
    <\output>
      S7 Scheme 10.6 (14-Apr-2023)
    </output>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (list-ref (list 1 2) 4)
    <|unfolded-io>
      out-of-range

      <\errput>
        \;

        ;list-ref second argument, 4, is out of range (it is too large)

        ; (list 1 2)

        \;
      </errput>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (/ 1 0)
    <|unfolded-io>
      division-by-zero

      <\errput>
        \;

        ;/: division by zero, (/ 1 0)

        ; (/ 1 0)

        ; ((lambda (hook lst) (when (or (not (proce...

        \;
      </errput>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (helllo)
    <|unfolded-io>
      unbound-variable

      <\errput>
        \;

        ;unbound variable helllo in (helllo)

        ; (helllo)

        ; ((lambda (hook lst) (when (or (not (proce...

        \;
      </errput>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      #b0003
    <|unfolded-io>
      unbound-variable

      <\errput>
        \;

        ;read-error ("#b0003 is not a number")

        ; (helllo)

        ; ((lambda (hook lst) (when (or (not (proce...

        \;

        \;

        ;unbound variable read-error

        ; (helllo)

        ; ((lambda (hook lst) (when (or (not (proce...

        \;
      </errput>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
    <associate|page-screen-margin|false>
  </collection>
</initial>