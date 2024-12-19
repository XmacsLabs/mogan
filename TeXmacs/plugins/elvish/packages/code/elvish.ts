<TeXmacs|2.1.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|Elvish|1.0>

    <\src-purpose>
      Markup for Elvish sessions.
    </src-purpose>

    <src-copyright|2024|Liii Network Inc>

    <\src-license>
      All rights reverved.
    </src-license>
  </src-title>>

  <use-module|(data elvish)>

  <assign|elvish-lang|<macro|body|<with|mode|prog|prog-language|elvish|font-family|rm|<arg|body>>>> 

  <assign|elvish-code|<\macro|body>
    <\pseudo-code>
      <elvish-lang|<arg|body>>
    </pseudo-code>
  </macro>>
</body>

<initial|<\collection>
</collection>>
