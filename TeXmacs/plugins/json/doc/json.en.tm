<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|json>>

<\body>
  <\hide-preamble>
    <assign|code-name|json>

    <assign|code-Name|JSON>
  </hide-preamble>

  <tmdoc-title|<value|code-Name> Code Plugin>

  <paragraph|<value|code-Name> style package>

  Click <menu|Document|Style|Add package|Code|<value|code-name>> or click
  <menu|<icon|tm_add.svg>|Code|<value|code-name>> on the focus toolbar to add
  the <value|code-Name> style package.

  In the <code-Name> style package, <markup|<value|code-name>> is provided
  for inline snippets and <markup|<value|code-name>-code> is provided for
  <code-Name> code blocks.

  <paragraph|<value|code-Name> syntax>

  The syntax of the <value|code-Name> language is defined in:

  <slink|$TEXMACS_PATH/plugins/dot/progs/code/<value|code-name>-lang.scm>

  Here is an example <value|code-Name> code block with syntax highlight:

  <\json-code>
    {

    \ \ "name": "\<#5F20\>\<#4E09\>",

    \ \ "age": 30,

    \ \ "isStudent": false,

    \ \ "email": "zhangsan@example.com",

    \ \ "address": {

    \ \ \ \ "street": "\<#5E78\>\<#798F\>\<#8DEF\>123\<#53F7\>",

    \ \ \ \ "city": "\<#5317\>\<#4EAC\>",

    \ \ \ \ "postalCode": "100000"

    \ \ },

    \ \ "phoneNumbers": [

    \ \ \ \ {

    \ \ \ \ \ \ "type": "home",

    \ \ \ \ \ \ "number": "123-456-7890"

    \ \ \ \ },

    \ \ \ \ {

    \ \ \ \ \ \ "type": "office",

    \ \ \ \ \ \ "number": "098-765-4321"

    \ \ \ \ }

    \ \ ],

    \ \ "hobbies": ["\<#9605\>\<#8BFB\>", "\<#65C5\>\<#6E38\>",
    "\<#6444\>\<#5F71\>", "\<#97F3\>\<#4E50\>"]

    }
  </json-code>

  <paragraph|<value|code-Name> editing>

  The editing of the <value|code-Name> code is defined in:

  <slink|$TEXMACS_PATH/plugins/<value|code-name>/progs/code/<value|code-name>-edit.scm>

  <tmdoc-copyright|2024|Darcy Shen>

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
    <associate|page-screen-margin|true>
  </collection>
</initial>