<TeXmacs|2.1.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Insufficient punctuation rules for CJK>

  <paragraph|Bug Metadata>

  <\itemize>
    <item>Reporter: Rui Zhang

    <item>Mogan Editor Version: v1.1.1
  </itemize>

  <paragraph|Description>

  <\note*>
    Set the document language to Chinese and add extra Chinese characters to
    reproduce the bug.
  </note*>

  <with|language|chinese|<tabular|<tformat|<cwith|1|1|1|1|cell-width|1in>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#FF08\>\<#73BB\>\<#7483\>\<#FF09\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>

    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#73BB\>\<#7483\>\<#FF08\>\<#800C\>\<#4E0D\>\<#FF09\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>

    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#73BB\>\<#7483\>\<#201C\>\<#800C\>\<#4E0D\>\<#201D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>

    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#73BB\>\<#7483\>\<#800C\>\<#2014\>\<#2014\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>

    \<#6211\>\<#80FD\>\<#541E\>\<#4E0B\>\<#3010\>\<#73BB\>\<#7483\>\<#3011\>\<#800C\>\<#4E0D\>\<#4F24\>\<#8EAB\>\<#4F53\>\<#3002\>
  </cell>>>>>>

  \;

  Problems:

  <\itemize>
    <item>Punctuations like \P, \<#FF08\> and \<#3010\> should not be allowed
    at the end of a line.

    <item>Punctuations like \Q, \<#FF09\> and \<#3011\> should not be allowed
    at the beginning of a line.
  </itemize>

  <tmdoc-copyright|2022|Rui Zhang>
</body>

<initial|<\collection>
</collection>>