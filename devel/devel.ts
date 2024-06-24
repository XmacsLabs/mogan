<TeXmacs|2.1.2>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|devel|0.1>

      <src-purpose|style for dev docs>
    </src-title>
  </active*>

  <use-package|tmdoc|british>

  <\active*>
    <\src-comment>
      Macro definitions.
    </src-comment>
  </active*>

  <assign|dlink|<macro|name|<hlink|[<arg|name>]|<merge|<arg|name>|.tmu>>>>

  Task Type:

  <assign|f|<strong|Feature>>

  <assign|b|<strong|Bug>>

  <assign|c|<strong|Chore>>

  Status of Task:

  <assign|ok|<strong|Done>>

  <assign|wip|<strong|WIP>>

  <assign|re|<strong|Ready>>

  <assign|li|<strong|Listed>>

  <assign|dis|<strong|Discarded>>

  Owers:

  <assign|da|Darcy>

  <assign|oy|Oyyko>

  <assign|jk|jingkaimori>

  <assign|tt|Paradisum>

  Useful shortcut:

  <assign|dr|<strong|Mogan Draw>>

  Links:

  <assign|svn|<macro|rev|<hlink|<arg|rev>|<merge|https://svn.savannah.gnu.org/viewvc/texmacs?view=revision&sortby=rev&revision=|<arg|rev>>>>>

  <assign|savannah-bug|<macro|rev|<hlink|<arg|rev>|<merge|https://savannah.gnu.org/bugs/?|<arg|rev>>>>>

  \;

  The status of a task can be divied into 4 stages: Listed, Ready, WIP, Done.
  And a special status Discarded will be used for discarded tasks.

  Listed means that the task has been included in a table.

  Ready means that the task has been considered for implementation and a
  detailed task document has been written.

  WIP means Work In Progress. It means that the code for this task is being
  written.

  Done means this task has been finished.
</body>

<initial|<\collection>
</collection>>
