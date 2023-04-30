<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|Decouple the kernel part of GNU <TeXmacs>>

  <section|Feature metadata>

  <\itemize>
    <item>Owner: Darcy Shen
  </itemize>

  <section|Description>

  <subsection|What>

  Darcy tried to separate the kernel part of GNU <TeXmacs> for students who
  are interested in GSOC 2019:

  <slink|https://github.com/texmacs/kernel>

  With xmake, we can now pick the source code and build the kernel part
  within the main Git repository. Here is the attempt for Mogan Editor
  v1.1.x:

  <slink|https://github.com/XmacsLabs/mogan/pull/400>

  <subsection|Why>

  <\itemize>
    <item>The <TeXmacs> kernel is for students who are insterested in Summer
    of Code

    <item>Playing with the <TeXmacs> kernel, students are able to get
    familiar with codebase via a small subset

    <item>Developers can improve the <TeXmacs> kernel without building the
    full codebase
  </itemize>

  <subsection|How>

  Provide the following script to build and test the <TeXmacs> kernel:

  <\shell-code>
    bin/test_kernel
  </shell-code>
</body>

<initial|<\collection>
</collection>>