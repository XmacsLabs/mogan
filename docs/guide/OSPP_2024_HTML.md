# Project: Conversion from TeXmacs format to HTML and docx
## For Students
+ Expected Code Percentage
  + Scheme Code >= 80%
  + C++ Code <= 20%
+ Selection Criteria
  1. One public pull request to add C++ unit tests to the Mogan repo
  2. One public pull request to add Scheme unit tests to the Mogan repo
  3. The students must implement pandoc binary plugin and docx data plugin by themselves privately to prove that he/she can complete the project

Please send the implementation of the pandoc integration to me privately via email: `da AT liii.pro`.

## Project Info
+ Name: Conversion from TeXmacs format to HTML and docx
+ Mentor: Darcy Shen
+ Difficulty: Basic

## Project Description
Exporting to docx format for TeXmacs documents is a frequently requested feature from TeXmacs users. We can use pandoc to achieve it.

In this project, we need to implement conversion for TM format -> HTML format -> DOCX format. The TM->HTML conversion has been implemented. And the HTML->DOCX could be completed by `pandoc`. And you need to improve the TM->HTML quality to improve the TM->DOCX quality.

The documents in Xmacs Planet could be served as the testing resources. Github Pages CD should be setup for the Xmacs Planet.


## Project Notes
Related official docs:
+ Binary Plugin: https://mogan.app/guide/plugin_binary.html
+ Data Plugin: https://mogan.app/guide/plugin_data.html (missing now on 2024/04/15, will be available later)

TODO and not TODO:
HTML->DOCX conversion is the only missing part now. You need to complete the missing part using `pandoc`, creating pull requests to `pandoc` to improve the HTML->DOCX conversion is not a goal of this project. And in this project, we are focusing on improving the TM->HTML conversion. HTML->TM conversion is not a goal of this project. We care more on code quality, please focus on a small area and keep improving the conversion quality.

Expectation on time scheduling:
`pandoc` binary plugin, `docx` binary plugin and Github/Gitee Pages Continous delivery should be completed within 1 or 2 weeks.

## Output Requirements
+ Implement a `pandoc` binary plugin (make it work on Linux/Windows/macOS with the help of mentors)
  + `(find-binary-pandoc)`
  + `(version-binary-pandoc)`
  + `(has-binary-pandoc?)`
+ Implement a `docx` data plugin
  + define conversion from HTML to DOCX
  + add the menu entry `File->Export->DOCX`
+ Github/Gitee Pages Continous delivery for the [planet](https://github.com/XmacsLabs/planet)
  + just choose one of Github Action and Gitee Go Pipeline
+ Improve TM2HTML conversion
  + Minimal reproducer and unit tests
  + Generated HTML should be HTML 5
  + Generated MathML should also be improved
  + Generated HTML should cover different styles like
    - book (eg. [interactive-sicp](https://github.com/XmacsLabs/interactive-sicp))
    - beamer
    - generic
    - course
    - exam
    - projector

## Project Technical Requirements
+ Scheme, xmake, C++
+ Github Action/Gitee Go
+ Understanding the data plugin in Mogan
+ Understanding the binary plugin in Mogan

## Project Repository
+ https://github.com/XmacsLabs/mogan
+ https://github.com/XmacsLabs/planet