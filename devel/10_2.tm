<TeXmacs|2.1.1>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|Show grid for the graphics tool by default>

  <section|What>

  For Mogan v1.0.1, <menu|Insert|Image|Draw Image>:

  <with|gr-mode|point|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|<graphics|>>

  Using the mode tool bar, we can show grid for the graphics tool.

  <with|gr-mode|point|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|cartesian|<point|0|0>|1>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|<graphics|>>

  Show grid should be enabled by default.

  \;

  <section|How>

  The graphics tool with Show grid enabled can be exported as Scheme using
  <menu|File|Export|TeXmacs Scheme<text-dots>> like:

  <\scm-code>
    (document (TeXmacs "2.1.1") (style (tuple "generic" "british")) (body
    (document (with "gr-mode" "point" "gr-frame" (tuple "scale" "1cm" (tuple
    "0.5gw" "0.5gh")) "gr-geometry" (tuple "geometry" "1par" "0.6par")
    "gr-grid" (tuple "cartesian" (point "0" "0") "1") "gr-grid-old" (tuple
    "cartesian" (point "0" "0") "1") "gr-edit-grid-aspect" (tuple (tuple
    "axes" "none") (tuple "1" "none") (tuple "10" "none")) "gr-edit-grid"
    (tuple "cartesian" (point "0" "0") "1") "gr-edit-grid-old" (tuple
    "cartesian" (point "0" "0") "1") (graphics "")))) (initial (collection
    (associate "page-medium" "paper"))))
  </scm-code>

  Let us simplify and format it manually:

  <\scm-code>
    (with

    \ \ "gr-mode" "point"

    \ \ "gr-frame" (tuple "scale" "1cm" (tuple "0.5gw" "0.5gh"))

    \ \ "gr-geometry" (tuple "geometry" "1par" "0.6par")

    \ \ "gr-grid" (tuple "cartesian" (point "0" "0") "1")

    \ \ "gr-grid-old" (tuple "cartesian" (point "0" "0") "1")

    \ \ "gr-edit-grid-aspect" (tuple (tuple "axes" "none") (tuple "1" "none")
    (tuple "10" "none"))

    \ \ "gr-edit-grid" (tuple "cartesian" (point "0" "0") "1")

    \ \ "gr-edit-grid-old" (tuple "cartesian" (point "0" "0") "1")

    \ \ (graphics ""))
  </scm-code>

  Finally, I removed <scm|gr-edit-grid-old> and <scm|gr-grid-old> and created
  the pull request.

  <section|How it was tested>

  Tested locally:

  <\itemize>
    <item>By default, the inserted graphics tool shows grid

    <item>Toggling <code*|Show grid> works fine
  </itemize>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>