<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british|devel>>

<\body>
  <tmdoc-title|L3 Kernel>

  L1 and L2 Kernel is completed in <dlink|3>. Starting from the L3 Kernel, S7
  Scheme will be included.

  <section|Tasks>

  <\wide-tabular>
    <tformat|<cwith|1|-1|2|2|cell-background|pastel
    green>|<table|<row|<\cell>
      Chore
    </cell>|<\cell>
      15_1
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Separate L1 and L2 glue and object
    </cell>>|<row|<\cell>
      Feature
    </cell>|<\cell>
      15_2
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Use xmake to manage the S7 Scheme package
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      15_3
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Separate glues for L1/L2/L3 and Plugins
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_1
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_string
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_2
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_tree
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_3
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_analyze
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_4
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_path
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_5
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_url
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_6
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_misc
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_7
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_file
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_8
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_modification
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_9
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_updater
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_10
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_font
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_11
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_bibtex
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_12
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_tmdb
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_13
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_widget
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_14
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Move glues to L1/L2/L3
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_15
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Move glues to src/Scheme/Plugins
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_16
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_ghostscript
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_17
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_pdf
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_18
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_patch
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_19
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_convert
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_20
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_xml
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_21
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      glue_tex
    </cell>>|<row|<\cell>
      \;
    </cell>|<\cell>
      15_3_22
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      format hpp code for glues
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      15_4
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Initial L3 Kernel
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      15_5
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Re-org on L1/L2/L3 Kernel
    </cell>>|<row|<\cell>
      Chore
    </cell>|<\cell>
      15_6
    </cell>|<\cell>
      Darcy
    </cell>|<\cell>
      Reduce usage of KERNEL_L3
    </cell>>>>
  </wide-tabular>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>