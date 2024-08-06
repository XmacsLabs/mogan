#include "Freetype/tt_tools.hpp"
#include "iterator.hpp"
#include "sys_utils.hpp"
#include "tm_url.hpp"
#include <QtTest/QtTest>

class TestOpenTypeMathTableParse : public QObject {
  Q_OBJECT
private:
  ot_mathtable table;
private slots:
  void init ();
  void test_math_constants ();
  void test_italic_correction ();
  void test_top_accent ();
  void test_extended_shape_coverage ();
  void test_math_kern_info ();
  void test_vertical_glyph_variants ();
  void test_horizontal_glyph_variants ();
  void test_vertical_glyph_assembly ();
  void test_horizontal_glyph_assembly ();
};

// Verification data for Asana-Math.otf, derived from fontforge and python package
// fontTools.ttLib
void
TestOpenTypeMathTableParse::init () {
  lolly::init_tbox ();
  url u= concretize_url ("$TEXMACS_PATH/tests/80_2-otmath-font/Asana-Math.otf");
  table= parse_mathtable (u);
}

void
TestOpenTypeMathTableParse::test_math_constants () {
  // MathConstants table
  auto& constants= table->constants_table;
  QVERIFY (constants.records[subscriptShiftDown].value == 210);
  QVERIFY (constants.records[upperLimitGapMin].value == 100);
  QVERIFY (constants.records[stackGapMin].value == 348);
  QVERIFY (constants.records[fractionNumeratorDisplayStyleShiftUp].value ==
           725);
  QVERIFY (constants.records[underbarVerticalGap].value == 175);
  QVERIFY (constants.records[radicalVerticalGap].value == 150);
  QVERIFY (constants.radicalDegreeBottomRaisePercent == 65);
  QVERIFY (constants.records[subscriptShiftDown].hasDevice == true);
  QVERIFY (constants.records[subscriptShiftDown].deviceTable.startSize == 12);
  QVERIFY (constants.records[subscriptShiftDown].deviceTable.endSize == 17);
  QVERIFY (constants.records[subscriptShiftDown].deviceTable.deltaFormat == 1);
  QVERIFY (constants.records[subscriptShiftDown].deviceTable.deltaValues ==
           0b01'00'00'00'01'01'00'00);
  // a constant in MathVariants Table
  QVERIFY (table->minConnectorOverlap == 100);
}

void
TestOpenTypeMathTableParse::test_italic_correction () {
  auto& italics_correction= table->italics_correction;
  // uni210B
  QVERIFY (italics_correction[373].value == 196);
  // u1D43E
  QVERIFY (italics_correction[1537].value == 78);
  // u1D752
  QVERIFY (italics_correction[2299].value == 183);
  // glyph2A0Cbigg
  QVERIFY (italics_correction[2770].value == 307);
  // integraltriplebigg
  QVERIFY (italics_correction[2842].value == 307);
  // no device table
  auto it= iterate (italics_correction);
  while (it->busy ()) {
    auto glyphID= it->next ();
    QVERIFY (italics_correction[glyphID].hasDevice == false);
  }
}

void
TestOpenTypeMathTableParse::test_top_accent () {
  auto& top_accent= table->top_accent;
  // gravecomb
  QVERIFY (top_accent[188].value == -190);
  // uni033F
  QVERIFY (top_accent[201].value == -216);
  // Omega
  QVERIFY (top_accent[238].value == 412);
  // u1D4D2
  QVERIFY (top_accent[1673].value == 550);
  // u1D4A9.salt
  QVERIFY (top_accent[2657].value == 483);
  // no device table
  auto it= iterate (top_accent);
  while (it->busy ()) {
    auto glyphID= it->next ();
    QVERIFY (top_accent[glyphID].hasDevice == false);
  }
}

void
TestOpenTypeMathTableParse::test_extended_shape_coverage () {
  auto& extended_shape_coverage= table->extended_shape_coverage;
  // uni2A11
  QVERIFY (extended_shape_coverage->contains (1229));
  // glyph3800
  QVERIFY (extended_shape_coverage->contains (2682));
  // sumbig3
  QVERIFY (extended_shape_coverage->contains (2896));
  // bracketBiggl
  QVERIFY (extended_shape_coverage->contains (2978));
}

void
TestOpenTypeMathTableParse::test_math_kern_info () {
  auto& math_kern_info= table->math_kern_info;
  // weierstrass
  QVERIFY (math_kern_info->contains (385));
  QVERIFY (math_kern_info (385).hasTopRight == true);
  QVERIFY (math_kern_info (385).topRight.heightCount == 0);
  QVERIFY (math_kern_info (385).topRight.kernValues[0].value == 94);
  QVERIFY (math_kern_info (385).hasTopLeft == false);
  QVERIFY (math_kern_info (385).hasBottomRight == true);
  QVERIFY (math_kern_info (385).bottomRight.heightCount == 0);
  QVERIFY (math_kern_info (385).bottomRight.kernValues[0].value == 49);
  QVERIFY (math_kern_info (385).hasBottomLeft == false);
  // uni210A
  QVERIFY (math_kern_info->contains (372));
  QVERIFY (math_kern_info (372).hasTopRight == true);
  QVERIFY (math_kern_info (372).topRight.heightCount == 1);
  QVERIFY (math_kern_info (372).topRight.kernValues[0].value == 0);
  QVERIFY (math_kern_info (372).topRight.kernValues[1].value == 112);
  QVERIFY (math_kern_info (372).topRight.correctionHeight[0].value == 319);
}

void
TestOpenTypeMathTableParse::test_vertical_glyph_variants () {
  auto& ver_glyph_variants    = table->ver_glyph_variants;
  auto& ver_glyph_variants_adv= table->ver_glyph_variants_adv;
  // arrowup -> [arrowup]
  QVERIFY (ver_glyph_variants[468][0] == 468);
  // product -> [product,productbig1,productbig2,productbig3]
  QVERIFY (ver_glyph_variants[594] ==
           array<unsigned int> (594, 2897, 2898, 2899));
  // fraction -> [fraction,fractionbig,fractionBig,fractionbigg,fractionBigg]
  QVERIFY (ver_glyph_variants[297] ==
           array<unsigned int> (297, 2948, 2949, 2950, 2951));
  // uni222D ->
  // [uni222D,integraltripleBig,integraltriplebigg,integraltripleBigg]
  QVERIFY (ver_glyph_variants[624] ==
           array<unsigned int> (624, 2841, 2842, 2843));
}

void
TestOpenTypeMathTableParse::test_horizontal_glyph_variants () {
  auto& hor_glyph_variants    = table->hor_glyph_variants;
  auto& hor_glyph_variants_adv= table->hor_glyph_variants_adv;
  // arrowdblright -> [arrowdblright]
  QVERIFY (hor_glyph_variants[533][0] == 533);
  // uni20D6 -> [uni20D6,glyph3674,glyph3675,glyph3676,glyph3677]
  QVERIFY (hor_glyph_variants[342] ==
           array<unsigned int> (342, 2556, 2557, 2558, 2559));
  // uni23B4 -> [uni23B4,glyph3853,glyph3854,glyph3855]
  QVERIFY (hor_glyph_variants[871] ==
           array<unsigned int> (871, 2735, 2736, 2737));
  // uni23E1 -> [uni23E1,glyph3844,glyph3845,glyph3846]
  QVERIFY (hor_glyph_variants[879] ==
           array<unsigned int> (879, 2726, 2727, 2728));
}

void
TestOpenTypeMathTableParse::test_vertical_glyph_assembly () {
  // integral
  QVERIFY (table->ver_glyph_assembly (622).italicsCorrection.value == 307);
  QVERIFY (table->ver_glyph_assembly (622).partRecords[0].glyphID == 843);
  QVERIFY (
      table->ver_glyph_assembly (622).partRecords[0].startConnectorLength == 0);
  QVERIFY (table->ver_glyph_assembly (622).partRecords[0].endConnectorLength ==
           10);
  QVERIFY (table->ver_glyph_assembly (622).partRecords[0].fullAdvance == 1413);
  QVERIFY (table->ver_glyph_assembly (622).partRecords[0].partFlags == 0);
}

void
TestOpenTypeMathTableParse::test_horizontal_glyph_assembly () {
  // arrowright
  QVERIFY (table->hor_glyph_assembly (469).italicsCorrection.value == 0);
  QVERIFY (table->hor_glyph_assembly (469).partRecords[0].glyphID == 2752);
  QVERIFY (
      table->hor_glyph_assembly (469).partRecords[0].startConnectorLength == 0);
  QVERIFY (table->hor_glyph_assembly (469).partRecords[0].endConnectorLength ==
           655);
  QVERIFY (table->hor_glyph_assembly (469).partRecords[0].fullAdvance == 771);
  QVERIFY (table->hor_glyph_assembly (469).partRecords[0].partFlags == 0);
}

QTEST_MAIN (TestOpenTypeMathTableParse)
#include "otmath_parse_test.moc"