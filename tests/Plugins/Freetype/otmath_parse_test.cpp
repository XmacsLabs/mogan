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
/*
Verification data for Asana-Math.otf (https://ctan.org/tex-archive/fonts/Asana-Math)
derived from fontforge (math table data)
and python package fontTools.ttLib (translates the glyph name to glyph ID).
In the comments, the glyph name is used to identify the glyph.
In the test, the glyph ID is used to identify the glyph.
*/
void
TestOpenTypeMathTableParse::init () {
  lolly::init_tbox ();
  url u= concretize_url ("$TEXMACS_PATH/fonts/opentype/Asana-Math.otf");
  table= parse_mathtable (u);
}

void
TestOpenTypeMathTableParse::test_math_constants () {
  // MathConstants table
  auto& constants= table->constants_table;
  QVERIFY (constants.records[subscriptShiftDown].value == 210);
  QVERIFY (constants.records[upperLimitGapMin].value == 111);
  QVERIFY (constants.records[stackGapMin].value == 348);
  QVERIFY (constants.records[fractionNumeratorDisplayStyleShiftUp].value ==
           725);
  QVERIFY (constants.records[underbarVerticalGap].value == 175);
  QVERIFY (constants.records[radicalVerticalGap].value == 74);
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
  QVERIFY (italics_correction[506].value == 259);
  // uni2A0C
  QVERIFY (italics_correction[1464].value == 307);
  // u1D47E
  QVERIFY (italics_correction[1922].value == 136);
  // glyph2A18Bigg
  QVERIFY (italics_correction[3260].value == 307);
  // integraltripleBigg
  QVERIFY (italics_correction[3296].value == 307);
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
  QVERIFY (top_accent[200].value == -190);
  // uni033F
  QVERIFY (top_accent[214].value == -216);
  // Omega
  QVERIFY (top_accent[252].value == 412);
  // u1D4D2
  QVERIFY (top_accent[1995].value == 727);
  // u1D4A9.salt
  QVERIFY (top_accent[3110].value == 483);
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
  // test glyph IDs that are in the coverage table
  // uni2A11
  QVERIFY (extended_shape_coverage->contains (1469));
  // glyph3800
  QVERIFY (extended_shape_coverage->contains (3135));
  // sumbig3
  QVERIFY (extended_shape_coverage->contains (3349));
  // bracketBiggl
  QVERIFY (extended_shape_coverage->contains (3431));
  // test glyph IDs that are not in the coverage table
  // uni2A0B
  QVERIFY (!extended_shape_coverage->contains (1463));
  // Omega
  QVERIFY (!extended_shape_coverage->contains (252));
  // radicalBigg
  QVERIFY (!extended_shape_coverage->contains (3304));
}

void
TestOpenTypeMathTableParse::test_math_kern_info () {
  auto& math_kern_info= table->math_kern_info;
  // glygh name : A (glyph ID : 35)
  QVERIFY (math_kern_info->contains (35));
  // top right kerning of A
  QVERIFY (math_kern_info (35).hasTopRight == true);
  QVERIFY (math_kern_info (35).topRight.heightCount == 0);
  QVERIFY (math_kern_info (35).topRight.kernValues[0].value == -82);
  // bottom right kerning of A
  QVERIFY (math_kern_info (35).hasBottomRight == true);
  QVERIFY (math_kern_info (35).bottomRight.heightCount == 1);
  QVERIFY (math_kern_info (35).bottomRight.kernValues[0].value == 49);
  QVERIFY (math_kern_info (35).bottomRight.correctionHeight[0].value == -200);
  QVERIFY (math_kern_info (35).bottomRight.kernValues[1].value == 222);
  // no top left and bottom left kerning of A
  QVERIFY (math_kern_info (35).hasTopLeft == false);
  QVERIFY (math_kern_info (35).hasBottomLeft == false);
  // glygh name : Sigma (glyph ID : 246)
  QVERIFY (math_kern_info->contains (246));
  // top right kerning of Sigma
  QVERIFY (math_kern_info (246).hasTopRight == true);
  QVERIFY (math_kern_info (246).topRight.heightCount == 0);
  QVERIFY (math_kern_info (246).topRight.kernValues[0].value == 100);
  // bottom right kerning of Sigma
  QVERIFY (math_kern_info (246).hasTopRight == true);
  QVERIFY (math_kern_info (246).bottomRight.heightCount == 0);
  QVERIFY (math_kern_info (246).bottomRight.kernValues[0].value == 49);
  // no top left and bottom left kerning of Sigma
  QVERIFY (math_kern_info (246).hasTopLeft == false);
  QVERIFY (math_kern_info (246).hasBottomLeft == false);
}

void
TestOpenTypeMathTableParse::test_vertical_glyph_variants () {
  auto& ver_glyph_variants    = table->ver_glyph_variants;
  auto& ver_glyph_variants_adv= table->ver_glyph_variants_adv;
  // arrowup -> [arrowup]
  QVERIFY (ver_glyph_variants[624][0] == 624);
  // product -> [product,productbig1,productbig2,productbig3]
  QVERIFY (ver_glyph_variants[750] ==
           array<unsigned int> (750, 3350, 3351, 3352));
  // fraction -> [fraction,fractionbig,fractionBig,fractionbigg,fractionBigg]
  QVERIFY (ver_glyph_variants[423] ==
           array<unsigned int> (423, 3401, 3402, 3403, 3404));
  // uni222D ->
  // [uni222D,integraltripleBig,integraltriplebigg,integraltripleBigg]
  QVERIFY (ver_glyph_variants[780] ==
           array<unsigned int> (780, 3294, 3295, 3296));
}

void
TestOpenTypeMathTableParse::test_horizontal_glyph_variants () {
  auto& hor_glyph_variants    = table->hor_glyph_variants;
  auto& hor_glyph_variants_adv= table->hor_glyph_variants_adv;
  // arrowdblright -> [arrowdblright]
  QVERIFY (hor_glyph_variants[689][0] == 689);
  // uni20D6 -> [uni20D6,glyph3674,glyph3675,glyph3676,glyph3677]
  QVERIFY (hor_glyph_variants[471] ==
           array<unsigned int> (471, 3009, 3010, 3011, 3012));
  // uni23B4 -> [uni23B4,glyph353,glyph354,glyph355]
  QVERIFY (hor_glyph_variants[1035] ==
           array<unsigned int> (1035, 3188, 3189, 3190));
  // uni23E1 -> [uni23E1,glyph3844,glyph3845,glyph3846]
  QVERIFY (hor_glyph_variants[1043] ==
           array<unsigned int> (1043, 3179, 3180, 3181));
}

void
TestOpenTypeMathTableParse::test_vertical_glyph_assembly () {
  // integral = integralbt + uni23AE + integraltp
  QVERIFY (table->ver_glyph_assembly (778).italicsCorrection.value == 407);
  // part1 = integralbt
  QVERIFY (table->ver_glyph_assembly (778).partRecords[0].glyphID == 1006);
  QVERIFY (
      table->ver_glyph_assembly (778).partRecords[0].startConnectorLength == 0);
  QVERIFY (table->ver_glyph_assembly (778).partRecords[0].endConnectorLength ==
           10);
  QVERIFY (table->ver_glyph_assembly (778).partRecords[0].fullAdvance == 1413);
  QVERIFY (table->ver_glyph_assembly (778).partRecords[0].partFlags == 0);
  // part2 = uni23AE
  QVERIFY (table->ver_glyph_assembly (778).partRecords[1].glyphID == 1031);
  // part3 = integraltp
  QVERIFY (table->ver_glyph_assembly (778).partRecords[2].glyphID == 1005);
}

void
TestOpenTypeMathTableParse::test_horizontal_glyph_assembly () {
  // arrowright = glyph3870 + uni23AF + glyph3871
  QVERIFY (table->hor_glyph_assembly (625).italicsCorrection.value == 0);
  // part1 = glyph3870
  QVERIFY (table->hor_glyph_assembly (625).partRecords[0].glyphID == 3205);
  QVERIFY (
      table->hor_glyph_assembly (625).partRecords[0].startConnectorLength == 0);
  QVERIFY (table->hor_glyph_assembly (625).partRecords[0].endConnectorLength ==
           655);
  QVERIFY (table->hor_glyph_assembly (625).partRecords[0].fullAdvance == 771);
  QVERIFY (table->hor_glyph_assembly (625).partRecords[0].partFlags == 0);
  // part2 = uni23AF
  QVERIFY (table->hor_glyph_assembly (625).partRecords[1].glyphID == 1032);
  // part3 = glyph3871
  QVERIFY (table->hor_glyph_assembly (625).partRecords[2].glyphID == 3206);
}

QTEST_MAIN (TestOpenTypeMathTableParse)
#include "otmath_parse_test.moc"
