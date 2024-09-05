
/******************************************************************************
 * MODULE     : tt_tools.hpp
 * DESCRIPTION: Direct access of True Type font (independent from FreeType)
 * COPYRIGHT  : (C) 2012  Joris van der Hoeven
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef TT_TOOLS_H
#define TT_TOOLS_H

#include "hashset.hpp"
#include "tree.hpp"
#include "url.hpp"

void        tt_dump (url u);
scheme_tree tt_font_name (url u);

#ifdef USE_FREETYPE

array<string> tt_analyze (string family);
double        characteristic_distance (array<string> a1, array<string> a2);
double        trace_distance (string v1, string v2, double m);
string        find_attribute_value (array<string> a, string s);

// quantities with respect to ex height
double get_M_width (array<string> a);
double get_lo_pen_width (array<string> a);
double get_lo_pen_height (array<string> a);
double get_up_pen_width (array<string> a);
double get_up_pen_height (array<string> a);

#else

inline array<string>
tt_analyze (string family) {
  (void) family;
  array<string> r;
  return r;
}

inline double
trace_distance (string v1, string v2, double m) {
  return 0;
}
inline string
find_attribute_value (array<string> a, string s) {
  return "";
}
inline int
characteristic_distance (array<string> a, array<string> s) {
  return 0;
}

inline double
get_M_width (array<string> a) {
  return 0.0;
}
inline double
get_lo_pen_width (array<string> a) {
  return 0.0;
}
inline double
get_lo_pen_height (array<string> a) {
  return 0.0;
}
inline double
get_up_pen_width (array<string> a) {
  return 0.0;
}
inline double
get_up_pen_height (array<string> a) {
  return 0.0;
}

#endif

/******************************************************************************
 * OpenType MATH table
 ******************************************************************************/
// e OpenType MATH table is not implemented.
// see https://docs.microsoft.com/en-gb/typography/opentype/spec/math

// index of the MathConstantsTable.records
enum MathConstantRecordEnum {
  mathLeading,
  axisHeight,
  accentBaseHeight,
  flattenedAccentBaseHeight,
  subscriptShiftDown,
  subscriptTopMax,
  subscriptBaselineDropMin,
  superscriptShiftUp,
  superscriptShiftUpCramped,
  superscriptBottomMin,
  superscriptBaselineDropMax,
  subSuperscriptGapMin,
  superscriptBottomMaxWithSubscript,
  spaceAfterScript,
  upperLimitGapMin,
  upperLimitBaselineRiseMin,
  lowerLimitGapMin,
  lowerLimitBaselineDropMin,
  stackTopShiftUp,
  stackTopDisplayStyleShiftUp,
  stackBottomShiftDown,
  stackBottomDisplayStyleShiftDown,
  stackGapMin,
  stackDisplayStyleGapMin,
  stretchStackTopShiftUp,
  stretchStackBottomShiftDown,
  stretchStackGapAboveMin,
  stretchStackGapBelowMin,
  fractionNumeratorShiftUp,
  fractionNumeratorDisplayStyleShiftUp,
  fractionDenominatorShiftDown,
  fractionDenominatorDisplayStyleShiftDown,
  fractionNumeratorGapMin,
  fractionNumDisplayStyleGapMin,
  fractionRuleThickness,
  fractionDenominatorGapMin,
  fractionDenomDisplayStyleGapMin,
  skewedFractionHorizontalGap,
  skewedFractionVerticalGap,
  overbarVerticalGap,
  overbarRuleThickness,
  overbarExtraAscender,
  underbarVerticalGap,
  underbarRuleThickness,
  underbarExtraDescender,
  radicalVerticalGap,
  radicalDisplayStyleVerticalGap,
  radicalRuleThickness,
  radicalExtraAscender,
  radicalKernBeforeDegree,
  radicalKernAfterDegree,
  otmathConstantsRecordsEnd // keep at the end
};

struct DeviceTable {
  unsigned int startSize;
  unsigned int endSize;
  unsigned int deltaFormat;
  unsigned int deltaValues;
};

struct MathValueRecord {
  int         value;
  bool        hasDevice;
  DeviceTable deviceTable;
  MathValueRecord () : hasDevice (false) {}
};

struct MathConstantsTable {
  int                    scriptPercentScaleDown;
  int                    scriptScriptPercentScaleDown;
  unsigned int           delimitedSubFormulaMinHeight;
  unsigned int           displayOperatorMinHeight;
  array<MathValueRecord> records;
  int                    radicalDegreeBottomRaisePercent;
  MathConstantsTable () : records (otmathConstantsRecordsEnd){};
};

struct MathKernTable {
  unsigned int           heightCount;
  array<MathValueRecord> correctionHeight;
  array<MathValueRecord> kernValues;
  MathKernTable ()= default;
  MathKernTable (unsigned int h)
      : heightCount (h), correctionHeight (h), kernValues (h + 1) {}
};

struct MathKernInfoRecord {
  MathKernTable topRight;
  MathKernTable topLeft;
  MathKernTable bottomRight;
  MathKernTable bottomLeft;
  bool          hasTopRight;
  bool          hasTopLeft;
  bool          hasBottomRight;
  bool          hasBottomLeft;
  MathKernInfoRecord ()
      : hasTopRight (false), hasTopLeft (false), hasBottomRight (false),
        hasBottomLeft (false) {}
};

struct GlyphPartRecord {
  unsigned int glyphID;
  unsigned int startConnectorLength;
  unsigned int endConnectorLength;
  unsigned int fullAdvance;
  unsigned int partFlags;
};

struct GlyphAssembly {
  MathValueRecord        italicsCorrection;
  array<GlyphPartRecord> partRecords;
};

struct ot_mathtable_rep : concrete_struct {
  unsigned int                               majorVersion, minorVersion;
  MathConstantsTable                         constants_table;
  unsigned int                               minConnectorOverlap;
  hashmap<unsigned int, MathValueRecord>     italics_correction;
  hashmap<unsigned int, MathValueRecord>     top_accent;
  hashset<unsigned int>                      extended_shape_coverage;
  hashmap<unsigned int, MathKernInfoRecord>  math_kern_info;
  hashmap<unsigned int, array<unsigned int>> ver_glyph_variants;
  hashmap<unsigned int, array<unsigned int>> ver_glyph_variants_adv;
  hashmap<unsigned int, array<unsigned int>> hor_glyph_variants;
  hashmap<unsigned int, array<unsigned int>> hor_glyph_variants_adv;
  hashmap<unsigned int, GlyphAssembly>       ver_glyph_assembly;
  hashmap<unsigned int, GlyphAssembly>       hor_glyph_assembly;
};

struct ot_mathtable {
  CONCRETE_NULL (ot_mathtable);
  ot_mathtable (ot_mathtable_rep* rep2) : rep (rep2) {}
};
CONCRETE_NULL_CODE (ot_mathtable);

ot_mathtable parse_mathtable (const string& buf);
ot_mathtable parse_mathtable (url u);

#endif // TT_TOOLS_H
