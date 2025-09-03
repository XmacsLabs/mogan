
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

#include "basic.hpp"
#include "hashmap.hpp"
#include "hashset.hpp"
#include "tm_debug.hpp"
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
  otmathConstantsRecordsEnd, // count the number of records
  scriptPercentScaleDown,
  scriptScriptPercentScaleDown,
  delimitedSubFormulaMinHeight,
  displayOperatorMinHeight,
  radicalDegreeBottomRaisePercent
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

  // cast to int
  operator int () const { return value; }
};

struct MathConstantsTable {
  int                    scriptPercentScaleDown;
  int                    scriptScriptPercentScaleDown;
  unsigned int           delimitedSubFormulaMinHeight;
  unsigned int           displayOperatorMinHeight;
  array<MathValueRecord> records;
  int                    radicalDegreeBottomRaisePercent;
  MathConstantsTable ()
      : records (MathConstantRecordEnum::otmathConstantsRecordsEnd) {};

  int operator[] (int i) {
    if (i >= 0 && i < MathConstantRecordEnum::otmathConstantsRecordsEnd)
      return records[i];
    switch (i) {
    case MathConstantRecordEnum::scriptPercentScaleDown:
      return scriptPercentScaleDown;
    case MathConstantRecordEnum::scriptScriptPercentScaleDown:
      return scriptScriptPercentScaleDown;
    case MathConstantRecordEnum::delimitedSubFormulaMinHeight:
      return delimitedSubFormulaMinHeight;
    case MathConstantRecordEnum::displayOperatorMinHeight:
      return displayOperatorMinHeight;
    case MathConstantRecordEnum::radicalDegreeBottomRaisePercent:
      return radicalDegreeBottomRaisePercent;
    }
    TM_FAILED ("MathConstantsTable: index out of range");
    return 0; // should never reach here
  }
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

  bool has_kerning (bool top, bool left);
  int  get_kerning (int height, bool top, bool left);
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
  int                    partCount;

  const GlyphPartRecord& operator[] (int i) { return partRecords[i]; }
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

  // helper functions and data
  hashmap<unsigned int, unsigned int> get_init_glyphID_cache;
  // for variant glyph, get the glyphID of the base glyph
  unsigned int get_init_glyphID (unsigned int glyphID);

  bool has_kerning (unsigned int glyphID, bool top, bool left);
  int  get_kerning (unsigned int glyphID, int height, bool top, bool left);
};

struct ot_mathtable {
  CONCRETE_NULL (ot_mathtable);
  ot_mathtable (ot_mathtable_rep* rep2) : rep (rep2) {}
};
CONCRETE_NULL_CODE (ot_mathtable);

ot_mathtable parse_mathtable (const string& buf);
ot_mathtable parse_mathtable (url u);

/******************************************************************************
 * OpenType CBDT/CBLC tables for bitmap glyphs (PNG emojis)
 * see https://docs.microsoft.com/en-us/typography/opentype/spec/cbdt
 * see https://docs.microsoft.com/en-us/typography/opentype/spec/cblc
 ******************************************************************************/

struct SbitLineMetrics {
  signed char   ascender;
  signed char   descender;
  unsigned char widthMax;
  signed char   caretSlopeNumerator;
  signed char   caretSlopeDenominator;
  signed char   caretOffset;
  signed char   minOriginSB;
  signed char   minAdvanceSB;
  signed char   maxBeforeBL;
  signed char   minAfterBL;
  signed char   pad1;
  signed char   pad2;
};

struct BitmapSizeRecord {
  unsigned int    indexSubTableArrayOffset;
  unsigned int    indexTablesSize;
  unsigned int    numberOfIndexSubTables;
  unsigned int    colorRef;
  SbitLineMetrics hori;
  SbitLineMetrics vert;
  unsigned short  startGlyphIndex;
  unsigned short  endGlyphIndex;
  unsigned char   ppemX;
  unsigned char   ppemY;
  unsigned char   bitDepth;
  signed char     flags;
};

struct IndexSubTableHeader {
  unsigned short indexFormat;
  unsigned short imageFormat;
  unsigned int   imageDataOffset;
};

struct IndexSubTable1 {
  IndexSubTableHeader header;
  array<unsigned int> offsetArray;
};

struct IndexSubTable2 {
  IndexSubTableHeader   header;
  unsigned int          imageSize;
  array<unsigned short> glyphArray;
};

struct IndexSubTable3 {
  IndexSubTableHeader   header;
  array<unsigned short> offsetArray;
};

struct BitmapGlyphData {
  unsigned int  offset;
  unsigned int  length;
  string        data;
  unsigned char format;
};

struct ot_cbdttable_rep : concrete_struct {
  unsigned short          majorVersion;
  unsigned short          minorVersion;
  array<BitmapSizeRecord> bitmapSizes;
  hashmap<unsigned int, array<BitmapGlyphData>>
      glyph_bitmaps; // ppem -> bitmap data
  hashmap<unsigned int, string>
      bitmap_cache; // glyphID -> PNG data for current size

  string get_png_from_glyphid (unsigned int glyphID, int ppem);
};

struct ot_cbdttable {
  CONCRETE_NULL (ot_cbdttable);
  ot_cbdttable (ot_cbdttable_rep* rep2) : rep (rep2) {}
};

CONCRETE_NULL_CODE (ot_cbdttable);
ot_cbdttable parse_cbdttable (const string& cblc_buf, const string& cbdt_buf);
ot_cbdttable parse_cbdttable (url u);

#endif // TT_TOOLS_H
