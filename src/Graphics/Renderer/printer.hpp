
/******************************************************************************
* MODULE     : printer.hpp
* DESCRIPTION: Windows under X
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PRINTER_H
#define PRINTER_H
#include "renderer.hpp"
#include "hashmap.hpp"
#include "url.hpp"

class printer_rep: public renderer_rep {
  url      ps_file_name;
  int      dpi;
  int      nr_pages;
  string   page_type;
  bool     landscape;
  double   paper_w;
  double   paper_h;
  int      psw;
  int      psh;
  bool     use_alpha;
  string   prologue;
  string   body;
  int      cur_page;
  int      linelen;

  color    fg, bg;
  int      opacity;
  pencil   pen;
  brush    bgb;
  int      ncols;
  SI       lw;
  int      nwidths;
  string   cfn;
  int      nfonts;
  SI       xpos, ypos;
  bool     tex_flag;
  tree     toc;

  hashmap<string,string> defs;
  hashmap<string,string> tex_chars;
  hashmap<string,string> tex_width;
  hashmap<string,string> tex_fonts;
  hashmap<string,array<int> > tex_font_chars;
  hashmap<string,string> metadata;

public:
  printer_rep (url ps_file_name, int dpi, int nr_pages,
	       string ptype, bool landsc, double paper_w, double paper_h);
  ~printer_rep ();
  bool is_printer ();
  void next_page ();

  /*********************** subroutines for printing **************************/

  void define (string comm, string defn);
  void sep ();
  void cr ();
  void print (string s);
  void print (SI x, SI y);
  void move_to (SI x, SI y);
  string define_alpha (int a);
  void select_color (color c);
  void select_line_width (SI w);

  /********************* subroutines for drawing text ************************/

  void make_tex_char (string name, QN c, glyph gl);
  void select_tex_font (string name);
  void generate_tex_fonts ();

  /************************ subroutines hyperlinks ***************************/

  void anchor (string label, SI x1, SI y1, SI x2, SI y2);
  void href (string label, SI x1, SI y1, SI x2, SI y2);
  void toc_entry (string kind, string title, SI x, SI y);
  void generate_toc_item (tree t);
  void generate_toc ();
  void set_metadata (string kind, string val);
  void generate_metadata ();

  /********************** routines from renderer.hpp *************************/

  void set_transformation (frame fr);
  void reset_transformation ();

  void   set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);
  pencil get_pencil ();
  brush  get_background ();
  void   set_pencil (pencil p);
  void   set_background (brush b);
  void   draw (int char_code, font_glyphs fn, SI x, SI y);
  void   line (SI x1, SI y1, SI x2, SI y2);
  void   lines (array<SI> x, array<SI> y);
  void   clear (SI x1, SI y1, SI x2, SI y2);
  void   fill (SI x1, SI y1, SI x2, SI y2);
  void   arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void   fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void   polygon (array<SI> x, array<SI> y, bool convex=true);

  void fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y);
  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);

  renderer shadow (picture& pic, SI x1, SI y1, SI x2, SI y2);
  void draw_picture (picture p, SI x, SI y, int alpha);
  void draw_scalable (scalable im, SI x, SI y, int alpha);
  void image (string name, string eps, SI x1, SI y1, SI x2, SI y2,
              SI w, SI h, SI x, SI y, int alpha);
};

renderer printer (url ps_file_name, int dpi, int nr_pages= 1,
		  string page_type= "a4", bool landscape= false,
		  double paper_w= 21.0, double paper_h= 29.7);

#endif // defined PRINTER_H
