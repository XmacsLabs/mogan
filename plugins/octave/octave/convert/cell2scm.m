
###############################################################################
##
## MODULE      : cell2scm.m
## DESCRIPTION : Convert an Octave cell to scheme code
## COPYRIGHT   : (C) 2020  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function ret= cell2scm (param)
  len= length (param);
  ret= "(enumerate-numeric (document";
  for i= 1:len
    ret= [ret, " (concat (item) ", obj2scm(param{i}),")"];
  endfor
  ret= [ret,"))"];
endfunction
