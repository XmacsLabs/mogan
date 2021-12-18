
###############################################################################
# MODULE     : makefile which can be included in administrative makefiles
# COPYRIGHT  : (C) 1999-2008  Joris van der Hoeven
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################

prefix = /usr/local
exec_prefix = ${prefix}
includedir = ${prefix}/include
libdir = ${exec_prefix}/lib64
bindir = ${exec_prefix}/bin
datarootdir = ${prefix}/share
datadir = ${datarootdir}
mandir = ${datarootdir}/man
tmdir = TeXmacs
tm_devel = TeXmacs-2.1.1
tm_devel_release = TeXmacs-2.1.1-1
tmorig = /home/magix/DEV/texmacs
tmsrc = /home/magix/DEV/texmacs/TeXmacs
tmbin = ${exec_prefix}/lib/TeXmacs
tmdata = ${datarootdir}/TeXmacs
so = so
os = x86_64-pc-linux-gnu

MKDIR = mkdir -p
RM = rm -f
CP = cp -r -f
MV = mv -f
LN = ln -f
CHMOD = chmod -f
GZIP = gzip -f
STRIP = strip
TOUCH = touch
TAR = tar
