#
# Makefile for VIM on Win32, using MinGW
#
# Also read INSTALLpc.txt!
#
# The old Make_ming.mak (maintained by Ron Aaron et al.) was merged into
# Make_cyg_ming.mak.
# This file contains MinGW specific settings. Common settings are contained
# in Make_cyg_ming.mak.
#
# Last updated by Ken Takata.
# Last Change: 2014 Oct 21


# uncomment 'PERL' if you want a perl-enabled version
#PERL=c:/perl

# uncomment 'LUA' if you want a Lua-enabled version
#LUA=c:/lua

# uncomment 'MZSCHEME' if you want a MzScheme-enabled version
#MZSCHEME=d:/plt

# uncomment 'PYTHON' if you want a python-enabled version
# Put the path to the python distro here.  If cross compiling from Linux, you
# will also need to convert the header files to unix instead of dos format:
#   for fil in *.h ; do vim -e -c 'set ff=unix|w|q' $fil
# and also, you will need to make a mingw32 'libpython20.a' to link with:
#   cd $PYTHON/libs
#   pexports python20.dll > python20.def
#   dlltool -d python20.def -l libpython20.a
# on my Linux box, I put the Python stuff here:
#PYTHON=/home/ron/ActivePython-2.0.0-202/src/Core
# on my NT box, it's here:
#PYTHON=c:/python20

# uncomment 'PYTHON3' if you want a python3-enabled version
#PYTHON3=c:/python31

# uncomment 'TCL' if you want a Tcl-enabled version
#TCL=c:/tcl

# uncomment 'RUBY' if you want a Ruby-enabled version
#RUBY=c:/ruby


# Do not change this.
UNDER_CYGWIN = no

LUA=C:/projects/lua53
DYNAMIC_LUA=yes
LUA_VER=53

PERL=C:/Perl
DYNAMIC_PERL=yes
PERL_VER=524
PER_VER_LONG=5.24.0

PYTHON=C:/Python/Python27
#PYTHON_HOME=C:/Python/Python27
#PYTHONINC=-IC:/Python/Python27/include
DYNAMIC_PYTHON=yes
PYTHON_VER=27
#DYNAMIC_PYTHON_DLL=python27.lib
#ARCH=x86-64
STATIC_STDCPLUS=yes

PYTHON3=C:/Python/Python35
DYNAMIC_PYTHON3=yes
PYTHON3_VER=35

RUBY=C:/Ruby23
DYNAMIC_RUBY=yes
RUBY_VER=23
RUBY_VER_LONG=2.3.0

TCL=C:/Tcl
DYNAMIC_TCL=yes
TCL_VER=86
TCL_VER_LONG=8.6

include Make_cyg_ming.mak
