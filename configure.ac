#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([Vine/FuzzBALL], [20130511], [bitblaze-users@googlegroups.com])
AC_CONFIG_SRCDIR([libasmir/src/vex/vexir.cpp])
AC_CONFIG_HEADERS([libasmir/config.h])

# "foreign" means, for our purposes, "don't complain about missing NEWS, etc."
AM_INIT_AUTOMAKE([-Wall subdir-objects foreign])

m4_include([./m4/ac_ocaml.m4])
m4_include([./m4/ax_compare_version.m4])

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_MAKE_SET
# If automake defines a special version of AR, request it. The main reason
# for this is to silence a warning with newer automake versions and -Wall,
# so we can leave -Wall turned on for other future warnings. The purpose
# of the underlying feature is to improve portability to systems whose
# "ar" is not Unix-like. It's not needed now, but might be useful if
# we ever port Windows, say.
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
AC_PROG_RANLIB
AC_PROG_OCAML
AC_PROG_OCAMLFIND
AC_PROG_CAMLIDL

AX_COMPARE_VERSION([$OCAMLVERSION], [ge], [4.06.0], [
                     ocaml_unsafe_string="-unsafe-string"
                     AC_SUBST(ocaml_unsafe_string)
                     AC_MSG_NOTICE([Enabling -unsafe-string based on OCaml version])
                   ], [
                     ocaml_unsafe_string=""])

# Checks for ocaml libraries
ocamlfind query extlib
if test $? != 0; then
    echo "vine needs libextlib-ocaml-dev"
    exit -1
fi

# Where is VEX ?
# stolen from valgrind's configure.in
# Nb: For the 2nd arg, the help string, AS_HELP_STRING is the proper way, but
#     older autoconfs don't support it... here's what it would say:
#
#    AS_HELP_STRING([--with-vex], [Vex directory]),
#
AC_ARG_WITH(vex, 
    [  --with-vex=/path/to/vex/dir    Vex directory],
[
    AC_CHECK_FILE($withval/pub/libvex.h,
        [VEX_DIR=$withval],
        [AC_MSG_ERROR([Directory '$withval' does not exist, or does not contain Vex])])
],
[
    VEX_DIR=''
])

if test "${VEX_DIR}" = ""; then
	echo " ** ERROR: "
	echo " ** You must supply --with-vex, which should"
	echo " ** point to the directory containing VEX"
	exit -1
fi

case $VEX_DIR in
    /*) ;;
    *)  echo " ** ERROR: "
        echo " ** Argument to --with-vex must be an absolute path"
        exit -1;;
esac

AC_ARG_WITH(vex-revision, 
    [  --with-vex-revision=<num>    Vex version],
[
        VEX_VERSION=$withval
],
[
        VEX_VERSION=`svn info ${VEX_DIR} | grep Revision | awk '{print $2}'`
])

if test -e "${VEX_DIR}/priv/main/vex_svnversion.h"; then
    VEX_VERSION=`tr -d \" <"${VEX_DIR}/priv/main/vex_svnversion.h"`
fi

if test -e "${VEX_DIR}/../.git" -a -e "${VEX_DIR}/../Makefile.vex.am"; then
    VEX_VERSION=$(expr 3400 + $(cd ${VEX_DIR}/..; git rev-list --first-parent HEAD --count -- VEX) - $(cd ${VEX_DIR}/..; git rev-list --first-parent HEAD --author 'umn\.edu' --count -- VEX) )
    echo Determined VEX version as ${VEX_VERSION} from git
fi

if test "${VEX_VERSION}" = ""; then
    echo "Could not determine VEX version (override with --with-vex-revision=<num>)"
    exit -1
fi

AC_CHECK_LIB([vex], [LibVEX_Version])

AC_SUBST(VEX_DIR)
AC_SUBST(VEX_VERSION)

AC_DEFINE_UNQUOTED([VEX_VERSION], [${VEX_VERSION}], [The VEX SVN Version])

# Pick an appropriate set of STP binaries. This is currently disabled,
# since distributing the binaries bloats the distribution, and
# architecture picking via "uname -m" doesn't work reliably enough.

# This doesn't always give the result we want if you have a 64-bit kernel
# but an entirely 32-bit userspace. Perhaps we could try using
# "gcc -dumpmachine" in addition. As a workaround, you can just copy
# the STP programs manually.
dnl case `uname -m` in
dnl     i?86) stp_arch=i386;;
dnl     x86_64) stp_arch=x86_64;;
dnl     arm*) stp_arch=arm;;
dnl     *) stp_arch=unknown;;
dnl esac;

dnl if test "${stp_arch}" = "unknown"; then
dnl     echo "We don't appear to have an STP binary for your architecture"
dnl     exit -1
dnl fi

dnl echo "Using $stp_arch version of STP"

dnl cp -f stp/$stp_arch/stp stp/stp
dnl cp -f stp/$stp_arch/libstp.a stp/libstp.a

AC_ARG_WITH(binutils, [  --with-binutils=/full/path/to/binutils])

if test "${with_binutils}" = ""; then
	echo "Using system-default binutils"
else
	if test ! -e "$with_binutils/lib/libbfd.a"; then
		echo " ** The --with-binutils directory "
		echo " ** $with_binutils"
		echo " ** should contain"
		echo " ** $with_binutils/lib/libbfd.a,"
		echo " ** but appears not to. Stopping."
		exit -1
	fi
	echo "Using $with_binutils for BFD"
	CFLAGS="-I$with_binutils/include -I$with_binutils/bfd $CFLAGS"
	# Setting CPPFLAGS is needed for AC_CHECK_HEADER
	CPPFLAGS="-I$with_binutils/include -I$with_binutils/bfd $CPPFLAGS"
	CXXFLAGS="-I$with_binutils/include -I$with_binutils/bfd $CXXFLAGS"
	LDFLAGS="-L$with_binutils/lib -L$with_binutils/bfd -L$with_binutils/libiberty $LDFLAGS"
fi

# Checks for libraries: Binutils:
AC_CHECK_LIB([bfd], [bfd_init])
AC_CHECK_LIB([iberty], [xmalloc])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
        [#include <bfd.h>
#include <dis-asm.h>],
        [disassembler_ftype d = disassembler(0);])],
    [AC_DEFINE([HAVE_FUNC_DISASSEMBLER_1], [1],
               [Define to 1 if your libopcodes disassembler() takes one argument.])])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
        [#include <bfd.h>
#include <dis-asm.h>],
        [disassembler_ftype d = disassembler(0, 0, 0, 0);])],
    [AC_DEFINE([HAVE_FUNC_DISASSEMBLER_4], [1],
               [Define to 1 if your libopcodes disassembler() takes four arguments.])])

AC_CHECK_HEADER([bfd.h])

# This function (and other similar ones) were renamed before
# Binutils 2.34. Note that confusingly, older Binutils versions had
# a bfd_section_size macro that incompatibly took two arguments, but
# that isn't what we want. We want the one-argument version, which
# was bfd_get_section_size when it existed, and is bfd_section_size
# after bfd_get_section_size doesn't exist. Right now just detecting
# the presence of these two symbols is enough information because the
# two changes happened at once. If needed in the future we could check
# the number of arguments to bfd_section_size as we do for
# disassembler above.
AC_CHECK_DECLS([bfd_get_section_size, bfd_section_size], [], [],
	       [#include <bfd.h>])

# We don't support any of these architectures. But because of the numbering
# of bfd_arch changes between Binutils versions, you need to include
# intermediate values if you want to replicate a definition of the enum.
# Eventually we just gave up and removed bfd_architecture from our
# interface, instead defining a smaller enum with only our supported
# architectures.
dnl AC_CHECK_DECLS([bfd_arch_spu, bfd_arch_l1om, bfd_arch_k1om, bfd_arch_plugin, bfd_arch_m9s12x, bfd_arch_m9s12xg],[],[],[[#include <bfd.h>]])

cp /dev/null libasmir/makefile.inc
echo "VEX_DIR=${VEX_DIR}" >> libasmir/makefile.inc
echo "LIBASMIR_CXXFLAGS=-I${VEX_DIR}/pub -I`pwd`/src/include $CXXFLAGS" >> libasmir/makefile.inc
echo "LIBASMIR_LDFLAGS=-L${VEX_DIR} -L`pwd`/src $LDFLAGS" >> libasmir/makefile.inc
echo "LIBASMIR_LIBS=-lasmir -lvex -lbfd -liberty" >> libasmir/makefile.inc
echo "OCAML_UNSAFE_STRING=${ocaml_unsafe_string}" >> libasmir/makefile.inc

# Checks for header files.
# AC_HEADER_STDC is obsolete in modern autoconf, but we probably never
# needed it because we have never supported pre-ANSI C.
# AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h stdint.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T

AC_CHECK_SIZEOF(bfd_vma, [], [#include <bfd.h>])

# Checks for standard library functions.
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_CHECK_FUNCS([bzero memset strstr strtoul])


# Check for library function versions

AC_CONFIG_FILES([Makefile
                 libasmir/Makefile
                 libasmir/apps/Makefile
                 libasmir/src/Makefile
                 libasmir/src/include/vex_version.h])
AC_OUTPUT
