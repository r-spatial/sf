dnl Process this file with autoconf to produce a configure script.
dnl GDAL stuff largely copied from rgdal, (c) Roger Bivand
AC_INIT
AC_CONFIG_SRCDIR([src/wkb.cpp])

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

RBIN="${R_HOME}/bin/R"
# https://github.com/r-spatial/sf/issues/1054:
# RVER=`"${RBIN}" --version | head -1 | cut -f3 -d" "`
RSCRIPT="${R_HOME}/bin/Rscript"
RVER=`"${RSCRIPT}" -e 'writeLines(paste(sep=".", base::version$major, base::version$minor))'`

RVER_MAJOR=`echo ${RVER} | cut  -f1 -d"."`
RVER_MINOR=`echo ${RVER} | cut  -f2 -d"."`
RVER_PATCH=`echo ${RVER} | cut  -f3 -d"."`

#if test [$RVER_MAJOR = "development"]; then
    CXX=`"${RBIN}" CMD config CXX`
#else
#    if test [$RVER_MAJOR -lt 3] -o [$RVER_MAJOR -eq 3 -a $RVER_MINOR -lt 3]; then
#        AC_MSG_ERROR([sf is not compatible with R versions before 3.3.0])
#    else
#        CXX=`"${RBIN}" CMD config CXX`
#    fi
#fi

# pick all flags for testing from R
: ${CC=`"${RBIN}" CMD config CC`}
: ${CFLAGS=`"${RBIN}" CMD config CFLAGS`}
: ${CPPFLAGS=`"${RBIN}" CMD config CPPFLAGS`}
: ${CXXFLAGS=`"${RBIN}" CMD config CXXFLAGS`}
: ${LDFLAGS=`"${RBIN}" CMD config LDFLAGS`}
# AC_SUBST([CC],["clang"])
# AC_SUBST([CXX],["clang++"])
AC_MSG_NOTICE([CC: ${CC}])
AC_MSG_NOTICE([CXX: ${CXX}])

# AC_MSG_NOTICE([${PACKAGE_NAME}: ${PACKAGE_VERSION}])

GENERIC_INSTALL_MESSAGE="
*** Installing this package from source requires the prior 
*** installation of external software, see for details 
*** https://r-spatial.github.io/sf/#installing"

#GDAL
GDAL_CONFIG="gdal-config"

GDAL_CONFIG_SET="no"

AC_ARG_WITH([gdal-config],
    AS_HELP_STRING([--with-gdal-config=GDAL_CONFIG],[the location of gdal-config]),
           [gdal_config=$withval])
if test [ -n "$gdal_config" ] ; then
    GDAL_CONFIG_SET="yes"
    AC_SUBST([GDAL_CONFIG],["${gdal_config}"])
    AC_MSG_NOTICE(gdal-config set to $GDAL_CONFIG)
fi

if test ["$GDAL_CONFIG_SET" = "no"] ; then
  AC_PATH_PROG([GDAL_CONFIG], ["$GDAL_CONFIG"],["no"])
  if test ["$GDAL_CONFIG" = "no"] ; then
    AC_MSG_RESULT(no)
    AC_MSG_ERROR([gdal-config not found or not executable. ${GENERIC_INSTALL_MESSAGE}])
  fi
else
  AC_MSG_CHECKING(gdal-config exists)
  if test -r "${GDAL_CONFIG}"; then
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR([gdal-config not found - configure argument error. ${GENERIC_INSTALL_MESSAGE}])
  fi
  AC_MSG_CHECKING(gdal-config executable)
  if test -x "${GDAL_CONFIG}"; then
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR([gdal-config not executable. ${GENERIC_INSTALL_MESSAGE}])
  fi
fi

AC_MSG_CHECKING(gdal-config usability)
if test `${GDAL_CONFIG} --version`;
then
	GDAL_CPPFLAGS=`${GDAL_CONFIG} --cflags`
	GDAL_VERSION=`${GDAL_CONFIG} --version`
	GDAL_LIBS=`${GDAL_CONFIG} --libs`
	GDAL_DEP_LIBS=`${GDAL_CONFIG} --dep-libs`
	GDAL_DATADIR=`${GDAL_CONFIG} --datadir`
	AC_MSG_RESULT(yes)
else
	AC_MSG_ERROR([gdal-config not found. ${GENERIC_INSTALL_MESSAGE}])
fi

AC_MSG_NOTICE([GDAL: ${GDAL_VERSION}])
AC_MSG_CHECKING([GDAL version >= 2.0.1])

GDAL_MAJ_VER=`echo $GDAL_VERSION | cut -d "." -f1`
GDAL_MIN_VER=`echo $GDAL_VERSION | cut -d "." -f2`
GDAL_PATCH_VER=`echo $GDAL_VERSION | cut -d "." -f3`
if test ${GDAL_MAJ_VER} -lt 2 ; then
  AC_MSG_RESULT(no)
  AC_MSG_ERROR([sf is not compatible with GDAL versions below 2.0.1])
else
   AC_MSG_RESULT(yes)
fi

#if test [${GDAL_MAJ_VER} -eq 3] -a [${GDAL_MIN_VER} -eq 6] -a [${GDAL_PATCH_VER} -eq 0] ; then
if test "${GDAL_VERSION}" = "3.6.0" ; then
  AC_MSG_ERROR([GDAL version 3.6.0 has been withdrawn, please update GDAL])
fi

INLIBS="${LIBS}"
INCPPFLAGS="${CPPFLAGS}"
INPKG_CPPFLAGS="${PKG_CPPFLAGS}"
INPKG_LIBS="${PKG_LIBS}"

AC_SUBST([PKG_CPPFLAGS], ["${INPKG_CPPFLAGS} ${GDAL_CPPFLAGS}"])
AC_SUBST([PKG_LIBS], ["${INPKG_LIBS} ${GDAL_LIBS}"])

# honor PKG_xx overrides
# for CPPFLAGS we will superfluously double R's flags
# since we'll set PKG_CPPFLAGS with this, but that shouldn't hurt
CPPFLAGS="${INCPPFLAGS} ${PKG_CPPFLAGS}"

gdalok=yes
AC_CHECK_HEADERS(gdal.h,,gdalok=no)
if test "${gdalok}" = no; then
   AC_MSG_ERROR([gdal.h not found in given locations. ${GENERIC_INSTALL_MESSAGE}])
fi

NEED_DEPS=no
LIBS="${INLIBS} ${PKG_LIBS}"
[cat > gdal_test.cpp <<_EOCONF
#include <gdal.h>
#ifdef __cplusplus
extern "C" {
#endif
int main(void) {
GDALAllRegister();
}
#ifdef __cplusplus
}
#endif
_EOCONF]

AC_MSG_CHECKING(GDAL: linking with --libs only)
${CXX} ${CPPFLAGS} -o gdal_test gdal_test.cpp ${LIBS} 2> errors.txt
if test `echo $?` -ne 0 ; then
gdalok=no
AC_MSG_RESULT(no)
else
AC_MSG_RESULT(yes)
fi

if test "${gdalok}" = no; then
AC_MSG_CHECKING(GDAL: linking with --libs and --dep-libs)
LIBS="${LIBS} ${GDAL_DEP_LIBS}"
gdalok=yes
${CXX} ${CPPFLAGS} -o gdal_test gdal_test.cpp ${LIBS} 2>> errors.txt
if test `echo $?` -ne 0 ; then
gdalok=no
fi
if test "${gdalok}" = yes; then
    NEED_DEPS=yes
    AC_MSG_RESULT(yes)
else
    AC_MSG_RESULT(no)
fi
fi

if test "${gdalok}" = no; then
   cat errors.txt
   AC_MSG_NOTICE([Install failure: compilation and/or linkage problems.])
   AC_MSG_ERROR([GDALAllRegister not found in libgdal. ${GENERIC_INSTALL_MESSAGE}])
fi

rm -f gdal_test errors.txt gdal_test.cpp

GDAL_GE_250="no"
GDAL_MAJ_VER=`echo $GDAL_VERSION | cut -d "." -f1`
GDAL_MOD_VER=`echo $GDAL_VERSION | cut -d "." -f2`
if test "${GDAL_MAJ_VER}" = 2 ; then
  if test "${GDAL_MOD_VER}" -ge 5 ; then
    GDAL_GE_250="yes" 
  fi
else 
  if test "${GDAL_MAJ_VER}" -ge 3 ; then
    GDAL_GE_250="yes" 
  fi
fi

GDAL_DATA_TEST_FILE="${GDAL_DATADIR}/pcs.csv"
AC_MSG_CHECKING(GDAL: ${GDAL_DATADIR}/pcs.csv readable)
if test -r "${GDAL_DATA_TEST_FILE}" ; then
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
  if test "${GDAL_GE_250}" = "no" ; then
    AC_MSG_ERROR([pcs.csv not found in GDAL data directory.])
  fi
fi

# Optional local copy of GDAL datadir and PROJ_LIB

data_copy=no
if test "${PROJ_GDAL_DATA_COPY}" ; then
    data_copy=yes
    AC_MSG_NOTICE([PROJ_GDAL_DATA_COPY used.])
else
    AC_ARG_WITH([data-copy],
        AS_HELP_STRING([--with-data-copy=yes/no],[local copy of data directories in package, default no]),
               [data_copy=$withval])
fi
if test "${data_copy}" = "yes" ; then
AC_MSG_NOTICE([Copy data for:])
  proj_lib0="${PROJ_LIB}"
  AC_ARG_WITH([proj-data],
    AS_HELP_STRING([--with-proj-data=DIR],[location of PROJ data directory]),
    [proj_lib1=$withval])
  if test -n "${proj_lib0}" ; then
    proj_lib="${proj_lib0}"
  else
    proj_lib="${proj_lib1}"
  fi
  if test -n "${proj_lib}" ; then
    if test -d "${proj_lib}" ; then
      cp -r "${proj_lib}" "${R_PACKAGE_DIR}"
      AC_MSG_NOTICE([  PROJ: ${proj_lib}])
    else
      AC_MSG_ERROR([PROJ data files not found; set environment variable PROJ_LIB=DIR or --with-proj-data=DIR.])
    fi
  else
      AC_MSG_ERROR([PROJ data files not found; set environment variable PROJ_LIB=DIR or --with-proj-data=DIR.])
  fi

  if test -d "${GDAL_DATADIR}" ; then
    cp -r "${GDAL_DATADIR}" "${R_PACKAGE_DIR}"
    AC_MSG_NOTICE([  GDAL: ${GDAL_DATADIR}])
  else
    AC_MSG_ERROR([GDAL data files not found.])
  fi
fi

#
# test whether PROJ is available to gdal:
#

gdal_has_proj=no
[cat > gdal_proj.cpp <<_EOCONF
#include <gdal.h>
#include <ogr_srs_api.h>
#include <ogr_spatialref.h>
int main(int argc, char *argv[]) {
	OGRSpatialReference *dest = new OGRSpatialReference;
	OGRSpatialReference *src = new OGRSpatialReference;
    src->importFromEPSG(4326);
    dest->importFromEPSG(3857);
	OGRCoordinateTransformation *ct = OGRCreateCoordinateTransformation(src, dest);
	return(ct == NULL); // signals PROJ is not available through gdal
}
_EOCONF]

AC_MSG_CHECKING(GDAL: checking whether PROJ is available for linking:)
${CXX} ${CPPFLAGS} -o gdal_proj gdal_proj.cpp ${LIBS} 2> errors.txt
if test `echo $?` -ne 0 ; then
gdal_has_proj=no
AC_MSG_RESULT(no)
else
gdal_has_proj=yes
AC_MSG_RESULT(yes)
fi

if test "${gdal_has_proj}" = no; then
   cat errors.txt
   AC_MSG_NOTICE([Install failure: compilation and/or linkage problems.])
   AC_MSG_ERROR([cannot link projection code])
fi

AC_MSG_CHECKING(GDAL: checking whether PROJ is available for running:)
if test "x$cross_compiling" = "xyes"; then
AC_MSG_RESULT(cross compiling, assuming yes)
else
./gdal_proj
if test `echo $?` -ne 0 ; then
gdal_has_proj=no
AC_MSG_RESULT(no)
else
gdal_has_proj=yes
AC_MSG_RESULT(yes)
fi
if test "${gdal_has_proj}" = no; then
   AC_MSG_ERROR([OGRCoordinateTransformation() does not return a coord.trans: PROJ not available?])
fi
fi
rm -fr errors.txt gdal_proj.cpp gdal_proj
AC_MSG_NOTICE([GDAL: ${GDAL_VERSION}])


# sqlite3
AC_ARG_WITH([sqlite3-lib],
    AS_HELP_STRING([--with-sqlite3-lib=LIB_PATH],[the location of sqlite3 libraries]),
               [sqlite3_lib_path=$withval])
if test [ -n "$sqlite3_lib_path" ] ; then
    AC_SUBST([SQLITE3_LIBS], ["-L${sqlite3_lib_path}"])
fi


#
# PROJ
#

PROJ_CONFIG="pkg-config proj"

if `$PROJ_CONFIG --exists` ; then
  AC_MSG_NOTICE([pkg-config proj exists, will use it])
  proj_config_ok=yes
else
  proj_config_ok=no
fi

AC_ARG_WITH([proj-include],
    AS_HELP_STRING([--with-proj-include=DIR],[location of proj header files]),
    [proj_include_path=$withval])
if test [ -n "$proj_include_path" ] ; then
   AC_SUBST([PROJ_CPPFLAGS],["-I${proj_include_path}"])
else
  if test "${proj_config_ok}" = yes; then
    PROJ_INCLUDE_PATH=`${PROJ_CONFIG} --cflags`
    AC_SUBST([PROJ_CPPFLAGS],["${PROJ_INCLUDE_PATH}"])
  fi
fi

# honor PKG_xx overrides
# for CPPFLAGS we will superfluously double R's flags
# since we'll set PKG_CPPFLAGS with this, but that shouldn't hurt

PROJ6="no"
PROJH="no"
if test "${proj_config_ok}" = yes; then
  PROJ_VERSION=`${PROJ_CONFIG} --modversion`
  PROJV1=`echo "${PROJ_VERSION}" | cut -d "." -f 1`
  if test "${PROJV1}" -ge 6; then
    PROJ6="yes"
    PROJ_CPPFLAGS="${PROJ_CPPFLAGS} -DHAVE_PROJ_H"
    if test "${proj_api}" = yes; then
        AC_MSG_NOTICE([using proj_api.h even with PROJ 5/6])
        PROJ_CPPFLAGS="${PROJ_CPPFLAGS} -DACCEPT_USE_OF_DEPRECATED_PROJ_API_H"
    else
        AC_MSG_NOTICE([using proj.h.])
        PROJH="yes"
    fi
  fi
else
  if test "${PROJH}" = no ; then
    PROJH=yes
    AC_CHECK_HEADERS(proj.h,,PROJH=no)
    if test "${PROJH}" = yes; then
      PROJ6="yes"
      PROJ_CPPFLAGS="${PROJ_CPPFLAGS} -DHAVE_PROJ_H"
    else
      AC_MSG_ERROR([proj.h not found; PROJ >= 6.0.0 is required.])
    fi
  fi
fi

CPPFLAGS="${INCPPFLAGS} ${PKG_CPPFLAGS} ${PROJ_CPPFLAGS}"

# dnl ditto for a library path
AC_ARG_WITH([proj-lib],
    AS_HELP_STRING([--with-proj-lib=LIB_PATH],[the location of proj libraries]),
               [proj_lib_path=$withval])
if test [ -n "$proj_lib_path" ] ; then
    AC_SUBST([PROJ_LIBS], ["-L${proj_lib_path} ${INPKG_LIBS} -lproj"])
else
  if test "${proj_config_ok}" = yes; then
    if test `uname` = "Darwin"; then
      PROJ_LIB_PATH=`${PROJ_CONFIG} --libs --static`
    else
      PROJ_LIB_PATH=`${PROJ_CONFIG} --libs`
    fi
    AC_SUBST([PROJ_LIBS], ["${PROJ_LIB_PATH} ${INPKG_LIBS}"])
	proj_version=`${PROJ_CONFIG} --modversion`
	AC_MSG_NOTICE([PROJ: ${proj_version}])
  else
    PROJ_LIBS="${PKG_LIBS} -lproj"
  fi
fi

LIBS="${PROJ_LIBS} ${INLIBS} ${PKG_LIBS}"

[cat > proj_conf_test.cpp <<_EOCONF
#include <stdio.h>
#include <stdlib.h>
#include <proj.h>

int main(void) {
	proj_context_create();
    exit(0);
}
_EOCONF]
#AC_CHECK_LIB(proj,proj_context_create,,proj6ok=no)
AC_MSG_CHECKING(PROJ: checking whether PROJ and sqlite3 are available for linking:)
${CXX} ${CPPFLAGS} -o proj_conf_test proj_conf_test.cpp ${LIBS} $SQLITE3_LIBS -lsqlite3 2> errors.txt
if test `echo $?` -ne 0 ; then
  proj6ok=no
  AC_MSG_RESULT(no)
else
  proj6ok=yes
  AC_MSG_RESULT(yes)
fi
if test "${proj6ok}" = no; then
  AC_MSG_ERROR([libproj or sqlite3 not found in standard or given locations. ${GENERIC_INSTALL_MESSAGE}])
fi

[cat > proj_conf_test.c <<_EOCONF
#include <stdio.h>
#include <stdlib.h>
#include <proj.h>

int main(void) {
    printf("%d.%d.%d\n", PROJ_VERSION_MAJOR, PROJ_VERSION_MINOR, PROJ_VERSION_PATCH);
    exit(0);
}
_EOCONF]

#AC_MSG_NOTICE([PKG_LIBS: ${PKG_LIBS}])

${CC} ${CFLAGS} -o proj_conf_test proj_conf_test.c ${PROJ_LIBS}

proj_version=`./proj_conf_test`

AC_ARG_WITH([proj-share],
    AS_HELP_STRING([--with-proj-share=SHARE_PATH],[the location of proj metadata files]),
               [proj_share_path=$withval])
if test [ -n "$proj_share_path" ] ; then
    AC_MSG_NOTICE([PROJ_LIB: ${proj_share_path}])
fi

#
# GEOS:
#
GEOS_CONFIG="geos-config"

GEOS_CONFIG_SET="no"

AC_ARG_WITH([geos-config],
    AS_HELP_STRING([--with-geos-config=GEOS_CONFIG],[the location of geos-config]),
           [geos_config=$withval])
if test [ -n "$geos_config" ] ; then
    GEOS_CONFIG_SET="yes"
    AC_SUBST([GEOS_CONFIG],["${geos_config}"])
    AC_MSG_NOTICE(geos-config set to $GEOS_CONFIG)
fi

if test ["$GEOS_CONFIG_SET" = "no"] ; then
  AC_PATH_PROG([GEOS_CONFIG], ["$GEOS_CONFIG"],["no"])
  if test ["$GEOS_CONFIG" = "no"] ; then
    AC_MSG_RESULT(no)
    AC_MSG_ERROR([geos-config not found or not executable. ${GENERIC_INSTALL_MESSAGE}])
  fi
else
  AC_MSG_CHECKING(geos-config exists)
  if test -r "${GEOS_CONFIG}"; then
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR([geos-config not found - configure argument error. ${GENERIC_INSTALL_MESSAGE}])
  fi
  AC_MSG_CHECKING(geos-config executable)
  if test -x "${GEOS_CONFIG}"; then
    AC_MSG_RESULT(yes)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR([geos-config not executable. ${GENERIC_INSTALL_MESSAGE}])
  fi
fi

AC_MSG_CHECKING(geos-config usability)
if test `${GEOS_CONFIG} --version`
then
  GEOS_CLIBS="`${GEOS_CONFIG} --clibs`"
  #GEOS_DEP_CLIBS=`geos-config --static-clibs` -- this gives -m instead of -lm, which breaks clang
  # fixed in 3.7.0 at https://github.com/libgeos/libgeos/pull/73#issuecomment-262208677
  GEOS_DEP_CLIBS=`${GEOS_CONFIG} --static-clibs | sed 's/-m/-lm/g'`
  GEOS_CPPFLAGS=`${GEOS_CONFIG} --cflags`
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
  AC_MSG_ERROR([${GEOS_CONFIG} not usable])
fi

GEOS_VERSION=`${GEOS_CONFIG} --version`
AC_MSG_NOTICE([GEOS: ${GEOS_VERSION}])
AC_MSG_CHECKING([GEOS version >= 3.4.0]) # GDAL 2.0.1 requires GEOS 3.1.0
GEOS_VER_DOT=`echo $GEOS_VERSION | tr -d ".[[:alpha:]]"`
if test ${GEOS_VER_DOT} -lt 340 ; then
  AC_MSG_RESULT(no)
  AC_MSG_ERROR([upgrade GEOS to 3.4.0 or later])
else
  AC_MSG_RESULT(yes)
fi

AC_SUBST([PKG_CPPFLAGS], ["${INPKG_CPPFLAGS} ${PROJ_CPPFLAGS} ${GDAL_CPPFLAGS} ${GEOS_CPPFLAGS}"])
AC_SUBST([PKG_LIBS], ["${INPKG_LIBS} ${GDAL_LIBS}"])
if test "${NEED_DEPS}" = yes; then
   AC_SUBST([PKG_LIBS], ["${PKG_LIBS} ${GDAL_DEP_LIBS}"])
fi

# honor PKG_xx overrides
# for CPPFLAGS we will superfluously double R's flags
# since we'll set PKG_CPPFLAGS with this, but that shouldn't hurt
CPPFLAGS="${CPPFLAGS} ${PKG_CPPFLAGS}"
LIBS="${LIBS} ${PKG_LIBS}"

geosok=yes
AC_CHECK_HEADERS(geos_c.h,,geosok=no)
if test "${geosok}" = no; then
   AC_MSG_ERROR([geos_c.h not found in given locations. ${GENERIC_INSTALL_MESSAGE}])
fi

[cat > geos_test.cpp <<_EOCONF
#include <geos_c.h>
#ifdef __cplusplus
extern "C" {
#endif
static void __errorHandler(const char *fmt, ...) {
    return;
}
static void __warningHandler(const char *fmt, ...) {
    return;
}
int main(void) {
    GEOSContextHandle_t r = initGEOS_r((GEOSMessageHandler) __warningHandler, (GEOSMessageHandler) __errorHandler);
    finishGEOS_r(r);
}
#ifdef __cplusplus
}
#endif
_EOCONF]

#echo "${CXX} ${CPPFLAGS} -o geos_test geos_test.cpp ${LIBS}"
AC_MSG_CHECKING(geos: linking with ${GEOS_CLIBS})
${CXX} ${CPPFLAGS} -o geos_test geos_test.cpp ${GEOS_CLIBS} 2> errors.txt
if test `echo $?` -ne 0 ; then
 geosok=no
 AC_MSG_RESULT(no)
else
 AC_SUBST([PKG_LIBS], ["${PKG_LIBS} ${GEOS_CLIBS}"])
 AC_MSG_RESULT(yes)
fi

if test "${geosok}" = no; then
  AC_MSG_CHECKING(geos: linking with ${GEOS_DEP_CLIBS})
  ${CXX} ${CPPFLAGS} -o geos_test geos_test.cpp ${GEOS_DEP_CLIBS} 2> errors.txt
  if test `echo $?` -ne 0 ; then
    geosok=no
    AC_MSG_RESULT(no)
    cat errors.txt
    AC_MSG_NOTICE([Install failure: compilation and/or linkage problems.])
    AC_MSG_ERROR([initGEOS_r not found in libgeos_c. ${GENERIC_INSTALL_MESSAGE}])
  else
    AC_SUBST([PKG_LIBS], ["${PKG_LIBS} ${GEOS_DEP_CLIBS}"])
    AC_MSG_RESULT(yes)
  fi
fi

rm -f geos_test errors.txt geos_test.cpp

#
# add PROJ_LIBS
#
AC_SUBST([PKG_LIBS], ["${PROJ_LIBS} ${PKG_LIBS}"])

#
# concluding substitution
#
AC_MSG_NOTICE([Package CPP flags: ${PKG_CPPFLAGS}])
AC_MSG_NOTICE([Package LIBS: ${PKG_LIBS}])

AC_CONFIG_FILES(src/Makevars)
AC_OUTPUT
