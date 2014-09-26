AC_INIT([Haskell CUDA MAGMA bindings], [0.1.0.0], [sherman@metsci.com], [magma])
AC_CONFIG_SRCDIR([Foreign/CUDA/Magma.hs])
AC_CONFIG_FILES([magma.buildinfo])
AC_PROG_CC

# Determine the target platform
#
AC_CANONICAL_TARGET

# Search the user's PATH for the 'nvcc' compiler. If it is found, add this
# prefix to the include and library search directories. Additionally, set nvcc
# as the C preprocessor for c2hs (only, or it won't try to link cudart)
#
AC_PATH_PROG(NVCC, nvcc)
if test "$NVCC" != ""; then
    cuda_prefix="$(dirname "$(dirname "$NVCC")")"
    cuda_c2hsflags="--cpp="$NVCC" --cppopts=-E "
    cuda_dir="${cuda_prefix}/include"
    CPPFLAGS+=" "-I${cuda_dir}" "
#    CC=${NVCC}

    case $target in
    x86_64*) cuda_lib_dir="${cuda_prefix}/lib64" ;;
    *)       cuda_lib_dir="${cuda_prefix}/lib"   ;;
    esac
    LDFLAGS+=" -L${cuda_lib_dir} "
fi

# Ben's crazy addition
magma_dir="/usr/local/magma/include"
magma_lib_dir="/usr/local/magma/lib"

CPPFLAGS+=" -I${magma_dir} "
LDFLAGS+=" -L${magma_lib_dir} "
# End Ben's crazy addition


AC_CHECK_HEADERS([cublas_v2.h],              [], [AC_MSG_ERROR(could not find CUBLAS headers${longerror})])
#AC_CHECK_HEADERS([magma.h],                  [], [AC_MSG_ERROR(could not find MAGMA headers)])
#It is a problem that Autoconf can't find the magma headers!!!!
#AC_SEARCH_LIBS(magma_num_gpus  , magma ,     [], [AC_MSG_ERROR(could not find MAGMA library)])

# Populate the buildinfo, with the search paths and any target specific options
#
cuda_cppflags="$CPPFLAGS "
cuda_ldflags="$LDFLAGS $LIBS "

AC_SUBST([magma_dir])
AC_SUBST([cuda_dir])
AC_SUBST([magma_lib_dir])

AC_SUBST([cuda_cppflags])
AC_SUBST([cuda_ldflags])
AC_SUBST([cuda_c2hsflags])
AC_OUTPUT

