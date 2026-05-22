# =========================================================================
# Set download locations depending on git origin
# =========================================================================
SET(LIBS_DLPATH "https://gitlab.iag.uni-stuttgart.de/")
# Origin pointing to IAG
IF("${GIT_ORIGIN}" MATCHES ".iag.uni-stuttgart.de" AND "${GIT_ORIGIN}" MATCHES "^git@")
  # SSH is expensive, run it only once
  IF(NOT DEFINED SSH_IAG_CACHED)
    # Check if IAG Gitlab is reachable with SSH
    EXECUTE_PROCESS(COMMAND ssh -T -o BatchMode=yes -o ConnectTimeout=5 git@gitlab.iag.uni-stuttgart.de
                    RESULT_VARIABLE SSH_IAG
                    OUTPUT_QUIET ERROR_QUIET)
    SET(SSH_IAG_CACHED "{SSH_IAG}" CACHE INTERNAL "Exit code of SSH check")
    IF(SSH_IAG EQUAL 0)
      SET(LIBS_DLPATH "git@gitlab.iag.uni-stuttgart.de:")
    ELSE()
      MESSAGE(STATUS "Cannot reach gitlab.iag.uni-stuttgart.de via SSH. Falling back to HTTPS.")
    ENDIF()
  ENDIF()
ENDIF()

# Unset leftover variables from previous runs
UNSET(linkedlibs CACHE)

# =========================================================================
# Add the libraries
# =========================================================================
# Set directory to compile external libraries
SET(LIBS_EXTERNAL_LIB_DIR ${CMAKE_CURRENT_SOURCE_DIR}/share/${CMAKE_Fortran_COMPILER_ID})
MARK_AS_ADVANCED(FORCE LIBS_EXTERNAL_LIB_DIR)

# =========================================================================
# HDF5 library
# =========================================================================
# Try to find system HDF5 using CMake
SET(LIBS_HDF5_CMAKE TRUE)

# Set preferences for HDF5 library
SET(HDF5_USE_STATIC_LIBRARIES TRUE)
SET(HDF5_PREFER_PARALLEL FALSE)
FIND_PROGRAM(HDF5_COMPILER h5cc)
MARK_AS_ADVANCED(FORCE HDF5_COMPILER)

# When using the configure version, CMake takes the directory of the first HDF5 compiler found
# > h5cc  - serial   version
# > h5pcc - parallel version
# > Thus, we need to prepend the PATH to ensure we are picking the correct one first
IF(NOT "${HDF5_COMPILER}" STREQUAL "" AND NOT "${HDF5_COMPILER}" STREQUAL "HDF5_COMPILER-NOTFOUND")
  SET(ORIGINAL_PATH_ENV "$ENV{PATH}")
  GET_FILENAME_COMPONENT(HDF5_PARENT_DIR ${HDF5_COMPILER} DIRECTORY)
  SET(ENV{PATH} "${HDF5_PARENT_DIR}:$ENV{PATH}")
ENDIF()

IF (NOT LIBS_BUILD_HDF5)
  FIND_PACKAGE(HDF5 QUIET COMPONENTS C Fortran)

  # Could not find the static version, look for the shared library
  IF(NOT HDF5_FOUND)
    UNSET(HDF5_USE_STATIC_LIBRARIES)
    FIND_PACKAGE(HDF5 QUIET COMPONENTS C Fortran)
  ENDIF()

  # HOPR can use a parallel system HDF5 if the self-built CGNS finds MPI, i.e. we need to link CGNS against MPI even though it does not use it
  # Therefore, along with CGNS_ENABLE_HDF5, enable HDF5_NEEDS_MPI for a parallel system HDF5, which invokes CGNS' own FindMPI script
  # see also: https://stackoverflow.com/questions/51561033/using-serial-hdf5-c-with-cmake
  # > HDF5_IS_PARALLEL is set by FIND_PACKAGE(HDF5)
  #  IF(HDF5_IS_PARALLEL)
  #    UNSET(HDF5_FOUND)
  #  ENDIF()

  # HDF5 1.14.5 and 1.14.6 contain a bug where they wrongly add an incomplete ZLIB::ZLIB target, which prevent CGNS compilation
  # see also https://github.com/CGNS/CGNS/pull/834
  IF(HDF5_FOUND AND NOT "${HDF5_VERSION}" STREQUAL "" AND "${HDF5_VERSION}" GREATER_EQUAL "1.14.5" AND "${HDF5_VERSION}" LESS_EQUAL "1.14.6")
    IF(TARGET hdf5-static)
      # if system HDF5 links against a faulty ZLIB:ZLIB target interface, use self-built HDF5 instead
      GET_TARGET_PROPERTY(check_hdf5_link_defs hdf5-static INTERFACE_LINK_LIBRARIES)
      IF ("$<LINK_ONLY:ZLIB::ZLIB>" IN_LIST check_hdf5_link_defs)
        UNSET(HDF5_FOUND)
      ENDIF()
    ENDIF()
  ENDIF()

  IF (HDF5_FOUND)
    MESSAGE (STATUS "[HDF5] found in system libraries [${HDF5_DIR}]")
    SET(LIBS_BUILD_HDF5 OFF CACHE BOOL "Compile and build HDF5 library")
  ELSE()
    MESSAGE (STATUS "[HDF5] not found in system libraries")
    SET(LIBS_BUILD_HDF5 ON  CACHE BOOL "Compile and build HDF5 library")
  ENDIF()
ENDIF()

# Use system HDF5
IF(NOT LIBS_BUILD_HDF5)
  # Unset leftover paths from old CMake runs
  UNSET(HDF5_VERSION CACHE)
  UNSET(HDF5_DEFINITIONS)
  UNSET(HDF5_LIBRARIES)
  UNSET(HDF5_INCLUDE_DIR_FORTRAN)
  UNSET(HDF5_INCLUDE_DIR)
  UNSET(HDF5_DIFF_EXECUTABLE)

  # If library is specifically requested, it is required
  FIND_PACKAGE(HDF5 REQUIRED COMPONENTS C Fortran)

  # If Fortran module files cannot be found in the HDF5_INCLUDE_DIR set by FIND_PACKAGE(HDF5), obtain the correct path from the target properties (supposedly HDF5_INCLUDE_DIR_FORTRAN)
  # > NOTE: Depending on HDF5 config (flag HDF5_INSTALL_MOD_FORTRAN) and version, mod-files can be located in include/ or mod/, or subdirectories
  FIND_FILE(PATH_MODFILES h5a.mod ${HDF5_INCLUDE_DIR})
  IF(NOT PATH_MODFILES)
    IF(HDF5_USE_STATIC_LIBRARIES)
      GET_TARGET_PROPERTY(HDF5_INCLUDE_DIR hdf5_fortran-static INTERFACE_INCLUDE_DIRECTORIES)
    ELSE()
      GET_TARGET_PROPERTY(HDF5_INCLUDE_DIR hdf5_fortran-shared INTERFACE_INCLUDE_DIRECTORIES)
    ENDIF()
  ENDIF()

  # Check if HDF5 is parallel
  # > NOTE: As of HDF5 v2.0.0 the library variables have been renamed: HDF5_ENABLE_PARALLEL to configure the HDF5 installation, HDF5_PROVIDES_PARALLEL to query the state of the installation.
  #         This has been adopted by FindHDF5 as of CMake v4.3.0, for earlier CMake versions we need to account for it manually (cf. https://gitlab.kitware.com/cmake/cmake/-/merge_requests/11573)
  IF(${CMAKE_VERSION} VERSION_LESS "4.3.0" AND ${HDF5_VERSION} VERSION_GREATER_EQUAL "2.0.0")
    SET(HDF5_IS_PARALLEL ${HDF5_PROVIDES_PARALLEL})
  ENDIF()

  # If HDF5 Fortran library is not set, get it from the Fortran target
  IF("${HDF5_C_LIBRARY_hdf5_c}" STREQUAL "")
    GET_PROPERTY(HDF5_C_LIBRARY_hdf5_c TARGET hdf5::hdf5 PROPERTY LOCATION)
  ENDIF()

  IF(NOT "${HDF5_C_LIBRARY_hdf5_c}" STREQUAL "")
    IF(APPLE)
      EXECUTE_PROCESS(COMMAND nm -gU      ${HDF5_C_LIBRARY_hdf5_c} COMMAND grep inflate OUTPUT_VARIABLE HDF5_USES_ZLIB RESULT_VARIABLE GREP_RESULT OUTPUT_STRIP_TRAILING_WHITESPACE)
    ELSE()
      EXECUTE_PROCESS(COMMAND readelf -Ws ${HDF5_C_LIBRARY_hdf5_c} COMMAND grep inflate OUTPUT_VARIABLE HDF5_USES_ZLIB RESULT_VARIABLE GREP_RESULT OUTPUT_STRIP_TRAILING_WHITESPACE)
    ENDIF()
  ELSE()
    SET(GREP_RESULT 1)
  ENDIF()

  IF(GREP_RESULT EQUAL 0)
    # HDF5 is linked against zlib, find it here
    SET(ZLIB_USE_STATIC_LIBS "ON")
    FIND_PACKAGE(ZLIB QUIET)

    # Could not find the static version, look for the shared library
    IF(NOT ZLIB_FOUND)
      UNSET(ZLIB_USE_STATIC_LIBS)
      FIND_PACKAGE(ZLIB REQUIRED)
    ENDIF()
  ENDIF()

  # Set build status to system
  SET(HDF5_BUILD_STATUS "system")
ELSE()
  MESSAGE(STATUS "Setting [HDF5] to self-build")
  # Origin pointing to Github
  IF("${GIT_ORIGIN}" MATCHES ".github.com")
    SET (HDF5DOWNLOAD "https://github.com/HDFGroup/hdf5.git")
  ELSE()
    SET (HDF5DOWNLOAD ${LIBS_DLPATH}libs/hdf5.git )
  ENDIF()
  SET(HDF5_DOWNLOAD ${HDF5DOWNLOAD} CACHE STRING "HDF5 Download-link")
  MESSAGE(STATUS "Setting [HDF5] download link: ${HDF5DOWNLOAD}")
  MARK_AS_ADVANCED(FORCE HDF5_DOWNLOAD)

  # Set HDF5 tag / version
  SET(HDF5_STR "1.14.5")
  SET(HDF5_TAG "hdf5_${HDF5_STR}" CACHE STRING   "HDF5 version tag")
  MARK_AS_ADVANCED(FORCE HDF5_TAG)
  MESSAGE(STATUS "Setting [HDF5] download tag:  ${HDF5_TAG}")

  # Set HDF5 build dir
  SET(LIBS_HDF5_DIR ${LIBS_EXTERNAL_LIB_DIR}/HDF5)

  # Check if HDF5 was already built
  UNSET(HDF5_FOUND)
  UNSET(HDF5_VERSION)
  UNSET(HDF5_INCLUDE_DIR)
  UNSET(HDF5_LIBRARIES)
  UNSET(HDF5_Fortran_LIBRARIES)
  FIND_PACKAGE(HDF5 ${HDF5_STR} QUIET COMPONENTS C Fortran HDF5_PREFER_PARALLEL=OFF PATHS ${LIBS_HDF5_DIR} NO_DEFAULT_PATH)

  IF(HDF5_FOUND)
    # If re-running CMake, it might wrongly pick-up the system HDF5
    IF(NOT EXISTS ${LIBS_HDF5_DIR}/lib/libhdf5.a)
      UNSET(HDF5_FOUND)
      SET(HDF5_VERSION     ${HDF5_STR})
    ENDIF()

    # CMake might fail to set the HDF5 paths
    IF(HDF5_FOUND AND "${HDF5_LIBRARIES}" STREQUAL "")
      SET(HDF5_INCLUDE_DIR       ${LIBS_HDF5_DIR}/include)
      # WARNING: The order of the following libraries matters! They need to be listed from the most dependent to the least dependent.
      SET(HDF5_LIBRARIES         ${LIBS_HDF5_DIR}/lib/libhdf5_fortran.a ${LIBS_HDF5_DIR}/lib/libhdf5_f90cstub.a ${LIBS_HDF5_DIR}/lib/libhdf5_hl_fortran.a ${LIBS_HDF5_DIR}/lib/libhdf5_hl_f90cstub.a ${LIBS_HDF5_DIR}/lib/libhdf5_hl.a ${LIBS_HDF5_DIR}/lib/libhdf5.a ${LIBS_HDF5_DIR}/lib/libhdf5_tools.a)
    ENDIF()
  ENDIF()

  # Check again if HDF5 was found
  IF(NOT HDF5_FOUND)
    # Set parallel build with maximum number of threads
    INCLUDE(ProcessorCount)
    PROCESSORCOUNT(N)

    # Let CMake take care of download, configure and build
    EXTERNALPROJECT_ADD(HDF5
      GIT_REPOSITORY     ${HDF5_DOWNLOAD}
      GIT_TAG            ${HDF5_TAG}
      GIT_PROGRESS       TRUE
      ${${GITSHALLOW}}
      PREFIX             ${LIBS_HDF5_DIR}
      UPDATE_COMMAND     ""
      # HDF5 explicitely needs "make" to configure
      CMAKE_GENERATOR    "Unix Makefiles"
      BUILD_COMMAND      make -j${N}
      # Set the CMake arguments for HDF5
      CMAKE_ARGS         -DCMAKE_BUILD_TYPE=None -DCMAKE_INSTALL_PREFIX=${LIBS_HDF5_DIR} -DHDF5_INSTALL_CMAKE_DIR=lib/cmake/hdf5 -DCMAKE_POLICY_DEFAULT_CMP0175=OLD -DBUILD_STATIC_LIBS=ON -DBUILD_SHARED_LIBS=OFF -DHDF5_BUILD_FORTRAN=ON -DHDF5_ENABLE_Z_LIB_SUPPORT=OFF -DHDF5_ENABLE_SZIP_SUPPORT=OFF -DHDF5_ENABLE_PARALLEL=OFF
      # Set the build byproducts
      # WARNING: The order of the following libraries matters! They need to be listed from the most dependent to the least dependent.
      BUILD_BYPRODUCTS ${LIBS_HDF5_DIR}/lib/libhdf5_fortran.a ${LIBS_HDF5_DIR}/lib/libhdf5_f90cstub.a ${LIBS_HDF5_DIR}/lib/libhdf5_hl_fortran.a ${LIBS_HDF5_DIR}/lib/libhdf5_hl_f90cstub.a ${LIBS_HDF5_DIR}/lib/libhdf5_hl.a ${LIBS_HDF5_DIR}/lib/libhdf5.a ${LIBS_HDF5_DIR}/lib/libhdf5_tools.a ${LIBS_HDF5_DIR}/bin/h5diff
    )

    # Add CMake HDF5 to the list of self-built externals
    LIST(APPEND SELFBUILTEXTERNALS HDF5)

    # Set HDF5 version and MPI support
    SET(HDF5_VERSION ${HDF5_STR})

    # Set HDF5 paths
    # > NOTE: For self-built HDF5, we use a specific version, of which we know the installation directory of the fortran module files
    SET(HDF5_INCLUDE_DIR       ${LIBS_HDF5_DIR}/include)
    SET(HDF5_DIFF_EXECUTABLE   ${LIBS_HDF5_DIR}/bin/h5diff)
    # WARNING: The order of the following libraries matters! They need to be listed from the most dependent to the least dependent.
    SET(HDF5_LIBRARIES         ${LIBS_HDF5_DIR}/lib/libhdf5_fortran.a ${LIBS_HDF5_DIR}/lib/libhdf5_f90cstub.a ${LIBS_HDF5_DIR}/lib/libhdf5_hl_fortran.a ${LIBS_HDF5_DIR}/lib/libhdf5_hl_f90cstub.a ${LIBS_HDF5_DIR}/lib/libhdf5_hl.a ${LIBS_HDF5_DIR}/lib/libhdf5.a ${LIBS_HDF5_DIR}/lib/libhdf5_tools.a)
  ENDIF()

  # Set build status to self-built
  SET(HDF5_BUILD_STATUS "self-built")
ENDIF()

# HDF5 1.14 references build directory
# > https://github.com/HDFGroup/hdf5/issues/2422
IF(HDF5_VERSION VERSION_EQUAL "1.14")
  LIST(FILTER HDF5_INCLUDE_DIR EXCLUDE REGEX "src/H5FDsubfiling")
ENDIF()

# Actually add the HDF5 paths (system/self-built) to the linking paths, including the library containing dlopen/dlclose (usually -ldl on UNIX machines)
# > INFO: We could also use the HDF5::HDF5/hdf5::hdf5/hdf5::hdf5_fortran targets here but they are not set before compiling self-built HDF5
INCLUDE_DIRECTORIES(BEFORE ${HDF5_INCLUDE_DIR})
LIST(PREPEND linkedlibs ${HDF5_LIBRARIES} ${CMAKE_DL_LIBS})
IF(${HDF5_IS_PARALLEL})
  MESSAGE(STATUS "Compiling with ${HDF5_BUILD_STATUS} [HDF5] (v${HDF5_VERSION}) with parallel support ${HDF5_MPI_VERSION}")
ELSE()
  MESSAGE(STATUS "Compiling with ${HDF5_BUILD_STATUS} [HDF5] (v${HDF5_VERSION}) without parallel support")
ENDIF()

# Hide all the HDF5 libs paths
MARK_AS_ADVANCED(FORCE HDF5_DIR)
MARK_AS_ADVANCED(FORCE HDF5_C_INCLUDE_DIR)
MARK_AS_ADVANCED(FORCE HDF5_DIFF_EXECUTABLE)
MARK_AS_ADVANCED(FORCE HDF5_Fortran_INCLUDE_DIR)
MARK_AS_ADVANCED(FORCE HDF5_C_LIBRARY_dl)
MARK_AS_ADVANCED(FORCE HDF5_C_LIBRARY_hdf5)
MARK_AS_ADVANCED(FORCE HDF5_C_LIBRARY_m)
MARK_AS_ADVANCED(FORCE HDF5_C_LIBRARY_sz)
MARK_AS_ADVANCED(FORCE HDF5_C_LIBRARY_z)
MARK_AS_ADVANCED(FORCE HDF5_Fortran_LIBRARY_dl)
MARK_AS_ADVANCED(FORCE HDF5_Fortran_LIBRARY_hdf5)
MARK_AS_ADVANCED(FORCE HDF5_Fortran_LIBRARY_hdf5_fortran)
MARK_AS_ADVANCED(FORCE HDF5_Fortran_LIBRARY_m)
MARK_AS_ADVANCED(FORCE HDF5_Fortran_LIBRARY_sz)
MARK_AS_ADVANCED(FORCE HDF5_Fortran_LIBRARY_z)
MARK_AS_ADVANCED(FORCE HDF5_hdf5_LIBRARY_hdf5)
MARK_AS_ADVANCED(FORCE HDF5_hdf5_LIBRARY_RELEASE)
MARK_AS_ADVANCED(FORCE HDF5_Fortran_LIBRARY_hdf5_fortran)
MARK_AS_ADVANCED(FORCE HDF5_Fortran_LIBRARY_hdf5_fortran_RELEASE)

# Restore the original PATH - only if it has been modified above, i.e. ORIGINAL_PATH_ENV is actually defined
IF(NOT "${HDF5_COMPILER}" STREQUAL "" AND NOT "${HDF5_COMPILER}" STREQUAL "HDF5_COMPILER-NOTFOUND")
  SET(ENV{PATH} "${ORIGINAL_PATH_ENV}")
ENDIF()


# =========================================================================
# Math libary
# =========================================================================
# Try to find system LAPACK/OpenBLAS
IF (NOT LIBS_BUILD_MATH_LIB)
  FIND_PACKAGE(LAPACK QUIET)
ENDIF()

IF (LAPACK_FOUND)
  MESSAGE (STATUS "[BLAS/Lapack] found in system libraries")
  SET(LIBS_BUILD_MATH_LIB OFF CACHE BOOL "Compile and build math library")
ELSE()
  MESSAGE (STATUS "[BLAS/Lapack] not found in system libraries")
  SET(LIBS_BUILD_MATH_LIB ON  CACHE BOOL "Compile and build math library")
ENDIF()

# Use system LAPACK/MKL
IF(NOT LIBS_BUILD_MATH_LIB)
  # If library is specifically requested, it is required
  FIND_PACKAGE(LAPACK REQUIRED)
  IF (LAPACK_FOUND)
    LIST(APPEND linkedlibs ${LAPACK_LIBRARIES})
    MESSAGE(STATUS "Compiling with system [BLAS/Lapack]")
  ENDIF()

# Build LAPACK/OpenBLAS in HOPR
ELSE()
  # Offer LAPACK and OpenBLAS
  SET (LIBS_BUILD_MATH_LIB_VENDOR LAPACK CACHE STRING "Choose the type of math lib vendor, options are: LAPACK, OpenBLAS.")
  SET_PROPERTY(CACHE LIBS_BUILD_MATH_LIB_VENDOR PROPERTY STRINGS LAPACK OpenBLAS)

  # Build LAPACK
  IF (LIBS_BUILD_MATH_LIB_VENDOR STREQUAL "LAPACK")
    # Origin pointing to Github
    IF("${GIT_ORIGIN}" MATCHES ".github.com")
      SET (MATHLIB_DOWNLOAD "https://github.com/Reference-LAPACK/lapack.git")
    ELSE()
      SET (MATHLIB_DOWNLOAD ${LIBS_DLPATH}libs/lapack.git)
    ENDIF()
    SET (MATH_LIB_DOWNLOAD ${MATHLIB_DOWNLOAD} CACHE STRING "LAPACK Download-link" FORCE)
    SET (MATH_LIB_TAG "v3.12.1")
    MARK_AS_ADVANCED(FORCE MATH_LIB_DOWNLOAD)
    MARK_AS_ADVANCED(FORCE MATH_LIB_TAG)
  # Build OpenBLAS
  ELSEIF (LIBS_BUILD_MATH_LIB_VENDOR STREQUAL "OpenBLAS")
    IF("${GIT_ORIGIN}" MATCHES ".github.com")
      SET (MATHLIB_DOWNLOAD "https://github.com/OpenMathLib/OpenBLAS.git")
    ELSE()
      SET (MATHLIB_DOWNLOAD ${LIBS_DLPATH}libs/OpenBLAS.git)
    ENDIF()
    SET (MATH_LIB_DOWNLOAD ${MATHLIB_DOWNLOAD} CACHE STRING "OpenBLAS Download-link" FORCE)
    SET (MATH_LIB_TAG "v0.3.29")
    MARK_AS_ADVANCED(FORCE MATH_LIB_DOWNLOAD)
    MARK_AS_ADVANCED(FORCE MATH_LIB_TAG)
  # Unknown math lib vendor
  ELSE()
    MESSAGE(FATAL_ERROR "Unknown math lib vendor")
  ENDIF()

  # Set math libs build dir
  SET(LIBS_MATH_DIR  ${LIBS_EXTERNAL_LIB_DIR}/${LIBS_BUILD_MATH_LIB_VENDOR})

  # Set parallel build with maximum number of threads
  INCLUDE(ProcessorCount)
  PROCESSORCOUNT(N)

  IF (LIBS_BUILD_MATH_LIB_VENDOR STREQUAL "LAPACK")
    # Check if math lib was already built
    IF (NOT EXISTS "${LIBS_MATH_DIR}/lib/liblapack.a")
      # Let CMake take care of download, configure and build
      EXTERNALPROJECT_ADD(${LIBS_BUILD_MATH_LIB_VENDOR}
        GIT_REPOSITORY     ${MATH_LIB_DOWNLOAD}
        GIT_TAG            ${MATH_LIB_TAG}
        GIT_PROGRESS       TRUE
        ${${GITSHALLOW}}
        PREFIX             ${LIBS_MATH_DIR}
        UPDATE_COMMAND     ""
        # LAPACK explicitely needs "make" to configure
        CMAKE_GENERATOR    "Unix Makefiles"
        BUILD_COMMAND      make -j${N}
        # Set the CMake arguments for LAPACK
        CMAKE_ARGS         -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_INSTALL_PREFIX=${LIBS_MATH_DIR} -DBLAS++=OFF -DLAPACK++=OFF -DBUILD_SHARED_LIBS=OFF -DCBLAS=OFF -DLAPACKE=OFF -DBUILD_TESTING=OFF
        # Set the build byproducts
        BUILD_BYPRODUCTS   ${LIBS_MATH_DIR}/lib/liblapack.a ${LIBS_MATH_DIR}/lib/libblas.a
      )

      LIST(APPEND SELFBUILTEXTERNALS ${LIBS_BUILD_MATH_LIB_VENDOR})
    ENDIF()
  ELSEIF (LIBS_BUILD_MATH_LIB_VENDOR STREQUAL "OpenBLAS")
    # Check if math lib was already built
    IF (NOT EXISTS "${LIBS_MATH_DIR}/lib/libopenblas.a")
      # Let CMake take care of download, configure and build
      EXTERNALPROJECT_ADD(${LIBS_BUILD_MATH_LIB_VENDOR}
        GIT_REPOSITORY     ${MATH_LIB_DOWNLOAD}
        GIT_TAG            ${MATH_LIB_TAG}
        GIT_PROGRESS       TRUE
        ${${GITSHALLOW}}
        PREFIX             ${LIBS_MATH_DIR}
        UPDATE_COMMAND     ""
        # OpenBLAS explicitely needs "make" to configure
        CMAKE_GENERATOR    "Unix Makefiles"
        BUILD_COMMAND      make -j${N}
        # Set the CMake arguments for LAPACK
        CMAKE_ARGS         -DBUILD_STATIC_LIBS=ON -DBUILD_TESTING=OFF
        # Set the CMake arguments for OpenBLAS
        BUILD_BYPRODUCTS   ${LIBS_MATH_DIR}/src/${LIBS_BUILD_MATH_LIB_VENDOR}/lib/libopenblas.a
        BUILD_IN_SOURCE    TRUE
        INSTALL_COMMAND    ""
      )

      LIST(APPEND SELFBUILTEXTERNALS ${LIBS_BUILD_MATH_LIB_VENDOR})
    ENDIF()
  ENDIF()

  IF (LIBS_BUILD_MATH_LIB_VENDOR STREQUAL "LAPACK")
    # Set math lib paths
    UNSET(MATH_LIB_LIBRARIES)
    SET(MATH_LIB_LIBRARIES              ${LIBS_MATH_DIR}/lib)

    UNSET(LAPACK_LIBRARY)
    UNSET(BLAS_LIBRARY)
    UNSET(LAPACK_LIBRARIES)

    SET(LAPACK_LIBRARY                  ${MATH_LIB_LIBRARIES}/liblapack.a)
    SET(BLAS_LIBRARY                    ${MATH_LIB_LIBRARIES}/libblas.a)
    SET(LAPACK_LIBRARIES                ${LAPACK_LIBRARY}${BLAS_LIBRARY})

    # Actually add the math lib paths to the linking paths
    INCLUDE_DIRECTORIES (${MATH_LIB_LIBRARIES})
    LIST(APPEND linkedlibs ${LAPACK_LIBRARY} ${BLAS_LIBRARY})
    MESSAGE(STATUS "Compiling with self-built [LAPACK]")
  ELSEIF (LIBS_BUILD_MATH_LIB_VENDOR STREQUAL "OpenBLAS")
    # Set math lib paths
    SET(MATH_LIB_LIBRARIES              ${LIBS_MATH_DIR}/src/${LIBS_BUILD_MATH_LIB_VENDOR}/lib)

    UNSET(LAPACK_LIBRARY)
    UNSET(BLAS_LIBRARY)
    UNSET(LAPACK_LIBRARIES)

    SET(LAPACK_LIBRARY                  ${MATH_LIB_LIBRARIES}/libopenblas.a)
    SET(LAPACK_LIBRARIES                ${LAPACK_LIBRARY}${BLAS_LIBRARY})

    # Actually add the math lib paths to the linking paths
    INCLUDE_DIRECTORIES (${MATH_LIB_LIBRARIES})
    LIST(APPEND linkedlibs ${LAPACK_LIBRARY} ${BLAS_LIBRARY})
    MESSAGE(STATUS "Compiling with self-built [OpenBLAS]")
  ENDIF()
ENDIF()


# =========================================================================
# CGNS library
# =========================================================================
# Try to find system LAPACK/OpenBLAS
OPTION(LIBS_USE_CGNS "Switch for using cgns as a library (needed for input/output of CGNS files)" ON)

IF (NOT LIBS_USE_CGNS)
  UNSET(LIBS_BUILD_CGNS     CACHE)
  UNSET(LIBS_BUILD_CGNS_INT CACHE)
  UNSET(LIBS_BUILD_CGNS_TAG CACHE)
ELSE()
  ADD_COMPILE_DEFINITIONS(PP_USE_CGNS=${LIBS_USE_CGNS})

  # Try to find system CGNS
  IF (NOT LIBS_BUILD_CGNS)
    FIND_PACKAGE(CGNS QUIET)
  ENDIF()

  IF(CGNS_FOUND)
    MESSAGE (STATUS "[CGNS] found in system libraries")
    SET(LIBS_BUILD_CGNS OFF CACHE BOOL "Compile and build CGNS library")
  ELSE()
    MESSAGE (STATUS "[CGNS] not found in system libraries")
    SET(LIBS_BUILD_CGNS ON  CACHE BOOL "Compile and build CGNS library")
  ENDIF()

  # Use system CGNS
  IF (NOT LIBS_BUILD_CGNS)
    # If library is specifically requested, it is required
    FIND_PACKAGE(CGNS REQUIRED)

    # Check the integer type
    # INCLUDE(CheckCSourceCompiles)
    #
    # SET(CGNS_64BIT_CHECK "
# #include <cgnslib.h>
# #if CG_BUILD_64BIT
#     int main(){return 1;}
# #else
#     int main(){return 0;}
# #endif
#     ")

    # CHECK_C_SOURCE_COMPILES("${CGNS_64BIT_CHECK}" CGNS_IS_64BIT)
    # IF (CGNS_IS_64BIT)
    #   ADD_COMPILE_DEFINITIONS(PP_CGNS_INT=64)
    # ELSE()
      ADD_COMPILE_DEFINITIONS(PP_CGNS_INT=32)
    # ENDIF()

    # Set build status to system
    SET(CGNS_BUILD_STATUS "system")
  ELSE()
    MESSAGE(STATUS "Setting [CGNS] to self-build")
    # Origin pointing to Github
    IF("${GIT_ORIGIN}" MATCHES ".github.com")
      SET (CGNSDOWNLOAD "https://github.com/CGNS/CGNS.git")
    ELSE()
      SET (CGNSDOWNLOAD ${LIBS_DLPATH}libs/cgns.git )
    ENDIF()
    SET(CGNS_DOWNLOAD ${CGNSDOWNLOAD} CACHE STRING "CGNS Download-link")
    MESSAGE(STATUS "Setting [CGNS] download link: ${CGNSDOWNLOAD}")
    MARK_AS_ADVANCED(FORCE CGNS_DOWNLOAD)

    # Offer CGNS Integer Type
    SET(LIBS_BUILD_CGNS_INT "64" CACHE STRING "Integer type in CGNS lib" FORCE)
    SET_PROPERTY(CACHE LIBS_BUILD_CGNS_INT PROPERTY STRINGS 32 64)
    ADD_COMPILE_DEFINITIONS(PP_CGNS_INT=${LIBS_BUILD_CGNS_INT})

    # Set CGNS_Tag
    # > Current version 4.5.0 has a regression, so stick with 4.4.0
    SET(CGNS_VERSION "4.4.0")
    SET(CGNS_TAG     "v${CGNS_VERSION}")
    SET(CGNS_STR     ${CGNS_TAG})
    MARK_AS_ADVANCED(FORCE CGNS_TAG)
    MARK_AS_ADVANCED(FORCE CGNS_VERSION)

    # Set CGNS build dir
    SET(LIBS_CGNS_DIR ${LIBS_EXTERNAL_LIB_DIR}/CGNS)

    # Check if CGNS was already built
    UNSET(CGNS_FOUND)
    UNSET(CGNS_VERSION)
    UNSET(CGNS_INCLUDE_DIR)
    UNSET(CGNS_LIBRARIES)
    FIND_PACKAGE(CGNS QUIET PATHS ${LIBS_CGNS_DIR} NO_DEFAULT_PATH)

    IF(CGNS_FOUND)
      # If re-running CMake, it might wrongly pick-up the system HDF5
      IF(NOT EXISTS ${LIBS_CGNS_DIR}/lib/libcgns.a)
      # IF (NOT EXISTS "${LIBS_CGNS_DIR}/lib/libcgns.a")
        UNSET(CGNS_FOUND)
        SET(CGNS_VERSION     ${CGNS_STR})
      ENDIF()
    ENDIF()

    # Check again if CGNS was found
    IF(NOT CGNS_FOUND)
      SET(CGNS_VERSION     ${CGNS_STR})
      # 64 Bit support should be enabled by default
      STRING(COMPARE EQUAL ${LIBS_BUILD_CGNS_INT} "64" LIBS_CGNS_64BIT)

      # Fallback for disabling HDF5 for CGNS compilation
      # OPTION(LIBS_BUILD_CGNS_ENABLE_HDF5 "Build CGNS library with -DCGNS_ENABLE_HDF5=ON" ON)
      # MESSAGE(STATUS "Build CGNS library with -DCGNS_ENABLE_HDF5=" ${LIBS_BUILD_CGNS_ENABLE_HDF5})

      # Set parallel build with maximum number of threads
      INCLUDE(ProcessorCount)
      PROCESSORCOUNT(N)

      IF("${CMAKE_GENERATOR}" STREQUAL "Ninja")
        SET(BUILD_COMMAND ninja)
      ELSEIF("${CMAKE_GENERATOR}" STREQUAL "Unix Makefiles")
        SET(BUILD_COMMAND make)
      ELSE()
        MESSAGE(FATAL_ERROR "Unknown build system")
      ENDIF()

      # Let CMake take care of download, configure and build
      EXTERNALPROJECT_ADD(cgns
        GIT_REPOSITORY     ${CGNS_DOWNLOAD}
        GIT_TAG            ${CGNS_TAG}
        GIT_PROGRESS       TRUE
        ${${GITSHALLOW}}
        PREFIX             ${LIBS_CGNS_DIR}
        # Set parallel build with maximum number of threads
        BUILD_COMMAND      ${BUILD_COMMAND} -j${N}
        # Set the CMake arguments for CGNS
        CMAKE_ARGS         -DCMAKE_INSTALL_PREFIX=${LIBS_CGNS_DIR} -DCMAKE_PREFIX_PATH=${LIBS_HDF5_DIR} -DCGNS_ENABLE_FORTRAN=ON -DCGNS_ENABLE_64BIT=${LIBS_CGNS_64BIT} -DCGNS_BUILD_SHARED=OFF -DCGNS_USE_SHARED=OFF -DCMAKE_BUILD_TYPE=Release -DCGNS_BUILD_CGNSTOOLS=OFF -DCGNS_ENABLE_HDF5=ON -DHDF5_NEED_MPI=${HDF5_IS_PARALLEL} -DCGNS_ENABLE_PARALLEL=OFF -DCGNS_ENABLE_TESTS=OFF -DCMAKE_SKIP_RPATH=ON
        # Set the build byproducts
        BUILD_BYPRODUCTS   ${LIBS_CGNS_DIR}/lib/libcgns.a
      )

      # Add CMake CGNS to the list of self-built externals
      LIST(APPEND SELFBUILTEXTERNALS cgns)

      # If HDF5 is built in HOPR, it must occur before the CGNS compilation (for the support of HDF5-based CGNS files)
      IF(LIBS_BUILD_HDF5)
        IF (NOT EXISTS "${LIBS_HDF5_DIR}/lib/libhdf5.a")
          ADD_DEPENDENCIES(cgns HDF5)
        ENDIF()
      ENDIF()
    ENDIF()

    # Set CGNS paths
    SET(CGNS_INCLUDE_DIR       ${LIBS_CGNS_DIR}/include)
    SET(CGNS_LIBRARIES         ${LIBS_CGNS_DIR}/lib/libcgns.a)

    # Set build status to self-built
    SET(CGNS_BUILD_STATUS "self-built")
  ENDIF()

  # Set pre-processor flag for CGNS version
  STRING(REPLACE "v" ""  CGNS_VERSION ${CGNS_VERSION})
  # Warn about version 4.5.0
  IF("${CGNS_VERSION}" VERSION_EQUAL "4.5.0")
    MESSAGE(WARNING "CGNS ${CGNS_VERSION} has issues with reading surface meshes. Consider downgrading to v4.4.0")
  ENDIF()
  MESSAGE(STATUS "Compiling with ${CGNS_BUILD_STATUS} [CGNS] (v${CGNS_VERSION})")

  STRING(REPLACE "." ";" VERSION_LIST ${CGNS_VERSION})
  # Ensure at least three elements (major, minor, patch)
  LIST(APPEND VERSION_LIST 0 0)
  # Access elements of the list
  LIST(GET VERSION_LIST 0 MAJOR)
  LIST(GET VERSION_LIST 1 MINOR)
  LIST(GET VERSION_LIST 2 PATCH)

  # Convert to version format
  MATH(EXPR CGNS_VERSION "${MAJOR} * 1000 + ${MINOR} * 10 + ${PATCH}")
  UNSET(VERSION_LIST)
  UNSET(MAJOR)
  UNSET(MINOR)
  UNSET(PATCH)
  ADD_COMPILE_DEFINITIONS(PP_CGNS_VERSION=${CGNS_VERSION})

  # Actually add the CGNS paths (system/self-built) to the linking paths
  INCLUDE_DIRECTORIES(BEFORE ${CGNS_INCLUDE_DIR})
  LIST(PREPEND linkedlibs ${CGNS_LIBRARIES} )
ENDIF()
