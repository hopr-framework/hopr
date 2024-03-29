# =========================================================================
# HOPR CMake configuration
# =========================================================================
# NOTE: use minimum CMake version required by tools/libs (Paraview, HDF5, FFTW)
CMAKE_MINIMUM_REQUIRED(VERSION 3.5.2)
# Policies (see https://cmake.org/cmake/help/v3.13/manual/cmake-policies.7.html)
# VERSION_GREATER_EQUAL is incompatible with CMAKE versions < 3.7
IF(NOT(${CMAKE_VERSION} VERSION_LESS "3.6.0"))
  SET(GITSHALLOW "GIT_SHALLOW ON")
ENDIF()
IF(NOT(${CMAKE_VERSION} VERSION_LESS "3.12.0"))
  CMAKE_POLICY(SET CMP0074 NEW)
ENDIF()
# Folder for custom CMake source files
LIST(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
SET_PROPERTY(GLOBAL PROPERTY USE_FOLDERS ON)
INCLUDE(ExternalProject)
INCLUDE(CMakeDependentOption)

# =========================================================================
# Git configuration
# =========================================================================
IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git)
  # Setup git hooks
  SET(PRECOMMIT_FILE ".githooks/pre-commit")

  # Check if the pre-commit hooks exits
  IF (NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git/hooks/pre-commit)
    # Create otherwise
    EXECUTE_PROCESS(COMMAND mkdir -p ${CMAKE_CURRENT_SOURCE_DIR}/.git/hooks)
    EXECUTE_PROCESS(COMMAND ln -s ${CMAKE_CURRENT_SOURCE_DIR}/${PRECOMMIT_FILE} ${CMAKE_CURRENT_SOURCE_DIR}/.git/hooks/pre-commit)
  ELSE()
    # Check if the hook is the correct symlink and warn otherwise
    EXECUTE_PROCESS(COMMAND readlink ${CMAKE_CURRENT_SOURCE_DIR}/.git/hooks/pre-commit OUTPUT_VARIABLE PRECOMMIT_LINK OUTPUT_STRIP_TRAILING_WHITESPACE)
    IF (NOT ${PRECOMMIT_LINK} MATCHES "${CMAKE_CURRENT_SOURCE_DIR}/${PRECOMMIT_FILE}")
      MESSAGE (WARNING "Custom git pre-commit hook detected. Please ensure to call ${PRECOMMIT_FILE} manually.")
    ENDIF()
  ENDIF()

  # Check if the hook actually gets loaded
  EXECUTE_PROCESS(COMMAND git config --get core.hooksPath OUTPUT_VARIABLE HOOKSPATH OUTPUT_STRIP_TRAILING_WHITESPACE)
  IF (DEFINED HOOKSPATH  AND NOT "${HOOKSPATH}" STREQUAL "" AND NOT "${HOOKSPATH}" STREQUAL ".git/hooks")
    # STRING(ASCII 27 ESCAPE)
    # MESSAGE (STATUS "${ESCAPE}[34mCustom hooks path detected. Please ensure to call ${PRECOMMIT_FILE} manually.${ESCAPE}[0m")
    MESSAGE (WARNING "Custom git hooks path detected. Please ensure to call ${PRECOMMIT_FILE} manually.")
  ENDIF()
ELSE()
  MESSAGE(STATUS "git not found")
ENDIF()

# =========================================================================
# Check gold support
# =========================================================================
EXECUTE_PROCESS(COMMAND ld.gold --version COMMAND grep "^GNU gold" COMMAND sed "s/^.* //g" OUTPUT_VARIABLE GNU_GOLD_VERSION)
IF (DEFINED GNU_GOLD_VERSION  AND NOT "${GNU_GOLD_VERSION}" STREQUAL "")
  STRING(STRIP "${GNU_GOLD_VERSION}" GNU_GOLD_VERSION)
  MESSAGE(STATUS "Setting linker to gold v${GNU_GOLD_VERSION}")
  # Shift responsibility of driving the final stages of compilation from collect2 to gold via the linker plugin
  # More information at: https://gcc.gnu.org/wiki/LinkTimeOptimization
  IF(CMAKE_VERSION VERSION_GREATER_EQUAL 3.13)
    ADD_LINK_OPTIONS("-fuse-ld=gold")
    # Make it abundantly clear we want to use gold
    FIND_PROGRAM(CMAKE_GOLD_LINKER NAMES ${_CMAKE_TOOLCHAIN_PREFIX}ld.gold${_CMAKE_TOOLCHAIN_SUFFIX} HINTS ${_CMAKE_TOOLCHAIN_LOCATION})
    SET (CMAKE_LINKER "${CMAKE_GOLD_LINKER}" CACHE FILEPATH "" FORCE)
    MARK_AS_ADVANCED(FORCE CMAKE_GOLD_LINKER)
  ELSE()
    SET (CMAKE_EXE_LINKER_FLAGS    "${CMAKE_EXE_LINKER_FLAGS}    -fuse-ld=gold")
    SET (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -fuse-ld=gold")
    # This currently breaks ar (binutils archiver)
    # SET (CMAKE_STATIC_LINKER_FLAGS "${CMAKE_STATIC_LINKER_FLAGS} -fuse-ld=gold")
    SET (CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} -fuse-ld=gold")
    # Make it abundantly clear we want to use gold
    FIND_PROGRAM(CMAKE_GOLD_LINKER NAMES ${_CMAKE_TOOLCHAIN_PREFIX}ld.gold${_CMAKE_TOOLCHAIN_SUFFIX} HINTS ${_CMAKE_TOOLCHAIN_LOCATION})
    SET (CMAKE_LINKER "${CMAKE_GOLD_LINKER}" CACHE FILEPATH "" FORCE)
    MARK_AS_ADVANCED(FORCE CMAKE_GOLD_LINKER)
  ENDIF()
ENDIF()

# =========================================================================
# Machine environment
# =========================================================================
INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/CMakeListsMachine.txt)

# =========================================================================
# Project definitions and output paths
# =========================================================================
PROJECT(Hopr)

# =========================================================================
# Check IPO support:
# =========================================================================
# we need to have languages enabled and compilers defined for this
IF(NOT(${CMAKE_VERSION} VERSION_LESS "3.9.0"))
  CMAKE_POLICY(SET CMP0069 NEW)
  INCLUDE(CheckIPOSupported)
  CHECK_IPO_SUPPORTED(RESULT HASIPO OUTPUT error)
ELSE()
  SET(HASIPO FALSE)
ENDIF()

# =========================================================================
# Output paths
# =========================================================================
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin)
SET(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)
SET(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)

# Set custom install dir (needs to be done after project has been defined!)
# MARK_AS_ADVANCED(FORCE CMAKE_INSTALL_PREFIX)
IF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  SET(CMAKE_INSTALL_PREFIX "${CMAKE_CURRENT_SOURCE_DIR}" CACHE PATH "Prefix prepended to install directories" FORCE)
ENDIF()
MARK_AS_ADVANCED(CMAKE_INSTALL_PREFIX)

# =========================================================================
# Build type
# =========================================================================
# make sure that the default is a RELEASE
IF (NOT CMAKE_BUILD_TYPE)
  SET (CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build, options are: Debug, Release, Profile, Sanitize (only GNU))." FORCE)
   IF (CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
     SET_PROPERTY(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Debug Release Profile Sanitize)
   ELSEIF (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
     SET_PROPERTY(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Debug Release Profile)
   ELSEIF (CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
     SET_PROPERTY(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Debug Release Profile)
   ENDIF()
ENDIF (NOT CMAKE_BUILD_TYPE)


STRING(TOLOWER ${CMAKE_BUILD_TYPE} BUILD_TYPE_LC)
IF (NOT BUILD_TYPE_LC MATCHES "debug" AND NOT BUILD_TYPE_LC MATCHES "sanitize" AND HASIPO)
  # enable IPO globally (IPO branding: Intel => IPO, GNU => LTO, PGI => IPA)
  IF (CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    # Check the GCC wrapper for the complete toolchain
    FIND_PROGRAM(CMAKE_GCC_AR     NAMES ${_CMAKE_TOOLCHAIN_PREFIX}gcc-ar${_CMAKE_TOOLCHAIN_SUFFIX} HINTS ${_CMAKE_TOOLCHAIN_LOCATION})
    FIND_PROGRAM(CMAKE_GCC_NM     NAMES ${_CMAKE_TOOLCHAIN_PREFIX}gcc-nm                           HINTS ${_CMAKE_TOOLCHAIN_LOCATION})
    FIND_PROGRAM(CMAKE_GCC_RANLIB NAMES ${_CMAKE_TOOLCHAIN_PREFIX}gcc-ranlib                       HINTS ${_CMAKE_TOOLCHAIN_LOCATION})
    MARK_AS_ADVANCED(FORCE CMAKE_GCC_AR)
    MARK_AS_ADVANCED(FORCE CMAKE_GCC_NM)
    MARK_AS_ADVANCED(FORCE CMAKE_GCC_RANLIB)
    # Do not use the standard CMake LTO option for GNU (-flto -fno-fat-lto-objects), as it does not allow speed-up during linking
    IF( CMAKE_GCC_AR AND CMAKE_GCC_NM AND CMAKE_GCC_RANLIB )
      MESSAGE(STATUS "Found GCC binutils wrappers for LTO. Enabling LTO linker plugin.")
      # Do not use the standard CMake LTO option for GNU (-flto -fno-fat-lto-objects), as it does not allow speed-up during linking
      SET(CMAKE_INTERPROCEDURAL_OPTIMIZATION FALSE)
      # Static libraries require either fat LTO objects (increases compilation time) or the use of linker plugins (per default enabled); the jobserver option reduces linking time
      # More information at: https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html#Optimize-Options
      IF (CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10.0)
        # GCC10 introduced the -flto=auto option, https://gcc.gnu.org/legacy-ml/gcc-patches/2019-07/msg01488.html
        SET(CMAKE_Fortran_FLAGS  "${CMAKE_Fortran_FLAGS} -flto=auto -fuse-linker-plugin")
      ELSE()
        # Otherwise we must rely on hardcoded GCC jobserver. If it is not available, we will get warnings in the console but it still works, albeit in serial mode
        SET(CMAKE_Fortran_FLAGS  "${CMAKE_Fortran_FLAGS} -flto=jobserver -fuse-linker-plugin")
      ENDIF()
      # Use the GCC wrapper for the complete toolchain to provide the path to the linker plugin (this might be the problem with using the CMAKE IPO)
      SET(CMAKE_AR     "${CMAKE_GCC_AR}"     CACHE FILEPATH "" FORCE)
      SET(CMAKE_NM     "${CMAKE_GCC_NM}"     CACHE FILEPATH "" FORCE)
      SET(CMAKE_RANLIB "${CMAKE_GCC_RANLIB}" CACHE FILEPATH "" FORCE)
      MARK_AS_ADVANCED(FORCE CMAKE_AR)
      MARK_AS_ADVANCED(FORCE CMAKE_NM)
      MARK_AS_ADVANCED(FORCE CMAKE_RANLIB)
    ELSE()
      MESSAGE(WARNING "GCC indicates LTO support, but binutils wrappers could not be found. Disabling LTO linker plugin." )
      SET(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)  # enable IPO globally
    ENDIF()
  ELSE()
    SET(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)  # enable IPO globally
  ENDIF()
ENDIF()

# =========================================================================
# Location of binary and filenames
# =========================================================================
# append relative filename-macro for __FILENAME__ in Stamp of abort function (see flexi.h)
# subst replacement is GNU make specific
IF("${CMAKE_GENERATOR}" MATCHES "Unix Makefiles")
  SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -D__FILENAME__='\"$(subst ${CMAKE_SOURCE_DIR}/,,$(abspath $<))\"'")
ENDIF()

# add basedir for location of corresponding userblock-file
ADD_DEFINITIONS("-DBASEDIR='\"${CMAKE_CURRENT_BINARY_DIR}/\"'")

# =========================================================================
# USERBLOCK + Preproc_flags
# =========================================================================
# A function to get all user defined variables with a specified prefix
function (getListOfVarsStartingWith _prefix _varResult)
    GET_CMAKE_PROPERTY(_vars CACHE_VARIABLES)
    STRING(REGEX MATCHALL "(^|;)${_prefix}[A-Za-z0-9_]*" _matchedVars "${_vars}")
    SET(${_varResult} ${_matchedVars} PARENT_SCOPE)
endfunction()

# A function to get all user defined variables with a specified prefix
SET(configuration ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/configuration.cmake)
FILE(WRITE ${configuration} "")
GETLISTOFVARSSTARTINGWITH("HOPR_" hoprVars)
FOREACH (_var IN LISTS hoprVars)
  GET_PROPERTY(currentHelpString CACHE "${_var}" PROPERTY HELPSTRING)
  SET(boolian (${${_var}} STREQUAL "ON" OR ${${_var}} STREQUAL "OFF"))
  IF(${boolian})
    FILE(APPEND ${configuration} "OPTION(${_var} \"${currentHelpString}\" ${${_var}})\n")
  ELSE()
    IF(${_var})
      STRING(REPLACE "\\" "\\\\" ${_var} ${${_var}})
    ENDIF()
    FILE(APPEND ${configuration} "SET(${_var} \"${${_var}}\" CACHE STRING \"${currentHelpString}\")\n")
  ENDIF()
ENDFOREACH()

# =========================================================================
# ADD LIBRARIES
# =========================================================================
INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/CMakeListsLib.txt)

# =========================================================================
# HOPR
# =========================================================================
INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/src/CMakeLists.txt)

# =========================================================================
# Store the current build information
# =========================================================================
SET(buildinfo ${CMAKE_CURRENT_SOURCE_DIR}/src/buildinfo.h)
FILE(WRITE ${buildinfo} "! Define the current build information. The default must remain empty, i.e., ''. Do not commit the changed file!\n#define GIT_CURRENT_COMMIT  ''\n#define BUILD_DATE          ''\n#define BUILD_VERSION_GCC   ''\n#define BUILD_VERSION_MPI   ''\n")
IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git)
  EXECUTE_PROCESS(COMMAND git rev-parse --short HEAD OUTPUT_VARIABLE GIT_COMMIT OUTPUT_STRIP_TRAILING_WHITESPACE)
  # MESSAGE(STATUS "Current git commit ${GIT_COMMIT} will be written to ${buildinfo}")
  EXECUTE_PROCESS(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/tools/cmake/setBuildInfo.sh ${buildinfo} ${GIT_COMMIT} ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION} ${LIBS_MPI_NAME} ${MPI_C_LIBRARY_VERSION})
ENDIF()
