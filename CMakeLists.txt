# =========================================================================
# HOPR information
# =========================================================================
SET(PROJECT_NAME "HOPR")
SET(PROJECT_DESC "HOPR (High Order Preprocessor) is an open-source software for the generation of three-dimensional unstructured high-order meshes")
SET(PROJECT_VER  "1.5.0")
SET(PROJECT_URL  "https://hopr.readthedocs.io/")

# =========================================================================
# HOPR CMake configuration
# =========================================================================
CMAKE_MINIMUM_REQUIRED(VERSION 3.20.0)

MESSAGE(STATUS "This is ${PROJECT_NAME} (v${PROJECT_VER})")
MESSAGE(STATUS "Generating with [cmake] (v${CMAKE_VERSION})")

# Policies (see https://cmake.org/cmake/help/v3.13/manual/cmake-policies.7.html)
SET(GITSHALLOW "GIT_SHALLOW ON")
CMAKE_POLICY(SET CMP0074 NEW)
CMAKE_POLICY(SET CMP0079 NEW)

# =========================================================================
# Load CMake modules
# =========================================================================
# Directory for custom CMake source files
LIST(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
SET_PROPERTY(GLOBAL PROPERTY USE_FOLDERS ON)
INCLUDE(SetGitHook)

# Load CMake internal extensions
INCLUDE(CMakeDependentOption)
INCLUDE(ExternalProject)

# =========================================================================
# Machine environment
# =========================================================================

INCLUDE(SetLinker)
# Languages are specified in CMakeListMachine.txt as Hawk requires them
# after specifiying CMAKE_COMPILER
INCLUDE(SetMachine)

PROJECT(Hopr
        VERSION      ${PROJECT_VER}
        DESCRIPTION  ${PROJECT_DESC}
        HOMEPAGE_URL ${PROJECT_URL})

INCLUDE(SetCompiler)
# Load CMake internal extensions
# > must be loaded after target architecture is known
INCLUDE(GNUInstallDirs)

# =========================================================================
# Store the current commit information
# =========================================================================
SET(buildinfo "${CMAKE_CURRENT_SOURCE_DIR}/src/buildinfo.h")

# Write an inline CMake update script to a temporary file.
FILE(WRITE "${CMAKE_CURRENT_BINARY_DIR}/update_buildinfo.cmake"
"EXECUTE_PROCESS(
  COMMAND git rev-parse --short HEAD
  WORKING_DIRECTORY \"${CMAKE_CURRENT_SOURCE_DIR}\"
  OUTPUT_VARIABLE GIT_COMMIT
  OUTPUT_STRIP_TRAILING_WHITESPACE
  ERROR_QUIET)
IF(NOT GIT_COMMIT)
  SET(GIT_COMMIT \"\")
ENDIF()

EXECUTE_PROCESS(
  COMMAND date \"+%b %d %Y, %H:%M:%S\"
  OUTPUT_VARIABLE BUILD_DATE
  OUTPUT_STRIP_TRAILING_WHITESPACE
  ERROR_QUIET)

FILE(WRITE \"\${buildinfo}\" \"! Define the current commit hash. The default must remain empty, i.e., ''.\n\
#define GIT_CURRENT_COMMIT \\\"\${GIT_COMMIT}\\\"\n\
#define BUILD_DATE         \\\"\${BUILD_DATE}\\\"\n\
#define BUILD_VERSION_${CMAKE_Fortran_COMPILER_ID}  \\\"${CMAKE_Fortran_COMPILER_VERSION}\\\"\n\
#define BUILD_VERSION_MPI  \\\"compiled without MPI\\\"\n\
\")
")

# Create a custom target that always updates the buildinfo.
# This target will run on every build invocation.
ADD_CUSTOM_TARGET(UpdateBuildInfo ALL
  COMMAND ${CMAKE_COMMAND}
          -Dbuildinfo=${buildinfo}
          -DCMAKE_Fortran_COMPILER_ID=${CMAKE_Fortran_COMPILER_ID}
          -DCMAKE_Fortran_COMPILER_VERSION=${CMAKE_Fortran_COMPILER_VERSION}
          -P ${CMAKE_CURRENT_BINARY_DIR}/update_buildinfo.cmake
  COMMENT "Updating commit header with current git commit"
  VERBATIM
)

# =========================================================================
# Check IPO support:
# =========================================================================
# we need to have languages enabled and compilers defined for this
CMAKE_POLICY(SET CMP0069 NEW)
INCLUDE(CheckIPOSupported)
CHECK_IPO_SUPPORTED(RESULT HASIPO OUTPUT error)

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

# =========================================================================
# Build type
# =========================================================================
INCLUDE(SetBuildType)

# =========================================================================
# Location of binary and filenames
# =========================================================================
# append relative filename-macro for __FILENAME__ in __STAMP__ of abort function (see hopr.h)
# subst replacement is GNU make specific
IF("${CMAKE_GENERATOR}" STREQUAL "Unix Makefiles")
SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -D__FILENAME__='\"$(subst ${CMAKE_SOURCE_DIR}/,,$(abspath $<))\"'")
ENDIF()

# add basedir for location of corresponding userblock-file
ADD_COMPILE_DEFINITIONS("BASEDIR='\"${CMAKE_CURRENT_BINARY_DIR}/\"'")

# =========================================================================
# Echo compile definitions
# =========================================================================
GET_DIRECTORY_PROPERTY(comp_defs DIRECTORY ${CMAKE_SOURCE_DIR} COMPILE_DEFINITIONS )
SET(comp_defs_str "COMPILE_DEFINITIONS = ")
FOREACH (def ${comp_defs})
   SET(comp_defs_str "${comp_defs_str} -D${def}")
ENDFOREACH()
ADD_CUSTOM_TARGET(preproc_defines COMMAND echo ${comp_defs_str})

# =========================================================================
# USERBLOCK + PREPROC_FLAGS
# =========================================================================
# A function to get all user defined variables with a specified prefix
FUNCTION (GETLISTOFVARSSTARTINGWITH _PREFIX _VARRESULT)
    GET_CMAKE_PROPERTY(_VARS CACHE_VARIABLES)
    STRING(REGEX MATCHALL "(^|;)${_PREFIX}[A-Za-z0-9_]*" _MATCHEDVARS "${_VARS}")
    SET(${_VARRESULT} ${_MATCHEDVARS} PARENT_SCOPE)
ENDFUNCTION()

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
INCLUDE(SetLibraries)

# =========================================================================
# HOPR
# =========================================================================
INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/src/CMakeLists.txt)

# =========================================================================
# Install
# =========================================================================
INSTALL(FILES ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/configuration.cmake DESTINATION bin)
