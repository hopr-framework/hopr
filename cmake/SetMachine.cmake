# =========================================================================
# Detect machine environments
# =========================================================================
# Try to read from environment variable, prevent possible FQDN hostname stall
IF(DEFINED ENV{CMAKE_HOSTNAME} AND NOT "$ENV{CMAKE_HOSTNAME}" STREQUAL "")
  SET(CMAKE_HOSTNAME  "$ENV{CMAKE_HOSTNAME}")
  SET(CMAKE_FQDN_HOST    "${CMAKE_HOSTNAME}")
ELSE()
  CMAKE_HOST_SYSTEM_INFORMATION(RESULT CMAKE_FQDN_HOST QUERY FQDN)
ENDIF()
MARK_AS_ADVANCED(FORCE CMAKE_FQDN_HOST)
MARK_AS_ADVANCED(FORCE CMAKE_HOSTNAME)
SITE_NAME(CMAKE_HOSTNAME)

# =========================================================================
# CMake generator settings
# =========================================================================
SET(USED_CMAKE_GENERATOR "${CMAKE_GENERATOR}" CACHE STRING "Expose CMAKE_GENERATOR (cannot be changed here)" FORCE)
IF("${CMAKE_GENERATOR}" STREQUAL "Ninja")
  # CMake introduced the CMAKE_COLOR_DIAGNOSTICS flag with 3.24.0, https://gitlab.kitware.com/cmake/cmake/-/merge_requests/6990
  IF(NOT(${CMAKE_VERSION} VERSION_LESS "3.24.0"))
    SET(CMAKE_COLOR_DIAGNOSTICS ON CACHE INTERNAL "Flag if CMake should attempt to color output")
  ELSE()
    SET(NINJA_COLOR_DIAGNOSTICS "-fdiagnostics-color=always" CACHE INTERNAL "Flag if Ninja should attempt to color output")
  ENDIF()
ENDIF()
MESSAGE(STATUS "Generating for [${CMAKE_GENERATOR}] build system")

# =========================================================================
# Some clusters requires setting the compilers by hand and invoking
# ENABLE_LANGUAGE afterwards, which is required for
# CMAKE_Fortran_COMPILER_ID that is used below
# > This block must be called before ENABLE_LANGUAGE
# =========================================================================
# Detect if CPE is used
IF (DEFINED ENV{PE_ENV})
  SET(CMAKE_C_COMPILER       cc)
  SET(CMAKE_CXX_COMPILER     CC)
  SET(CMAKE_Fortran_COMPILER ftn)
# SuperMUC
# ELSEIF(CMAKE_FQDN_HOST MATCHES "sng\.lrz\.de$"
# LUMI
ELSEIF(CMAKE_FQDN_HOST MATCHES "\.can$")
  SET(CMAKE_C_COMPILER       cc)
  SET(CMAKE_CXX_COMPILER     CC)
  SET(CMAKE_Fortran_COMPILER ftn)
# IAG Prandtl/Grafik01/Grafik02
ELSEIF(CMAKE_FQDN_HOST MATCHES "^(prandtl|grafik.*)\.iag\.uni\-stuttgart\.de")
  SET(CMAKE_C_COMPILER       gcc)
  SET(CMAKE_CXX_COMPILER     c++)
  SET(CMAKE_Fortran_COMPILER gfortran)
ENDIF()

