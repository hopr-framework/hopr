#============================================================================================================ xX =================
#        _____     _____    _______________    _______________   _______________            .xXXXXXXXx.       X
#       /    /)   /    /)  /    _____     /)  /    _____     /) /    _____     /)        .XXXXXXXXXXXXXXx  .XXXXx
#      /    //   /    //  /    /)___/    //  /    /)___/    // /    /)___/    //       .XXXXXXXXXXXXXXXXXXXXXXXXXx
#     /    //___/    //  /    //   /    //  /    //___/    // /    //___/    //      .XXXXXXXXXXXXXXXXXXXXXXX`
#    /    _____     //  /    //   /    //  /    __________// /    __      __//      .XX``XXXXXXXXXXXXXXXXX`
#   /    /)___/    //  /    //   /    //  /    /)_________) /    /)_|    |__)      XX`   `XXXXX`     .X`
#  /    //   /    //  /    //___/    //  /    //           /    //  |    |_       XX      XXX`      .`
# /____//   /____//  /______________//  /____//           /____//   |_____/)    ,X`      XXX`
# )____)    )____)   )______________)   )____)            )____)    )_____)   ,xX`     .XX`
#                                                                           xxX`      XXx
# Copyright (C) 2022 Stephen Copplestone
# This file is part of HOPR, a software for the generation of high-order meshes.
#
# HOPR is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#
# HOPR is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with HOPR. If not, see <http://www.gnu.org/licenses/>.
#=================================================================================================================================

#add_build_test(<target> <sources>...)
#
# Adds test executable, <target>, built from <sources> and
# adds the test so that CTest will run it.
# Both the executable and the test will be named <target>.
#
function(add_build_test target)

  # Add this test to the global list of tests
  SET(BUILD_TEST_TARGETS ${BUILD_TEST_TARGETS} hopr PARENT_SCOPE)

  # split string
  GET_FILENAME_COMPONENT(TUTORIAL_PATH ${ARGN} DIRECTORY)
  GET_FILENAME_COMPONENT(TUTORIAL ${TUTORIAL_PATH} NAME)
  GET_FILENAME_COMPONENT(PARAMETER_INI ${ARGN} NAME)

  # display info
  #MESSAGE(STATUS "${CMAKE_CURRENT_BINARY_DIR}/buildTests/${TUTORIAL}_${PARAMETER_INI}  +  ${CMAKE_CURRENT_BINARY_DIR}/bin/hopr  +  ${ARGN}")

  # Create subdirectory for each test to run in parallel
  FILE(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/buildTests/${TUTORIAL}_${PARAMETER_INI})

  # copy original folder contents (e.g. CGNS files etc.) to new directory where the test is run
  FILE(COPY ${TUTORIAL_PATH}/ DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/buildTests/${TUTORIAL}_${PARAMETER_INI}/.)

  # add each test to list
  ADD_TEST(NAME ${target}
           WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/buildTests/${TUTORIAL}_${PARAMETER_INI}
           COMMAND ${CMAKE_CURRENT_BINARY_DIR}/bin/hopr ${ARGN}
           )

endfunction()

# Enable testing in general
ENABLE_TESTING()
# ... and make it parallel
INCLUDE(ProcessorCount)
PROCESSORCOUNT(N)
SET(ENV{CTEST_PARALLEL_LEVEL} "${N}")
#SET(ENV{CTEST_PARALLEL_LEVEL} "1")
MESSAGE(STATUS "Executing build tests with $ENV{CTEST_PARALLEL_LEVEL} processes")

# Add the build tests
#add_build_test(buildTest:Tutorial1   /tutorials/1-01-cartbox/)

# Loop all directories in the tutorials folder
SUBDIRLIST(TUTORIALS ${CMAKE_CURRENT_SOURCE_DIR}/tutorials)
FOREACH(TUTORIAL ${TUTORIALS})
  FILE(GLOB PARAMETER_INI_FILES ${CMAKE_CURRENT_SOURCE_DIR}/tutorials/${TUTORIAL}/*.ini)
  FOREACH(PARAMETER_INI_PATH ${PARAMETER_INI_FILES})
    GET_FILENAME_COMPONENT(PARAMETER_INI ${PARAMETER_INI_PATH} NAME)
    add_build_test(buildTest:${TUTORIAL}:${PARAMETER_INI}   ${PARAMETER_INI_PATH})
  ENDFOREACH()
ENDFOREACH()

# Perform build tests when the last test has been added
ADD_CUSTOM_TARGET( all_build_tests ALL
                   DEPENDS ${BUILD_TEST_TARGETS})

ADD_DEPENDENCIES(all_tests hopr)

ADD_CUSTOM_COMMAND(TARGET all_build_tests
                   COMMENT "Run build tests"
                   POST_BUILD COMMAND ctest --output-on-failure -j${N}
                   WORKING_DIRECTORY  ${CMAKE_BINARY_DIR}/buildTests)
                   #WORKING_DIRECTORY  ${CMAKE_BINARY_DIR})
                   #WORKING_DIRECTORY  ${CMAKE_CURRENT_SOURCE_DIR}/tutorials)
