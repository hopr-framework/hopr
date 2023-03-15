#!/bin/bash
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
# Copyright (C) 2017 Claus-Dieter Munz <munz@iag.uni-stuttgart.de>
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
# Description:  This script will set the supplied build information hash under src/buildinfo.h
#************************************************************************************

# $1: path to buildinfo.h

GITCOMMIT=$2
BUILDDATE=$(date +'%b %d %Y, %H:%M:%S')
BUILDGCC="$3 $4"
if [ -n "$5" ]; then
  BUILDMPI="$5 $6"
fi

if [ -f "$1" ]; then
  # Override git commit
  GITCOMMITQUOTES="'$GITCOMMIT'"
  sed -i -e 's/.*#define GIT_CURRENT_COMMIT.*/#define GIT_CURRENT_COMMIT  '"$GITCOMMITQUOTES"'/' "$1"
  # Override build date
  BUILDDATEQUOTES="'$BUILDDATE'"
  sed -i -e 's/.*#define BUILD_DATE.*/#define BUILD_DATE          '"$BUILDDATEQUOTES"'/' "$1"
  # Override compiler version
  BUILDGCCQUOTES="'$BUILDGCC'"
  sed -i -e 's/.*#define BUILD_VERSION_GCC.*/#define BUILD_VERSION_GCC   '"$BUILDGCCQUOTES"'/' "$1"
  # Override MPI version
  if [ -n "$5" ]; then
    BUILDMPIQUOTES="'$BUILDMPI'"
  else
    BUILDMPIQUOTES="'compiled without MPI'"
  fi
  sed -i -e 's/.*#define BUILD_VERSION_MPI.*/#define BUILD_VERSION_MPI   '"$BUILDMPIQUOTES"'/' "$1"
fi

