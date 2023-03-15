!============================================================================================================ xX =================
!        _____     _____    _______________    _______________   _______________            .xXXXXXXXx.       X
!       /    /)   /    /)  /    _____     /)  /    _____     /) /    _____     /)        .XXXXXXXXXXXXXXx  .XXXXx
!      /    //   /    //  /    /)___/    //  /    /)___/    // /    /)___/    //       .XXXXXXXXXXXXXXXXXXXXXXXXXx
!     /    //___/    //  /    //   /    //  /    //___/    // /    //___/    //      .XXXXXXXXXXXXXXXXXXXXXXX`
!    /    _____     //  /    //   /    //  /    __________// /    __      __//      .XX``XXXXXXXXXXXXXXXXX`
!   /    /)___/    //  /    //   /    //  /    /)_________) /    /)_|    |__)      XX`   `XXXXX`     .X`
!  /    //   /    //  /    //___/    //  /    //           /    //  |    |_       XX      XXX`      .`
! /____//   /____//  /______________//  /____//           /____//   |_____/)    ,X`      XXX`
! )____)    )____)   )______________)   )____)            )____)    )_____)   ,xX`     .XX`
!                                                                           xxX`      XXx
! Copyright (C) 2017 Claus-Dieter Munz <munz@iag.uni-stuttgart.de>
! This file is part of HOPR, a software for the generation of high-order meshes.
!
! HOPR is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License
! as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!
! HOPR is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
! of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with HOPR. If not, see <http://www.gnu.org/licenses/>.
!=================================================================================================================================
#include "buildinfo.h"

MODULE MOD_Commandline_Arguments
!=================================================================================================================================
!> Module to handle commandline arguments
!=================================================================================================================================
IMPLICIT NONE

! Global variables for command line argument parsing
INTEGER                              :: nArgs              ! number of command line arguments
CHARACTER(LEN=255),ALLOCATABLE       :: Args(:)

INTERFACE ParseCommandlineArguments
  MODULE PROCEDURE ParseCommandlineArguments
END INTERFACE ParseCommandlineArguments

!==================================================================================================================================
CONTAINS

!==================================================================================================================================
!> Reads all commandline arguments
!==================================================================================================================================
SUBROUTINE ParseCommandlineArguments()
! MODULES
USE MOD_Globals
USE MOD_ReadInTools     ,ONLY: FillStrings,STRICMP
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------------------
! INPUT/OUTPUT VARIABLES
!----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES
INTEGER                 :: iArg,nArgs_tmp
CHARACTER(LEN=255)      :: tmp
LOGICAL,ALLOCATABLE     :: alreadyRead(:)
!==================================================================================================================================
! Get number of command line arguments
nArgs_tmp = COMMAND_ARGUMENT_COUNT()
ALLOCATE(alreadyRead(nArgs_tmp))
alreadyRead = .FALSE.

! Get keyword arguments (arbitrary order)
nArgs = nArgs_tmp
DO iArg = 1, nArgs_tmp
  CALL GET_COMMAND_ARGUMENT(iArg,tmp)
  IF (STRICMP(tmp, "--version").OR.STRICMP(tmp,"-V")) THEN
    doPrintVersion    = doPrintVersion + 1
    alreadyRead(iArg) = .TRUE.
    nArgs = nArgs - 1
  END IF
END DO ! iArg = 1, nArgs

SELECT CASE(doPrintVersion)
  CASE(0)
    ! Do nothing
  CASE(1)
    ! Print version and exit
    WRITE(UNIT_stdOut,'(A)')"hopr version "&
        //TRIM(int2strf(MajorVersion))//"."//TRIM(int2strf(MinorVersion))//"."//TRIM(int2strf(PatchVersion))
    STOP
  CASE DEFAULT ! 2+
    ! Print extended version and exit
    WRITE(UNIT_stdOut,'(A)')"hopr version "&
        //TRIM(int2strf(MajorVersion))//"."//TRIM(int2strf(MinorVersion))//"."//TRIM(int2strf(PatchVersion))&
        //" ("//TRIM(GIT_CURRENT_COMMIT)//", "//TRIM(BUILD_DATE)       //")"                                &
        //" ["//TRIM(BUILD_VERSION_GCC) //", "//TRIM(BUILD_VERSION_MPI)//"]"
    STOP
END SELECT

! Get all remaining parameters
nArgs = MAX(1,nArgs) ! at least one argument is generated (empty)
ALLOCATE(Args(nArgs))
Args(1) = ""
nArgs = 0
DO iArg = 1,nArgs_tmp
  IF (.NOT.alreadyRead(iArg)) THEN
    nArgs = nArgs + 1
    CALL GET_COMMAND_ARGUMENT(iArg,Args(nArgs))
    alreadyRead(iArg) = .TRUE.
  END IF
END DO
DEALLOCATE(alreadyRead)

! Fill strings from ini file
CALL FillStrings(Args(1))

END SUBROUTINE ParseCommandlineArguments


!==================================================================================================================================
!> Convert an Integer to a String
!==================================================================================================================================
!SUBROUTINE int2strf(str,int_number,stat)
PURE FUNCTION int2strf(int_number)
!===================================================================================================================================
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES
CHARACTER(len=3) :: int2strf
INTEGER,INTENT(IN) :: int_number
!===================================================================================================================================
WRITE(int2strf,'(I0)')  int_number
int2strf = TRIM(ADJUSTL(int2strf))
END FUNCTION


!==================================================================================================================================
!> Deallocate all commandline arguments
!==================================================================================================================================
SUBROUTINE FinalizeCommandlineArguments()
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!===================================================================================================================================
IF (Allocated(Args)) DEALLOCATE(Args)
END SUBROUTINE FinalizeCommandlineArguments

END MODULE MOD_Commandline_Arguments
