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
#include "hopr.h"
MODULE MOD_PrismBasis
!===================================================================================================================================
! ?
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
PRIVATE
!-----------------------------------------------------------------------------------------------------------------------------------
! GLOBAL VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! Private Part ---------------------------------------------------------------------------------------------------------------------
! Public Part ----------------------------------------------------------------------------------------------------------------------
INTERFACE getPrismBasis
  MODULE PROCEDURE getPrismBasis
END INTERFACE

INTERFACE getBasisMappingPrism
  MODULE PROCEDURE getBasisMappingPrism
END INTERFACE

PUBLIC::getPrismBasis,getBasisMappingPrism
!===================================================================================================================================

CONTAINS
SUBROUTINE getPrismBasis(Deg,nNodes1D,Vdm,GradVdm)
!===================================================================================================================================
! given the degree of the orthogonal basis and the number of 1D nodes, equidistant nodes in the tetrahedron are generated and
! we compute the Vadndermonde matrix and the Vandermondematrix of the gradient of the basis function
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
INTEGER, INTENT(IN)          :: Deg  ! ?
INTEGER, INTENT(IN)          :: nNodes1D  ! ?
REAL,ALLOCATABLE,INTENT(OUT) :: Vdm(:,:),GradVdm(:,:,:)   ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES
REAL,ALLOCATABLE        :: r(:),s(:),t(:)    ! coordinates in tetrahedra reference space  r,s,t in [-1,1]
REAL                    :: r1D(0:nNodes1D-1) ! equidistant 1D Lobatto nodes in [-1,1]
INTEGER,ALLOCATABLE     :: bMap(:,:)         ! basis mapping iAns=>i,j,k
INTEGER,ALLOCATABLE     :: nodeMap(:,:)      ! mapping for equidistant nodes iNode=>i,j,k
INTEGER                 :: nNodes
INTEGER                 :: nAns  ! ?
INTEGER                 :: i,j,iNode  ! ?
!===================================================================================================================================
CALL getBasisMappingPrism(Deg,nAns,bMap)
CALL getBasisMappingPrism(nNodes1D-1,nNodes,nodeMap)
ALLOCATE(Vdm(nNodes,nAns),GradVdm(nNodes,nAns,3))
ALLOCATE(r(nNodes),s(nNodes),t(nNodes))
WRITE(*,*)'============================================'
WRITE(*,*)'getPrismBasis for Degree', Deg
WRITE(*,*)'============================================'
!equidistant nodes
r1D=0.
DO i=0,nNodes1D-1
  r1D(i)=-1.+2.*REAL(i)/REAL(nNodes1D-1)
END DO
r(:)=r1D(nodeMap(:,1))
s(:)=r1D(nodeMap(:,2))
t(:)=r1D(nodeMap(:,3))
DO iNode=1,nNodes
  WRITE(*,'(a,3I4,3F10.4)')'DEBUG, node pos',nodeMap(iNode,:),r(iNode),s(iNode),t(iNode)
END DO
CALL VandermondePrism(nNodes,nAns,bMap,r,s,t,VdM)
WRITE(*,*)'Vandermonde'
DO j=1,nAns
  WRITE(*,'(A1,I2,A1,I2,A1,I2,A1)',ADVANCE='NO') '(',bMap(j,1),',',bMap(j,2),',',bMap(j,3),')'
END DO
WRITE(*,*)' '
DO iNode=1,nNodes
  DO j=1,nAns
    WRITE(*,'(F10.4,1X)',ADVANCE='NO') VdM(iNode,j)
  END DO
  WRITE(*,*)' '
END DO
CALL GradVandermondePrism(nNodes,nAns,bMap,r,s,t,gradVdM)
WRITE(*,*)'GradVandermonde,1'
DO j=1,nAns
  WRITE(*,'(A1,I2,A1,I2,A1,I2,A1)',ADVANCE='NO') '(',bMap(j,1),',',bMap(j,2),',',bMap(j,3),')'
END DO
WRITE(*,*)' '
DO iNode=1,nNodes
  DO j=1,nAns
    WRITE(*,'(F9.4,1X)',ADVANCE='NO') gradVdM(iNode,j,1)
  END DO
  WRITE(*,*)' '
END DO
WRITE(*,*)'GradVandermonde,2'
DO j=1,nAns
  WRITE(*,'(A1,I2,A1,I2,A1,I2,A1)',ADVANCE='NO') '(',bMap(j,1),',',bMap(j,2),',',bMap(j,3),')'
END DO
WRITE(*,*)' '
DO iNode=1,nNodes
  DO j=1,nAns
    WRITE(*,'(F9.4,1X)',ADVANCE='NO') gradVdM(iNode,j,2)
  END DO
  WRITE(*,*)' '
END DO
WRITE(*,*)'GradVandermonde,3'
DO j=1,nAns
  WRITE(*,'(A1,I2,A1,I2,A1,I2,A1)',ADVANCE='NO') '(',bMap(j,1),',',bMap(j,2),',',bMap(j,3),')'
END DO
WRITE(*,*)' '
DO iNode=1,nNodes
  DO j=1,nAns
    WRITE(*,'(F9.4,1X)',ADVANCE='NO') gradVdM(iNode,j,3)
  END DO
  WRITE(*,*)' '
END DO
DEALLOCATE(bMap,nodeMap)
END SUBROUTINE getPrismBasis


SUBROUTINE getBasisMappingPrism(Deg,nAns,bMap,bMap2,bMapInv)
!===================================================================================================================================
! mapping from iAns -> i,j  in [0,Deg], can be used for nodeMap too: CALL getBasisMapping(nNodes1D-1,nodeMap)
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
INTEGER, INTENT(IN)                      :: Deg  ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
INTEGER, INTENT(OUT)                     :: nAns  ! ?
INTEGER,ALLOCATABLE,INTENT(OUT)          :: bMap(:,:)  ! ?
INTEGER,ALLOCATABLE,OPTIONAL,INTENT(OUT) :: bMap2(:,:)  ! ?
INTEGER,ALLOCATABLE,OPTIONAL,INTENT(OUT) :: bMapInv(:,:,:)  ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES
INTEGER                      :: iAns,i,j,k  ! ?
!===================================================================================================================================
nAns=(Deg+1)*(Deg+1)*(Deg+2)/2
ALLOCATE(bMap(nAns,3))
iAns=0
DO k=0,Deg
  DO j=0,Deg
    DO i=0,Deg-j
    !DO i=Deg-j,0,-1
      iAns=iAns+1
      bMap(iAns,:)=(/k,i,j/)
      ! bMap(iAns,:)=(/i,j,k/)
    END DO
  END DO
END DO
IF(PRESENT(bMap2))THEN
  ALLOCATE(bMap2(nAns,3))
  iAns=0
  DO k=0,Deg
    DO j=Deg,0,-1
      DO i=Deg-j,Deg
      !DO i=Deg,Deg-j,-1
        iAns=iAns+1
        bMap2(iAns,:)=(/k,j,i/)
      END DO
    END DO
  END DO
END IF
IF(PRESENT(bMapInv))THEN
  ALLOCATE(bMapInv(0:Deg,0:Deg,0:Deg))
  bMapInv=0
  iAns=0
  DO k=0,Deg
    DO j=0,Deg
      DO i=0,Deg-j
        iAns=iAns+1
        bMapInv(i,j,k)=iAns
      END DO
    END DO
  END DO
END IF
END SUBROUTINE getBasisMappingPrism


SUBROUTINE rst2abcPrism(nNodes,r,s,t,a,b,c)
!===================================================================================================================================
! Transforms reference coordinates of the prism (r,s,t) r,s,t >=-1, r+s<=1 t<=1,
! to tensorial coordinates (a,b,c) [-1,1]^3
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
INTEGER, INTENT(IN)          :: nNodes  ! ?
REAL,INTENT(IN)              :: r(nNodes),s(nNodes),t(nNodes)   ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
REAL,INTENT(OUT)             :: a(nNodes),b(nNodes),c(nNodes)   ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES
INTEGER                     :: iNode  ! ?
!===================================================================================================================================
WRITE(*,*)'entering rst2abcPrism'
DO iNode=1,nNodes
  IF(ABS(1.-s(iNode)).LT.PP_RealTolerance)THEN
    a(iNode)=-1.
  ELSE
    a(iNode) = 2.*(1.+r(iNode))/(1.-s(iNode))-1.
  END IF
END DO !iNode
b=s
c=t
END SUBROUTINE rst2abcPrism


SUBROUTINE VandermondePrism(nNodes,nAns,bMap,r,s,t,VdM)
!===================================================================================================================================
! For a given vector of nNodes in reference coordinates (r,s,t) of the prism, and nAns=(Deg+1)*(Deg+1)*(Deg+2)/2
! basis functions, we compute the 3D Vandermonde matrix.
! The polynomial basis is orthonormal with respect to the reference prism r,s,t>=-1, r+s<=1 , t<=1
!===================================================================================================================================
! MODULES
USE MOD_Basis1D,ONLY:JacobiP
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
INTEGER, INTENT(IN)          :: nNodes,nAns  ! ?
INTEGER, INTENT(IN)          :: bMap(nAns,3)   ! basis mapping iAns=>i,j,k
REAL,INTENT(IN)              :: r(nNodes),s(nNodes),t(nNodes)   ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
REAL,INTENT(OUT)             :: VdM(nNodes,nAns)   ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES
REAL,DIMENSION(nNodes)      :: a,b,c,Pa,Pb,Pc  ! ?
INTEGER                     :: iAns,i,j,k  ! ?
REAL                        :: norm  ! ?
!===================================================================================================================================
norm=SQRT(2.)
WRITE(*,*)'entering VandermondePrism'
CALL rst2abcPrism(nNodes,r,s,t,a,b,c)
DO iAns=1,nAns
  i=bMap(iAns,1)
  j=bMap(iAns,2)
  k=bMap(iAns,3)
  ! compute Vandermonde
  CALL JacobiP(nNodes,a,     0, 0,i,Pa)
  CALL JacobiP(nNodes,b, 2*i+1, 0,j,Pb)
  CALL JacobiP(nNodes,c,     0, 0,k,Pc)
  VdM(:,iAns)=norm*(Pa*Pb*Pc) !orthonormal
  IF(i.GT.0) VdM(:,iAns)=VdM(:,iAns)*((1.-b)**i)
END DO
END SUBROUTINE VandermondePrism


SUBROUTINE GradVandermondePrism(nNodes,nAns,bMap,r,s,t,gradVdM)
!===================================================================================================================================
! For a given vector of nNodes in reference coordinates (r,s,t) of the prism, and nAns=(Deg+1)*(Deg+1)*(Deg+2)/2
! basis functions, we compute the 3D Gradient Vandermonde matrix.
! The polynomial basis is orthonormal with respect to the reference prism r,s,t>=-1, r+s<=1 , t<=1
!===================================================================================================================================
! MODULES
USE MOD_Basis1D,ONLY:JacobiP,GradJacobiP
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
INTEGER, INTENT(IN)          :: nNodes,nAns  ! ?
INTEGER, INTENT(IN)          :: bMap(nAns,3)   ! basis mapping iAns=>i,j,k
REAL,INTENT(IN)              :: r(nNodes),s(nNodes),t(nNodes)   ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
REAL,INTENT(OUT)             :: gradVdM(nNodes,nAns,3)   ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES
REAL,DIMENSION(nNodes)      :: a,b,c,Pa,Pb,Pc,dPa,dPb,dPc,dPhi_da,dPhi_db,dPhi_dc ! ?
INTEGER                     :: iAns,i,j,k  ! ?
REAL                        :: norm  ! ?
!===================================================================================================================================
norm=SQRT(2.)
WRITE(*,*)'entering GradVandermondePrism'
CALL rst2abcPrism(nNodes,r,s,t,a,b,c)
DO iAns=1,nAns
  i=bMap(iAns,1)
  j=bMap(iAns,2)
  k=bMap(iAns,3)

  CALL JacobiP(nNodes,a,     0, 0,i,Pa)
  CALL JacobiP(nNodes,b, 2*i+1, 0,j,Pb)
  CALL JacobiP(nNodes,c,     0, 0,k,Pc)
  CALL GradJacobiP(nNodes,a,     0, 0,i,dPa)
  CALL GradJacobiP(nNodes,b, 2*i+1, 0,j,dPb)
  CALL GradJacobiP(nNodes,c,     0, 0,k,dPc)
  !dphi/da*(1-b)^-1
  dPhi_da=norm*(dPa*Pb*Pc)
  IF(i.GT.1) dPhi_da=dPhi_da*((1.-b)**(i-1))
  !dphi/db
  dPhi_db=dPb
  IF(i.GT.0) dPhi_db=dPhi_db*(1.-b)**i
  IF(i.EQ.1) dPhi_db=dPhi_db-Pb
  IF(i.GT.1) dPhi_db=dPhi_db-i*(1.-b)**(i-1)*Pb
  dPhi_db=norm*Pa*Pc*dPhi_db
  !dphi/dc
  dPhi_dc=dPc
  dPhi_dc=norm*Pa*Pb*dPhi_dc
  IF(i.GT.0) dPhi_dc=dPhi_dc*(1.-b)**i
  ! r-derivative
  gradVdM(:,iAns,1)=2.*dPhi_da
  ! s-derivative
  gradVdM(:,iAns,2)=(1.+a)*dPhi_da+dPhi_db
  ! t-derivative
  gradVdM(:,iAns,3)=dPhi_dc
END DO
END SUBROUTINE GradVandermondePrism

END MODULE MOD_PrismBasis
