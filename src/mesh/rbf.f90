#include "hopr.h"

MODULE MOD_RBF
!===================================================================================================================================
! Contains subroutine to build and evaluate a RBF interpolation. We use this to interpolate the surface curving to the volume.
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
PRIVATE
!-----------------------------------------------------------------------------------------------------------------------------------
! GLOBAL VARIABLES (PUBLIC)
!-----------------------------------------------------------------------------------------------------------------------------------
! Public Part ----------------------------------------------------------------------------------------------------------------------

INTERFACE RBFVolumeCurving
  MODULE PROCEDURE RBFVolumeCurving
END INTERFACE

PUBLIC::RBFVolumeCurving
!===================================================================================================================================

CONTAINS

SUBROUTINE RBFVolumeCurving()
!----------------------------------------------------------------------------------------------------------------------------------!
! Volume curving based on an radial basis function interpolation of the curved boundary surfaces.
! General structure:
!  * Build RBF basis. We need to build a list of all boundary points (unique!), and save both the curved coordinates as well
!    as the linear ones. To build the list we loop over all sides, which means we visit each boundary point multiple times.
!    To keep the list unique we use a temporary marker to mark all already visited points.
!    Then the RBF equation system can be built: The matrix is built by evaluating each radial basis function at
!    each control point (e.g. all uncurved boundary points). The right hand side is built by taking the difference between the linear
!    and the curved coordinates of the control points - we interpolate the displacement.  Non-curved boundary points are included
!    in the RBF system by setting the R.H.S. to zero.
!    To get the coordinates of the uncurved high-order mesh points we perform a "manual" change basis (e.g. MATMUL with Vandermodne)
!    matrix from the corner points of the curved element (which are of course equal to the uncurved points) to the boundary order.
!    We then invert the matrix to solve the system directly.
!  * In another loop over all elements we evaluate the interpolation of the displacement for each volume point of the mesh.
!    We add the interpolation to the uncurved mesh point coordinates to get the curved volume position. On the boundary points
!    this will recover the curved coordinates which served as our input.
!----------------------------------------------------------------------------------------------------------------------------------!
! MODULES                                                                                                                          !
USE MOD_Globals
USE MOD_HexaBasis,      ONLY: getHexaBasis,getBasisMappingHexa
USE MOD_QuadBasis,      ONLY: getQuadBasis,getBasisMappingQuad
USE MOD_Basis_Vars,     ONLY: QuadMapInv,HexaMapInv
USE MOD_Mesh_Vars
!----------------------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
! INPUT / OUTPUT VARIABLES 
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES
TYPE(tElem),POINTER    :: Elem
TYPE(tSide),POINTER    :: Side
INTEGER                :: i,j,nAns
REAL,ALLOCATABLE       :: RefCoordinates(:,:)            ! Coordinates of boundary points
REAL,ALLOCATABLE       :: RBFMatrix(:,:)                 ! Matrix of the linear system
REAL,ALLOCATABLE       :: RBFMatrix_tmp(:,:)
REAL,ALLOCATABLE       :: RBFRHS(:,:)
INTEGER                :: nBP                            ! Number of boundary points considered
REAL,ALLOCATABLE       :: Vdm_SurfBiLinear(:,:),Vdm_VolTriLinear(:,:)
REAL,ALLOCATABLE       :: D_tmp(:,:,:)
REAL,ALLOCATABLE       :: RBFResult(:,:)
INTEGER,ALLOCATABLE    :: QuadMapInvLinear(:,:)
INTEGER,ALLOCATABLE    :: HexaMapInvLinear(:,:,:)
INTEGER,ALLOCATABLE    :: QuadMapLinear(:,:)
INTEGER,ALLOCATABLE    :: HexaMapLinear(:,:)
REAL                   :: xTriLinear(3,BoundaryOrder**3),xBiLinear(3,BoundaryOrder**2)
REAL                   :: xCornerVol(1:3,8),xCornerSurf(1:3,4)
REAL                   :: dist,x(3),xTmp(3),rbfvalue
INTEGER                :: iBP
!===================================================================================================================================

WRITE(UNIT_StdOut,'(132("-"))')
WRITE(UNIT_stdOut,'(A)') ' RBF VOLUME CURVING'

! Build 2D Vandermonde from corner nodes to bi-linear side nodes
CALL getQuadBasis(1,BoundaryOrder,Vdm_SurfBiLinear,D_tmp)
DEALLOCATE(D_tmp)

! Build 3D Vandermonde from corner nodes to tri-linear side nodes
CALL getHexaBasis(1,BoundaryOrder,Vdm_VolTriLinear,D_tmp)
DEALLOCATE(D_tmp)

! Build mapping between 1D and tensor product ordering for a bi-linear surface
CALL getBasisMappingQuad(1,nAns,QuadMapLinear,QuadMapInvLinear)
DEALLOCATE(QuadMapLinear)

! Build mapping between 1D and tensor product ordering for a bi-linear surface
CALL getBasisMappingHexa(1,nAns,HexaMapLinear,HexaMapInvLinear)
DEALLOCATE(HexaMapLinear)

! Reset the tmp variable later used to mark the boundary point
Elem=>FirstElem
DO WHILE(ASSOCIATED(Elem))
  Side=>Elem%firstSide
  DO WHILE(ASSOCIATED(Side))
    ! Cycle for non-bc sides
    IF(.NOT.ASSOCIATED(Side%BC)) THEN
      Side=>Side%nextElemSide
      CYCLE
    END IF
    IF(Side%BC%BCType.EQ.1) THEN
      Side=>Side%nextElemSide
      CYCLE
    END IF
    DO i=1,Side%nCurvedNodes
      Side%CurvedNode(i)%np%tmp = 0
    END DO
    Side=>Side%nextElemSide
  END DO
  Elem=>Elem%nextElem
END DO
! Loop over all sides and count the number of unique boundary points
nBP = 0
Elem=>FirstElem
DO WHILE(ASSOCIATED(Elem))
  Side=>Elem%firstSide
  DO WHILE(ASSOCIATED(Side))
    ! Cycle for non-bc sides
    IF(.NOT.ASSOCIATED(Side%BC)) THEN
      Side=>Side%nextElemSide
      CYCLE
    END IF
    IF(Side%BC%BCType.EQ.1) THEN
      Side=>Side%nextElemSide
      CYCLE
    END IF
    DO i=1,Side%nCurvedNodes
      ! Skip already marked boundary points
      IF (Side%CurvedNode(i)%np%tmp.EQ.1) CYCLE
      ! Increase count of boundary points
      nBP = nBP + 1
      ! Mark this side node as included in the unique list of boundary points
      Side%CurvedNode(i)%np%tmp = 1
    END DO
    Side=>Side%nextElemSide
  END DO
  Elem=>Elem%nextElem
END DO

! Output of number of RBF control points
WRITE(*,*) 'Number of RBF control points: ',nBP

! Allocate arrays used to store RBF data
ALLOCATE(RefCoordinates(1:3,1:nBP))    ! Coordinates of UNCURVED boundary points
ALLOCATE(RBFMatrix(1:nBP+4,1:nBP+4))   ! Matrix of RBF system
ALLOCATE(RBFRHS(1:nBP+4,1:3))          ! Right hand side of RBF system, last index = component of coordinates
ALLOCATE(RBFResult(1:nBP+4,3))         ! Coefficients of RBF interpolation = result of EQS, last index = component of coordinates


! Reset the tmp variable later used to mark the boundary point
Elem=>FirstElem
DO WHILE(ASSOCIATED(Elem))
  Side=>Elem%firstSide
  DO WHILE(ASSOCIATED(Side))
    ! Cycle for non-bc sides
    IF(.NOT.ASSOCIATED(Side%BC)) THEN
      Side=>Side%nextElemSide
      CYCLE
    END IF
    IF(Side%BC%BCType.EQ.1) THEN
      Side=>Side%nextElemSide
      CYCLE
    END IF
    DO i=1,Side%nCurvedNodes
      Side%CurvedNode(i)%np%tmp = 0
    END DO
    Side=>Side%nextElemSide
  END DO
  Elem=>Elem%nextElem
END DO

! Store the RefCoordinates (uncurved) for the boundary points and the displacement between uncurved and curved coordinates
iBP = 0
Elem=>FirstElem
DO WHILE(ASSOCIATED(Elem))
  Side=>Elem%firstSide
  DO WHILE(ASSOCIATED(Side))
    ! Cycle for non-bc sides
    IF(.NOT.ASSOCIATED(Side%BC)) THEN
      Side=>Side%nextElemSide
      CYCLE
    END IF
    IF(Side%BC%BCType.EQ.1) THEN
      Side=>Side%nextElemSide
      CYCLE
    END IF
    IF (Side%CurveIndex.LE.0) THEN
      ! Linear surfaces
      DO i=1,Side%nCurvedNodes
        ! Skip already marked boundary points
        IF (Side%CurvedNode(i)%np%tmp.EQ.1) CYCLE
        ! Increase index of boundary points
        iBP = iBP + 1
        ! Store reference coordinates, which are the curved node coordinates for linear surfaces
        RefCoordinates(1:3,iBP) = Side%CurvedNode(i)%np%x
        ! Set displacement to zero
        RBFRHS(iBP,1:3) = 0.
        ! Mark this side node as done
        Side%CurvedNode(i)%np%tmp = 1
      END DO
    ELSE
      ! Curved surfaces
      ! Build coordinates of linear side. First, fill an temp array with the corner nodes
      xCornerSurf(:,QuadMapInvLinear(0,0)) = Side%CurvedNode(QuadMapInv(0,0))%np%x
      xCornerSurf(:,QuadMapInvLinear(0,1)) = Side%CurvedNode(QuadMapInv(0,N))%np%x
      xCornerSurf(:,QuadMapInvLinear(1,0)) = Side%CurvedNode(QuadMapInv(N,0))%np%x
      xCornerSurf(:,QuadMapInvLinear(1,1)) = Side%CurvedNode(QuadMapInv(N,N))%np%x
      ! Then, apply Vandermonde matrix, e.g. evaluate bi-linear mapping on all curved side nodes
      xBiLinear(1,:) = MATMUL(Vdm_SurfBiLinear,xCornerSurf(1,:))
      xBiLinear(2,:) = MATMUL(Vdm_SurfBiLinear,xCornerSurf(2,:))
      xBiLinear(3,:) = MATMUL(Vdm_SurfBiLinear,xCornerSurf(3,:))
      DO i=1,Side%nCurvedNodes
        ! Skip already marked boundary points
        IF (Side%CurvedNode(i)%np%tmp.EQ.1) CYCLE
        ! Increase index of boundary points
        iBP = iBP + 1
        ! Store reference coordinates, e.g. the bi-linear nodes
        RefCoordinates(1:3,iBP) = xBiLinear(:,i)
        ! Calculate displacement, e.g. difference between bi-linear and curved nodes
        RBFRHS(iBP,1:3) = Side%CurvedNode(i)%np%x - xBiLinear(:,i)
        ! Mark this side node as done
        Side%CurvedNode(i)%np%tmp = 1
      END DO
    END IF ! linear or curved side
    Side=>Side%nextElemSide
  END DO
  Elem=>Elem%nextElem
END DO

! Set the rows of the R.H.S. associated with the linear polynomial to zero
RBFRHS(nBP+1:nBP+4,:) = 0.

! Build the RBF matrix
! Upper left part: M_i,j = phi(abs(X(i)-X(j)))
DO j=1,nBP; DO i=1,nBP
  dist = Distance(RefCoordinates(:,i),RefCoordinates(:,j))
  RBFMatrix(i,j) = EvaluateRBF(dist)
END DO; END DO ! j,i=1,nBP
! Upper right part: P_b - row i equal to [1 x_i y_i z_i]
! Lower left part: transpose of P_b
DO i=1,nBP
   RBFMatrix(i,nBP+1) = 1.
   RBFMatrix(nBP+1,i) = 1.
   RBFMatrix(i,nBP+2) = RefCoordinates(1,i)
   RBFMatrix(nBP+2,i) = RefCoordinates(1,i)
   RBFMatrix(i,nBP+3) = RefCoordinates(2,i)
   RBFMatrix(nBP+3,i) = RefCoordinates(2,i)
   RBFMatrix(i,nBP+4) = RefCoordinates(3,i)
   RBFMatrix(nBP+4,i) = RefCoordinates(3,i)
END DO ! i=1,nBP
! Lower right part: zeros
DO j=nBP+1,nBP+4; DO i=nBP+1,nBP+4
  RBFMatrix(i,j) = 0.
END DO; END DO ! j,i=nBP+1,nBP+4

! Now compute the inverse using LAPACK and store in the original array
ALLOCATE(RBFMatrix_tmp(1:nBP+4,1:nBP+4))
RBFMatrix_tmp = GetInverse(nBP+4,RBFMatrix(1:nBP+4,1:nBP+4))
DEALLOCATE(RBFMatrix)
ALLOCATE(RBFMatrix(1:nBP+4,1:nBP+4))
RBFMatrix = RBFMatrix_tmp
DEALLOCATE(RBFMatrix_tmp)

! Evaluate the coefficients of the RBF interpolation by applying the inverse of the matrix to the R.H.S.
RBFResult(1:nBP,1:nBP) = MATMUL(RBFMatrix(1:nBP,1:nBP),RBFRHS(1:nBP,:))

Elem=>FirstElem
DO WHILE(ASSOCIATED(Elem))
  ! Calculate the tri-linear coordinates of the curved nodes
  xCornerVol(:,HexaMapInvLinear(0,0,0)) = Elem%CurvedNode(HexaMapInv(0,0,0))%np%x
  xCornerVol(:,HexaMapInvLinear(0,1,0)) = Elem%CurvedNode(HexaMapInv(0,N,0))%np%x
  xCornerVol(:,HexaMapInvLinear(1,0,0)) = Elem%CurvedNode(HexaMapInv(N,0,0))%np%x
  xCornerVol(:,HexaMapInvLinear(1,1,0)) = Elem%CurvedNode(HexaMapInv(N,N,0))%np%x
  xCornerVol(:,HexaMapInvLinear(0,0,1)) = Elem%CurvedNode(HexaMapInv(0,0,N))%np%x
  xCornerVol(:,HexaMapInvLinear(0,1,1)) = Elem%CurvedNode(HexaMapInv(0,N,N))%np%x
  xCornerVol(:,HexaMapInvLinear(1,0,1)) = Elem%CurvedNode(HexaMapInv(N,0,N))%np%x
  xCornerVol(:,HexaMapInvLinear(1,1,1)) = Elem%CurvedNode(HexaMapInv(N,N,N))%np%x
  ! Then, apply Vandermonde matrix, e.g. evaluate bi-linear mapping on all curved side nodes
  xTriLinear(1,:) = MATMUL(Vdm_VolTriLinear,xCornerVol(1,:))
  xTriLinear(2,:) = MATMUL(Vdm_VolTriLinear,xCornerVol(2,:))
  xTriLinear(3,:) = MATMUL(Vdm_VolTriLinear,xCornerVol(3,:))
  DO i=1,Elem%nCurvedNodes
    ! Evaluate the RBF interpolation at each linear volume point
    x = xTriLinear(:,i)
    xTmp = x
    DO iBP=1,nBP
      dist = Distance(x,RefCoordinates(:,iBP))
      RBFValue = EvaluateRBF(dist)
      xTmp(1) = xTmp(1) + RBFValue*RBFResult(iBP,1)
      xTmp(2) = xTmp(2) + RBFValue*RBFResult(iBP,2)
      xTmp(3) = xTmp(3) + RBFValue*RBFResult(iBP,3)
    END DO
    ! Take linear polynomial into account
    xTmp = xTmp + RBFResult(nBP+1,:) ! constant part
    xTmp(1) = xTmp(1) + DOT_PRODUCT(RBFResult(nBP+2:nBP+4,1),x(:)) ! Linear part
    xTmp(2) = xTmp(2) + DOT_PRODUCT(RBFResult(nBP+2:nBP+4,2),x(:)) ! Linear part
    xTmp(3) = xTmp(3) + DOT_PRODUCT(RBFResult(nBP+2:nBP+4,3),x(:)) ! Linear part
    ! Overwrite curved node position by interpolation
    Elem%CurvedNode(i)%np%x = xTmp
  END DO
  Elem=>Elem%nextElem
END DO

WRITE(UNIT_stdOut,'(A)')' VOLUME CURVING DONE!'

END SUBROUTINE RBFVolumeCurving



FUNCTION EvaluateRBF(dist)
!===================================================================================================================================
! Evaluates the choosen RBF at distance dist with support radius supportRadius (only applicable for compact support RBFs).
! Numbering and choice of RBFs is taken from: A. de Boer et al. / Computers and Structures 85 (2007) 784–795
! The multiquadratic biharmonic RBFs (type 10 and 11) take an additional parameter (a) which controls the shape of the function.
! Typical values for this parameter are in the range 10E-5 - 10E-3.
!===================================================================================================================================
USE MOD_Globals
USE MOD_Mesh_Vars,    ONLY: supportRadius,RBFType
! INPUT VARIABLES
REAL,INTENT(IN) :: dist         ! Euclidean distance used in RBF evaluation
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
REAL            :: EvaluateRBF
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES
REAL             :: xi
REAL,PARAMETER   :: a=10E-3
!===================================================================================================================================
! normalized distance, input variable for local support RBFs
xi = dist/supportRadius

SELECT CASE(RBFType)
! Local support RBFs
CASE(1)
  IF (xi.GT.1.) THEN
    EvaluateRBF = 0.
  ELSE
    EvaluateRBF = (1.-xi)**2.
  END IF
CASE(2)
  IF (xi.GT.1.) THEN
    EvaluateRBF = 0.
  ELSE
    EvaluateRBF=(1.-xi)**4.*(4.*xi+1.)
  END IF
CASE(3)
  IF (xi.GT.1.) THEN
    EvaluateRBF = 0.
  ELSE
    EvaluateRBF = (1.-xi)**6.*(35./3.*xi**2.+6.*xi+1.)
  END IF
CASE(4)
  IF (xi.GT.1.) THEN
    EvaluateRBF = 0.
  ELSE
    EvaluateRBF = (1.-xi)**8.*(32.*xi**3.+25.*xi**2.+8.*xi+1.)
  END IF
CASE(5)
  IF (xi.GT.1.) THEN
    EvaluateRBF = 0.
  ELSE
    EvaluateRBF = (1.-xi)**5.
  END IF
CASE(8)
  IF (xi.GT.1.) THEN
    EvaluateRBF = 0.
  ELSE
    EvaluateRBF = 1.-20.*xi**2.+80.*xi**3.-45.*xi**4.-16.*xi**5.+60.*xi**4.*LOG(xi)
  END IF
! Global support RBFs
CASE(9)
    EvaluateRBF = dist**2.*LOG(dist)
CASE(10)
    EvaluateRBF = SQRT(a**2.+dist**2.)
CASE(11)
    EvaluateRBF = SQRT(1./(a**2.+dist**2.))
CASE(12)
    EvaluateRBF = 1.+dist**2.
CASE(13)
    EvaluateRBF = 1./(dist**2.)
CASE(14)
    EvaluateRBF = EXP(-dist**2.)
CASE DEFAULT
  CALL Abort(__STAMP__,'RBF Type is unknown')
END SELECT

END FUNCTION EvaluateRBF


FUNCTION Distance(x,y)
!===================================================================================================================================
! Calculate euclidean distance between two points.
!===================================================================================================================================
! INPUT VARIABLES
REAL,INTENT(IN) :: x(1:3)    ! Coordinates of first point
REAL,INTENT(IN) :: y(1:3)    ! Coordinates of second point
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
REAL            :: Distance
!===================================================================================================================================

! d(x,y) = \|x-y\|_2 = \sqrt{(x_{1} - y_{1})^2 + \cdots + (x_{n} - y_{n})^2} = \sqrt{\sum_{i=1}^n (x_i-y_i)^2}
Distance = SQRT((x(1)-y(1))**2.+(x(2)-y(2))**2.+(x(3)-y(3))**2.)

END FUNCTION Distance


END MODULE MOD_RBF
