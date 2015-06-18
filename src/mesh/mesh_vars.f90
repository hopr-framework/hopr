#include "defines.f90"
MODULE MOD_Mesh_Vars
!===================================================================================================================================
! ?
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
PUBLIC
!-----------------------------------------------------------------------------------------------------------------------------------
! GLOBAL VARIABLES 
!-----------------------------------------------------------------------------------------------------------------------------------
! User Defined Types ---------------------------------------------------------------------------------------------------------------

! First intermediate pointer types -------------------------------------------------------------------------------------------------
TYPE tElemPtr
  TYPE(tElem),POINTER                 ::       EP                     ! Local element pointer
END TYPE tElemPtr

TYPE tSidePtr
  TYPE(tSide),POINTER                 ::       SP                     ! side pointer
END TYPE tSidePtr

TYPE tEdgePtr
  TYPE(tEdge),POINTER                 ::       EDP                    ! edge pointer
END TYPE tEdgePtr

TYPE tNodePtr
  TYPE(tNode),POINTER                 ::       NP                     ! node pointer
END TYPE tNodePtr

! Actual derived types ------------------------------------------------------------------------------------------------------------
TYPE tElem ! provides data structure for local element
  ! Derived data types -----------------------------------------------!
  TYPE(tSide), POINTER                ::       firstSide              ! pointer to element's first side
  TYPE(tNodePtr),POINTER              ::       node(:)                ! pointer to element's nodes used for restart and meshing
  TYPE(tNodePtr),POINTER              ::       curvedNode(:)          ! ? 
  TYPE(tElem),POINTER                 ::       nextElem               ! pointer to next     element in order to continue a loop
  TYPE(tElem),POINTER                 ::       prevElem               ! pointer to previous element in order to continue a loop   
  REAL                                ::       DetT                   ! element mapping depandant on the element type
  ! INTEGER ----------------------------------------------------------!
  INTEGER                             ::       TYPE                   ! element type, for memory efficiency (involved tolerance) 
  !                                                                   ! triangle/quadrangle etc.. is set in findElemType
  INTEGER                             ::       zone                   ! zone for zonal concept is given indirectly in the inifile
  INTEGER                             ::       nNodes                 ! total number of nodes for one Elem
  INTEGER                             ::       nCurvedNodes           ! Used for writing curveds to hdf5 mesh format
  INTEGER                             ::       ind                    ! unique Element index for each element on all processors
  INTEGER                             ::       tmp
  ! CHARACTER --------------------------------------------------------!
  ! LOGICAL ----------------------------------------------------------!
END TYPE tElem

TYPE tSide ! provides data structure for local side
  ! Derived data types -----------------------------------------------!
  TYPE(tBC),POINTER                   ::       BC                     ! pointer to boundary condition information
  TYPE(tNodePtr),POINTER              ::       node(:)                ! node pointer
  TYPE(tNodePtr),POINTER              ::       curvedNode(:)          ! ? 
  TYPE(tNodePtr),POINTER              ::       orientednode(:)        ! node pointer to oriented node used mainly in curved
  TYPE(tEdgePtr),POINTER              ::       Edge(:)                ! Edge pointer
  TYPE(tElem),POINTER                 ::       elem                   ! Local element pointer 
  TYPE(tSide),POINTER                 ::       connection             ! pointer to connected side
  TYPE(tSide),POINTER                 ::       nextElemSide           ! pointer to next element's side
  ! INTEGER ----------------------------------------------------------!
  INTEGER                             ::       nNodes                 ! total number of nodes on that side
  INTEGER                             ::       nCurvedNodes           ! Used for writing curveds to hdf5 mesh format
  INTEGER                             ::       LocSide                ! CGNS ordering of element sides 
  !                                                                   ! for blended sides
  INTEGER                             ::       curveIndex             ! IF curveIndex .NE. 0 we also have a numbered curved side
  INTEGER                             ::       ind                    ! index for grouping together curved sides
  INTEGER                             ::       tmp
  INTEGER                             ::       tmp2
  INTEGER                             ::       flip                   ! orientation of side-to-side connection
                                                                      ! (node corresponding to first neighbor node)
  ! CHARACTER --------------------------------------------------------!
  ! LOGICAL ----------------------------------------------------------!
  LOGICAL,ALLOCATABLE                 ::       edgeOrientation(:)     ! size: nEdges(=nNodes)
  LOGICAL                             ::       isCurved               ! true if side is curved
END TYPE tSide

TYPE tEdge ! provides data structure for local edge
  TYPE(tNodePtr)                      ::       Node(2)                ! pointer to node always 2
  TYPE(tNodePtr),POINTER              ::       CurvedNode(:)          ! pointer to interpolation nodes of curved sides
  TYPE(tEdge),POINTER                 ::       nextEdge               ! only used to assign edges 
END TYPE tEdge

TYPE tNode ! provides data structure for local node
  TYPE(tNormal),POINTER               ::       firstNormal            ! pointer to first normal of node
  TYPE(tEdge),POINTER                 ::       firstEdge              ! pointer to first normal of node
  REAL                                ::       x(3)                   ! node coordinates
  INTEGER                             ::       ind                    ! node counter
  INTEGER                             ::       tmp    ! ?
  INTEGER                             ::       refCount               ! In general nodes are used by more than 
  !                                                                   ! one side / element -> Node%refCount > 1
  !                                                                   ! Node%refCount = 0 means that node is not used any more
END TYPE tNode

TYPE tNormal
  REAL                                ::       normal(3)              ! Normals(nDim) normals vector of a node
  INTEGER,ALLOCATABLE                 ::       FaceID(:)              ! FaceID(maxFaceIDs) normal vector from CAD geometry
  TYPE(tNormal),POINTER               ::       prevNormal             ! Pointer to previous normal vector 
  TYPE(tNormal),POINTER               ::       nextNormal             ! Pointer to next normal vector 
END TYPE 

TYPE tNormalPtr
  TYPE(tNormal),POINTER               ::       np                     ! first Normal in list 
END TYPE 

! Used for connect 3d
TYPE tsuperNode ! provides data structure for nodes that are shared by "aSide" and "bSide". 
  !             ! For each shared node a new SuperNode is created and put in a list of
  !             ! SuperNodes that starts with "firstSuperNode".
  TYPE(tNode),POINTER                 ::       meshNode               ! pointer to node
  TYPE(tNode),POINTER                 ::       periodicNode           ! pointer to periodic node
  TYPE(tsuperNode),POINTER            ::       nextSuperNode          ! pointer to next SuperNode
  REAL                                ::       aCoeff(2)              ! Position of node on edge of aSide
  REAL                                ::       bCoeff(2)              ! Position of node on edge of bSide
  INTEGER                             ::       aEdge(2)               ! Node number of first/last edge on aSide
  INTEGER                             ::       bEdge(2)               ! Node number of first/last edge on bSide
  LOGICAL                             ::       periodic               ! Default: not periodic but ste to .TRUE. 
  !                                                                   ! for periodic boundary sides
END TYPE tsuperNode

TYPE tBC  ! container for boundary condition information
  INTEGER                             ::       BCtype                 !  CGNS boundary types which are mapped by the code
  !                                                                   !  = Null
  !                                                                   !  = BCExtrapolate          Used as periodic which 
  !                                                                   !                           doesent exist in CGNS (WHY??)
  !                                                                   !  = BCGeneral              General means exactfunction here
  !                                                                   !  = BCWallViscousHeatFlux  We have always adiabatic now
  !                                                                   !  = BCWallViscousIsothermal
  !                                                                   !  = BCInflowSupersonic
  !                                                                   !  = BCInflowSubsonic
  !                                                                   !  = BCOutflowSupersonic
  !                                                                   !  = BCOutflowSubsonic
  !                                                                   !  = BCWallInviscid
  !                                                                   !  = UserDefined
  !                                                                   !  = UserDefined
  INTEGER                             ::       BCstate                ! boundary state (from ref states)
  INTEGER                             ::       BCalphaind             ! ????
  INTEGER                             ::       BCIndex                ! index of BC in UserDefinedBoundary array
END TYPE tBC

! Private Part ---------------------------------------------------------------------------------------------------------------------
! Public Part ----------------------------------------------------------------------------------------------------------------------
INTEGER                        :: MeshMode               ! Mesh mode: 1-internal, cartesian  2-mesh from Gambit file
!                                                        !            3-mesh from CGNS file  4-mesh from ANSA file
INTEGER                        :: nZones=0               ! number of mesh zones in domain
INTEGER                        :: nMeshfiles=0           ! number of mesh files: each mesh file = one zone
INTEGER                        :: SplitMeshMode          ! how to read splitElemFile. default=meshMode, must be 3 (star) or 4(cgns)
CHARACTER(LEN=255),ALLOCATABLE :: MeshFileName(:)        ! filename(nMeshfiles) filename for the mesh
CHARACTER(LEN=255)             :: NormalVectFile         ! file with normals if using CAD-normals
CHARACTER(LEN=255)             :: SplitElemFile          ! file with subdivided surface mesh for curving
CHARACTER(LEN=255)             :: SpecElemFile           ! curved nodes file for CGNS(ICEM)
LOGICAL                        :: ConformConnect         ! If mesh is knwon to be conform, this switch enhances connect speed 
LOGICAL                        :: useBinary              ! read in special binary GAMBIT files
LOGICAL                        :: BugFix_ANSA_CGNS       ! for ANSA unstructured CGNS Ansa Files, to set Boundary Condition
                                                         ! PointList always to an ElementList, default is false
LOGICAL                        :: MeshInitDone=.FALSE.
LOGICAL                        :: checkElemJacobians     ! check if Jacobians are positiv over curved Elements 
! REAL -----------------------------------------------------------------!
!-----------------------------------------------------------------------------------------------------------------------------------
! GEOMETRY
!-----------------------------------------------------------------------------------------------------------------------------------
TYPE(tElem) ,POINTER           :: firstElem              ! pointer to first element in order to start a loop
TYPE(tElem) ,POINTER           :: firstSplitElem         ! pointer to first element in splitted elem list for curveds
LOGICAL                        :: doScale                ! scaling factor gt realtolerance
LOGICAL                        :: postScale              ! apply scaling after readin or before output
REAL                           :: MeshScale              ! scaling factor applied to Node Coordinates during read in
REAL                           :: SpaceQuandt            ! Characteristic length in the mesh. Used as tolerance 
REAL                           :: minDX                  ! smallest edge length
REAL                           :: maxDX(3)               ! Used for search mesh
REAL                           :: jacobianTolerance      ! smallest value of jacobian permitted (e.g. 1.e-16)
INTEGER                        :: nMeshElems    =0       ! number of elements in the mesh
INTEGER                        :: nInnerSides   =0       ! number of unique innner sides in the mesh 
INTEGER                        :: nBoundarySides=0       ! number of boundary sides in the mesh
INTEGER                        :: NodeCount=0,SideCount=0,ElemCount=0  ! Counter for nodes,sides and elements.
INTEGER                        :: nNodesElemSideMapping(8,6) ! mapping matrix for Elem side mappings following the CGNS standard
INTEGER                        :: ElemSideMapping(8,6,4)     ! mapping matrix for Elem side mappings following the CGNS standard
INTEGER                        :: TypeIndex(8)           ! typeIndex mapping of index for different element types
!                                                        !  used for urElem administration
INTEGER                        :: TypeIndex_surf(4)      ! typeIndex_surf(nNodes) determines the typeIndex of the Surface(nNodes)
!                                                        !
!-----------------------------------------------------------------------------------------------------------------------------------
! CURVED
!-----------------------------------------------------------------------------------------------------------------------------------
REAL                           :: minNormalAngle         ! cos of min angle between normals, below which edges in geometry are no 
                                                         ! longer resolved
INTEGER                        :: curvingMethod          ! parameter to be specified in ini file to choose the method to be used to
                                                         ! generate curved meshes (set zero curved meshes, otherwise curved
                                                         ! boundaries will be regenerated 
INTEGER                        :: normalsType            ! source of normals vectors used for curving (1=reconstr, 2=CAD, 3=exact)
INTEGER,ALLOCATABLE            :: ExactNormals(:)        ! for 3D spline patches, an analytical normal can be used 
INTEGER                        :: N                      ! polynomial degree of boundary element discretization 
INTEGER                        :: BoundaryOrder          ! N+1
INTEGER                        :: nSkip                  ! for structured CGNS readin
INTEGER                        :: nSkipZ                 ! for structured CGNS readin
INTEGER                        :: nBoundarySplines=0     ! Counter for boundary splines
INTEGER                        :: nPeriodicSplines=0     ! Counter for splines at periodic boundaries
INTEGER                        :: nInnerSplines   =0     ! Counter for inner splines
LOGICAL                        :: useCurveds             ! switch .TRUE.= we want to use curved elements (INPUT)
LOGICAL                        :: rebuildCurveds         ! switch .TRUE.= if curveds are already present in the mesh, delete them
                                                         ! and rebuild them using our methods
LOGICAL                        :: meshIsAlreadyCurved    ! flag: mesh is already curved (GMSH, HDF5, block CGNS)
!-----------------------------------------------------------------------------------------------------------------------------------
! CURVE GRID GENERATOR
!-----------------------------------------------------------------------------------------------------------------------------------
INTEGER          :: nElems(3),BCIndex(6)  ! ?
INTEGER          :: CurvedMeshType,WhichMapping   ! ?
REAL             :: R_0,R_INF,DY   ! R_0...radius of cylinder, R_INF...radius of domain, DY...extension in - and + y dir
INTEGER          :: StretchType(3) ! Type of Strechting: 1 (default): dx(i)=dx(i-1)*fac, 2. DxMaxToDxMin-> fac 3. bell shaped 
REAL             :: fac(3),fac2(3)  ! ?
REAL             :: DxMaxToDxMin(3)  ! ?
REAL             :: X0(3)   ! MeshType=1: origin of the physical domain  
REAL             :: DX(3)   ! MeshType=1: dimensions of the physical domain
REAL             :: XP(3,8) ! MeshType=2: 8 Corner Points 
!-----------------------------------------------------------------------------------------------------------------------------------
! BOUNDARY CONDITIONS
!-----------------------------------------------------------------------------------------------------------------------------------
REAL,POINTER                   :: VV(:,:)                ! vv(nDim,nVV) side connection vectors for periodic boundaries, the 
                                                         ! BcalphaInd of one side is the positive vector index the other one is 
                                                         ! the negative vector index (INPUT)
INTEGER                        :: nVV=0                  ! number of side connection vectors for periodic boundaries
INTEGER                        :: nUserDefinedBoundaries=0 ! number of boundary conditions given in inifile
INTEGER,ALLOCATABLE            :: BoundaryType(:,:)      ! 4 integer code for each bound. cond. (BCType,CurveInd,BCState,BCalpha) 
CHARACTER(LEN=255),ALLOCATABLE :: BoundaryName(:)        ! Name of boundary condition, size: nUserDefinedBoundaries
!-----------------------------------------------------------------------------------------------------------------------------------
! 2.5D MESH 
!-----------------------------------------------------------------------------------------------------------------------------------
REAL                           :: zLength                ! 2.5D mesh: lenght in z-direction
REAL                           :: dz                     ! MESH%zLength/MESH%nElemsZ
INTEGER                        :: MeshDim                ! Mesh dimesnions: does not need to be equal to nDim (2.5D mesh)
INTEGER                        :: n2dNodes=0             ! Number of nodes in the 2D mesh
INTEGER                        :: nElemsZ=0              ! 2.5D mesh: #elements in z-direction
INTEGER                        :: lowerZ_BC(4)           ! Boundary condition for the z=0. boundary
INTEGER                        :: upperZ_BC(4)           ! Boundary condition for the z=MESH%zLength boundary
INTEGER                        :: lowerZ_BC_Ind          ! Boundary condition index for the z=0. boundary
INTEGER                        :: upperZ_BC_Ind          ! Boundary condition index for the z=MESH%zLength boundary
!-----------------------------------------------------------------------------------------------------------------------------------
! zcorrection 
!-----------------------------------------------------------------------------------------------------------------------------------
LOGICAL                        :: doZcorrection 
REAL                           :: zstart 
LOGICAL                        :: zPeriodic 
!-----------------------------------------------------------------------------------------------------------------------------------
! Splitting of Elements 
!-----------------------------------------------------------------------------------------------------------------------------------
LOGICAL                        :: AdaptedMesh=.FALSE.    ! set to true if using splitToHex, nFineHexa  
LOGICAL                        :: SplitToHex             ! split all elements to hexas (works only for tetra,prism and hex) 
INTEGER                        :: nFineHexa              ! split all hexa mesh. nFineHexa=2-> 8 Elems nFineHexa=3 -> 27 Elems...
!-----------------------------------------------------------------------------------------------------------------------------------
! exact surface projection 
!-----------------------------------------------------------------------------------------------------------------------------------
LOGICAL                        :: doExactSurfProjection 
INTEGER                        :: nExactSurfFuncs
INTEGER,ALLOCATABLE            :: ExactSurfFunc(:)
!-----------------------------------------------------------------------------------------------------------------------------------
LOGICAL                        :: OrientZ
!-----------------------------------------------------------------------------------------------------------------------------------
! Post deformation functions deform a domain (typically [-1,1]^3) to arbirary other domain
!-----------------------------------------------------------------------------------------------------------------------------------
INTEGER                        :: MeshPostDeform ! Function index (off: 0) 
REAL                           :: PostDeform_R0  
REAL                           :: PostDeform_Rtorus  

! INTERFACES -----------------------------------------------------------------------------------------------------------------------
INTERFACE getNewElem
  MODULE PROCEDURE getNewElem
END INTERFACE

INTERFACE getNewSide
   MODULE PROCEDURE getNewSide
END INTERFACE

INTERFACE getNewEdge
  MODULE PROCEDURE getNewEdge
END INTERFACE

INTERFACE getNewNode
  MODULE PROCEDURE getNewNode
END INTERFACE

INTERFACE getNewQuad
  MODULE PROCEDURE getNewQuad
END INTERFACE

INTERFACE getNewBC
  MODULE PROCEDURE getNewBC
END INTERFACE

INTERFACE getNewSuperNode
  MODULE PROCEDURE getNewSuperNode
END INTERFACE

INTERFACE getNewNodeAndIndex
  MODULE PROCEDURE getNewNodeAndIndex
END INTERFACE

INTERFACE copyBC
  MODULE PROCEDURE copyBC
END INTERFACE

INTERFACE deleteElem
   MODULE PROCEDURE deleteElem
END INTERFACE

INTERFACE DisconnectElem
   MODULE PROCEDURE DisconnectElem
END INTERFACE

INTERFACE deleteSide
   MODULE PROCEDURE deleteSide
END INTERFACE

INTERFACE DisconnectSide
   MODULE PROCEDURE DisconnectSide
END INTERFACE

INTERFACE deleteEdge
  MODULE PROCEDURE deleteEdge
END INTERFACE

INTERFACE deleteNode
   MODULE PROCEDURE deleteNode
END INTERFACE

INTERFACE deleteBC
  MODULE PROCEDURE deleteBC
END INTERFACE

INTERFACE deleteSuperNodes
  MODULE PROCEDURE deleteSuperNodes
END INTERFACE

!===================================================================================================================================

CONTAINS
! GET NEW OBJECT -------------------------------------------------------------------------------------------------------------------
SUBROUTINE getNewElem(Elem)
!===================================================================================================================================
! Allocate and initialize new element
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
TYPE(tElem),POINTER,INTENT(OUT) :: Elem  ! New element
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
!===================================================================================================================================
ALLOCATE(Elem)
NULLIFY(Elem%Node,&
        Elem%prevElem,Elem%nextElem,Elem%firstSide)
NULLIFY(Elem%CurvedNode)
Elem%nCurvedNodes  = 0
Elem%ind           = 0
Elem%detT          = 0.
Elem%zone          = 0
ElemCount=ElemCount+1
END SUBROUTINE getNewElem


SUBROUTINE getNewSide(Side,nNodes)
!===================================================================================================================================
! Allocate and initialize new side "Side" with "nNodes" nodes
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
INTEGER,INTENT(IN)             :: nNodes ! Number of nodes of side "Side"
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
TYPE(tSide),POINTER,INTENT(OUT) :: Side   ! New side
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
INTEGER             :: iNode   ! ?
!===================================================================================================================================
ALLOCATE(Side)
Side%nNodes=nNodes
ALLOCATE(Side%orientedNode(nNodes),Side%Node(nNodes))
ALLOCATE(Side%Edge(nNodes),Side%EdgeOrientation(nNodes))
NULLIFY(Side%BC, &
        Side%Connection, &
        Side%Elem,Side%nextElemSide)
DO iNode=1,nNodes
  NULLIFY(Side%Node(iNode)%np,Side%orientedNode(iNode)%np,Side%Edge(iNode)%edp)
END DO
Side%isCurved     = .FALSE.
Side%curveIndex   = 0
Side%LocSide = 0
Side%ind          = 0
Side%tmp          = 0
Side%tmp2         = 0
Side%EdgeOrientation = .FALSE.
SideCount=SideCount+1
Side%nCurvedNodes=0
NULLIFY(Side%CurvedNode)
END SUBROUTINE getNewSide


SUBROUTINE getNewEdge(Edge,Node1,Node2)
!===================================================================================================================================
! Create "Edge" with nodes "Node1" and "Node2"
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
TYPE(tNode),POINTER,INTENT(IN) :: Node1 ! Node pointers
TYPE(tNode),POINTER,INTENT(IN) ::  Node2 ! Node pointers
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
TYPE(tEdge),POINTER,INTENT(INOUT) :: Edge         ! New edge
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
!===================================================================================================================================
ALLOCATE(Edge)
Edge%Node(1)%np=>Node1
Edge%Node(2)%np=>Node2
NULLIFY(Edge%nextEdge)
NULLIFY(Edge%curvedNode)
END SUBROUTINE getNewEdge


SUBROUTINE getNewNode(Node,refCount)
!===================================================================================================================================
! Allocate and initialize new node "Node"
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
INTEGER,OPTIONAL,INTENT(IN)    :: refCount ! number of sides / elements that use this node
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
TYPE(tNode),POINTER,INTENT(INOUT) :: Node     ! New node
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
!===================================================================================================================================
ALLOCATE(Node)
Node%ind=0
IF(PRESENT(refCount)) THEN
  Node%refCount=refCount
ELSE
  Node%refCount=0
END IF
NodeCount=NodeCount+1
NULLIFY(Node%firstNormal)
NULLIFY(Node%firstEdge)
END SUBROUTINE getNewNode



SUBROUTINE GetNewQuad(FirstElem_in,CornerNode)
!===================================================================================================================================
! Build new hexahedron for cartesian mesh.
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
TYPE(tElem),POINTER,INTENT(INOUT)             :: FirstElem_in
TYPE(tNodePtr),INTENT(IN)                  :: CornerNode(4)
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES
TYPE(tElem),POINTER             :: aElem 
INTEGER                         :: i
!===================================================================================================================================
CALL getNewElem(aElem)
aElem%nNodes=4
ALLOCATE(aElem%Node(aElem%nNodes))
DO i=1,4
  aElem%Node(i)%NP=>CornerNode(i)%NP
END DO

! Add elements to list
IF(.NOT.ASSOCIATED(FirstElem_in))THEN
  FirstElem_in=>aElem
ELSE
  aElem%nextElem          => FirstElem_in
  aElem%nextElem%prevElem => aElem
  FirstElem_in          => aElem
END IF
NULLIFY(aElem)
END SUBROUTINE GetNewQuad

SUBROUTINE getNewBC(BC)
!===================================================================================================================================
! Allocates and initializes new boundary condition "BC"
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
TYPE(tBC),POINTER,INTENT(OUT) :: BC  ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
!===================================================================================================================================
ALLOCATE(BC)
BC%BCType     = 0
BC%BCstate    = 0
BC%BCalphaInd = 0
BC%BCIndex    = 0
END SUBROUTINE getNewBC


SUBROUTINE getNewSuperNode(superNode,meshNode,aNode,bNode,aSide_nNodes,bSide_nNodes,aCoeff,bCoeff)
!===================================================================================================================================
! Allocate and initialize new SuperNode (used for connect 3d)
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
TYPE(tNode),POINTER,INTENT(IN)      :: meshNode     ! "Real" mesh node
INTEGER,INTENT(IN)                  :: aNode        ! Node number on aSide
INTEGER,INTENT(IN)                  :: bNode        ! Node number on bSide
INTEGER,INTENT(IN)                  :: aSide_nNodes ! Number of nodes of aSide
INTEGER,INTENT(IN)                  :: bSide_nNodes ! Number of nodes of bSide
REAL,INTENT(IN)                     :: aCoeff       ! Position of node on edge of aSide
REAL,INTENT(IN)                     :: bCoeff       ! Position of node on edge of bSide
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
TYPE(tsuperNode),POINTER,INTENT(OUT) :: superNode    ! New SuperNode
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
!===================================================================================================================================
ALLOCATE(superNode)
! Set mesh node
superNode%meshNode=>meshNode
NULLIFY(superNode%nextSuperNode,superNode%periodicNode)
! Default: not periodic
superNode%periodic=.FALSE.
! aSide edge numbers
superNode%aEdge(1)=aNode  ! First edge
superNode%aCoeff(1)=aCoeff  ! Position on first edge
! Set second edge - side vertices only
IF((aNode.NE.0).AND.(aCoeff.EQ.0.))THEN
  IF(aNode.EQ.1)THEN
    superNode%aEdge(2)=aSide_nNodes
  ELSE
    superNode%aEdge(2)=aNode-1
  END IF
  superNode%bCoeff(2)=1.
ELSE
  superNode%aEdge(2)=0
  superNode%aCoeff(2)=0.
END IF
! bSide edge numbers
superNode%bEdge(1)=bNode  ! First edge
superNode%bCoeff(1)=bCoeff  ! Position on first edge
! Set second edge - side vertices only
IF((bNode.NE.0).AND.(bCoeff.EQ.0.))THEN
  IF(bNode.EQ.1)THEN
    superNode%bEdge(2)=bSide_nNodes
  ELSE
    superNode%bEdge(2)=bNode-1
  END IF
  superNode%bCoeff(2)=1.
ELSE
  superNode%bEdge(2)=0
  superNode%bCoeff(2)=0.
END IF
END SUBROUTINE getNewSuperNode

SUBROUTINE getNewNodeAndIndex(Node,maxInd)
!===================================================================================================================================
! ?
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
TYPE(tNode),POINTER,INTENT(INOUT)            :: Node  ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
INTEGER,INTENT(INOUT)          :: maxInd  ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
!===================================================================================================================================
CALL getNewNode(node)
maxInd=maxInd+1
node%ind=maxInd
END SUBROUTINE getNewNodeAndIndex

SUBROUTINE copyBC(BCSide,Side)
!===================================================================================================================================
! ?
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
TYPE(tSide),POINTER,INTENT(IN)            :: BCSide  ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
TYPE(tSide),POINTER,INTENT(INOUT)            :: Side  ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
!===================================================================================================================================
CALL getNewBC(Side%BC) 
Side%BC%BCType     = BCSide%BC%BCType    
Side%CurveIndex    = BCSide%CurveIndex   
Side%BC%BCstate    = BCSide%BC%BCstate   
Side%BC%BCalphaInd = BCSide%BC%BCalphaInd
Side%BC%BCIndex    = BCSide%BC%BCIndex   
END SUBROUTINE copyBC

! DELETE OBJECTS -------------------------------------------------------------------------------------------------------------------
SUBROUTINE DeleteElem(firstElem,Elem)
!===================================================================================================================================
! Deletes element "Elem"
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
TYPE(tElem),POINTER,INTENT(INOUT)  :: firstElem ! First element in list
TYPE(tElem),POINTER,INTENT(INOUT)  :: Elem      ! Element that will be deleted
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
TYPE(tElem),POINTER  :: firstOut   ! ?
TYPE(tSide),POINTER  :: Side   ! ?
INTEGER              :: iNode   ! ?
!===================================================================================================================================
IF(.NOT.ASSOCIATED(Elem))RETURN
firstOut=>firstElem  ! Save first element
! Delete sides
DO WHILE(ASSOCIATED(Elem%firstSide))
  Side=>Elem%firstSide
  CALL deleteSide(Elem%firstSide,Side)
END DO
CALL DisconnectElem(firstOut,Elem)  ! Disconnect element from mesh
! Delete nodes
IF(ASSOCIATED(Elem%Node)) THEN
  DO iNode=1,Elem%nNodes
    CALL deleteNode(Elem%Node(iNode)%np)
  END DO
  DEALLOCATE(Elem%Node)
END IF
DEALLOCATE(Elem)
ElemCount=ElemCount-1
firstElem=>firstOut  ! Restore first element
END SUBROUTINE deleteElem


SUBROUTINE DisconnectElem(firstElem,Elem)
!===================================================================================================================================
! Disconnects element "Elem" from mesh. 
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
TYPE(tElem),POINTER,INTENT(INOUT) :: firstElem ! First element in list
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem      ! Element that will be Disconnected from mesh
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
TYPE(tElem),POINTER :: firstOut ! Local element pointer
!===================================================================================================================================
firstOut=>firstElem  ! Save first element
IF(.NOT.ASSOCIATED(Elem))RETURN
IF(.NOT.ASSOCIATED(Elem%prevElem))THEN  ! Elem is first element in mesh
  firstOut=>Elem%nextElem
  IF(ASSOCIATED(Elem%nextElem)) NULLIFY(Elem%nextElem%prevElem)
ELSEIF(.NOT.ASSOCIATED(Elem%nextElem))THEN
  NULLIFY(Elem%prevElem%nextElem)
ELSE
  Elem%prevElem%nextElem=>Elem%nextElem
  Elem%nextElem%prevElem=>Elem%prevElem
END IF
NULLIFY(Elem%prevElem,Elem%nextElem)
firstElem=>firstOut  ! Restore first element
END SUBROUTINE DisconnectElem


RECURSIVE SUBROUTINE DeleteSide(firstSide,Side)
!===================================================================================================================================
! Deletes side
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
TYPE(tSide),POINTER,INTENT(INOUT) :: firstSide ! First side of element
TYPE(tSide),POINTER,INTENT(INOUT) :: Side      ! Side that will be deleted
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
TYPE(tSide),POINTER :: firstOut   ! ?
INTEGER             :: iNode   ! ?
!===================================================================================================================================
IF(.NOT. ASSOCIATED(Side)) RETURN
firstOut=>firstSide  ! Save first side
CALL DisconnectSide(firstOut,Side)  ! Disconnect side from element
IF (ASSOCIATED(Side%BC)) CALL deleteBC(Side%BC)  ! Delete boundary conditions
SDEALLOCATE(Side%curvedNode)
! Delete nodes
IF(ASSOCIATED(Side%Node)) THEN
  DO iNode=1,Side%nNodes
    CALL deleteNode(Side%Node(iNode)%np)
  END DO
  DEALLOCATE(Side%Node)
END IF
! Delete oriented nodes
IF(ASSOCIATED(Side%orientedNode)) THEN
  DO iNode=1,Side%nNodes
    CALL deleteNode(Side%orientedNode(iNode)%np)
  END DO
  DEALLOCATE(Side%orientedNode)
END IF
IF (ASSOCIATED(Side%Connection)) THEN
  NULLIFY(Side%Connection%Connection)
END IF
NULLIFY(Side%elem)
SDEALLOCATE(Side)
SideCount=SideCount-1
firstSide=>firstOut  ! Restore first side
END SUBROUTINE DeleteSide


SUBROUTINE DisconnectSide(firstSide,Side)
!===================================================================================================================================
! Disconnect side from element
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
TYPE(tSide),POINTER,INTENT(INOUT) :: firstSide ! first side of element
TYPE(tSide),POINTER,INTENT(IN)    :: Side      ! Side that will be Disconnected
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
TYPE(tSide),POINTER :: aSide  ! ?
!===================================================================================================================================
IF(.NOT. ASSOCIATED(Side)) RETURN

IF(ASSOCIATED(firstSide,Side))THEN
  firstSide=>Side%nextElemSide
ELSE
  aSide=>firstSide
  DO WHILE(ASSOCIATED(aSide%nextElemSide))
    IF(ASSOCIATED(aSide%nextElemSide,Side))THEN
      aSide%nextElemSide=>Side%nextElemSide
      EXIT
    END IF
    aSide=>aSide%nextElemSide
  END DO
END IF
END SUBROUTINE DisconnectSide


SUBROUTINE deleteEdge(Edge)
!===================================================================================================================================
! Delete "Edge"
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
TYPE(tEdge),POINTER,INTENT(INOUT) :: Edge   ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
!===================================================================================================================================
SDEALLOCATE(Edge)
END SUBROUTINE deleteEdge


SUBROUTINE deleteNode(Node)
!===================================================================================================================================
! Delete node "Node"
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
TYPE(tNode),POINTER,INTENT(INOUT) :: Node   ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
!===================================================================================================================================
IF(.NOT. ASSOCIATED(Node)) RETURN
Node%refCount=Node%refCount-1   ! In general nodes are used by more than one side / element -> Node%refCount > 1
IF(Node%refCount .LE. 0)THEN  ! Node%refCount = 0 means that node is not used any more
!  DEALLOCATE(Node)
  NULLIFY(Node)
  NodeCount=NodeCount-1
END IF
END SUBROUTINE deleteNode

SUBROUTINE DeleteBC(BC)
!===================================================================================================================================
! Deletes boundary condition "BC"
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
TYPE(tBC),POINTER,INTENT(INOUT) :: BC   ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
!===================================================================================================================================
DEALLOCATE(BC)
NULLIFY(BC)
END SUBROUTINE deleteBC


SUBROUTINE deleteSuperNodes(firstSuperNode)
!===================================================================================================================================
! Deletes a list of SuperNodes
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
TYPE(tsuperNode),POINTER,INTENT(INOUT) :: firstSuperNode ! First super node in list
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
TYPE(tsuperNode),POINTER :: superNode   ! ?
!===================================================================================================================================
superNode=>firstSuperNode
DO WHILE(ASSOCIATED(superNode))
  firstSuperNode=>superNode%nextSuperNode
  NULLIFY(superNode%nextSuperNode,superNode%periodicNode,superNode%MeshNode)
  DEALLOCATE(superNode)
  superNode=>firstSuperNode
END DO
END SUBROUTINE deleteSuperNodes

END MODULE MOD_Mesh_Vars