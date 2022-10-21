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
MODULE MOD_Readin_GMSH
!===================================================================================================================================
! ?
!===================================================================================================================================
! MODULES
USE MOD_Globals
USE MOD_Mesh_Vars,ONLY:tBC
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
PRIVATE
!-----------------------------------------------------------------------------------------------------------------------------------
! GLOBAL VARIABLES 
!-----------------------------------------------------------------------------------------------------------------------------------
! Private Part ---------------------------------------------------------------------------------------------------------------------
! Public Part ----------------------------------------------------------------------------------------------------------------------
INTERFACE ReadGMSH
  MODULE PROCEDURE ReadGMSH
END INTERFACE

PUBLIC::ReadGMSH
!===================================================================================================================================

TYPE tBCTemp
  INTEGER               :: nodeInds(4) !tri+quad (lazy)
  INTEGER               :: curveIndex
  TYPE(tBC),POINTER     :: BC
  TYPE(tBCTemp),POINTER :: nextBC
END TYPE

TYPE tBCTempPtr
  TYPE(tBCTemp),POINTER :: bp
END TYPE

CONTAINS

FUNCTION GETNNODES(ElementType,bOrd)
!===================================================================================================================================
! Get nNodes from Element Type 
!===================================================================================================================================
! MODULES
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
INTEGER, INTENT(IN)                :: ElementType  ! ?
INTEGER, INTENT(IN)                :: bOrd  ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
INTEGER                            :: GETNNODES  ! ?
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES
!===================================================================================================================================
  SELECT CASE(ElementType)
  CASE(3)
    GETNNODES=3
  CASE(6)
    GETNNODES=bOrd*(bOrd+1)/2
  CASE(4,5)
    GETNNODES=4
  CASE(7)
    GETNNODES=bOrd*bOrd
  CASE(104,204)
    GETNNODES=4
  CASE(105,115,205)
    GETNNODES=5
  CASE(106,116,206)
    GETNNODES=6
  CASE(108,118,208)
    GETNNODES=8
  END SELECT
END FUNCTION GETNNODES

SUBROUTINE readGMSH()
!===================================================================================================================================
! Read mesh from GMSH ascii or binary file. Called by fillMesh.
! Read-in can be performed by just one or all processors
!===================================================================================================================================
! MODULES
USE MOD_Mesh_Vars,ONLY:tElem,tElemPtr,tSide,tSidePtr,tNode,tNodePtr
USE MOD_Mesh_Vars,ONLY:FirstElem
USE MOD_Mesh_Vars,ONLY:nMeshFiles,MeshFileName
USE MOD_Mesh_Vars,ONLY:nUserDefinedBoundaries,BoundaryName
USE MOD_Mesh_Vars,ONLY:getNewElem,getNewNode,getNewBC
USE MOD_Mesh_Vars,ONLY:MeshDim,n2dNodes
USE MOD_Readin_GMSH_Vars
USE MOD_ReadinTools,ONLY:TRYREAD
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
! nMeshFiles                : Number of mesh files (INI-File)
! MeshFileName(iFile)       : Filename of mesh file iFile (INI-File)
! nZones                    : Number of mesh zones (INI-File)
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
TYPE(tNodePtr),POINTER :: Nodes(:)  ! ?
TYPE(tElemPtr),POINTER :: Elems(:)  ! ?
TYPE(tElem),POINTER    :: aElem  ! ?
TYPE(tSide),POINTER    :: aSide  ! ?
TYPE(tBCTemp),POINTER  :: aBCTemp  ! ?
TYPE(tBCTempPtr),POINTER :: BCList(:)  ! ?
INTEGER                :: os,iFile  ! ?
INTEGER                :: i,iElem,nElems,iNode,nNodes
INTEGER                :: elemCount                         !> Counter for 3D elements
INTEGER                :: elemType                          !> Read-in element type from gmsh
INTEGER                :: nodeInds(1337)                    !> Node IDs per element
INTEGER                :: minInd,tempNodeInds(4)
INTEGER                :: iBC,whichDim,BCTag
LOGICAL                :: isBCSide,BCFound(nUserDefinedBoundaries),found,s  ! ?
CHARACTER(LEN=255)     :: BCName
INTEGER                 :: nPoints, nCurves, nSurf, nVolumes, dummy, nNodesInBlock, entityDim, entityTag
INTEGER                 :: nBCs_Entity, iTag, nElemsPerTag, asciiBinary, BCDim
INTEGER,ALLOCATABLE     :: MapBCToGmshTag(:)
REAL                    :: dummy_array(1:100), version
!===================================================================================================================================
WRITE(UNIT_stdOut,'(132("~"))')
CALL Timer(.TRUE.)
WRITE(UNIT_stdOut,'(A)')'Reading GMSH mesh...'
CALL buildTypes()

! Start reading mesh
DO iFile=1,nMeshFiles
  OPEN(UNIT   = 104,                  &
       FILE   = MeshFileName(iFile),  &
       STATUS = 'OLD',                &
       ACTION = 'READ',               &
       ACCESS = 'SEQUENTIAL',         &
       IOSTAT = os                    )
  WRITE(UNIT_stdOut,*)  'Reading mesh from ASCII file: ',TRIM(MeshFileName(iFile))
  ! Read version and other infos
  s=TRYREAD(104,'$MeshFormat')
  READ(104,*) version, asciiBinary, dummy
  WRITE(*,'(A,F0.1)') ' Version gmsh format: ', version
  IF(asciiBinary.NE.0) THEN
    CALL abort(__STAMP__, 'ERROR gmsh: Binary format not supported!')
  END IF
  s=TRYREAD(104,'$EndMeshFormat')

  ! Read-in of boundary conditions (in gmsh: physical groups)
  s=TRYREAD(104,'$PhysicalNames')
  ! Number of BCs and allocate mapping to BC and BCTag
  ! Format: numPhysicalNames(ASCII int)
  READ(104,*) nBCs_GMSH
  ALLOCATE(MapBCToGmshTag(nUserDefinedBoundaries))
  BCFound=.FALSE.
  DO iBC=1,nBCs_GMSH
    ! Format: dimension(ASCII int) physicalTag(ASCII int) "name"(127 characters max)
    READ(104,*) whichDim, BCTag, BCName
    IF(MeshDim.EQ.2) THEN
      BCDim = 1
    ELSE IF(MeshDim.EQ.3) THEN
      BCDim = 2
    END IF
    IF(whichDim.EQ.BCDim)THEN
      found=.FALSE.
      ! Mapping of gmsh boundary counter to boundary names given by user in hopr.ini
      DO i=1,nUserDefinedBoundaries
        IF(INDEX(TRIM(BCName),TRIM(BoundaryName(i))).NE.0) THEN
          found=.TRUE. 
          BCFound(i)=.TRUE.
          WRITE(*,*)'BC found: ',TRIM(BCName),' -->  mapped to: ',TRIM(BoundaryName(i)), ' with index: ', i
          ! Mapping of boundary counter to physical tag, defined (automatically) in gmsh
          MapBCToGmshTag(i) = BCTag
          EXIT
        END IF
      END DO
      IF(.NOT.found) CALL abort(__STAMP__, 'UserDefinedBoundary condition missing: '//TRIM(BCName),nUserDefinedBoundaries)
    ELSE
      CALL abort(__STAMP__, 'ERROR: Boundaries do not have the expected dimension!')
    END IF
  END DO
  s=TRYREAD(104,'$EndPhysicalNames')

  ! Read-in of elementary model entities containing the coordinates (0D) or dimensions (1D/2D/3D) and mapping to physical groups
  s=TRYREAD(104,'$Entities')
  ! Format: numPoints(size_t) numCurves(size_t) numSurfaces(size_t) numVolumes(size_t)
  READ(104,*) nPoints, nCurves, nSurf, nVolumes
  WRITE(*,*) 'Mesh with: ', nPoints, ' Points, ', nCurves, ' Curves', nSurf, ' Surfaces', nVolumes, ' Volumes'

  IF(nVolumes.EQ.0.AND.MeshDim.NE.2) THEN
    CALL abort(__STAMP__, 'ERROR: No volumes found or 2D mesh given (extrude mesh with MeshDim = 2 and zLength/nElemsZ)!')
  END IF

  ! Skip points
  DO i=1,nPoints
    READ(104,*)
  END DO
  ! Skip points and curves definition, depending whether the mesh is 2D or 3D
  IF(MeshDim.EQ.2) THEN
    ! Mapping from number of surfaces to BCTag
    ALLOCATE(MapEntityToBC(nCurves))
    MapEntityToBC = -1
    DO i=1,nCurves
      ! Format: curveTag minX minY minZ maxX maxY maxZ numPhysicalTags physicalTag numBoundingPoints pointTag
      ! Skipping the surfaceTag (equivalent to the i-variable) and the bounding box; nBCs_Entity defines the number of physicalTag(s);
      ! skipping the following bounding curves
      READ(104,*) dummy, dummy_array(1:6), nBCs_Entity, BCTag, dummy, dummy_array(1:dummy)
      ! Currently a surface cannot belong to multiple BCs
      IF(nBCs_Entity.GT.1) CALL abort(__STAMP__, 'ERROR: Curve is overdefined with more than one BC!')
      ! Compare the BCTag of the surface with the BCTag of the BC and map surface index to BC index
      DO iBC=1,nUserDefinedBoundaries
        IF(MapBCToGmshTag(iBC).EQ.BCTag) THEN
          MapEntityToBC(i) = iBC
        END IF
      END DO
    END DO
    ! Every surface has to be associated with a BC
    IF(ANY(MapEntityToBC.EQ.-1)) CALL abort(__STAMP__, 'ERROR: Curve is not associated with a BC!')
    DO i=1,nSurf
      READ(104,*)
    END DO
  ELSE IF(MeshDim.EQ.3) THEN
    DO i=1,nCurves
      READ(104,*)
    END DO
    ! Mapping from number of surfaces to BCTag
    ALLOCATE(MapEntityToBC(nSurf))
    MapEntityToBC = -1
    DO i=1,nSurf
      ! Format: surfaceTag minX minY minZ maxX maxY maxZ numPhysicalTags physicalTag numBoundingCurves curveTag
      ! Skipping the surfaceTag (equivalent to the i-variable) and the bounding box; nBCs_Entity defines the number of physicalTag(s);
      ! skipping the following bounding curves
      READ(104,*) dummy, dummy_array(1:6), nBCs_Entity, BCTag, dummy, dummy_array(1:dummy)
      ! Currently a surface cannot belong to multiple BCs
      IF(nBCs_Entity.GT.1) CALL abort(__STAMP__, 'ERROR: Surface is overdefined with more than one BC!')
      ! Compare the BCTag of the surface with the BCTag of the BC and map surface index to BC index
      DO iBC=1,nBCs_GMSH
        IF(MapBCToGmshTag(iBC).EQ.BCTag) THEN
          MapEntityToBC(i) = iBC
        END IF
      END DO
    END DO
    ! Every surface has to be associated with a BC
    IF(ANY(MapEntityToBC.EQ.-1)) CALL abort(__STAMP__, 'ERROR: Surface is not associated with a BC!')
  END IF
  ! Skip volume definitions
  DO i=1,nVolumes
    READ(104,*)
  END DO
  s=TRYREAD(104,'$EndEntities')

  ! Read-in of nodes, which are grouped into "entity blocks"
  s=TRYREAD(104,'$Nodes')
  ! Format: numEntityBlocks, numNodes, minNodeTag, maxNodeTag,
  READ(104,*) dummy, nNodes, dummy, dummy
  WRITE(*,*) 'Found', nNodes, ' nodes.'
  ALLOCATE(Nodes(nNodes))
  iNode = 1
  DO WHILE (iNode.LE.nNodes)
    ! Format: entityDim, entityTag, parametric(int; 0 or 1) numNodesInBlock
    READ(104,*) entityDim, entityTag, dummy, nNodesInBlock
    ! Skip over node tags
    DO i = 1, nNodesInBlock
      READ(104,*)
    END DO
    ! Get new node and store coordinates
    DO i = 1, nNodesInBlock
      ! Get new nodes
      CALL GetNewNode(Nodes(iNode)%np,0)
      Nodes(iNode)%np%ind = iNode
      READ(104,*) Nodes(iNode)%np%x
      iNode = iNode + 1
    END DO
  END DO
  s=TRYREAD(104,'$EndNodes')

  ! Initialize BCList pointer
  ALLOCATE(BCList(nNodes))
  DO i=1,nNodes
    NULLIFY(BCList(i)%bp)
  END DO

  ! Read elements (contains everything, points, lines, surfaces, hexahedra); elements are grouped by the dimension and physical groups
  s=TRYREAD(104,'$Elements')
  ! Format: numEntityBlocks, numElements, minElementTag, maxElementTag
  READ(104,*) dummy, nElems, dummy, dummy
  WRITE(*,*) 'Found', nElems, ' elements (points, lines, surfaces & volume elements).'
  ALLOCATE(Elems(nElems))
  ! Counter for actual 3D elements
  elemCount = 0
  ! Counter of total number of elements as defined by gmsh
  iElem = 1
  DO WHILE (iElem.LE.nElems)
    ! Read-in elements, which are sorted by the dimension (starting with points, 0) and grouped by tags, e.g. a surfaceTag contains
    ! multiple quad elements, usually all volume elements are within a single block
    ! Format: entityDim entityTag elementType nElems (points, lines, surfaces, hexahedra)
    READ(104,*) entityDim, iTag, elemType, nElemsPerTag
    ! Loop over the number of elements per tag
    IF(MeshDim.EQ.2) THEN
      ! Read-in of 1D and 2D elements
      entityDim = entityDim + 1
    END IF
    DO i = 1, nElemsPerTag
      ! Format: elementTag nodeTag(s)
      READ(104,*) dummy, nodeInds(1:GMSH_TYPES(3,elemType))
      SELECT CASE(entityDim)
      CASE(2)   ! Surface elements, boundary conditions, 2D (or curves for 2D meshes)
        CALL addToBCs(BCList,iTag,elemType,nodeInds)
      CASE(3)   ! Volume elements, 3D (or surface elements for 2D meshes)
        CALL buildElem(Elems(elemCount+1),elemCount,elemType,Nodes,nodeInds)
      END SELECT
      iElem = iElem + 1
    END DO
  END DO ! nElems

  IF(MeshDim.EQ.2) THEN
    WRITE(*,*) 'Found', elemCount, ' 2D elements.'
  ELSE
    WRITE(*,*) 'Found', elemCount, ' 3D elements.'
  END IF

  IF(elemCount.LT.1) CALL abort(__STAMP__,'ERROR gmsh read-in: No 3D elements found!')
  ! Assign Boundary Conditions
  DO iElem=1,elemCount
    aSide=>Elems(iElem)%ep%firstSide
    DO WHILE(ASSOCIATED(aSide))
      !get minimum index of side
      tempNodeInds=HUGE(1337)
      DO i=1,aSide%nNodes
        tempNodeInds(i)=aSide%node(i)%np%ind
      END DO
      minInd=MINVAL(tempNodeInds)
      ! get BC by minInd and check if primary nodes identical
      aBCTemp=>BCList(minInd)%bp
      isBCSide=.FALSE.
      DO WHILE(ASSOCIATED(aBCTemp))
        isBCSide=.TRUE.
        DO i=1,aSide%nNodes
          IF(COUNT(aBCTemp%nodeInds.EQ.tempNodeInds(i)).NE.1)THEN
            isBCSide=.FALSE.
            EXIT
          END IF
        END DO
        IF(isBCSide) EXIT
        aBCTemp=>aBCTemp%nextBC
      END DO
      IF(isBCSide)THEN
        aSide%curveIndex=aBCTemp%curveIndex
        aSide%BC=>aBCTemp%BC
      END IF
      aSide=>aSide%nextElemSide
    END DO
  END DO
  
  ! Build Pointer list from elements
  aElem=>Elems(1)%ep
  firstElem=>aElem
  DO iElem=2,elemCount
    aElem%nextElem=>Elems(iElem)%ep
    Elems(iElem)%ep%prevElem=>aElem
    aElem=>Elems(iElem)%ep
  END DO
  CLOSE(104)
! remember to delete all stuff (BCList etc.
  DO iNode=1,nNodes !throw away  
    IF (Nodes(iNode)%np%refCount.EQ.0) DEALLOCATE(Nodes(iNode)%np)
    DO WHILE(ASSOCIATED(BCList(iNode)%bp))
      aBCTemp=>BCList(iNode)%bp
      BCList(iNode)%bp=>aBCTemp%nextBC
      DEALLOCATE(aBCTemp)
    END DO
  END DO
  DEALLOCATE(Nodes,Elems,BCList)

  ! Set total number of 2d nodes. This is needed to generate unique node indices in fill25DMesh.
  IF(MeshDim.EQ.2) n2dNodes=nNodes

  IF(MeshDim.EQ.3) THEN
    DO i=1,nUserDefinedBoundaries
      IF(.NOT.BCFound(i)) CALL abort(__STAMP__,&
              'One or more userdefined boundary conditions specified in ini file has not been found.',999,999.)
    END DO
  END IF
END DO
WRITE(UNIT_stdOut,'(132("~"))')
END SUBROUTINE readGMSH

SUBROUTINE addToBCs(BCList,iTag,gmshElemType,nodeInds)
!===================================================================================================================================
!> Assigning the nodes of 1D/2D elements to the surface tag (iTag), which has been mapped to a BC previously
!===================================================================================================================================
! MODULES
USE MOD_Mesh_Vars,ONLY:BoundaryType
USE MOD_Mesh_Vars,ONLY:getNewElem,tBC
USE MOD_Readin_GMSH_Vars
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
INTEGER,INTENT(IN)         :: iTag                                  !> Surface tag
INTEGER,INTENT(IN)         :: gmshElemType                          !> Surface tag
INTEGER,INTENT(IN)         :: nodeInds(GMSH_TYPES(1,gmshElemType))  !> only take primary nodes of side
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
TYPE(tBCTempPtr),POINTER,INTENT(INOUT) :: BCList(:)  !> Pointer structure, associating nodes with a boundary
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
INTEGER                    :: i,iBC,minInd
TYPE(tBCTemp),POINTER      :: aBCTemp
!-----------------------------------------------------------------------------------------------------------------------------------

iBC = MapEntityToBC(iTag)

ALLOCATE(aBCTemp)
DO i=1,GMSH_TYPES(1,gmshElemType) !primary nodes
  aBCTemp%nodeInds(i)=nodeInds(i)
END DO
ALLOCATE(aBCTemp%BC)
! BCType
aBCTemp%BC%BCType    =BoundaryType(iBC,1)
! curveIndex
aBCTemp%curveIndex   =BoundaryType(iBC,2)
! BCState
aBCTemp%BC%BCState   =BoundaryType(iBC,3)
! BCAlphaInd
aBCTemp%BC%BCAlphaInd=BoundaryType(iBC,4)
! BCInd
aBCTemp%BC%BCIndex   =iBC

minInd=MINVAL(nodeInds)
IF(ASSOCIATED(BCList(minInd)%bp))THEN
  aBCTemp%nextBC => BCList(minInd)%bp
  BCList(minInd)%bp => aBCTemp
ELSE
  BCList(minInd)%bp => aBCTemp
  NULLIFY(aBCTemp%nextBC)
END IF
END SUBROUTINE

SUBROUTINE buildElem(elem,elemCount,gmshElemType,Nodes,nodeInds)
!===================================================================================================================================
!> Assigning the nodes of a 2D/3D element
!===================================================================================================================================
! MODULES
USE MOD_Basis_Vars,ONLY:TetraMap,PyraMap,PrismMap,HexaMap
USE MOD_Mesh_Vars,ONLY:tElem,tElemPtr,tSide,tNode,tNodePtr
USE MOD_Mesh_Vars,ONLY:N, MeshDim
USE MOD_Mesh_Vars,ONLY:getNewElem,getNewBC
USE MOD_Mesh_Vars,ONLY:useCurveds,rebuildCurveds
USE MOD_Mesh_Basis,ONLY:createSides
USE MOD_Readin_GMSH_Vars,ONLY:bOrd,getGMSHVolumeMapping,GMSH_TYPES
USE MOD_Readin_GMSH_Vars,ONLY:tetMapGMSH,pyrMapGMSH,priMapGMSH,hexMapGMSH
USE MOD_Readin_GMSH_Vars,ONLY:tetMapCGNSToGMSH,pyrMapCGNSToGMSH,priMapCGNSToGMSH,hexMapCGNSToGMSH
USE MOD_Readin_GMSH_Vars,ONLY:quadMapCGNSToGMSH
! IMPLICIT VARIABLE HANDLING
IMPLICIT NONE
!-----------------------------------------------------------------------------------------------------------------------------------
! INPUT VARIABLES
INTEGER,INTENT(IN)                  :: gmshElemType
TYPE(tNodePtr),POINTER,INTENT(IN)   :: Nodes(:)
INTEGER,INTENT(IN)                  :: nodeInds(GMSH_TYPES(3,gmshElemType))
!-----------------------------------------------------------------------------------------------------------------------------------
! OUTPUT VARIABLES
TYPE(tElemPtr),INTENT(OUT) :: elem
INTEGER, INTENT(INOUT)     :: elemCount
!-----------------------------------------------------------------------------------------------------------------------------------
! LOCAL VARIABLES 
INTEGER                    :: i
!-----------------------------------------------------------------------------------------------------------------------------------

IF (bOrd .EQ.0) THEN
  bOrd = GMSH_TYPES(4,gmshElemType)+1
  IF ((bOrd .NE. N+1).AND.useCurveds.AND..NOT.rebuildCurveds) & 
    CALL abort(__STAMP__,&
    'Mesh boundary order not equal to boundary order from ini file! Mesh order: ',N+1)
  CALL getGMSHVolumeMapping()
ELSE
  IF (bOrd.NE.GMSH_TYPES(4,gmshElemType)+1) &
    CALL abort(__STAMP__,&
    'All elements in the mesh are required to have the same boundary order!')
END IF

! Advance number of 3D elements
elemCount = elemCount + 1 ! element is valid, raise number of mesh elements by one
CALL getNewElem(elem%ep)
elem%ep%ind    = elemCount
elem%ep%Type   = GMSH_TYPES(2,gmshElemType)
elem%ep%nNodes = GETNNODES(GMSH_TYPES(2,gmshElemType),bOrd)
ALLOCATE(Elem%ep%Node(elem%ep%nNodes))

DO i=1,elem%ep%nNodes
  SELECT CASE(elem%ep%nNodes)
  CASE(4)
    IF(MeshDim.EQ.2) THEN
      elem%ep%node(i)%np => Nodes(nodeInds(quadMapCGNSToGMSH(i)))%np
    ELSE
      elem%ep%node(i)%np => Nodes(nodeInds(TetMapCGNSToGMSH(i)))%np
    END IF
  CASE(5)
    elem%ep%node(i)%np => Nodes(nodeInds(PyrMapCGNSToGMSH(i)))%np
  CASE(6)
    elem%ep%node(i)%np => Nodes(nodeInds(PriMapCGNSToGMSH(i)))%np
  CASE(8)
    elem%ep%node(i)%np => Nodes(nodeInds(HexMapCGNSToGMSH(i)))%np
  CASE DEFAULT
    STOP 'Unknown element type!'
  END SELECT
  elem%ep%node(i)%np%refCount = elem%ep%node(i)%np%refCount+1
END DO
CALL createSides(elem%ep,.TRUE.)

! assign curved elements if present, if curveds should be used and not rebuilt using our methods
IF(useCurveds .AND. (bOrd.GT.2) .AND.(.NOT.rebuildCurveds))THEN
  IF(MeshDim.EQ.2) CALL abort(__STAMP__, 'ERROR in Gmsh Read-in: Extruding high-order 2D meshes not implemented!')
  ALLOCATE(elem%ep%curvedNode(GMSH_TYPES(3,gmshElemType)))
  elem%ep%nCurvedNodes=GMSH_TYPES(3,gmshElemType)
  DO i=1,GMSH_TYPES(3,gmshElemType)
    SELECT CASE(elem%ep%nNodes)
    CASE(4)
      elem%ep%curvedNode(i)%np => Nodes(nodeInds(tetMapGMSH(tetraMap(i,1),tetraMap(i,2),tetraMap(i,3))))%np
    CASE(5)
      STOP 'High order pyramids not implemented yet for GMSH!'
      elem%ep%curvedNode(i)%np => Nodes(nodeInds(pyrMapGMSH(pyraMap(i,1),pyraMap(i,2),pyraMap(i,3))))%np
    CASE(6)
      STOP 'High order prisms not implemented yet for GMSH!'
      elem%ep%curvedNode(i)%np => Nodes(nodeInds(priMapGMSH(prismMap(i,1),prismMap(i,2),prismMap(i,3))))%np
    CASE(8)
      elem%ep%curvedNode(i)%np => Nodes(nodeInds(hexMapGMSH(hexaMap(i,1),hexaMap(i,2),hexaMap(i,3))))%np
    END SELECT
    !print*, tetraMap(i,1),tetraMap(i,2),tetraMap(i,3)
    !print*, elem%ep%curvedNode(i)%np%x
    !print*, tetMapGMSH(tetraMap(i,1),tetraMap(i,2),tetraMap(i,3))
    elem%ep%curvedNode(i)%np%refCount=elem%ep%curvedNode(i)%np%refCount+1
  END DO
END IF
END SUBROUTINE buildElem


END MODULE MOD_Readin_GMSH
