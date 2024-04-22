# List of Parameters

```{table} List of Parameters
---
name: tab:List of Parameters
width: 50px
---
| Parameters         | Example      | Data Type    | Array Dim.   | Default Value | Description |
| :--------------   | :--------| :---------- | :---------- | :----------  | :--------- |
| `BCIndex`          | `BCIndex=(/1,2,3,4,5,6/)` | Int          |  6           |  MANDATORY                   |  The `BCIndex` parameter assigns a bondary condition to each surface of the cartesian box in order of the <br>surfaces. The number of a vector's component represents the nth boundary condition in order of its position <br>in the file. Hence, each position refers to the six box sides `(/z-,y-,x+,y+,x-,z+/)`. In this example six different <br>boundary conditions were assigned to the box surfaces. In case of a boundary condition defined on several <br>faces, the components belonging to these faces will be equal in the `BCIndex` vector. In case of multiple <br>cartesian boxes there are surfaces which coincide with other ones. To such surfaces no boundary condition <br>can assigned. Therefore, The number of the corresponding vector's component is set to 0. For example, if all <br>components of the parameter `BCIndex` are set to 0 `(/0,0,0,0,0,0/)` the box will be surrounded completely <br>by six other boxes so that no boundary condition can assigned to a single surface.             |
| `BoundaryName`       | `BoundaryName=BC_zminus`    | Str          |  1           |  MANDATORY                   |  Name of the boundary condition |
| `BoundaryOrder`      | `BoundaryOrder=5`           | Int          |  1           |  2                           |  Order of spline-reconstruction for curved surfaces  |
| `BoundaryType`       | `BoundaryType=(/4,0,0,-1/)` | Int          |  4           |  MANDATORY                   | The Type parameter consists of four components to set: `(/ Type, curveIndex, State, alpha /)`. For a single <br>cartesian box only the component Type has to be set to a arbitrary number > 1. <br>The other components have to be set 0;<br>For a periodic boundary condition the component Type has always to set to "1". The fourth component alpha <br>assigns a displacement vector `vv` and its direction (-/+) to the periodic boundary. An alpha of "-1" means that <br>the first ("1") defined displacement vector is assigned to this surface in the opposite direction ("-1") as he was <br>defined.For curved-structured meshes the component curveIndex has to set to "1".  |
| `checkElemJacobians` | `checkElemJacobians=T`      | Logical      |  1           |  MANDATORY<br>if useCurveds=T   | T (True): The Jacobian matrices are checked and scaled for each element <br> F (False): The Jacobian matrices are not checked and scaled for each element  |
| `conformConnect` | `conformConnect=T`      | Logical      |  1           |  T   | T (True): Fast connect for conforming meshes (coarse geometry-adapted search mesh) <br> F (False): Finer search mesh for non-conforming meshes   |
| `Corner` | `Corner=(/0.,0.,0. ,,1.,0.,0. ,,1.,1.,0. ,,0.,1.,0. ,,0.,0.,1. ,,1.,0.,1. ,,1.,1.,1. ,,0.,1.,1. /)`       | Real      |  24           |  MANDATORY<br>if Mode=1   | Coordinates of the box's corner nodes in the three-dimensional cartesian coordinate system. For proper <br>operation the nodes have to be in the order as illustrated in Figure {numref}`fig:CartmeshZone_3d` at the right and each node with <br>x,y,z coordinates. Furthermore the corner nodes define the six surfaces of the cartesian box, see {numref}`tab:Corner` below.|
| `curvingMethod` | `curvingMethod=1` | Int      |  1           | 0  | 0: No curving method activated.<br>1: Curving with normal vectors at surface points.<br>3: Curving with subdivided surface mesh.    |
| `Debugvisu` | `Debugvisu=T` | Logical      |  1           | F  | T (True): Files will be generated, which enable you to visualize the mesh and the boundary mesh for <br>debugging. These files can be found in the directory of the executed parameter.ini file.<br>F (False): Files for visualization will not generated during executing of the parameter.ini file.    |
| `DebugvisuLevel` | `DebugvisuLevel=1` | Int      |  1           | 0  | 0: Visualization of linear mesh and BC (default).<br>1: Visualization of linear mesh and BC and an additional curved surface visualization (_SplineSurf.\*) if `useCurveds=T`.<br>2: Visualization of linear mesh and BC and an additional curved volume visualization (_SplineVol.\*) if `useCurveds=T`.  |
| `doExactSurfProjection` | `doExactSurfProjection=F` | Logical      |  1           | F  | T (True): Project high order nodes onto analytical description of the surface.<br>F (False): No projection.  |
| `dozcorrection` | `dozcorrection=F` | Logical      |  1           | F  | T (True): All elements are aligned exactly along z-direction to suppress grid generator tolerances<br>F (False): Correction is disabled   |
| `DXmaxToDXmin` | `DXmaxToDXmin=(/6.,100.,1./)` | Real      |  3           | (/0.,0.,0./)   | This parameter specify the frame ratio of the maximum element size to the minimum element size for the <br>stretched element arrangement for curved-structured meshes. The value 1 is used typically for a deactivated stretching.    |
| `DZ` | `DZ=2` | Real      |  1           | MANDATORY<br>if Mode=11   | Dimension in z-direction: [-DZ,DZ]     |
| `elemtype` | `elemtype=108` | Int      |  1           | MANDATORY   | Type of cells/elements used for discretization:<br>104: Tetrahedron<br>105: Pyramid<br>106: Prism with triangular base<br>108: Hexahedron     |
| `ExactNormals` | `ExactNormals=(/1,1/)` | Int/Real      |  2           | (/0,0/)    | `(/ BC curveIndex, number of analytical formula/)`. Build in formulas (see src/mesh/curved.f90):<br>1: Sphere with origin (0,0,0)<br>2: Cylinder around z-axis      |
| `ExactSurfFunc` | `ExactSurfFunc=(/1,1/)` | Int      |  2           | (/0,0/)    | `(/curv index, surface function index/)`. Surface function:<br>1: Sphere with origin (0,0,0) and radius 0.5<br>2: Cylinder around zaxis and radius 0.5<br>3: NACA 0012 profile in xy plane with length 1 and origin in leading edge      |
| `fac` | `fac=(/1.5,2.2,10/)` | Real      |  3           | (/0,0,0/)    | Stretching factor of the elements in the direction of the turned local cylindrical coordinate axis. The value 0 <br>is only allowed if the stretching function for this axis is deactivated (`stretchType` vector component for this <br>axis is 0). A value of the intervall (0,1) means a decrease. The value 1 does not affect the element sizes and <br>means an deactivation of the stretching function for this axis. A value >1 means an increase of the element <br>size in the direction of the coordinate axis. Furthermore the stretching behaviour can be mirrored by adding <br>a negative sign to the values. If the `stretchType` vector component for an axis is 3, the factor will be multiplied <br>by -1 if the half distance is reached. In addition, fac has not the significant influence on the element arrangement <br>anymore but the parameter `DXmaxToDXmin`.      |
| `factor` | `factor=(/-1.75,1,-1.5/)` | Real      |  3           | (/0,0,0/)    | Stretching factor of the elements in the direction of the cartesian coordinate axes. The value 0 is only allowed <br>if the stretching function for this axis is deactivated (`stretchType` vector component for this axis is 0). A value <br>of the intervall (0,1) means a decrease. The value 1 does not affect the element sizes and means an deactivation <br>of the stretching function for this axis. A value >1 means an increase of the element size in the direction of <br>the coordinate axis. Furthermore the stretching behaviour can be mirrored by adding a negative sign to the <br>values. A combination with the parameter l0 ignores the element number of the defined box.      |
| `filename` | `filename=spheremesh` | Str      |  1           | MANDATORY<br>if Mode=3/4    | The name of the external mesh file. The belonging files have to be available in the directory of the executed <br>parameter file as *.cngs files.     |
| `jacobianTolerance` | `jacobianTolerance=1.E-16` | Real      |  1           |     | If Jacobian at a sampling point is lower than this value, a bilinear or curved element is considered broken.     |
| `l0` | `l0=(/0,1,5,0/)` | Real      |  3           | (/0,0,0/)     | The length of the first element of a stretched element arrangement of a cartesian box. Each component of <br>the vector stands for an axis of the cartesian coordinate system. The value 0 means an deactivation of the <br>stretching function for this axis. A negative sign defines the length of the first element of the other side of <br>the box. A combination with the parameter factor ignores the element number of the defined box.     |
| `lowerZ_BC,upperZ_BC` | `lowerZ_BC=(/2,1,0,0/)` | Int      |  4           | MANDATORY<br>if MeshDim=2     | Defines boundary conditions for 2D meshes which are extruded to 3D in the extrusion direction, same <br>structure as for `BoundaryType`, BC names will be `lowerZ_BC`,`upperZ_BC`   |
| `jacobianTolerance` | `jacobianTolerance=1.E-16` | Real      |  1           |      |  If Jacobian at a sampling point is lower than this value, a bilinear or curved element is considered broken.    |
| `MeshIsAlreadyCurved` | `MeshIsAlreadyCurved=T` | Logical      |  1           | F     |  T (True): Enables the agglomeration<br>F (False): Disables the agglomeration    |
| `meshscale` | `meshscale=0.001` | Real      |  1           | 1     |  Scales all input meshes by a factor     |
| `Meshtype` |  `Meshtype=3` | Int      |  1           | MANDATORY<br>if Mode=11     |  1: Cube (origin + dimensions)<br>2: Bilinear (8 points CGNS notation)<br>3: Curved (add WhichMapping)      |
| `Mode` | `Mode=1` | Int      |  1           | MANDATORY    |  Mode of mesh generation:<br>1: Cartmesh (intern)<br>3: CFD General Notation System (CGNS, extern)<br>4: STAR-CD (extern)<br>11: Curved-structured mesh       |
| `nAnalyze` |  `nAnalyze=5` | Int      |  1           | BoundaryOrder+2    |  Number of points used for mesh analysis, e.g. defines the number of points to sample an elements' Jacobian <br>if `checkElemJabians` is active.       |
| `nCurvedBoundaryLayers` |  `nAnalyze=3` | Int      |  1           | -1    |  If domain is curved, try to uncurve it and leave only the sides with BCs speciefied (i.e. `curveIndex >0`) curved<br>-1: deactivated,<br>0: only boundary is curved,<br>1: only first element is curved,<br>2-n: first n layers from the boundary are curved       |
| `nElems` |  `nElems=(/2,3,4/)` | Int      |  3           | MANDATORY<br>if Mode=1    |  Number of elements per box in the direction of the coordinate axes; `(/nElemX,nElemY,nElemZ/)`       |
| `nElemsZ` |  `nElems=1` | Int      |  1           | MANDATORY<br>if MeshIsAl.=T    | The number of curved elements        |
| `nExactNormals` |  `nExactNormals=1` | Int      |  1           | MANDATORY<br>if NormalsType=3    |Number of association between BC `CurveIndex` and analytical normal        |
| `nFineHexa` |  `nFineHexa=2` | Int      |  1           | 1    | Split all hexahedra in each direction by this factor, no refinement if `nFineHexa=1`         |
| `NormalsType` |  `NormalsType=2` | Int      |  1           | 1    | Source of the normal:<br>1: Reconstructed (no additional parameters, `CurveIndex` of BC must be >0).<br>2: NormalVectFile(point normal vector file) needed<br>3: Analytical normals        |
| `NormalVectFile` |  `NormalVectFile=filename` | Str      |  1           | MANDATORY<br>if NormalsType=2    | special file format associating surface points and normal vectors         |
| `nSkip` |   `nSkip=2` | Int      |  1           | MANDATORY<br>if MeshIsAl.=T    | Coarsen block-structured meshes:<br>1: no skip<br>2: use every second point <br>...          |
| `nSkipZ` |   `nSkipZ=2` | Int      |  1           | 1    |  If mesh is z-extruded a different skip can be given in z-direction.           |
| `NVisu` |   `NVisu=5` | Int      |  1           | 0    |  Number of visualization points per element edge if `useCurveds=T`.       |
| `nZones` |  `nZones=1` | Int      |  1           |  MANDATORY    |  Number of zones / cartesian boxes if Mode=1: The Parameter has to set to 1 if Mode=11     |
| `outputFormat` |  `outputFormat=1` | Int      |  1           |  0    |  0: Paraview vtk (ASCII)<br>1: Tecplot (ASCII)<br>2: CGNS (binary)     |
| `ProjectName` |  `ProjectName=cartbox` | Str      |  1           |   MANDATORY     |  Part of the output files' name which will be generated during the execution. These Files can be found in the <br>directory of the executed parameter.ini file.    |
| `R_0` |   `R_0=0.5` | Real      |  1           |   MANDATORY<br>if Mode=11     |  Inner radius of curved structured mesh. The Value 0 is not allowed.    |
| `R_INF` |  `R_INF=20` | Real      |  1           |   MANDATORY<br>if Mode=11     |  Outer radius of curved structured mesh     |
| `SpaceQuandt` |  `SpaceQuandt=1.0` | Real      |  1       | 0.1     |   Characteristic length of the mesh      |
| `SplitElemFile` |  `SplitElemFile=filename` | Str      |  1       | MANDATORY<br>if curvingMethod=3    |    Name of suvdivided surface mesh       |
| `SplitToHex` |   	`SplitToHex=T` | Logical      |  1       | F    |   If `SplitToHex=F` then tetrahedra and prisms are subdivided to yield pure hexahedral meshes. <br>Hexahedra are also subdivided to guarantee a conforming connection. <br>NOTE: Pyramids cannot be split, this function cannot be used with pyramids present in the mesh.        |
| `stretchType` |   `stretchType=(/3,1,0/)` | Int      |  3       | (/0,0,0/)     |   This parameter manages the (de)activation of the stretching functions for all axis. For this reason the parameter <br>is a vector with three components.<br>0: Stretching in direction of the axis is deactivated<br>1: Stretching in direction of the axis is activated<br>3: Stretching in direction of the axis is activated and from the second half of the mesh distance on the stretching <br>factor is multiplied by -1.     |
| `useCurveds` | `useCurveds=T` | Logical      |  1       | F     |   T (True): If curved boundaries are defined<br>F (False): If no curved boundaries are defined     |
| `vv` | `vv=(/0,0,1./)` | Real      |  3       |  (/0,0,0/)     |   The displacement vector has to specify in the three-dimensional cartesian coordinate system and has to be <br>normal to a surface the vector was assigned to. In addition the displacement vector has to show to the inside <br>of the cartesian box. In case of two parallel surface-planes, both with periodic boundary conditions, just one <br>displacement vector has to be defined. Therefore the different directions of the vectors can be compensated <br>by switching the sign of alpha, the fourth component of the `BoundaryType` vector.<br>The displacement vector has to be as long as the distance between the surfaces the vector was assigned to. <br>The index of a displacement vector is defined by the position of its definition like the parameter `BCIndex`. <br>Several definitions of boundary conditions between two definitions of displacement vectors will not affect <br>the index of the displacement vectors.     |
| `WhichMapping` | `WhichMapping=4` | Int      |  1       |  MANDATORY<br>if Meshtype=3     |   Type of mapping using 6 boundary faces to build the curved structured mesh:<br>3: Half cylinder<br>4: Full cylinder     |
| `zLength` |  `zLength=1.0` | Real      |  1       |  1    |  Change (3D) or define (2D) length of domain in z-direction.    |
| `zperiodic` |  `zperiodic=T` | Logical      |  1       |  F    |  T (True): The Boundary conditions z_plus und z_minus are set to periodic ones.<br>F (False): The Boundary conditions z_plus und z_minus remain non-periodic.    |
| `zstart` |  `zstart=0.` | Real      |  1       |  0    |  Change minimum z-coordinate  |
```

```{figure} figures/CartmeshZone_3d.jpg
---
name: fig:CartmeshZone_3d
width: 400px
align: left
---

The Cartesian Box.
```

```{table} Corner.
---
name: tab:Corner
---
  | Surface | Corner Nodes | Position in the Cart. coordinate system | 
  | :------ | :----------: | :---------------------------     |
  | 1       | 1-2-3-4      | z < 0 plane (z-)                 |
  | 2       | 1-2-5-6      | y < 0 plane (y-)                 |
  | 3       | 2-3-6-7      | x > 0 plane (x+)                 |
  | 4       | 3-4-7-8      | y > 0 plane (y+)                 |
  | 5       | 1-4-5-8      | x < 0 plane (x-)                 |
  | 6       | 5-6-7-8      | z > 0 (z+)                       |

```