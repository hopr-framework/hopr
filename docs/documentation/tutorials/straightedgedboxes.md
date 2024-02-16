# Straight-Edged Boxes
HOPR has several simple inbuilt mesh generators.
<figure class="align-center" id="fig-cartbox-multiple-stretch-mesh">
    <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_multiple_stretch_mesh.png"><img alt="../../../tutorials/figures/Cartbox_multiple_stretch_mesh.png" src="../../../tutorials/figures/Cartbox_multiple_stretch_mesh.png" style="width: 700px;" /></a>
    <figcaption>
    <p><span class="caption-number">Fig. 1.1 </span><span class="caption-text">HOPR output: Mesh of multiple cartesian boxes with a stretched element arrangement.</span><a class="headerlink" href="#fig-cartbox-multiple-stretch-mesh" title="Permalink to this image"></a></p>
    </figcaption>
</figure>

## Cartesian Box
This tutorial shows how to generate a simple mesh of a cubical box and the definition of boundary conditions.
The parameter file can be found in

    tutorials/1-01-cartbox/parameter.ini

See {ref}`tutorials/straightedgedboxes:Cartesian Box: Exemplary Variations of Boundary Conditions` for cases with different boundary conditions.

### Cartesian Box: Description of Parameters
In the following all parameters of the parameter file are explained. This description consists only of parameters which are necessary to generate a cartesian box mesh. A description of all parameters can be found in {ref}`userguide/parameters:List of Parameters`. 

```{table} Parameters Cartesian Box.
---
name: tab:Parameters Cartesian Box
---
  | Parameters      |Setting                | Description | 
  | :------         | :----------:          | :---------------------------     |
  | `ProjectName`   | `cartbox `            |  Part of the output files' name which will be generated during the execution (cartbox_mesh.h5, cartbox_Debugmesh.dat, cartbox_Debugmesh_BC.dat, ...) . <br>These Files can be found in the directory of the executed parameter.ini file.                  |
  | `Debugvisu`     | `T`                   | T (true) : The files *_Debugmesh.dat and *_Debugmesh_BC.dat will be generated, which enable you to visualize the mesh and the boundary mesh for debugging. <br>These Files can be found in the directory of the executed parameter.ini file<br>F (False): Files for visualization (cartbox_Debugmesh.dat, cartbox_Debugmesh_BC.dat) will not generated during executing of the parameter.ini file.                  |
  | `Mode`          | `1`                   | Mode of mesh generation; 1: Cartmesh (intern), 3: CFD General Notation System (CGNS, extern)                 |
  | `nZones`        | `1`                   | Number of cartesian boxes             |
  | `Corner`        | `(/0.,0.,0. ,,1.,0.,0. ,,1.,1.,0. ,,0.,1.,0. ,,0.,0.,1. ,,1.,0.,1. ,,1.,1.,1. ,,0.,1.,1. /)`      | Coordinates of the box's corner nodes in the three-dimensional cartesian coordinate system. For proper operation the nodes have to be in the order as illustrated in {numref}`fig:CartmeshZone_3d`<br> at the right and each node with x,y,z coordinates. Furthermore the corner nodes define the six surfaces of the cartesian box, see {numref}`tab:Corner`.                 |
  | `nElems`        | `(/2,3,4/)`           | Number of elements per box in the direction of the cartesian coordinate axes; `(/nElemX,nElemY,nElemZ/)`                       |
  | `BCIndex`       | `(/1,2,3,4,5,6/)`     |  The `BCIndex` parameter assigns a bondary condition to each surface of the cartesian box in order of the surfaces. The number of a vector's component represents the nth boundary<br> condition in order of its position in the file. Hence, each position refers to the six box sides `(/z-,y-,x+,y+,x-,z+/)`. Here, six different boundary conditions were assigned to the box surfaces. <br>In case of a boundary condition defined on several faces, the components belonging to these faces will be equal in the BCIndex vector, see {ref}`tutorials/straightedgedboxes:Cartesian Box: Exemplary Variations of Boundary Conditions` <br>for details.                        |
  | `elemtype`      | `108`                 | Type of cells/elements used for discretization; 104: Tetrahedron, 105: Pyramid, 106:Prism with triangular base, 108: Hexahedron                       |
  | `BoundaryName`  | `BC_zminus`           | Name of the boundary condition                       |
  | `BoundaryType`  | `(/4,0,0,0/)`         | For each boundary condition two parameters are mandatory, the Name and the Type. The Type parameter consists of four components to set: (/ Type, curveIndex, State, alpha /). <br>For a single cartesian box only the component Type has to be set to a arbitrary number > 1. The other components have to be set 0; The further description of the components can be <br>found in the next tutorials or in the {ref}`userguide/parameters:List of Parameters`.                        |
```
### Cartesian Box: Boundary Conditions and Sketch
<a class="reference internal" href="#fig-cartbox-sketch"><span class="std std-numref">Fig. 1.2</span></a> shows the sketch of the current problem. On the left side, however, one can see an excerpt of the parameter file which deals with the boundary conditions. In this code's excerpt some text elements are colored to show the connection between the surfaces and their assigned boundary conditions. The same colors are used for the visualization below. 

<table align="center">
  <tr>
    <td>
        <figure id="fig-cartbox-sketch">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_sketch.jpg"><img alt="../../../tutorials/figures/Cartbox_sketch.jpg" src="../../../tutorials/figures/Cartbox_sketch.jpg" style="height: 300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.2 </span><span class="caption-text">Sketch of the current problem</span><a class="headerlink" href="#fig-cartbox-sketch" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure class id="fig-cartbox-ini">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_ini.jpg"><img alt="../../../tutorials/figures/Cartbox_ini.jpg" src="../../../tutorials/figures/Cartbox_ini.jpg" style="height: 300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.3 </span><span class="caption-text">Cartesian Box Boundary Conditions ini-File</span><a class="headerlink" href="#fig-cartbox-ini" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

The index of the first component of the `BCIndex` vector `1` says that the boundary condition on position one, `BC_zminus`, is assigned to the first surface. Furthermore, the index of the second component of the BCIndex vector `2` means that the second boundary condition `BC_yminus` is assigned to the second surface of the cartesian box. The other boundary conditions are assigned on the same way to their surfaces. For more examples go to {ref}`tutorials/straightedgedboxes:Cartesian Box: Exemplary Variations of Boundary Conditions`. 

### Cartesian Box: Output Visualization
If there is a need for assistance of visualizing the HOPR output visit {ref}`tutorials/index_visualization:Visualization`. 

<h4>Mesh<a class="headerlink" href="#mesh-unlisted" title="Permalink to this heading"></a></h4>

This is a visualization of the `cartbox_Debugmesh.dat` file.
<figure class="align-center" id="fig-cartbox-mesh">
<a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_mesh.jpg"><img alt="../../../tutorials/figures/Cartbox_mesh.jpg" src="../../../tutorials/figures/Cartbox_mesh.jpg" style="width: 25%;" /></a>
<figcaption>
<p><span class="caption-number">Fig. 1.4 </span><span class="caption-text">Mesh of the cartesian box</span><a class="headerlink" href="#fig-cartbox-mesh" title="Permalink to this image"></a></p>
</figcaption>
</figure>

<h4>Boundary Conditions<a class="headerlink" href="#boundary-conditions-unlisted" title="Permalink to this heading"></a></h4>

This is a visualization of the `cartbox_Debugmesh_BC.dat`file. The colors of the surfaces represent the boundary conditions and are the same as in the excerpt of the parameter file.

<table align="center" style="width:100%">
  <tr>
    <td style="width:33%">
        <figure id="fig-cartbox-bc1">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_BC1.jpg"><img alt="../../../tutorials/figures/Cartbox_BC1.jpg" src="../../../tutorials/figures/Cartbox_BC1.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.5 </span><span class="caption-text">Boundary condition 1 (BC_wall) is assigned to surface 1</span><a class="headerlink" href="#fig-cartbox-bc1" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-cartbox-bc2">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_BC2.jpg"><img alt="../../../tutorials/figures/Cartbox_BC2.jpg" src="../../../tutorials/figures/Cartbox_BC2.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.6 </span><span class="caption-text">Boundary condition 2 (BC_inflow) is assigned to surface 2</span><a class="headerlink" href="#fig-cartbox-bc2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-cartbox-bc3">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_BC3.jpg"><img alt="../../../tutorials/figures/Cartbox_BC3.jpg" src="../../../tutorials/figures/Cartbox_BC3.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.7 </span><span class="caption-text">Boundary condition 3 (BC_outflow) is assigned to surface 3</span><a class="headerlink" href="#fig-cartbox-bc3" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

<table align="center" style="width:100%">
  <tr>
    <td style="width:33%">
        <figure id="fig-cartbox-bc4">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_BC4.jpg"><img alt="../../../tutorials/figures/Cartbox_BC4.jpg" src="../../../tutorials/figures/Cartbox_BC4.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.8 </span><span class="caption-text">Boundary condition 4 (BC_yplus) is assigned to surface 4</span><a class="headerlink" href="#fig-cartbox-bc4" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-cartbox-bc5">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_BC5.jpg"><img alt="../../../tutorials/figures/Cartbox_BC5.jpg" src="../../../tutorials/figures/Cartbox_BC5.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.9 </span><span class="caption-text">Boundary condition 5 (BC_xminus) is assigned to surface 5</span><a class="headerlink" href="#fig-cartbox-bc5" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-cartbox-bc6">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_BC6.jpg"><img alt="../../../tutorials/figures/Cartbox_BC6.jpg" src="../../../tutorials/figures/Cartbox_BC6.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.10 </span><span class="caption-text">Boundary condition 6 (BC_zplus) is assigned to surface 6</span><a class="headerlink" href="#fig-cartbox-bc6" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

### Cartesian Box: Exemplary Variations of Boundary Conditions
For a better understanding of the interaction between the parameter `BCIndex` and the declaration of the boundary conditions two different examples are presented below. Both examples are slight variations of the tutorial Cartesian Box in that way, that there are just three different boundary conditions: `WALL`, `INFLOW` and `OUTFLOW`. This means that components belonging to the surfaces will be equal in the `BCIndex` vector (see {ref}`userguide/parameters:List of Parameters`).

#### Example 1

<h4>Parameters and Sketch<a class="headerlink" href="#parameters-unlisted" title="Permalink to this heading"></a></h4>

The parameterfile of this example can be found in 

    /tutorials/1-01-cartbox/parameter_ex1.ini

<a class="reference internal" href="#fig-cartbox-ex1-sketch"><span class="std std-numref">Fig. 1.11</span></a> shows the sketch of the current problem and <a class="reference internal" href="#fig-cartbox-ex1-ini"><span class="std std-numref">Fig. 1.12</span></a> shows an excerpt of the parameter file which deals with the boundary conditions. In this code's excerpt some text elements are colored to show the connection between the surfaces and their assigned boundary conditions. The same colors are used for the visualization below.

The first four components of the `BCIndex` vector are equal. The index of these components `1` says that the boundary condition on position one, `BC_wall`, is assigned to the surfaces one to four. Furthermore, the fifth component of the `BCIndex` vector with the index `2` means that the second boundary condition `BC_inflow` is assigned to the fifth surface of the cartesian box. At least the third boundary condition `BC_outflow` is assigned to the the sixth surface. Therefore, the last or the sixth component of the `BCIndex` vector is set to `3`. 

<table align="center">
  <tr>
    <td>
        <figure id="fig-cartbox-ex1-sketch">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_ex1_sketch.jpg"><img alt="../../../tutorials/figures/Cartbox_ex1_sketch.jpg" src="../../../tutorials/figures/Cartbox_ex1_sketch.jpg" style="height: 300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.11 </span><span class="caption-text">Sketch of Example 1</span><a class="headerlink" href="#fig-cartbox-ex1-sketch" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure class id="fig-cartbox-ex1-ini">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_ex1_ini.jpg"><img alt="../../../tutorials/figures/Cartbox_ex1_ini.jpg" src="../../../tutorials/figures/Cartbox_ex1_ini.jpg" style="height: 300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.12 </span><span class="caption-text">Cartesian Box example 1 Boundary Conditions ini-File</span><a class="headerlink" href="#fig-cartbox-ex1-ini" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

<h4>Output Vizualisation<a class="headerlink" href="#output-and-vizualisation-unlisted" title="Permalink to this heading"></a></h4>

If there is a need for assistance of visualizing the HOPR output visit {ref}`tutorials/index_visualization:Visualization`. 

<h4>Mesh<a class="headerlink" href="#mesh-unlisted" title="Permalink to this heading"></a></h4>

This is a visualization of the `cartbox_ex1_Debugmesh.dat` file.
<figure class="align-center" id="fig-cartesianboxsketchex1mesh">
  <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_ex1_mesh.jpg"><img alt="../../../tutorials/figures/Cartbox_ex1_mesh.jpg" src="../../../tutorials/figures/Cartbox_ex1_mesh.jpg" style="width: 400px;" /></a>
  <figcaption>
  <p><span class="caption-number">Fig. 1.13 </span><span class="caption-text">Mesh of the cartesian box</span><a class="headerlink" href="#fig-cartesianboxsketchex1mesh" title="Permalink to this image"></a></p>
  </figcaption>
</figure>

<h4>Boundary Conditions<a class="headerlink" href="#boundary-conditions-unlisted" title="Permalink to this heading"></a></h4>

This is a visualization of the `cartbox_ex1_Debugmesh_BC.dat` file. The colors of the surfaces represent the boundary conditions and are the same as in the excerpt of the parameter file. 

<table align="center" style="width:100%">
  <tr>
    <td style="width:33%">
        <figure id="fig-cartbox-ex1-bc1">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_ex1_BC1.jpg"><img alt="../../../tutorials/figures/Cartbox_ex1_BC1.jpg" src="../../../tutorials/figures/Cartbox_ex1_BC1.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.14 </span><span class="caption-text">Boundary condition 1 (BC_wall) is assigned to surface 1-4</span><a class="headerlink" href="#fig-cartbox-ex1-bc1" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-cartbox-ex1_bc2">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_ex1_BC2.jpg"><img alt="../../../tutorials/figures/Cartbox_ex1_BC2.jpg" src="../../../tutorials/figures/Cartbox_ex1_BC2.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.15 </span><span class="caption-text">Boundary condition 2 (BC_inflow) is assigned to surface 5</span><a class="headerlink" href="#fig-cartbox-ex1-bc2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-cartbox-ex1-bc3">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_ex1_BC3.jpg"><img alt="../../../tutorials/figures/Cartbox_ex1_BC3.jpg" src="../../../tutorials/figures/Cartbox_ex1_BC3.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.16 </span><span class="caption-text">Boundary condition 3 (BC_inflow) is assigned to surface 6</span><a class="headerlink" href="#fig-cartbox-ex1-bc3" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

#### Example 2

The parameterfile of this example can be found in 

    /tutorials/1-01-cartbox/parameter_ex2.ini

<a class="reference internal" href="#fig-cartbox-ex2-sketch"><span class="std std-numref">Fig. 1.17</span></a> shows the sketch of the current problem and <a class="reference internal" href="#fig-cartbox-ex2-ini"><span class="std std-numref">Fig. 1.18</span></a> shows an excerpt of the parameter file which deals with the boundary conditions. In this code's excerpt some text elements are colored to show the connection between the surfaces and their assigned boundary conditions. The same colors are used for the visualization below. 

In this example the first, the third and the sixth component of the `BCIndex` vector are equal. The index of these components "1" says that the boundary condition on position one, `BC_wall`, is assigned to the surfaces one, three and six. Furthermore, the fifth component of the `BCIndex` vector with the index 2 means that the second boundary condition `BC_inflow` is assigned to the fifth surface of the cartesian box. At least the third boundary condition `BC_outflow` is assigned to the second and the forth surface. Therefore, the second and the fourth component of the `BCIndex` vector is set to 3. 

<h4>Parameters and Sketch<a class="headerlink" href="#parameteres-and-sketch-unlisted" title="Permalink to this heading"></a></h4>

<table align="center">
  <tr>
    <td>
        <figure id="fig-cartbox-ex2-sketch">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_ex1-sketch.jpg"><img alt="../../../tutorials/figures/Cartbox_ex1-sketch.jpg" src="../../../tutorials/figures/Cartbox_ex1-sketch.jpg" style="height: 300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.17 </span><span class="caption-text">Sketch of Example 2</span><a class="headerlink" href="#fig-cartbox-ex1-sketch" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure class id="fig-cartbox-ex2-ini">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_ex2_ini.jpg"><img alt="../../../tutorials/figures/Cartbox_ex2_ini.jpg" src="../../../tutorials/figures/Cartbox_ex2_ini.jpg" style="height: 300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.18 </span><span class="caption-text">Cartesian Box example 2 Boundary Conditions ini-File</span><a class="headerlink" href="#fig-cartbox-ex1-ini" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

<h4>Output Vizualisation<a class="headerlink" href="#output-visualization-unlisted" title="Permalink to this heading"></a></h4>

If there is a need for assistance of visualizing the HOPR output visit {ref}`tutorials/index_visualization:Visualization`. 

<h4>Mesh<a class="headerlink" href="#mesh-unlisted" title="Permalink to this heading"></a></h4>

This is a visualization of the `cartbox_ex2_Debugmesh.dat` file. 

<figure class="align-center" id="fig-ex2_cartbox_mesh">
  <a class="reference internal image-reference" href="../../../tutorials/figures/Ex2_cartbox_mesh.jpg"><img alt="../../../tutorials/figures/Ex2_cartbox_mesh.jpg" src="../../../tutorials/figures/Ex2_cartbox_mesh.jpg" style="width: 400px;" /></a>
  <figcaption>
  <p><span class="caption-number">Fig. 1.19 </span><span class="caption-text">Mesh of the cartesian box</span><a class="headerlink" href="#fig-cartbox-ex2-mesh" title="Permalink to this image"></a></p>
  </figcaption>
</figure>


<h4>Boundary Conditions<a class="headerlink" href="#boundary-conditions-unlisted" title="Permalink to this heading"></a></h4>

This is a visualization of the `cartbox_ex2_Debugmesh_BC.dat` file. The colors of the surfaces represent the boundary conditions and are the same as in the excerpt of the parameter file. 

<table align="center" style="width:100%">
  <tr>
    <td style="width:33%">
        <figure id="fig-cartbox_ex2_bc1">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_ex2_BC1.jpg"><img alt="../../../tutorials/figures/Cartbox_ex2_BC1.jpg" src="../../../tutorials/figures/Cartbox_ex2_BC1.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.20 </span><span class="caption-text">Boundary condition 1 (BC_wall) is assigned to surfaces 1, 3 and 6</span><a class="headerlink" href="#fig-cartbox-ex2-bc1" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-cartbox_ex2_bc2">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_ex2_BC2.jpg"><img alt="../../../tutorials/figures/Cartbox_ex2_BC2.jpg" src="../../../tutorials/figures/Cartbox_ex2_BC2.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.21 </span><span class="caption-text"> Boundary condition 2 (BC_inflow) is assigned to surface 5</span><a class="headerlink" href="#fig-cartbox-ex2-bc2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-cartbox-ex2-bc3">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_ex2_BC3.jpg"><img alt="../../../tutorials/figures/Cartbox_ex2_BC3.jpg" src="../../../tutorials/figures/Cartbox_ex2_BC3.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.22 </span><span class="caption-text">Boundary condition 3 (BC_outflow) is assigned to surface 2 and 4</span><a class="headerlink" href="#fig-cartbox-ex2-bc3" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>





## Periodic Boundary Conditions
This tutorial shows how to define periodic boundary conditions, only slightly changing the definitions from the tutorial Cartesian Box.
The parameter file can be found in 

    tutorials/1-02-cartbox_periodic/parameter.ini

### Periodic Boundary Conditions: Description of Parameters
In the following parameters of the parameter file are explained. This description consists only of parameters which are necessary to generate a periodic boundary condition. A description of all parameters of the parameterfile can be found in the previous tutorial Cartesian Box and {ref}`userguide/parameters:List of Parameters`.

```{table} Parameters Periodic Boundary Conditions.
---
name: tab:Parameters Periodic Boundary Conditions
---
  | Parameters      |Setting                | Description | 
  | :------         | :----------:          | :---------------------------     |
  | `BoundaryName`   | `BC_zminus`         |   	Name of the boundary condition                   |
  | `BoundaryType`     | ` 	(/1,0,0,-1/)`   | For each periodic boundary condition three parameters are mandatory, the BoundaryName, the BoundaryType and the displacement vector vv. The Type parameter consists of four components to set: (/ Type, curveIndex, State, alpha /). For a periodic boundary condition the component Type has always to set to "1". The fourth component alpha assigns a displacement vector vv and its direction (-/+) to the periodic boundary. An alpha of "-1" means that the first ("1") defined displacement vector is assigned to this surface in the opposite direction ("-1") as he was defined.<br>For a simple cartesian box the other components curveIndex and State have to be set 0; The further description of the components can be found in the next tutorials or in the {ref}`userguide/parameters:List of Parameters`.                   |
  | `vv`          | `(/0.,0.,1./)`          |The displacement vector has to specify in the three-dimensional cartesian coordinate system, has to be normal to a surface the vector was assigned to. In addition the displacement vector has to show to the inside of the cartesian box. In case of two parallel surface-planes, both with periodic boundary conditions, just one displacement vector has to be defined. Therefore the different directions of the vectors can be compensated by switching the sign of alpha, the fourth component of the BoundaryType vector.<br>It has to be taken into account that the displacement vector has to be as long as the distance between the surfaces the vector was assigned to. Also the index of a displacement vector is defined by the position of its definition like the parameter BCIndex. Several definitions of boundary conditions between two definitions of displacement vectors will not affect the index of the displacement vectors.                  |
```
### Periodic Boundary Conditions: Boundary Conditions and Sketch
<a class="reference internal" href="#fig-cartbox-periodic-sketch"><span class="std std-numref">Fig. 1.23</span></a> shows the sketch of the current problem. It is similar to the problem in the tutorial Cartesian Box but instead of Dirichlet periodic boundary conditions are assigned to the surfaces one, two, four and six. Further below one can see an excerpt of the parameter file which deals with the periodic boundary conditions. In this code's excerpt some text elements are colored to show the connection between boundary conditions and their related displacement vectors. The same colors are used for the visualization in <a class="reference internal" href="#fig-cartbox-periodic-sketch"><span class="std std-numref">Fig. 1.17</span></a>. 
<figure class="align-center" id="fig-cartbox-periodic-sketch">
    <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_periodic_sketch.jpg"><img alt="../../../tutorials/figures/Cartbox_periodic_sketch.jpg" src="../../../tutorials/figures/Cartbox_periodic_sketch.jpg" style="width: 50%;" /></a>
    <figcaption>
    <p><span class="caption-number">Fig. 1.23 </span><span class="caption-text">Sketch of the current problem; For a greater clarity in this figure the displacement vectors are shown shorter than they are. In truth the vector arrows are as long as the side length of the cartesian box.</span><a class="headerlink" href="#fig-cartbox-periodic-sketch" title="Permalink to this image"></a></p>
    </figcaption>
 </figure>


As one can see the first four boundary conditions are periodic because the last alpha components of the BoundaryType parameters are not equal to zero. In the definitions of the first two boundary conditions which are assigned to the surfaces on and six (see BCindex) the alpha component is set to 1. That means that the related displacement vector is the first defined displacement vector in the parameterfile. The sign of alpha can be explained by the position of the related surfaces. On the one hand the vector has to show to the inside of the cartesian box. On the other hand the vector also has to show to the other surfaces which was assigned with the periodic boundary condition. The vector itself has to show in the direction of the z-axis because it has to be normal to the surface. In addition, the side lengths of the cartesian box is one and so all defined displacement vectors has a length of one.
For the other two periodic boundary conditions of the surfaces two and four the second defined displacement vector is consulted (see alpha value of the BoundaryType parameters). The components of the displacement vector (/0.,1.,0./) results from the necessary perpendicularity to the surfaces and the side length of the cartesian box 

<figure class="align-center" id="fig-cartbox-periodic-ini">
    <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_periodic_ini.jpg"><img alt="../../../tutorials/figures/Cartbox_periodic_ini.jpg" src="../../../tutorials/figures/Cartbox_periodic_ini.jpg" style="width: 60%;" /></a>
    <figcaption>
    <p><span class="caption-number">Fig. 1.24 </span><span class="caption-text">Cartbox periodic ini-File.</span><a class="headerlink" href="#fig-cartbox-periodic-ini" title="Permalink to this image"></a></p>
    </figcaption>
 </figure>


## Multiple Cartesian Boxes
This tutorial shows how to generate a mesh consisting of mutliple cartesian boxes and how to link them via boundary conditions.
The parameter file can be found in 

    tutorials/1-03-cartbox_multiple/parameter.ini

### Multiple Cartesian Boxes: Definition of Multiple Cartesian Boxes
A general excerpt of the parameter file below shows how multiple cartesian boxes have to be defined in the parameter file. 

    !====================================================================== !
    ! MESH
    !====================================================================== !
      Mode         =1                       ! Mode for Cartesian boxes
      nZones       =n                       ! number of boxes
  
    ! ===  zone 1 ===
      ...      
 
    ! ===  zone 2  ===
      ...
 
      .
      .
      .

    ! ===  zone n  ===
     ...

At first the parameter `nZones` has to be adapted to the number of cartesian boxes one going to define. The cartesian boxes can defined simply by writing them and their specifications `(Corner, nElems, BCIndex, elemtype, ...)` among themselves. Furthermore it is important that the boxes are defined correctly to each other. If there shall be a contact between two boxes, it is mandatory that the corresponding surfaces will coincide. That means that also the surfaces' corner nodes have to coincide.

However, a correct defining of the corner nodes is not sufficient for the functionality. Therefore, the parameter `BCIndex` which assigns boundary conditions to box's surfaces has to be adapted. 

```{table} Multiple Cartesian Boxes.
---
name: tab:Multiple Cartesian Boxes
---
  | Parameters      |Setting                | Description | 
  | :------         | :----------:          | :---------------------------     |
  | `BCIndex`       | `(/0,0,0,0,0,0/)`     | The `BCIndex` parameter assigns a bondary condition to each surface of the cartesian box in order of the surfaces. The number of a vector's component represents the nth boundary condition in order of its position in the file. Hence, each position refers to the six box sides `(/z-,y-,x+,y+,x-,z+/)`.<br>In case of multiple cartesian boxes there are surfaces which coincide with other ones. To such surfaces no boundary condition can assign. Therefore, The number of the corresponding vector's component is set to 0. Here, all components of the parameter `BCIndex` are set to 0 (`(/0,0,0,0,0,0/)`).<br>That means that the this box is surounded completely by six other boxes so that no boundary condition can assign to a single surface.                    |
```

A description of all parameters of the parameterfile can be found in {ref}`userguide/parameters:List of Parameters`. 

### Multiple Cartesian Boxes: Sketch
<a class="reference internal" href="#fig-Cartbox-multiple-sketch"><span class="std std-numref">Fig. 1.25</span></a> shows the sketch of the current problem. As on can see the generated mesh shall consist of three cartesian boxes. These zones are thereby defined in the following order in the parameter file: 

    1st zone: lower left zone

                BCIndex setting: (/-,-,-,-,-,0/)

    2nd zone: upper left zone

                BCIndex setting: (/0,-,-,-,-,-/)

    3rd zone: upper right zone

                BCIndex setting: (/-,-,-,-,0,-/)

For a better understanding the different settings of the parameter `BCIndex` are also given. The given settings just consider the components which are set to 0 because of coinciding surfaces are given.

<figure class="align-center" id="fig-Cartbox-multiple-sketch">
    <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_multiple_sketch.jpg"><img alt="../../../tutorials/figures/Cartbox_multiple_sketch.jpg" src="../../../tutorials/figures/Cartbox_multiple_sketch.jpg" style="width: 60%;" /></a>
    <figcaption>
    <p><span class="caption-number">Fig. 1.25 </span><span class="caption-text">Sketch of the current problem; For a greater clarity in this figure the displacement vectors are shown shorter than they are. In truth the vector arrows are as long as the side length of the cartesian box.</span><a class="headerlink" href="#fig-Cartbox-multiple-sketch" title="Permalink to this image"></a></p>
    </figcaption>
 </figure>

### Multiple Cartesian Boxes: Output Visualization
If there is a need for assistance of visualizing the HOPR output visit {ref}`tutorials/index_visualization:Visualization`.

<h4>Mesh<a class="headerlink" href="#mesh-unlisted" title="Permalink to this heading"></a></h4>

This is a visualization of the `cartbox_multiple_Debugmesh.dat` file. 

<figure class="align-center" id="fig-Cartbox-multiple-mesh">
    <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_multiple_mesh.jpg"><img alt="../../../tutorials/figures/Cartbox_multiple_mesh.jpg" src="../../../tutorials/figures/Cartbox_multiple_mesh.jpg" style="width: 30%;" /></a>
    <figcaption>
    <p><span class="caption-number">Fig. 1.26 </span><span class="caption-text">Mesh of the multiple cartesian boxes.</span><a class="headerlink" href="#fig-Cartbox-multiple-mesh" title="Permalink to this image"></a></p>
    </figcaption>
 </figure>

<h4>Boundary Conditions<a class="headerlink" href="#boundary-conditions-unlisted" title="Permalink to this heading"></a></h4>

This is a visualization of the `cartbox_multiple_Debugmesh_BC.dat` file. The colors of the surfaces represent the boundary conditions and are the same as in the excerpt of the parameter file. 

<table align="center" style="width:100%">
  <tr>
    <td style="width:33%">
        <figure id="fig-cartbox-multiple-bc1">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_multiple_BC1.jpg"><img alt="../../../tutorials/figures/Cartbox_multiple_BC1.jpg" src="../../../tutorials/figures/Cartbox_multiple_BC1.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.27 </span><span class="caption-text">Boundary condition 1 (BC_slipwall) is assigned to surface 1 of the first box</span><a class="headerlink" href="#fig-cartbox-multiple-bc1" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-cartbox-multiple-bc2">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_multiple_BC2.jpg"><img alt="../../../tutorials/figures/Cartbox_multiple_BC2.jpg" src="../../../tutorials/figures/Cartbox_multiple_BC2.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.28 </span><span class="caption-text">Boundary condition 2 (BC_upperwall) is assigned to surface 6 of the second and the third box</span><a class="headerlink" href="#fig-cartbox-multiple-bc2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-cartbox-multiple-bc3">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_multiple_BC3.jpg"><img alt="../../../tutorials/figures/Cartbox_multiple_BC3.jpg" src="../../../tutorials/figures/Cartbox_multiple_BC3.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.29 </span><span class="caption-text">Boundary condition 3 (BC_lowerwall) is assigned to surface 3 of the first box and to surface 1 of the third box</span><a class="headerlink" href="#fig-cartbox-multiple-bc3" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
  <tr>
    <td style="width:33%">
        <figure id="fig-cartbox-multiple-bc4">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_multiple_BC4.jpg"><img alt="../../../tutorials/figures/Cartbox_multiple_BC4.jpg" src="../../../tutorials/figures/Cartbox_multiple_BC4.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.30 </span><span class="caption-text">Boundary condition 4 (BC_inflow) is assigned to surface 5 of the first and the second box</span><a class="headerlink" href="#fig-cartbox-multiple-bc4" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-cartbox-multiple-bc5">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_multiple_BC5.jpg"><img alt="../../../tutorials/figures/Cartbox_multiple_BC5.jpg" src="../../../tutorials/figures/Cartbox_multiple_BC5.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.31 </span><span class="caption-text">Boundary condition 5 (BC_outflow) is assigned to surface 3 of the third box</span><a class="headerlink" href="#fig-cartbox-multiple-bc5" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-cartbox-multiple-bc6">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_multiple_BC6.jpg"><img alt="../../../tutorials/figures/Cartbox_multiple_BC6.jpg" src="../../../tutorials/figures/Cartbox_multiple_BC6.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.32 </span><span class="caption-text">Periodic boundary condition 6 (BC_yminus) is assigned to surface 2 of the first, the second and the third box</span><a class="headerlink" href="#fig-cartbox-multiple-bc6" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
  <tr>
    <td style="width:33%">
        <figure id="fig-cartbox-multiple-bc7">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_multiple_BC7.jpg"><img alt="../../../tutorials/figures/Cartbox_multiple_BC7.jpg" src="../../../tutorials/figures/Cartbox_multiple_BC7.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.33 </span><span class="caption-text">Periodic boundary condition 7 (BC_yplus) is assigned to surface 4 of the first, the second and the third box</span><a class="headerlink" href="#fig-cartbox-multiple-bc7" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>




## Stretching Functions
This tutorial shows how to generate a mesh consisting of a boxes with a stretched element arrangement.
The parameter file can be found in 

    tutorials/1-04-cartbox_multiple_stretch/parameter.ini

### Stretching Functions: Definition of Stretching Functions
With stretching functions one can generate a mesh consisting of a boxes with a stretched element arrangement. Therefore two new parameters can be defined in the parameter file: `factor` and `l0`. Each one can used to stretch the elements of a box. Their meaning and connection is shown as one-dimensional case in <a class="reference internal" href="#fig-stretch-functions"><span class="std std-numref">Fig. 1.28</span></a>.
<figure id="fig-stretch-functions">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Stretch_functions.jpg"><img alt="../../../tutorials/figures/Stretch_functions.jpg" src="../../../tutorials/figures/Stretch_functions.jpg" style="width: 25%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.34 </span><span class="caption-text">"Stretch parameters" `factor` and `l0`</span><a class="headerlink" href="#fig-stretch-functions" title="Permalink to this image"></a></p>
        </figcaption>
</figure>


In case of a stretched element arrangement the next element of a box is always stretched by a factor f in the direction of the coordinate axis.Thereby the value of factor f can have a positive or a negative sign. Here f has either a negative and an absolute value >1 or a positive sign and an abolute value <1. The length of the first element is called l_{0}. All streched N elements together has the length l_{ges}. 

### Stretching Functions: Building a Cartesian Box with Stretched Elements
To get a single cartesian box with a stretched element arrangement it is important to know that the parameters l_{0} and N are defined even before one define the stretching parameters factor and l0. The length l_{ges} is defined by the boundarys of the cartesian box and the number of elements per box in the direction of the cartesian coordinate axes, called N in the following, was defined with the parameter nElems.

For a stretched element arrangement either the parameter `factor` or the parameter `l0` has to be defined. The other missing parameter will be calculated internally by using the following equation:

<script
  src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  type="text/javascript">
</script>

$$ \frac{l_{ges}}{l_{0}} = \sum_{i=1}^{N} f^{i-1} = \frac{1 - f^{N}}{1 - f } $$

The structure of both parameters are explained below. A description of all parameters of the parameterfile can be found in {ref}`userguide/parameters:List of Parameters`. 

```{table} Stretching Functions.
---
name: tab:Stretching Functions
---
  | Parameters      |Setting                | Description | 
  | :------         | :----------:          | :---------------------------     |
  | `factor`       | ` 	(/-1.75,1,-1.5/)`   | Stretching factor of the elements in the direction of the cartesian coordinate axes. A value >1 means an increase of the element size in the direction of the coordinate axis, however, a value of the intervall (0,1) means a decrease. The Value 1 does not affect the element sizes just as the value 0 which means an deactivation of the stretching function for this axis. Furthermore the stretching behaviour can be mirrored by adding a negative sign to the values. A combination with the parameter `l0` ignores the element number of the defined box.<br>In case of (/-1.75,1,-1.5/) each following element in the direction of the x-axis is compressed by the factor -1.75 and in the direction of the z-axis by the factor -1.5. The element arrangement in the direction of the y-axis was not changed.  |
  | `l0`             | `(/0,1,5,0/)`   |  The length of the first element of a stretched element arrangement of a cartesian box. Each component of the vector stands for an axis of the cartesian coordinate system. The value 0 means an deactivation of the stretching function for this axis. A negative sign defines the length of the first element of the other side of the box. A combination with the parameter `factor` ignores the element number of the defined box. Here the stretching function is deactivated for the x- and z-axis, whereas the first element in the direction of the y-axis has a size of 1.5. |
```

<h4>Variable Definition of Stretching Functions<a class="headerlink" href="#variable-definition-of-stretching-functions-unlisted" title="Permalink to this heading"></a></h4>

There are several ways to get a stretched element arrangement due to the two different stretching parameters `factor` and `l0`. These cases are:

-Defining of `factor`: In this case the next element of a box is stretched by a factor f in the direction of the coordinate axis. The parameter l0 will be calculated internally by the equation provided above. The number of elements N which is defined by the parameter nElems is retained.

-Defining of `l0`: In this case the length of the first element (or the last by using a negative sign) is defined in the direction of the coordinate axis. The parameter factor will be calculated internally by the equation provided above. The number of elements N which is defined by the parameter nElems is retained.

-Defining of `factor` and `l0`: In this case the parameters have to be defined by the equation provided above manually. Otherwise the stretched element arrangement will most likely not achieved the desired shape. In case of an inaccurate definition the parameter `factor` is adjusted to the parameter l0 which means that factor will changed internally. Furthermore, the number of elements N which is defined by the parameter nElems is probably not retained. Instead, N is rounded to nearest natural number.

These three different cases are presented below with a small cube with an edge length of one and with four elements per axis. For a better understanding just x- and y- values were changed und visualized.

<table align="center" style="width:100%">
  <tr>
    <td style="width:25%">
        <figure id="fig-stretch-example">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Stretch_example.png"><img alt="../../../tutorials/figures/Stretch_example.png" src="../../../tutorials/figures/Stretch_example.png" style="height: 200px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.35 </span><span class="caption-text">Non-stretched element arrangement <br><code class="docutils literal notranslate"><span class="pre">nElems   =(/4,4,4/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">factor   =(/0,0,0/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">l0       =(/0,0,0/)</span></code></span><a class="headerlink" href="#fig-stretch-example" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-stretch-example-f">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Stretch_example_f.png"><img alt="../../../tutorials/figures/Stretch_example_f.png" src="../../../tutorials/figures/Stretch_example_f.png" style="height: 200px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.36 </span><span class="caption-text">Stretched element arrangement. The element size in the direction of the x-axis increases by a factor of 1.5. In the direction of the y-axis it decreases by the factor of -1.2.<br><code class="docutils literal notranslate"><span class="pre">nElems   =(/4,4,4/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">factor   =(/1.5,-1.2,0/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">l0       =(/0,0,0/)</span></code></span><a class="headerlink" href="#fig-stretch-example-f" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-stretch-example-l0">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Stretch_example_l0.png"><img alt="../../../tutorials/figures/Stretch_example_l0.png" src="../../../tutorials/figures/Stretch_example_l0.png" style="height: 200px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.37 </span><span class="caption-text">Stretched element arrangement. The first element in the direction of the x-axis has a length of 0.5 and a length of 0.2 in the direction of the y-axis. The parameter factor is adjusted.<br><code class="docutils literal notranslate"><span class="pre">nElems   =(/4,4,4/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">factor   =(/0,0,0/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">l0       =(/0.5,0.2,0/)</span></code></span><a class="headerlink" href="#fig-stretch-example-l0" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-stretch-example-fl0">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Stretch_example_fl0.png"><img alt="../../../tutorials/figures/Stretch_example_fl0.png" src="../../../tutorials/figures/Stretch_example_fl0.png" style="height: 200px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.38 </span><span class="caption-text">Stretched element arrangement with a combination of factor and l0. The parameter l0 defines the side lengths of the first element. The following element sizes are multiplied by the compontens of the parameter factor. The number of elements N which is defined by the parameter nElems is not retained.<br><code class="docutils literal notranslate"><span class="pre">nElems   =(/4,4,4/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">factor   =(/1.5,-1.2,0/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">l0       =(/0.5,0.2,0/)</span></code></span><a class="headerlink" href="#fig-stretch-example-fl0" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>
<br>
<br>

### Stretching Functions: Building Multiple Cartesian Boxes with Stretched Elements
For building a mesh consisting of multiple cartesian boxes with a stretched element arrangement at least one of the parameters factor and l0 has to be defined for each cartesian box. The reason for this is that if there shall be a contact between two boxes the surfaces' corner nodes have to coincide. This means that defining a stretch function to a cartesian box leads to a need of a stretch function of the adjacent cartesian box. A visualization of this issue one can see in the sketch of the tutorial's problem. 

### Stretching Functions: Sketch
In the following an exemplary mesh of multiple cartesian boxes with a stretched element arrangement is presented. The belonging parameter file can be found in Parameterfile Stretching Functions. The arrangement of the cartesian boxes corresponds to the one of the tutorial Multiple Cartesian Boxes but instead of equidistant elements a stretched element arrangement is produced by inserting the parameters factor and l0. Furthermore the number of elements of each box were increased by changing the parameter nElems for a better visualization of the stretched element arrangement. The figure below shows the way the elements will be stretched. 


<figure id="fig-cartbox-multiple-stretch-sketch">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_multiple_stretch_sketch.jpg"><img alt="../../../tutorials/figures/Cartbox_multiple_stretch_sketch.jpg" src="../_images/Cartbox_multiple_stretch_sketch.jpg" style="width: 30%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.39 </span><span class="caption-text">Sketch of a mesh with multiple cartesian boxes with a stretched element arrangement. The arrows show in which direction the elements are compressed. The elements are getting smaller the closer the elements get to the cartesian x-y-plane and y-z-plane. The elements in the direction of the cartesian y-axis remain equidistant.</span><a class="headerlink" href="#fig-cartbox-multiple-stretch-sketch" title="Permalink to this image"></a></p>
        </figcaption>
</figure>


It has to be taken into account that the figure above just demonstrates in which directions the elements are stretched respectively compressed. Wether the number of elements per box nor the cartesian boxes'sizes represented here corresponds to the parameters of the Parameterfile Stretching Functions. 

### Stretching Functions: Output Visualization
If there is a need for assistance of visualizing the HOPR output visit {ref}`tutorials/index_visualization:Visualization`. 
These are visualizations of the `cartbox_multiple_stretch_mesh.h5` file. 

<figure id="fig-cartbox-multiple-stretch-side">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_multiple_stretch_side.png"><img alt="../../../tutorials/figures/Cartbox_multiple_stretch_side.png" src="../_images/Cartbox_multiple_stretch_side.png" style="width: 30%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.40 </span><span class="caption-text">Side view of the mesh of the multiple cartesian boxes with a stretched element arrangement.</span><a class="headerlink" href="#fig-cartbox-multiple-stretch-side" title="Permalink to this image"></a></p>
        </figcaption>
</figure>
<figure id="fig-cartbox-multiple-stretch-mesh">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Cartbox_multiple_stretch_mesh.png"><img alt="../../../tutorials/figures/Cartbox_multiple_stretch_mesh.png" src="../../../tutorials/figures/Cartbox_multiple_stretch_mesh.png" style="width: 30%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.41 </span><span class="caption-text">Mesh of the multiple cartesian boxes with a stretched element arrangement.</span><a class="headerlink" href="#fig-cartbox-multiple-stretch-mesh" title="Permalink to this image"></a></p>
        </figcaption>
</figure>