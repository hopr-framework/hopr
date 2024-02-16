# External Meshes without Curved Boundaries
This tutorial shows how to read in externally generated unstructured and structured meshes with straight-edged elements.

The parameter file can be found in

    tutorials/2-01-external_mesh_sphere/parameter.ini

## External Mesh
The external mesh which shall read in have to be available in the directory of the executed parameter file as CGNS file. This file is read-in by introducing the parameter `filename`. As one can see from the `parameter.ini`'s excerpt and <a class="reference internal" href="#fig-cgnsviewer-spheremesh04"><span class="std std-numref">Fig. 1.1</span></a>, the parameters of the parameter file have to be adapted to the definitions in the CGNS mesh file. This means that the parameters Mode, `nZones`, `BoundaryName` and `BoundaryType` can not be set freely anymore because the structure of the external mesh must be retained. In this case, the external mesh `spheremesh02` is available as CGNS file and consists of three zones. Therefore, the settings of the parameters are `Mode=3`, `nZones=3`, `filename=spheremesh02.cgns`.

Another important fact is that for external meshes no `BCIndex` parameter is needed which assigns normally bondary conditions to the surfaces of the mesh. The reason for this is that the boundary conditions are assigned to their belonging surfaces by their names. The boundary condition, for example, of `Zone_1` of the CGNS file (`BC_sphere`) has to be defined as `BoundaryName=sphere` in the parameter file. 

    !================================================================= !
    ! MESH
    !================================================================= !
    Mode    =3                  ! 1 Cartesian 3 CGNS 4 STAR-CD V3 
    nZones  =3                  ! number of zones
    filename=spheremesh02.cgns  ! name of mesh file
    ...
    ...
    
    !================================================================= !
    ! CURVED
    !================================================================= !
    useCurveds=F                ! T to generate curved boundaries 
    
    
    !================================================================= !
    ! BOUNDARY CONDITIONS
    !================================================================= !
    BoundaryName=sphere         ! BC_Name must be defined in mesh file
    BoundaryType=(/4,1,0,0/)    
    BoundaryName=inflow               
    BoundaryType=(/2,0,0,0/)
    BoundaryName=outflow             
    BoundaryType=(/2,0,0,0/)
    BoundaryName=mantel           
    BoundaryType=(/2,0,0,0/)


<figure class="align-center" id="fig-cgnsviewer-spheremesh04">
    <a class="reference internal image-reference" href="../_images/CGNSviewer_spheremesh04.png"><img alt="../_images/CGNSviewer_spheremesh04.png" src="../_images/CGNSviewer_spheremesh04.png" style="width: 30%;" /></a>
    <figcaption>
    <p><span class="caption-number">Fig. 1.1 </span><span class="caption-text">Screenshot of the folder structure of a CGNS mesh.</span><a class="headerlink" href="#fig-cgnsviewer-spheremesh04" title="Permalink to this image"></a></p>
    </figcaption>
</figure>

Furthermore, the `BoundaryType` parameter has to be adapted to the definitions in the CGNS mesh file. If a boundary of the external mesh is curved the `curveIndex` component (2nd component) of the `BoundaryType` parameter has to be an value unequal to zero. Wether curved boundaries shall be generated or not can be controlled by the parameter `useCurveds`. In this tutorial `useCurveds=F`. The case `useCurveds=T` is the topic of the next tutorial which explaines how to use mesh curving techniques to get curved boundaries for your mesh.

All new parameters of the parameter file of this tutorial are explained below.

```{table} External Mesh Parameters.
---
name: tab:External Mesh Parameters
---
  | Parameters      |Setting                | Description | 
  | :------         | :----------:          | :---------------------------     |
  | `filename`      | `spheremesh.cgns`     |  	The name of the external mesh file. The belonging files have to be available in the directory of the executed parameter file as CGNS files. |
  | `meshscale`      | ` 	0.001`     |  	Scales all input meshes by a factor |
  | `SpaceQuandt`      | ` 	1000`     |  	Characteristic length of the mesh  |
  | `useCurveds`      | `T`     |  	 	T (True): If curved boundaries are defined<br>F (False): If no curved boundaries are defined . |
```

A description of all parameters of the parameterfile can be found in {ref}`userguide/parameters:List of Parameters`.

## Output Visualization
If there is a need for assistance of visualizing the HOPR output visit {ref}`tutorials/index_visualization:Visualization`.

The figures below show the visualizations of the `SPHERE_Debugmesh.vtu` file. In Addition, a visualization of the surfaces the first boundary condition sphere was assigned to (the `curveIndex` of the `BoundaryType` parameter is set to 1) of the `SPHERE_Debugmesh_BC.vtu` file is shown for each external mesh (see <a class="reference internal" href="#fig-exmeshwo-spheremesh01-innerbc"><span class="std std-numref">Fig. 1.4</span></a>, <a class="reference internal" href="#fig-exmeshwo-spheremesh02-innerbc"><span class="std std-numref">Fig. 1.7</span></a>, <a class="reference internal" href="#fig-exmeshwo-spheremesh04-innerbc"><span class="std std-numref">Fig. 1.10</span></a>)

<h4>spheremesh01<a class="headerlink" href="#spheremesh01" title="Permalink to this heading"></a></h4>

<table align="center" style="width:100%">
  <tr>
    <td style="width:33%">
        <figure id="fig-exmeshwo-spheremesh01-surfaces">
        <a class="reference internal image-reference" href="../_images/Exmeshwo_spheremesh01_surfaces.jpg"><img alt="../_images/Exmeshwo_spheremesh01_surfaces.jpg" src="../_images/Exmeshwo_spheremesh01_surfaces.jpg" style="height:300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.2 </span><span class="caption-text">HOPR output of spheremesh01.cgns</span><a class="headerlink" href="#fig-exmeshwo-spheremesh01-surfaces" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-exmeshwo-spheremesh01-mesh">
        <a class="reference internal image-reference" href="../_images/Exmeshwo_spheremesh01_mesh.jpg"><img alt="../_images/Exmeshwo_spheremesh01_mesh.jpg" src="../_images/Exmeshwo_spheremesh01_mesh.jpg" style="height:300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.3 </span><span class="caption-text">HOPR output of spheremesh01.cgns with extracted edges.</span><a class="headerlink" href="#fig-exmeshwo-spheremesh01-mesh" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td style="width:33%">
        <td>
        <figure id="fig-exmeshwo-spheremesh01-innerbc">
        <a class="reference internal image-reference" href="../_images/Exmeshwo_spheremesh01_innerbc.jpg"><img alt="../_images/Exmeshwo_spheremesh01_innerbc.jpg" src="../_images/Exmeshwo_spheremesh01_innerbc.jpg" style="height:300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.4 </span><span class="caption-text">Element surfaces (6) of spheremesh01.cgns the boundary condition sphere was assigned to.</span><a class="headerlink" href="#fig-exmeshwo-spheremesh01-innerbc" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

<h4>spheremesh02<a class="headerlink" href="#spheremesh02" title="Permalink to this heading"></a></h4>

<table align="center" style="width:100%">
  <tr>
    <td style="width:33%">
        <figure id="fig-exmeshwo-spheremesh02-surfaces">
        <a class="reference internal image-reference" href="../_images/Exmeshwo_spheremesh02_surfaces.jpg"><img alt="../_images/Exmeshwo_spheremesh02_surfaces.jpg" src="../_images/Exmeshwo_spheremesh02_surfaces.jpg" style="height:300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.5 </span><span class="caption-text">HOPR output of spheremesh02.cgns</span><a class="headerlink" href="#fig-exmeshwo-spheremesh02-surfaces" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-exmeshwo-spheremesh02-mesh">
        <a class="reference internal image-reference" href="../_images/Exmeshwo_spheremesh02_mesh.jpg"><img alt="../_images/Exmeshwo_spheremesh02_mesh.jpg" src="../_images/Exmeshwo_spheremesh02_mesh.jpg" style="height:300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.6 </span><span class="caption-text">HOPR output of spheremesh02.cgns with extracted edges.</span><a class="headerlink" href="#fig-exmeshwo-spheremesh02-mesh" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td style="width:33%">
        <td>
        <figure id="fig-exmeshwo-spheremesh02-innerbc">
        <a class="reference internal image-reference" href="../_images/Exmeshwo_spheremesh02_innerbc.jpg"><img alt="../_images/Exmeshwo_spheremesh02_innerbc.jpg" src="../_images/Exmeshwo_spheremesh02_innerbc.jpg" style="height:300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.7 </span><span class="caption-text">Element surfaces (24) of spheremesh02.cgns the boundary condition sphere was assigned to.</span><a class="headerlink" href="#fig-exmeshwo-spheremesh02-innerbc" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

<h4>spheremesh04<a class="headerlink" href="#spheremesh04" title="Permalink to this heading"></a></h4>

<table align="center" style="width:100%">
  <tr>
    <td style="width:33%">
        <figure id="fig-exmeshwo-spheremesh04-surfaces">
        <a class="reference internal image-reference" href="../_images/Exmeshwo_spheremesh04_surfaces.jpg"><img alt="../_images/Exmeshwo_spheremesh04_surfaces.jpg" src="../_images/Exmeshwo_spheremesh04_surfaces.jpg" style="height:300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.8 </span><span class="caption-text">HOPR output of spheremesh04.cgns</span><a class="headerlink" href="#fig-exmeshwo-spheremesh04-surfaces" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-exmeshwo-spheremesh04-mesh">
        <a class="reference internal image-reference" href="../_images/Exmeshwo_spheremesh04_mesh.jpg"><img alt="../_images/Exmeshwo_spheremesh04_mesh.jpg" src="../_images/Exmeshwo_spheremesh04_mesh.jpg" style="height:300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.9 </span><span class="caption-text">HOPR output of spheremesh04.cgns with extracted edges.</span><a class="headerlink" href="#fig-exmeshwo-spheremesh02-mesh" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td style="width:33%">
        <td>
        <figure id="fig-exmeshwo-spheremesh04-innerbc">
        <a class="reference internal image-reference" href="../_images/Exmeshwo_spheremesh04_innerbc.jpg"><img alt="../_images/Exmeshwo_spheremesh04_innerbc.jpg" src="../_images/Exmeshwo_spheremesh04_innerbc.jpg" style="height:300px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.10 </span><span class="caption-text">Element surfaces (64) of spheremesh04.cgns the boundary condition sphere was assigned to.</span><a class="headerlink" href="#fig-exmeshwo-spheremesh04-innerbc" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>