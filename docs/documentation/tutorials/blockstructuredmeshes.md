# Block-Structured Meshes
This tutorial shows how to agglomerate block-structured grids with linear elements to get a curved mesh consisting of fully curved hexahedral elements.

The parameter file can be found in

    tutorials/3-01-agglomeration_NACA/parameter.ini

## Restrictions on the block-structured mesh readin

- If meshes are used to create curved elements, a **unique factor** should be present in the i,j,k element count of **all blocks**!!
- Boundary Conditions:
    - Attention: **One block face** can only be associated to **one boundary condition**. Block faces with mixed BCs or BC and internal faces a likely to produce wrong mesh topology!!
    **Split blocks** in this case, else you will get problems in the mesh connect step!
    - Each boundary conditions must be specified in the inifile by its name and an associated Boundary Type:

            BoundaryName = wall_1
            BoundaryType = (/4,0,0,0/)
            BoundaryName = wall_2
            BoundaryType = (/4,0,0,0/)


You can combine multiple BCs by using only a common part of the boundary name string (here for example `BoundaryName=wall`) 

## Initial Meshes

The use of block-structured meshes enables the generation of fully three-dimensional curved elements by agglomeration of a specific block of elements, leading to a very simple and robust curving technique. In order to illustrate block-structuring and agglomeration, enabled by the new parameter `MeshIsAlreadyCurved`, two initally fine meshes of a NACA-profile are provided as CGNS files, mesh 1 without boundary layer refinement and mesh 2 with boundary layer refinement: 

<h5>Mesh1: NACA0012_icem_32elems.cgns<a class="headerlink" href="#Mesh1" title="Permalink to this heading"></a></h4>

<table align="center">
  <tr>
    <td>
        <figure id="fig-aggl-nv-fern">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-fern.jpg"><img alt="../../../tutorials/figures/Aggl-nv-fern.jpg" src="../../../tutorials/figures/Aggl-nv-fern.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.1 </span><span class="caption-text">Overall view of initial mesh 1</span><a class="headerlink" href="#fig-aggl-nv-fern" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure id="fig-aggl-nv-nah">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-nah.jpg"><img alt="../../../tutorials/figures/Aggl-nv-nah.jpg" src="../../../tutorials/figures/Aggl-nv-nah.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.2 </span><span class="caption-text">Close-up image of mesh 1 of the leading edge of the NACA-profile</span><a class="headerlink" href="#fig-aggl-nv-nah" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

<h5>Mesh2: NACA0012_icem_viscous_32elems.cgns<a class="headerlink" href="#Mesh1" title="Permalink to this heading"></a></h4>

<table align="center">
  <tr>
    <td>
        <figure id="fig-aggl-v-fern">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-v-fern.jpg"><img alt="../../../tutorials/figures/Aggl-v-fern.jpg" src="../../../tutorials/figures/Aggl-v-fern.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.3 </span><span class="caption-text">Overall view of initial mesh 2</span><a class="headerlink" href="#fig-aggl-v-fern" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure id="fig-aggl-v-nah">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-v-nah.jpg"><img alt="../../../tutorials/figures/Aggl-v-nah.jpg" src="../../../tutorials/figures/Aggl-v-nah.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.4 </span><span class="caption-text">Close-up image of mesh 2 of the leading edge of the NACA-profile</span><a class="headerlink" href="#fig-aggl-v-nah" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

All of the following figures were created with mesh 1. 

<h4>Agglomeration Curving Technique<a class="headerlink" href="#agglomeration-curving-technique" title="Permalink to this heading"></a></h4>
When using agglomeration, the important parameters are

    MeshIsAlreadyCurved=T
    useCurved=T
    BoundaryOrder=5

and they lead to a coarsening in all three dimensions of the structured mesh, using the internal points as interpolation points for the curved mapping. The number of elements in each direction of the structured block must be a multiple number of `BoundaryOrder`-1!!! This situation is explained on an exemplary mesh in <a class="reference internal" href="#fig-nskip"><span class="std std-numref">Fig. 1.5</span></a>. For `BoundaryOrder = 2` the initial linear mesh is found and no agglomeration is done.

<figure class="align-center" id="fig-nskip">
    <a class="reference internal image-reference" href="../../../tutorials/figures/Nskip.jpg"><img alt="../../../tutorials/figures/Nskip.jpg" src="../../../tutorials/figures/Nskip.jpg" style="width: 50%;" /></a>
    <figcaption>
    <p><span class="caption-number">Fig. 1.5 </span><span class="caption-text">Block-structuring with the parameter <code class="docutils literal notranslate"><span class="pre">BoundaryOrder=2/3/5</span></code>, (<code class="docutils literal notranslate"><span class="pre">BoundaryOrder</span></code>-1)^3 elements are grouped together.</span><a class="headerlink" href="#fig-nskip" title="Permalink to this image"></a></p>
    </figcaption>
</figure>

In addtion, the parameter `nskip` to coarsen the initial mesh. 

## Description of Parameters

Below all new parameters that are associated with agglomeration are explained. A description of all parameters can be found in {ref}`userguide/parameters:List of Parameters`. 

```{table} Block-Structured Meshes: Description of Parameters.
---
name: tab:Block-Structured Meshes Description of Parameters
---
  | Parameters      |Setting                | Description | 
  | :------         | :----------:          | :---------------------------     |
  | `MeshIsAlreadyCurved`      | `T`     |   	T (True): Enables the agglomeration<br>F (False): Disables the agglomeration   |
  | `nskip`      | `2`     | Coarsen block-structured meshes:<br>1: no skip<br>2: use every second point<br>...     |
  | `nskipZ`      | `2`     | 	Only if the mesh is extruded in z-direction, a different nskip can be given in z-direction.     |
```

<table align="center" style="width:100%">
  <tr>
    <td style="width:33%">
        <figure id="fig-aggl-c1">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-c1.jpg"><img alt="../../../tutorials/figures/Aggl-c1.jpg" src="../../../tutorials/figures/Aggl-c1.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.6 </span><span class="caption-text">Front view of the leading edge with the initial mesh configuration. All elements and edges are linear.</span><a class="headerlink" href="#fig-aggl-c1" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-aggl-c2">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-c2.jpg"><img alt="../../../tutorials/figures/Aggl-c2.jpg" src="../../../tutorials/figures/Aggl-c2.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.7 </span><span class="caption-text">Front view of the leading edge of the mesh with the following setting: <code class="docutils literal notranslate"><span class="pre">BoundaryOrder = 5</span></code>. One block consists of 4x4x4 = 64 elements. The blocks' edges (blue lines) are the boundary of the curved elements. The initial structured mesh is shown in grey. All nodes/connections of the white lines are interpolation points</span><a class="headerlink" href="#fig-aggl-c2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-aggl-c3">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-c3.jpg"><img alt="../../../tutorials/figures/Aggl-c3.jpg" src="../../../tutorials/figures/Aggl-c3.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.8 </span><span class="caption-text">Generated curved high order mesh by agglomeration.</span><a class="headerlink" href="#fig-aggl-c3" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

<h4>Coarsening<a class="headerlink" href="#coarsening" title="Permalink to this heading"></a></h4>

For coarsening two new parameters are provided: `nskip` applies to all structured directions equally, and `nskipZ` can be used for z-extruded meshes.<br>

`nskip` Variations: <br>

<table align="center" style="width:100%">
  <tr>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip0">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip0.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip0.jpg" src="../../../tutorials/figures/Aggl-nv-skip0.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.9 </span><span class="caption-text">Initial mesh 1 <code class="docutils literal notranslate"><span class="pre">nskip =1</span></code></span><a class="headerlink" href="#fig-aggl-nv-skip0" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip2">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip2.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip2.jpg" src="../../../tutorials/figures/Aggl-nv-skip2.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.10 </span><span class="caption-text">Mesh by following parameter setting: <code class="docutils literal notranslate"><span class="pre">nskip =2</span></code></span><a class="headerlink" href="#fig-aggl-nv-skip2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip4">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip4.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip4.jpg" src="../../../tutorials/figures/Aggl-nv-skip4.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.11 </span><span class="caption-text">Generated curved high order mesh by agglomeration.</span><a class="headerlink" href="#fig-aggl-nv-skip4" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip8">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip8.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip8.jpg" src="../../../tutorials/figures/Aggl-nv-skip8.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.12 </span><span class="caption-text">Generated curved high order mesh by agglomeration.</span><a class="headerlink" href="#fig-aggl-nv-skip8" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

The mesh on the left side shows the initial mesh with an initial element, shown in grey. The parameter `nskip=1` uses every point of the initial mesh. The mesh in the middle shows the mesh if `nskip` is set to 2. That means that one node in each direction of the respective coordinate system is skipped and that the size of the new element reaches to the next node. The skipped nodes will not be used for the mesh anymore, also not for curving.
The parameter `nskipZ` is has the same function as `nskip` but only for the z-direction. As a consequence of a setting of this parameter the skip level in z-direction by the parameter `nskip` will be repealed.<br>

In section Output Visualization a few different parameter settings are illustrated for the NACA-profile. Take into account that the block-structuring will only work if the value for the parameter(s) `nskip` (and `nskipZ`) is a common divisor of the number of all mesh elements for each axis!!!<br>

`nskip`-`nskipZ` Combinations: 

<table align="center" style="width:100%">
  <tr>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip2z-">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip2z-.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip2z-.jpg" src="../../../tutorials/figures/Aggl-nv-skip2z-.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.13 </span><span class="caption-text">View of mesh 1 by following parameter setting: <code class="docutils literal notranslate"><span class="pre">nskip  = 2</span></code> <code class="docutils literal notranslate"><span class="pre">!nskipZ = ..</span></code></span><a class="headerlink" href="#fig-aggl-nv-skip2z-" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip2z1">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip2z1.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip2z1.jpg" src="../../../tutorials/figures/Aggl-nv-skip2z1.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.14 </span><span class="caption-text">View of mesh 1 by following parameter setting: <code class="docutils literal notranslate"><span class="pre">nskip  = 2</span></code> <code class="docutils literal notranslate"><span class="pre">nskipZ = 1</span></code></span><a class="headerlink" href="#fig-aggl-nv-skip2z1" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip2z-">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip2z-.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip2z-.jpg" src="../../../tutorials/figures/Aggl-nv-skip2z-.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.15 </span><span class="caption-text">View of mesh 1 by following parameter setting: <code class="docutils literal notranslate"><span class="pre">nskip  = 2</span></code> <code class="docutils literal notranslate"><span class="pre">nskipZ = 2</span></code></span><a class="headerlink" href="#fig-aggl-nv-skip2z-" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip2z2">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip2z2.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip2z2.jpg" src="../../../tutorials/figures/Aggl-nv-skip2z2.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.16 </span><span class="caption-text">Oblique view of mesh 1 by following parameter setting: <code class="docutils literal notranslate"><span class="pre">nskip  = 2</span></code> <code class="docutils literal notranslate"><span class="pre">nskipZ = 4</span></code></span><a class="headerlink" href="#fig-aggl-nv-skip2z2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

<h4>Correction for z-extruded Meshes<a class="headerlink" href="#correction-for-z-extruded-meshes" title="Permalink to this heading"></a></h4>

This function does not only apply to block-structured meshes, but to all meshes which are extruded along the z-direction! There are five more new parameters by which one can change special properties of z-extruded meshes like for example length, starting point. 

```{table} Correction for z-extruded Meshes: Description of Parameters.
---
name: tab:Correction for z-extruded Meshes Description of Parameters
---
  | Parameters      |Setting                | Description | 
  | :------         | :----------:          | :---------------------------     |
  | `doZcorrection` | `F`                   |   	 	T (True): Can only be applied to z-extruded meshes: All elements are aligned exactly along z-direction to suppress grid generator errors<br>F (False): Correction is disabled    |
  | `nElemsZ`       | `1`                   | The number of elements in z-direction (after agglomeration!)     |
  | `zstart`        | `0.`                  | Set minimum z-coordinate      |
  | `zLength`       | `1.0`                 | Set length of domain in z-direction       |
  | `zperiodic`     | `T`                   | T (True): The Boundary conditions (here `z_plus` and `z_minus`) are set to periodic ones.<br>F (False): Default. No change of the boundary condition.        |
```

The boundary periodicity can be set here to be applied after the mesh connectivity process. The boundary condition type of the z-faces should then be set to >1,(because of tolerance issues, problems during connectivity process are found if the boundary conditions are set to periodic) and then periodicity is then applied afterwards in the z-correction process. 

## Output Visualization

The figures here were visualizations of the mesh NACA0012_icem_32elems.cgns. Therefore, the files NACA0012_VISCOUS_Debugmesh.vtu (<a class="reference internal" href="#fig-aggl-nv-fern"><span class="std std-numref">Fig. 1.1</span></a> - <a class="reference internal" href="#fig-aggl-v-nah"><span class="std std-numref">Fig. 1.4</span></a>, <a class="reference internal" href="#fig-aggl-nv-skip0"><span class="std std-numref">Fig. 1.9</span></a> - <a class="reference internal" href="#fig-aggl-nv-skip2z2"><span class="std std-numref">Fig. 1.16</span></a>) and NACA0012_VISCOUS_SplineVol.vtu (<a class="reference internal" href="#fig-aggl-c1"><span class="std std-numref">Fig. 1.6</span></a> - <a class="reference internal" href="#fig-aggl-c3"><span class="std std-numref">Fig. 1.8</span></a>) were used. The volume visualization can become quickly a large file. The number of elements can be reduced by the parameter

    Visu_sJ_limit= 0.1                  ! for DebugvisuLevel=2, only write elements with a scaled Jacobian < given limit

that skips elements with a scaled Jacobian > limit.<br>

If there is a need for assistance of visualizing the HOPR output visit {ref}`tutorials/index_visualization:Visualization`. 