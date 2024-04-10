# Block-Structured Meshes
This tutorial shows how to agglomerate block-structured grids with linear elements to get a high-order mesh consisting of fully curved hexahedral elements. The parameter file can be found in

    tutorials/3-01-agglomeration_NACA/parameter.ini

## Restrictions on the block-structured meshes

HOPR poses the following restrictions on the provided block-structured meshes:
- **One block face** must be exclusively associated to **one boundary condition**. Block faces with multiple BCs or a combination of boundary and internal element faces on a single block face are likely to produce wrong mesh topology! The user is responsible for **splitting blocks** in such cases. Otherwise, HOPR will be unable to perform the mesh connect step!
- When providing a block-structured mesh for agglomeration, only one **unique factor** is permitted in the `I,J,K` dimension of **all blocks**!

Each boundary conditions present in the provided mesh must be matched in the parameter file by its name and an associated boundary type:

            BoundaryName = wall_1
            BoundaryType = (/4,0,0,0/)
            BoundaryName = wall_2
            BoundaryType = (/4,0,0,0/)


Boundary names can match multiple BCs by specifying only the common part of the boundary name string. In the example above, `BoundaryName=wall` would match all boundary conditions.

## Initial Meshes

Agglomeration of block-structured meshes provides a simple and robust curving technique for the generation of three-dimensional high-order meshes. In HOPR, mesh agglomeration is controlled by the `MeshIsAlreadyCurved` parameter. The application of mesh agglomeration is demonstrated on two meshes of a NACA-profile provided as CGNS files. Here, mesh 1 is suitable for inviscid simulations without boundary layer refinement and mesh 2 features boundary layer refinement.

<h5>Mesh1: NACA0012_icem_32elems.cgns<a class="headerlink" href="#Mesh1" title="Permalink to this heading"></a></h4>

<table align="center">
  <tr>
    <td>
        <figure id="fig-aggl-nv-fern">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-fern.jpg"><img alt="../../../tutorials/figures/Aggl-nv-fern.jpg" src="../../../tutorials/figures/Aggl-nv-fern.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.1 </span><span class="caption-text">Cross-section of initial mesh 1</span><a class="headerlink" href="#fig-aggl-nv-fern" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure id="fig-aggl-nv-nah">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-nah.jpg"><img alt="../../../tutorials/figures/Aggl-nv-nah.jpg" src="../../../tutorials/figures/Aggl-nv-nah.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.2 </span><span class="caption-text">Zoom on the leading edge of mesh 1</span><a class="headerlink" href="#fig-aggl-nv-nah" title="Permalink to this image"></a></p>
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
        <p><span class="caption-number">Fig. 1.3 </span><span class="caption-text">Cross-section of initial mesh 2</span><a class="headerlink" href="#fig-aggl-v-fern" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure id="fig-aggl-v-nah">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-v-nah.jpg"><img alt="../../../tutorials/figures/Aggl-v-nah.jpg" src="../../../tutorials/figures/Aggl-v-nah.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.4 </span><span class="caption-text">Zoom on the leading edge of mesh 2</span><a class="headerlink" href="#fig-aggl-v-nah" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

All of the following figures were created with mesh 1. 

<h4>Agglomeration Curving Technique<a class="headerlink" href="#agglomeration-curving-technique" title="Permalink to this heading"></a></h4>
When using agglomeration, the important parameters are

    MeshIsAlreadyCurved = T
    useCurved           = T
    BoundaryOrder       = 5

which lead to a coarsening in all three dimensions of the structured mesh, using the internal points as interpolation points for the curved mapping. The number of elements in each direction of the structured block must be a multiple number of `BoundaryOrder`-1! This situation is illustrated on an exemplary mesh in <a class="reference internal" href="#fig-nSkip"><span class="std std-numref">Fig. 1.5</span></a>. For `BoundaryOrder=2`, the initial linear mesh retained without agglomeration.

<figure class="align-center" id="fig-nSkip">
    <a class="reference internal image-reference" href="../../../tutorials/figures/nSkip.jpg"><img alt="../../../tutorials/figures/nSkip.jpg" src="../../../tutorials/figures/nSkip.jpg" style="width: 50%;" /></a>
    <figcaption>
    <p><span class="caption-number">Fig. 1.5 </span><span class="caption-text">Block-structuring with the parameter <code class="docutils literal notranslate"><span class="pre">BoundaryOrder=2/3/5</span></code>, (<code class="docutils literal notranslate"><span class="pre">BoundaryOrder</span></code>-1)^3 elements are grouped together.</span><a class="headerlink" href="#fig-nSkip" title="Permalink to this image"></a></p>
    </figcaption>
</figure>

In addtion, the parameter `nSkip` can be set to coarsen the initial mesh. 

## Description of Parameters

The following table describes all parameters associated with agglomeration. A description of all parameters is given in {ref}`userguide/parameters:List of Parameters`. 

```{table} Block-Structured Meshes: Description of Parameters.
---
name: tab:Block-Structured Meshes Description of Parameters
---
  | Parameters            | Setting      | Description                                                                                 |
  | :------               | :----------: | :---------------------------                                                                |
  | `MeshIsAlreadyCurved` | `T`          | Enables the agglomeration                                                                   |
  | `nSkip`               | `2`          | Coarsing of block-structured meshes<br>1: no skip<br>2: use every second point<br>...       |
  | `nSkipZ`              | `2`          | Only if the mesh is extruded in z-direction, a different nSkip can be given in z-direction. |
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
        <p><span class="caption-number">Fig. 1.7 </span><span class="caption-text">Front view of the leading edge of the mesh (<code class="docutils literal notranslate"><span class="pre">BoundaryOrder=5</span></code>). One block consists of 4x4x4 = 64 elements. The blocks' edges (blue lines) are the boundary of the curved elements. The initial structured mesh is shown in grey. All nodes/connections of the white lines are interpolation points</span><a class="headerlink" href="#fig-aggl-c2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:33%">
        <figure id="fig-aggl-c3">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-c3.jpg"><img alt="../../../tutorials/figures/Aggl-c3.jpg" src="../../../tutorials/figures/Aggl-c3.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.8 </span><span class="caption-text">High-order curved mesh generated through agglomeration.</span><a class="headerlink" href="#fig-aggl-c3" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

<h4>Coarsening<a class="headerlink" href="#coarsening" title="Permalink to this heading"></a></h4>

Mesh coarsening is controlled by two parameters: `nSkip` applies to all structured directions equally, and `nSkipZ` can be used for z-extruded meshes.<br>

<!-- `nSkip` Variations: <br> -->

<table align="center" style="width:100%">
  <tr>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip0">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip0.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip0.jpg" src="../../../tutorials/figures/Aggl-nv-skip0.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.9 </span><span class="caption-text">Initial mesh (<code class="docutils literal notranslate"><span class="pre">nSkip=1</span></code>)</span><a class="headerlink" href="#fig-aggl-nv-skip0" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip2">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip2.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip2.jpg" src="../../../tutorials/figures/Aggl-nv-skip2.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.10 </span><span class="caption-text">Coarsened mesh (<code class="docutils literal notranslate"><span class="pre">nSkip=2</span></code>)</span><a class="headerlink" href="#fig-aggl-nv-skip2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip4">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip4.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip4.jpg" src="../../../tutorials/figures/Aggl-nv-skip4.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.11 </span><span class="caption-text">High-order curved mesh generated through agglomeration.</span><a class="headerlink" href="#fig-aggl-nv-skip4" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip8">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip8.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip8.jpg" src="../../../tutorials/figures/Aggl-nv-skip8.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.12 </span><span class="caption-text">High-order curved mesh generated through agglomeration.</span><a class="headerlink" href="#fig-aggl-nv-skip8" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

The figure on the left side shows the initial mesh. The parameter `nSkip=1` uses every point of the initial mesh. The mesh in the middle-left shows the mesh if `nSkip` is set to 2. That means that one node in each direction of the respective coordinate system is skipped and that the size of the new element reaches to the next node. The skipped nodes will no longer be used for mesh generation, including curving. The parameter `nSkipZ` has the same function as `nSkip` but only applies towards the z-direction. By providing the `nSkipZ` parameter, the corresponding entry in `nSkip` is ignored.<br>

The following figures illustrate possible combinations of the `nSkip` and `nSkipZ` parameters, each applied to the identical initial mesh. Keep in mind that reading in a block-structured mesh only works if the value for the parameter(s) `nSkip` (and `nSkipZ`) is a common divisor of the number of all mesh elements for each axis.<br>

<!-- `nSkip`-`nSkipZ` Combinations:  -->

<table align="center" style="width:100%">
  <tr>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip2z-">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip2z-.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip2z-.jpg" src="../../../tutorials/figures/Aggl-nv-skip2z-.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.13 </span><span class="caption-text">Mesh 1 with following parameter settings: <code class="docutils literal notranslate"><span class="pre">nSkip=2</span></code> <code class="docutils literal notranslate"><span class="pre">!nSkipZ = ..</span></code></span><a class="headerlink" href="#fig-aggl-nv-skip2z-" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip2z1">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip2z1.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip2z1.jpg" src="../../../tutorials/figures/Aggl-nv-skip2z1.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.14 </span><span class="caption-text">Mesh 1 with following parameter settings: <code class="docutils literal notranslate"><span class="pre">nSkip=2</span></code> <code class="docutils literal notranslate"><span class="pre">nSkipZ=1</span></code></span><a class="headerlink" href="#fig-aggl-nv-skip2z1" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip2z-">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip2z-.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip2z-.jpg" src="../../../tutorials/figures/Aggl-nv-skip2z-.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.15 </span><span class="caption-text">Mesh 1 with following parameter settings: <code class="docutils literal notranslate"><span class="pre">nSkip=2</span></code> <code class="docutils literal notranslate"><span class="pre">nSkipZ=2</span></code></span><a class="headerlink" href="#fig-aggl-nv-skip2z-" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-aggl-nv-skip2z2">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Aggl-nv-skip2z2.jpg"><img alt="../../../tutorials/figures/Aggl-nv-skip2z2.jpg" src="../../../tutorials/figures/Aggl-nv-skip2z2.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.16 </span><span class="caption-text">Mesh 1 with following parameter settings: <code class="docutils literal notranslate"><span class="pre">nSkip=2</span></code> <code class="docutils literal notranslate"><span class="pre">nSkipZ=4</span></code></span><a class="headerlink" href="#fig-aggl-nv-skip2z2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

<h4>Correction for z-Extruded Meshes<a class="headerlink" href="#correction-for-z-extruded-meshes" title="Permalink to this heading"></a></h4>

Extruded meshes often suffer from issues with limited coordinate precision, preventing HOPR from finding corresponding surface elements on the upper/lower boundary. HOPR included a built-in correction routine for such cases. This function is not restricted to block-structured meshes, but can be applied to all meshes which are extruded along the z-direction. Enabling this setting requires five more parameters to be set.

```{table} Correction for z-extruded meshes: Description of parameters.
---
name: tab:Correction for z-extruded meshes Description of parameters
---
  | Parameters      | Setting      | Description                                                                          |
  | :------         | :----------: | :---------------------------                                                         |
  | `doZcorrection` | `F`          | All elements are aligned exactly along z-direction to suppress grid generator errors |
  | `nElemsZ`       | `1`          | The number of elements in z-direction (after agglomeration!)                         |
  | `zStart`        | `0.`         | Set minimum z-coordinate                                                             |
  | `zLength`       | `1.0`        | Set length of domain in z-direction                                                  |
  | `zPeriodic`     | `T`          | Boundary conditions (`z_plus` and `z_minus`) are set to periodic.                    |
```

<!-- ## Output Visualization -->
<!---->
<!-- The figures here were visualizations of the mesh NACA0012_icem_32elems.cgns. Therefore, the files NACA0012_VISCOUS_Debugmesh.vtu (<a class="reference internal" href="#fig-aggl-nv-fern"><span class="std std-numref">Fig. 1.1</span></a> - <a class="reference internal" href="#fig-aggl-v-nah"><span class="std std-numref">Fig. 1.4</span></a>, <a class="reference internal" href="#fig-aggl-nv-skip0"><span class="std std-numref">Fig. 1.9</span></a> - <a class="reference internal" href="#fig-aggl-nv-skip2z2"><span class="std std-numref">Fig. 1.16</span></a>) and NACA0012_VISCOUS_SplineVol.vtu (<a class="reference internal" href="#fig-aggl-c1"><span class="std std-numref">Fig. 1.6</span></a> - <a class="reference internal" href="#fig-aggl-c3"><span class="std std-numref">Fig. 1.8</span></a>) were used. The volume visualization can become quickly a large file. The number of elements can be reduced by the parameter -->
<!---->
<!--     Visu_sJ_limit= 0.1                  ! for DebugvisuLevel=2, only write elements with a scaled Jacobian < given limit -->
<!---->
<!-- that skips elements with a scaled Jacobian > limit.<br> -->
<!---->
<!-- If there is a need for assistance of visualizing the HOPR output visit {ref}`tutorials/index_visualization:Visualization`.  -->
