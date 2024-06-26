# Mesh uncurving

As high order numerical methods require a high order representation of the boundary, a number of methods have been developed to provide this level of accuracy. In some curving techniques, such as agglomerated meshes, it is not just the boundary that is curved, but the entire volume of the mesh is made up of curved cells. However, if the cell is not adjacent to curved boundaries, it is numerically preferable to use linear cells. Thus, HOPR provides a function for uncurving curved meshes, optionally at a given distance to the boundary. The parameter `nCurvedBoundaryLayers` specifies the number of cells from a curved boundary (i.e. boundaries with `curveIndex>0`) that stay curved. For the remaining mesh, a (tri-)linear mapping using only the corner nodes is applied.

- For `nCurvedBoundaryLayers=-1` the whole mesh stays curved (default).
- For `nCurvedBoundaryLayers=0` in the first cell only the boundary itself remains curved, all other faces and cells in the mesh will be linear.
- For `nCurvedBoundaryLayers=1-n` the first n cells away from the boundary remain curved.

These choices are depicted in the figures bellow, and the table lists the scaled Jacobian ranges for the elements. The mesh becomes significantly less distorted if only the first cell is curved. Note that it is often required to curve more than the first cell, especially for fine curved boundary layer meshes.

<table align="center" style="width:100%">
  <tr>
    <td style="width:25%">
        <figure id="fig-all-curved">
        <a class="reference internal image-reference" href="../All_curved.png"><img alt="../All_curved.png" src="../All_curved.png" style="height: 200px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.1 </span><span class="caption-text">NACA-profile with all elements curved</span><a class="headerlink" href="#fig-all-curved" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-0-curved">
        <a class="reference internal image-reference" href="../0_curved.png"><img alt="../0_curved.png" src="../0_curved.png" style="height: 200px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.2 </span><span class="caption-text">NACA-profile with only the profile boundary curved</span><a class="headerlink" href="#fig-0-curved" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-1-curved">
        <a class="reference internal image-reference" href="../1_curved.png"><img alt="../1_curved.png" src="../1_curved.png" style="height: 200px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.3 </span><span class="caption-text">NACA-profile with the first cell curved</span><a class="headerlink" href="#fig-1-curved" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-3-curved">
        <a class="reference internal image-reference" href="../3_curved.png"><img alt="../3_curved.png" src="../3_curved.png" style="height: 200px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.4 </span><span class="caption-text">NACA-profile with the first three cells curved</span><a class="headerlink" href="#fig-3-curved" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

```{table} Distribution of the smallest scaled Jacobians per element.
---
name: tab:Distribution of the smallest scaled Jacobians per element 
---
  | **Number of elements with scaled Jacobians ranging between:**      |  <0  | <0.1 | <0.2  | <0.3 | <0.4 | <0.5 | <0.6 | <0.7 | <0.8 | <0.9 | <1.0 |  
  | :------                                                            | :--: | :--: | :--:  | :--: | :--: | :--: | :--: | :--: | :--: | :--: | :--: |
  | **Only boundary is curved:**                                       |  0   |  0   |  0    |   0  |  8   |   4  |   8  |  208 | 100  | 276  | 1124 |
  | **First element is curved:**                                       |  0   |  0   |  0    |   0  |  8   |   4  |  16  |  208 | 100  | 288  | 1104 |
  | **First 3 elements are curved:**                                   |  0   |  0   |  0    |   0  |  16  |  12  |  32  |  224 |  92  | 284  | 1068 | 
  | **All elements are curved:**                                       |  0   |  0   |  0    |   0  |  136 | 596  | 748  |  200 |  12  |  36  |    0 |
```

<h4>Parameter File<a class="headerlink" href="#parameter-file" title="Permalink to this heading"></a></h4>

To test this feature, add `nCurvedBoundaryLayers=n` to the parameter file, which is also found in

    tutorials/3-01-agglomeration_NACA/parameter.ini