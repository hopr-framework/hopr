# Mesh Refinement

It is often desirable to refine existing meshes, which is done by subdividing the elements into smaller elements.
Up to now, the refinement only works for hexahedra with linear edges.
For meshes containing other element types than hexahedra, this option is not applicable.
The feature is controlled by the flag `nFineHexa=x`, where x specifies the number of subdivisions of each element in each spatial direction.

<table align="center" style="width:100%">
  <tr>
    <td style="width:50%">
        <figure id="fig-hopr-nfine1">
        <a class="reference internal image-reference" href="../Hopr_nfine1.png"><img alt="../Hopr_nfine1.png" src="../Hopr_nfine1.png" style="height: 350px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.1 </span><span class="caption-text">Standard mesh</span><a class="headerlink" href="#fig-hopr-nfine1" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:50%">
        <figure id="fig-Hopr-nfine2">
        <a class="reference internal image-reference" href="../Hopr_nfine2.png"><img alt="../Hopr_nfine2.png" src="../Hopr_nfine2.png" style="height: 350px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.2 </span><span class="caption-text">All elements refined by a factor of 2</span><a class="headerlink" href="#fig-Hopr-nfine2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>


<h4>Parameter File<a class="headerlink" href="#parameter-file" title="Permalink to this heading"></a></h4>

To test this feature, add `nFineHexa=2` to the parameter file, which is also found in

    tutorials/2-01-external_meshes_sphere/parameter.ini