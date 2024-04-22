# Generation of Hexahedral Meshes

Since many solvers require purely hexahedral meshes, HOPR implements a subdivision strategy to split meshes consisting of tetrahedra, prisms and hexahedra into purely hexahedral meshes. This feature is activated using the parameter `splitToHex=T`. Note, that pyramids cannot be decomposed to hexahedra in a straightforward way, thus this feature cannot be applied to meshes containing pyramids. Also note that this option is currently limited to linear meshes.

<table align="center" style="width:100%">
  <tr>
    <td style="width:50%">
        <figure id="fig-splittohex0">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Splittohex0.png"><img alt="../../../tutorials/figures/Splittohex0.png" src="../../../tutorials/figures/Splittohex0.png" style="height: 350px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 3.1 </span><span class="caption-text">Mesh consisting of 6 tetrahedra</span><a class="headerlink" href="#fig-splittohex0" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:50%">
        <figure id="fig-splittohex1">
        <a class="reference internal image-reference" href="../../../tutorials/figures/Splittohex1.png"><img alt="../../../tutorials/figures/Splittohex1.png" src="../../../tutorials/figures/Splittohex1.png" style="height: 350px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 3.2 </span><span class="caption-text">Each tetrahedron subdivided into 4 hexahedra</span><a class="headerlink" href="#fig-splittohex1" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

<h4>Parameter File<a class="headerlink" href="#parameter-file" title="Permalink to this heading"></a></h4>

To test this feature, set `elemtype` to either `104` (Tetrahedron) or `106` (Prism with triangular base) and add `splitToHex=T` to the parameter file, which is found in

    tutorials/1-01-cartbox/parameter.ini