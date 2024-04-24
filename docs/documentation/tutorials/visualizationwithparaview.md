# Visualization with Paraview

<figure class="align-center" id="fig-paraview-logo">
    <a class="reference internal image-reference" href="../Paraview-logo.png"><img alt="../Paraview-logo.png" src="../Paraview-logo.png" style="width: 30%;" /></a>
    <figcaption>
    <p><span class="caption-number">Fig. 1.1 </span><span class="caption-text"></span><a class="headerlink" href="#fig-paraview-logo" title="Permalink to this image"></a></p>
    </figcaption>
</figure>

Open source multiple-platform application for interactive, scientific visualization. For more Information visit [https://www.paraview.org/](https://www.paraview.org/)

## Parameter Settings
    Debugvisu=T
    outputFormat=0

## Recommended Settings
ParaView is susceptible for defective output visualizations in cases of using high order meshes or increasing the polynomial degree of supersampling. For this reason, it is recommended to edit several settings to get a correct high resolution visualization. At first the way of projection should be changed. Therefore, go to Edit > View Settings ... (<a class="reference internal" href="#fig-pw-viewsettings"><span class="std std-numref">Fig. 1.2</span></a>) and activate use parallel projection (<a class="reference internal" href="#fig-pw-parallel"><span class="std std-numref">Fig. 1.3</span></a>).

<table align="center">
  <tr>
    <td>
        <figure id="fig-pw-viewsettings">
        <a class="reference internal image-reference" href="../PW-viewsettings.jpg"><img alt="../PW-viewsettings.jpg" src="../PW-viewsettings.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.2 </span><span class="caption-text"></span><a class="headerlink" href="#fig-pw-viewsettings" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure id="fig-pw-parallel">
        <a class="reference internal image-reference" href="../PW-parallel.jpg"><img alt="../PW-parallel.jpg" src="../PW-parallel.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.3 </span><span class="caption-text"></span><a class="headerlink" href="#fig-pw-parallel" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

If the polynomial degree of supersampling is higher than 1 the mesh elements will be subdivided depending of the polynomial degree. As a result, it is not apparent anymore where the elements begin and where they end. Because of that the two filters "Extract Surfaces" and "Extract Edges" have to be applied which make the actual elements visible again in high resolution. They can be found under Filters > Alphabetical.

<table align="center">
  <tr>
    <td>
        <figure id="fig-pw-extractsurface">
        <a class="reference internal image-reference" href="../PW-extractsurface.jpg"><img alt="../PW-extractsurface.jpg" src="../PW-extractsurface.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.4 </span><span class="caption-text"></span><a class="headerlink" href="#fig-pw-extractsurface" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure id="fig-pw-featureedges">
        <a class="reference internal image-reference" href="../PW-featureedges.jpg"><img alt="../PW-featureedges.jpg" src="../PW-featureedges.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.5 </span><span class="caption-text"></span><a class="headerlink" href="#fig-pw-featureedges" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

