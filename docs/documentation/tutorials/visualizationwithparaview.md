# Visualization with Paraview

<figure class="align-center" id="fig-paraview-logo">
    <a class="reference internal image-reference" href="../../../tutorials/figures/Paraview-logo.png"><img alt="../../../tutorials/figures/Paraview-logo.png" src="../../../tutorials/figures/Paraview-logo.png" style="width: 30%;" /></a>
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
        <a class="reference internal image-reference" href="../../../tutorials/figures/PW-viewsettings.jpg"><img alt="../../../tutorials/figures/PW-viewsettings.jpg" src="../../../tutorials/figures/PW-viewsettings.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.2 </span><span class="caption-text"></span><a class="headerlink" href="#fig-pw-viewsettings" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure id="fig-pw-parallel">
        <a class="reference internal image-reference" href="../../../tutorials/figures/PW-parallel.jpg"><img alt="../../../tutorials/figures/PW-parallel.jpg" src="../../../tutorials/figures/PW-parallel.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.3 </span><span class="caption-text"></span><a class="headerlink" href="#fig-pw-parallel" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

If the polynomial degree of supersampling is higher than 1 the mesh elements will be subdivided depending of the polynomial degree. As a result, it is not apparent anymore where the elements begin and where they end. Because of that two filters have to be applied which make the actual elements visible again in high resolution.

<table align="center">
  <tr>
    <td>
        <figure id="fig-pw-extractsurface">
        <a class="reference internal image-reference" href="../../../tutorials/figures/PW-extractsurface.jpg"><img alt="../../../tutorials/figures/PW-extractsurface.jpg" src="../../../tutorials/figures/PW-extractsurface.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.4 </span><span class="caption-text"></span><a class="headerlink" href="#fig-pw-extractsurface" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure id="fig-pw-featureedges">
        <a class="reference internal image-reference" href="../../../tutorials/figures/PW-featureedges.jpg"><img alt="../../../tutorials/figures/PW-featureedges.jpg" src="../../../tutorials/figures/PW-featureedges.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.5 </span><span class="caption-text"></span><a class="headerlink" href="#fig-pw-featureedges" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

## Custom Filter HO-visualization
For simplifing and accelerating this visualization prozess of the HOPR output a custom filter named HO-visualization is provided. This custom filter converts the steps of filter selection to a single step prozedure. After the custom filter was created it has to be imported into ParaView. Therefore, go to Tools > Manage Custom Filters ... (<a class="reference internal" href="#fig-pw-custom"><span class="std std-numref">Fig. 1.6</span></a>), click on Import and add the custom filter HO-visualization (<a class="reference internal" href="#fig-pw-custommanager"><span class="std std-numref">Fig. 1.7</span></a>).

<table align="center">
  <tr>
    <td>
        <figure id="fig-pw-custom">
        <a class="reference internal image-reference" href="../../../tutorials/figures/PW-custom.jpg"><img alt="../../../tutorials/figures/PW-custom.jpg" src="../../../tutorials/figures/PW-custom.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.6 </span><span class="caption-text"></span><a class="headerlink" href="#fig-pw-custom" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure id="fig-pw-custommanager">
        <a class="reference internal image-reference" href="../../../tutorials/figures/PW-custommanager.jpg"><img alt="../../../tutorials/figures/PW-custommanager.jpg" src="../../../tutorials/figures/PW-custommanager.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 1.7 </span><span class="caption-text"></span><a class="headerlink" href="#fig-pw-custommanager" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

Once the filter is imported it appears in the alphabetically sorted list of the filters (<a class="reference internal" href="#fig-pw-ho"><span class="std std-numref">Fig. 1.8</span></a>) 

<figure class="align-center" id="fig-ho">
    <a class="reference internal image-reference" href="../../../tutorials/figures/PW-ho.jpg"><img alt="../../../tutorials/figures/PW-ho.jpg" src="../../../tutorials/figures/PW-ho.jpg" style="width: 30%;" /></a>
    <figcaption>
    <p><span class="caption-number">Fig. 1.8 </span><span class="caption-text"></span><a class="headerlink" href="#fig-ho" title="Permalink to this image"></a></p>
    </figcaption>
</figure>

and can be selected (<a class="reference internal" href="#fig-pw-final"><span class="std std-numref">Fig. 1.9</span></a>).

<figure class="align-center" id="fig-final">
    <a class="reference internal image-reference" href="../../../tutorials/figures/PW-final.jpg"><img alt="../../../tutorials/figures/PW-final.jpg" src="../../../tutorials/figures/PW-final.jpg" style="width: 30%;" /></a>
    <figcaption>
    <p><span class="caption-number">Fig. 1.9 </span><span class="caption-text"></span><a class="headerlink" href="#fig-final" title="Permalink to this image"></a></p>
    </figcaption>
</figure>