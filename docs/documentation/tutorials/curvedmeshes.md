# Curved Meshes
## Curved Structured Mesh
This tutorial shows how to generate a curved structured mesh with an equidistant or with a stretched element arrangement alternatively.
The parameter file can be found in 

    tutorials/1-05-curved_structured/parameter.ini


### Curved Structured Mesh: Description of Parameters
To generate a curved structured mesh the following parameter settings are mandatory: 
- `Mode=11` (curved structured block with hexahedral elements):<br>
  This mode activates a transformation of the cartesian coordinate system to a turned cylindrical coordinate system. The element distribution which the user can determine by the parameter `nElems` refers subsequently to the new coordinate system.

  <figure class="align-center" id="fig-carttocurve">
      <a class="reference internal image-reference" href="../_images/Carttocurve.jpg"><img alt="../_images/Carttocurve.jpg" src="../_images/Carttocurve.jpg" style="width: 30%;" /></a>
      <figcaption>
      <p><span class="caption-number">Fig. 2.1 </span><span class="caption-text">Transformation of the coordinate system.</span><a class="headerlink" href="#fig-carttocurve" title="Permalink to this image"></a></p>
      </figcaption>
  </figure>

- `nZones=1`
- `MeshType=3` (for curved mesh)

The HOPR user has to choose whether he wants to generate a half or a full cylindrical mesh. Therefore the new parameter `WhichMapping` is provided. For specifying the general shape of the (half) cylinder three parameters are provided: `R_0`, `R_INF` and `DZ`. Their meaning is visualized in <a class="reference internal" href="#fig-curvecos"><span class="std std-numref">Fig. 2.2</span></a>. It must be taken into account that the value for the inner radius (`R_0`) must not be zero and the value for `DZ` corresponds to the half thickness of the (half) cylinder. 

<figure class="align-center" id="fig-curvecos">
    <a class="reference internal image-reference" href="../_images/CurveCOS.jpg"><img alt="../_images/CurveCOS.jpg" src="../_images/CurveCOS.jpg" style="width: 30%;" /></a>
    <figcaption>
    <p><span class="caption-number">Fig. 2.2 </span><span class="caption-text">Visualization of the parameters which determine the shape.</span><a class="headerlink" href="#fig-curvecos" title="Permalink to this image"></a></p>
    </figcaption>
</figure>

The assignment of the boundary conditions to the surfaces refers also to the new coordinate system but are defined as before by the parameter `BCIndex (z-,y-,x+,y+,x-,z+)`. For the case that the parameter `WhichMapping` is set to 4 (full cylindrical mesh) the third and fith surface (x+, x-) coincide and the corresponding components of the `BCIndex` vector has to set to zero.

All new abovementioned parameters are explained below. A description of all parameters of the parameter file can be found in {ref}`userguide/parameters:List of Parameters`. 

```{table} Curved Structured Mesh: Description of Parameter.
---
name: tab:Curved_Structured_Mesh_Description_of_Parameter
---
  | Parameters      |Setting                | Description | 
  | :------         | :----------:          | :---------------------------     |
  | `Meshtype`      | `3`                   | 1: Cube (origin + dimensions)<br>2: Bilinear (8 points CGNS notation)<br>3: Curved (add WhichMapping)   |
  | `WhichMapping`  | `4`                   |  Type of mapping using 6 boundary faces to build the curved structured mesh:<br>3: Half cylinder<br>4: Full cylinder  |
  | `R_0`           | `0.5`                 |  Inner radius of curved structured mesh. The Value 0 is not allowed.   |
  | `R_INF`         | `20`                  |  Outer radius of curved structured mesh.    |
  | `DZ`            | `2`                   |  Dimension in z-direction: `[-DZ,DZ]`     |
```

### Curved Structured Mesh: Stretching Functions
Similar to straight-edged boxes one can generate curved structured meshes with a stretched element arrangement. Therefore three new parameters have to be defined in the parameter file: stretchType, fac and DXmaxToDXmin. In contrast to a cartesian box where two parameters can used individually or in combination to stretch the elements (factor, l0) here a type of stretching has to select with the parameter stretchType. For this reason and the fact that for different stretchTypes different stretching functions are required it is advisable to define all three parameters. 

All three parameters are explained below. A description of all parameters of the parameter file can be found in {ref}`userguide/parameters:List of Parameters`. 
```{table} Curved Structured Mesh: Stretching Functions.
---
name: tab:Curved_Structured_Mesh_Stretching_Functions
---
  | Parameters             |Setting                | Description | 
  | :------                | :----------:          | :---------------------------     |
  | `stretchTypetype`      | ` 	(/3,1,0/)`         | This parameter manages the (de)activation of the stretching functions for all axis. For this reason the parameter is a vector with three components.<br>0: Stretching is deactivated<br>1: Stretching with a factor<br>2: Stretching with a legnth ratio<br>3: Stretching with a bell function.   |
  | `fac         `         | `(/1.5,2.2,10/)`                   |  Stretching factor of the elements in the direction of the turned local cylindrical coordinate axis. A value >1 means an increase of the element size in the direction of the coordinate axis, however, a value of the intervall (0,1) means a decrease. The value 1 does not affect the element sizes and means an deactivation of the stretching function for this axis. The value 0 is only allowed if the stretching function for this axis is deactivated (`stretchType` vector component for this axis is 0). Furthermore the stretching behaviour can be mirrored by adding a negative sign to the values. If the `stretchType` vector component for an axis is 3, the factor will be multiplied by -1 if the half distance is reached. In addition, `fac` has not the significant influence on the element arrangement anymore but the parameter `DXmaxToDXmin`.<br> In case of `(/1.5,2.2,10/)` each following element in x-direction is stretched by the factor 1.5, in y-direction by the factor 2.2 and in the direction of the z-axis by the factor 10 (dependent on `stretchType`)   |
  | `DXmaxToDXmin`         | ` 	(/6.,100.,1./)`                 |  This parameter specify the frame ratio of the maximum element size to the minimum element size for the stretched element arrangement. If the `stretchType` vector component for an axis is 3, the element arrangement is affected significantly by `DXmaxToDXmin` instead of the parameter `fac`.<br> In case of `(/6,100,1/)` the maximum element size in x-direction can be 6 times larger than the minimum element size. In y-direction the maximum element size can be 100 times larger than the minimum element size. The value 1, here set for ratio of the z-direction, is used typically for a deactivated stretching.    |
```

<h4>Calculation Formulas<a class="headerlink" href="#calculation_formulas" title="Permalink to this heading"></a></h4>
For a better understanding how the element sizes are calculated the formulas for different `stretchType` settings are shown below.

- Calculation of the element size for `stretchType = 1`:

  $$ {\Delta x_{i+1} = f \cdot \Delta x_{i} f = \left(\frac{\Delta x_{max}}{\Delta x_{min}} \right)^{1/(nElems - 1)} }$$

- Calculation of the element size for `stretchType = 3`:<br>

  $$ \Delta x(\xi) \sim 1 + \left( \frac{\Delta x_{max}}{\Delta x_{min}}-1\right)\cdot \left( \frac{\exp[-(\xi \cdot f)^2] - \exp[-f^2]}{\exp[0] - \exp[-f^2]}\right) $$

<figure class="align-center" id="fig-stretching-math">
    <a class="reference internal image-reference" href="../_images/Stretching-math.jpg"><img alt="../_images/Stretching-math.jpg" src="../_images/Stretching-math.jpg" style="width: 30%;" /></a>
    <figcaption>
    <p><span class="caption-number">Fig. 2.3 </span><span class="caption-text">Plot of the calculation function if the parameter `stretchType` is set to 3 ($ f $ means `fac`, $ \frac{\Delta x_{max}}{\Delta x_{min}} $ means `DXmaxToDXmin`). If the value of fac increases, the peakedness will increase and the element sizes near the bonudaries will decrease.</span><a class="headerlink" href="#fig-stretching-math" title="Permalink to this image"></a></p>
    </figcaption>
</figure>


<h4>Exemplary Stretching Cases<a class="headerlink" href="#exemplary-stretching-cases" title="Permalink to this heading"></a></h4>

Furthermore, three different stretching cases are presented below with a full circle (`WhichMapping=4`) and an element distribution `nElems=(/8,6,4/)`. Just x- and y-values were visualized. 

<table align="center" style="width:100%">
  <tr>
    <td style="width:25%">
        <figure id="fig-stretch-curve-ex1">
        <a class="reference internal image-reference" href="../_images/Stretch-curve_ex1.jpg"><img alt="../_images/Stretch-curve_ex1.jpg" src="../_images/Stretch-curve_ex1.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.4 </span><span class="caption-text">Non-stretched element arrangement.<br><code class="docutils literal notranslate"><span class="pre">nElems        =(/8,6,4/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">stretchType   =(/1,1,0/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">fac           =(/1,1,0./)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">DXmaxToDXmin  =(/100.,100.,1/)</span></code></span><a class="headerlink" href="#fig-stretch-curve-ex1" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-stretch-curve-ex2">
        <a class="reference internal image-reference" href="../_images/Stretch-curve_ex2.jpg"><img alt="../_images/Stretch-curve_ex2.jpg" src="../_images/Stretch-curve_ex2.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.5 </span><span class="caption-text">Stretched element arrangement. The element size in the direction of the x-axis increases by a factor of 1.5. In the direction of the y-axis it increases by the factor of 2.2.<br><code class="docutils literal notranslate"><span class="pre">nElems        =(/8,6,4/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">stretchType   =(/1,1,0/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">fac           =(/1.5,2.2,0./)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">DXmaxToDXmin  =(/100.,100.,1/)</span></code></span><a class="headerlink" href="#fig-stretch-curve-ex2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-stretch-curve-ex3">
        <a class="reference internal image-reference" href="../_images/Stretch-curve_ex3.jpg"><img alt="../_images/Stretch-curve_ex3.jpg" src="../_images/Stretch-curve_ex3.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.6 </span><span class="caption-text">The `stretchType` parameter is set to 3 for the x-axis. The plot of the belonging calculation function shows that the element sizes increase immediately. In the direction of the y-axis the element size increases by the factor of 2.2.<br><code class="docutils literal notranslate"><span class="pre">nElems        =(/8,6,4/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">stretchType   =(/3,1,0/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">fac           =(/1.5,2.2,0./)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">DXmaxToDXmin  =(/100.,100.,1/)</span></code></span><a class="headerlink" href="#fig-stretch-curve-ex3" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:25%">
        <figure id="fig-stretch-curve-ex3">
        <a class="reference internal image-reference" href="../_images/Stretch-curve_ex4.jpg"><img alt="../_images/Stretch-curve_ex4.jpg" src="../_images/Stretch-curve_ex4.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.7 </span><span class="caption-text">The `stretchType` parameter is set to 3 for the x-axis and the y-axis. although the fac values are different the plots of the belonging calculation function looks very similar to each other.<br><code class="docutils literal notranslate"><span class="pre">nElems        =(/8,6,4/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">stretchType   =(/3,3,0/)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">fac           =(/1.5,2.2,0./)</span></code><br>
        <code class="docutils literal notranslate"><span class="pre">DXmaxToDXmin  =(/100.,100.,1/)</span></code></span><a class="headerlink" href="#fig-stretch-curve-ex3" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>
<br>
<br>


### Curved Structured Mesh: Sketchs

In the following two exemplary curved structured meshes are presented. The first mesh shall consist of twelve elements in x-direction, eight elements in y-direction and four elements in z-direction, all equidistant. The sketch of this problem is shown in <a class="reference internal" href="#fig-cylinder"><span class="std std-numref">Fig. 2.8</span></a>. The belonging parameter file can be found in Parameterfile Curved Structured Mesh.<br>
The second one consists of the same number of elements in each direction but instead of an equidistant element arrangement the elements shall be stretched. This sketch is presented in <a class="reference internal" href="#fig-cylinder2"><span class="std std-numref">Fig. 2.9</span></a>. To get the corresponding output by HOPR one have to edit the Parameterfile Curved Structured Mesh manually before executing because the stretching functions `stretchType`, `fac` and `DXmintoDXmax` are commended out.

<table align="center" style="width:100%">
  <tr>
    <td style="width:50%">
        <figure id="fig-cylinder">
        <a class="reference internal image-reference" href="../_images/Cylinder.jpg"><img alt="../_images/Cylinder.jpg" src="../_images/Cylinder.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.8 </span><span class="caption-text">Sketch of the 1 zone curved structured mesh. The full cirlce mesh (<code class="docutils literal notranslate"><span class="pre">WhichMapping=4</span></code>) shall consist of twelve elements in x-direction, eight elements in y-direction and four elements in z-direction, all equidistant.</span><a class="headerlink" href="#fig-cylinder" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:50%">
        <figure id="fig-cylinder2">
        <a class="reference internal image-reference" href="../_images/Cylinder2.jpg"><img alt="../_images/Cylinder2.jpg" src="../_images/Cylinder2.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.9 </span><span class="caption-text">Sketch of the 1 zone curved structured mesh with a stretched element arrangement. For the x-direction the <code class="docutils literal notranslate"><span class="pre">stretchType</span></code> parameter was set to 3. The parameter <code class="docutils literal notranslate"><span class="pre">DXmaxToDXmin</span></code> was set to 6 and the parameter <code class="docutils literal notranslate"><span class="pre">fac</span></code> to 1.5. For the y-direction the <code class="docutils literal notranslate"><span class="pre">stretchType</span></code> parameter was set to 1 and the elements were stretched by the factor 2.2. The elements in z-direction remain equidistant.</span><a class="headerlink" href="#fig-cylinder2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

### Curved Structured Mesh: Outout Visualization
If there is a need for assistance of visualizing the HOPR output visit {ref}`tutorials/index_visualization:Visualization`.

<h4>Curved Structured Mesh without Stretched Elements<a class="headerlink" href="#curved-structured-mesh-without-stretched-elements" title="Permalink to this heading"></a></h4>

<table align="center" style="width:100%">
  <tr>
    <td style="width:50%">
        <figure id="fig-curvedtotal">
        <a class="reference internal image-reference" href="../_images/Curvedtotal.jpg"><img alt="../_images/Curvedtotal.jpg" src="../_images/Curvedtotal.jpg" style="height: 400px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.10 </span><span class="caption-text">Curved structured mesh</span><a class="headerlink" href="#fig-curvedtotal" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:50%">
        <figure id="fig-curvedinner">
        <a class="reference internal image-reference" href="../_images/Curvedinner.jpg"><img alt="../_images/Curvedinner.jpg" src="../_images/Curvedinner.jpg" style="height: 400px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.11 </span><span class="caption-text">Inner domain of the curved structured mesh</span><a class="headerlink" href="#fig-curvedinner" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

<h4>Curved Structured Mesh with Stretched Elements<a class="headerlink" href="#curved-structured-mesh-with-stretched-elements" title="Permalink to this heading"></a></h4>

<table align="center" style="width:100%">
  <tr>
    <td style="width:50%">
        <figure id="fig-curvedstretchedtotal">
        <a class="reference internal image-reference" href="../_images/Curvedstretchedtotal.jpg"><img alt="../_images/Curvedstretchedtotal.jpg" src="../_images/Curvedstretchedtotal.jpg" style="height: 400px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.12 </span><span class="caption-text">Curved structured mesh with stretched element arrangement.</span><a class="headerlink" href="#fig-curvedstretchedtotal" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:50%">
        <figure id="fig-curvedstretchedinner">
        <a class="reference internal image-reference" href="../_images/Curvedstretchedinner.jpg"><img alt="../_images/Curvedstretchedinner.jpg" src="../_images/Curvedstretchedinner.jpg" style="height: 400px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.13 </span><span class="caption-text">Inner domain of the curved structured mesh with stretched element arrangement</span><a class="headerlink" href="#fig-curvedstretchedinner" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

## Mesh Curving by Post-Deformation
This tutorial shows how to generate a curved multi-block mesh, composed of several structured boxes, which are first assembled and then globally mapped to a curved domain
The parameter file can be found in 

    tutorials/1-06-curved-postdeform/parameter.ini

### Mesh Curving by Post-Deformation: User-defined Variables
User-defined varibles can be used to parametrize the parameterfile.

User-defined variables are searched and replaced in **all other lines(!)** of the parameterfile (all strings between the = and ! sign are searched). They are either an Integer or Real value and defined in the parameterfile as 

    DEFVAR=(INT):    i0 = 002    ! no. elems in inner square  i0xi0
    DEFVAR=(REAL):   ri = 0.5    ! inner square dim

Note that each variable is searched and replaced one after the other, so that names should be absolutely unique. In the example, a variable called ri0 would not be allowed. 

### Mesh Curving by Post-Deformation: Post-Deformation from a box to a cylinder
The idea is to build first a simple box using the internal mesh procedures explained in Multiple Cartesian Boxes and then use a deformation function to obtain a cylinder. The post-deformation parameter is 

    MeshPostDeform=1

The undeformed and deformed mesh is shown in <a class="reference internal" href="#fig-nopost"><span class="std std-numref">Fig. 2.14</span></a> and <a class="reference internal" href="#fig-withpost"><span class="std std-numref">Fig. 2.15</span></a>.

The order of the curved element mapping can be chosen arbitrarily 

    useCurveds   =T 
    BoundaryOrder=5

<table align="center">
  <tr>
    <td>
        <figure id="fig-nopost">
        <a class="reference internal image-reference" href="../_images/Nopost.jpg"><img alt="../_images/Nopost.jpg" src="../_images/Nopost.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.14 </span><span class="caption-text"><code class="docutils literal notranslate"><span class="pre">MeshPostDeform=0</span></code>
        </span><a class="headerlink" href="#fig-nopost" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure id="fig-withpost">
        <a class="reference internal image-reference" href="../_images/Withpost.jpg"><img alt="../_images/Withpost.jpg" src="../_images/Withpost.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.15 </span><span class="caption-text"><code class="docutils literal notranslate"><span class="pre">MeshPostDeform=1</span></code></span><a class="headerlink" href="#fig-withpost" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

The mapping function maps the xy [-1;1]^2 coordinates to a circular domain of radius 1, but smoothed towards the center to avoid a singular mesh. The radius can be scaled with the parameter

    MeshPostDeform_R0=1.0

We choose a periodic boundary condition in z direction.

### Mesh Curving by Post-Deformation: Parameter Variations
In a variant of the parameterfile, parameter2.ini, the extend of the domain in xy is [-2;2]^2 and is mapped to a circular domain with a radius of 2. The part of the domain inside [-1;1]^2 is mapped like in the example above, but ouside of [-1;1]^2, the mapping is perfectly circular. A final radius of 1 is then achieved by setting the scaling factor to

    MeshPostDeform_R0=0.5

see <a class="reference internal" href="#fig-cylinder-param2"><span class="std std-numref">Fig. 2.16</span></a>.

In another variant of the parameterfile, parameter3.ini, a mesh with 9 zones in built, and refined at a specific radius, using the stretching functions explained in Stretching Functions, see <a class="reference internal" href="#fig-cylinder-param3"><span class="std std-numref">Fig. 2.17</span></a>. 

<table align="center">
  <tr>
    <td>
        <figure id="fig-cylinder-param2">
        <a class="reference internal image-reference" href="../_images/Cylinder_param2.jpg"><img alt="../_images/Cylinder_param2.jpg" src="../_images/Cylinder_param2.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.16 </span><span class="caption-text">Outer circular mapping with parameter2.ini </span><a class="headerlink" href="#fig-cylinder-param2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure id="fig-cylinder-param3">
        <a class="reference internal image-reference" href="../_images/Cylinder_param3.jpg"><img alt="../_images/Cylinder_param3.jpg" src="../_images/Cylinder_param3.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.17 </span><span class="caption-text">9 block mesh with stretching parameter3.ini</span><a class="headerlink" href="#fig-cylinder-param2" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

### Mesh Curving by Post-Deformation: Output Visualization
If there is a need for assistance of visualizing the HOPR output visit {ref}`tutorials/index_visualization:Visualization`. 

## Curved Torus
The same Post-Deformation applied to generate a torus
The parameter file can be found in 

    tutorials/1-07-curved-torus/parameter.ini

### Curved Torus: Post-Deformation from a box to a torus
Analogously to the previous tutorial Mesh Curving by Post-Deformation, we deform a box to a torus with a circular cross section. We only add the main radius of the torus as a parameter

    MeshPostDeform    = 1                            ! deforms [-1,1]^2 to a cylinder with radius Postdeform_R0
    PostDeform_R0     = s0                           ! here domain is [-2,2]^2 mapped to a cylinder with radius 0.5*2 = 1
    PostDeform_Rtorus = rz                           ! z must be inside [0,1] and periodic

Since the connectivity of the mesh is created before the deformation, the boundary condition in z direction must be periodic on the undeformed mesh. The torus then has the correct connectivity. 

<table align="center" style="width:100%">
  <tr>
    <td style="width:50%">
        <figure id="fig-nopost-torus">
        <a class="reference internal image-reference" href="../_images/Nopost_torus.jpg"><img alt="../_images/Nopost_torus.jpg" src="../_images/Nopost_torus.jpg" style="height: 350px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.18 </span><span class="caption-text"><code class="docutils literal notranslate"><span class="pre">MeshPostDeform=0</span></code></span><a class="headerlink" href="#fig-nopost-torus" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td style="width:50%">
        <figure id="fig-torusmesh-q0">
        <a class="reference internal image-reference" href="../_images/Torusmesh_q0.jpg"><img alt="../_images/Torusmesh_q0.jpg" src="../_images/Torusmesh_q0.jpg" style="height: 350px;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.19 </span><span class="caption-text"><code class="docutils literal notranslate"><span class="pre">MeshPostDeform=1</span></code></span><a class="headerlink" href="#fig-torusmesh-q0" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>


## Curved Sphere
The same Post-Deformation applied to generate a mesh of a sphere and a sphherical shell
The parameter file can be found in 

    tutorials/1-08-curved-sphere/parameter.ini

### Curved Sphere: Post-Deformation from a box to a sphere
Analogously to the tutorial Mesh Curving by Post-Deformation, we deform a box to a sphere.

    MeshPostDeform=2
    PostDeform_R0=0.5

The initial box consists of 1 central zone and 6 neighbor zones, and forms a cube of [-2;2]^3 , being mapped to a sphere of radius 2. Again, PostDeform_R0 can be used to scale the radius. The mapping of the domain inside [-1;1]^3 is again smoothed to avoid singular elements, and outside [-1,1]^3 is perfectly spherical, see <a class="reference internal" href="#fig-nopost-sphere"><span class="std std-numref">Fig. 2.20</span></a> and <a class="reference internal" href="#fig-withpost-sphere"><span class="std std-numref">Fig. 2.21</span></a> .

<table align="center">
  <tr>
    <td>
        <figure id="fig-nopost-sphere">
        <a class="reference internal image-reference" href="../_images/Nopost_sphere.jpg"><img alt="../_images/Nopost_sphere.jpg" src="../_images/Nopost_sphere.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.20 </span><span class="caption-text"><code class="docutils literal notranslate"><span class="pre">MeshPostDeform=0</span></code></span><a class="headerlink" href="#fig-nopost-sphere" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure id="fig-withpost-sphere">
        <a class="reference internal image-reference" href="../_images/Withpost_sphere.jpg"><img alt="../_images/Withpost_sphere.jpg" src="../_images/Withpost_sphere.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.21 </span><span class="caption-text"><code class="docutils literal notranslate"><span class="pre">MeshPostDeform=2</span></code></span><a class="headerlink" href="#fig-withpost-sphere" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>

### Curved Sphere: Spherical shell
In a variant of the parameterfile, parameter_shell.ini, only 6 domains without the central domain are used and a spherical shell is generated. The boundary conditions have to be changed, and the central hole has a size of [-1,1]^3, see <a class="reference internal" href="#fig-nopost-shell"><span class="std std-numref">Fig. 2.22</span></a>  and <a class="reference internal" href="#fig-withpost-shell"><span class="std std-numref">Fig. 2.23</span></a>, where also the inner boundary face is shown. 

<table align="center">
  <tr>
    <td>
        <figure id="fig-nopost-shell">
        <a class="reference internal image-reference" href="../_images/Nopost_shell.jpg"><img alt="../_images/Nopost_shell.jpg" src="../_images/Nopost_shell.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.22 </span><span class="caption-text">parameter_shell.ini, <code class="docutils literal notranslate"><span class="pre">MeshPostDeform=0</span></code></span><a class="headerlink" href="#fig-nopost-shell" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
    <td>
        <figure id="fig-withpost-shell">
        <a class="reference internal image-reference" href="../_images/Withpost_shell.jpg"><img alt="../_images/Withpost_shell.jpg" src="../_images/Withpost_shell.jpg" style="width: 80%;" /></a>
        <figcaption>
        <p><span class="caption-number">Fig. 2.23 </span><span class="caption-text">parameter_shell.ini, <code class="docutils literal notranslate"><span class="pre">MeshPostDeform=2</span></code></span><a class="headerlink" href="#fig-withpost-shell" title="Permalink to this image"></a></p>
        </figcaption>
        </figure>
    </td>
  </tr>
</table>