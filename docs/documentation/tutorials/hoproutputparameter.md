# HOPR Output Parameter

```{table} HOPR Output Parameter.
---
name: tab:HOPR Output Parameter
---
  | Parameters      | Example               | Data Type | Array Dim. | Default Value | Description |
  | :------         | :----------:          | :------:  | :------:   | :------:      | :------     |
  | `Debugvisu`     | `Debugvisu=T`         |  Logical  |   1        |    F          | T (True): Files will be generated, which enable you to visualize the mesh and the boundary mesh for debugging. These files can be found in the directory of the executed `parameter.ini` file.<br>F (False): Files for visualization will not generated during executing of the `parameter.ini` file.             |
  | `DebugvisuLevel`| `DebugvisuLevel=1`    |  Int      |   1        |    0          | 0: Visualization of linear mesh and BC (default).<br>1: Visualization of linear mesh and BC and an additional curved surface visualization (`_SplineSurf.*`) if `useCurveds=T`.<br>2: Visualization of linear mesh and BC and an additional curved volume visualization (`_SplineVol.*`) if `useCurveds=T`.             |
  | `NVisu`         | `NVisu=5`             |  Int      |   1        |    0          | Number of visualization points per element edge if `useCurveds=T`.             |
  | `outputFormat`  | `outputFormat=1`      |  Int      |   1        |    0          | 0: Paraview vtk (ASCII)<br>1: Tecplot (ASCII)<br>2: CGNS (binary)             |
  | `ProjectName`   | `ProjectName=cartbox` |  Str      |   1        | OBLIGATORY    | Part of the output files' name which will be generated during the execution. These Files can be found in the directory of the executed `parameter.ini` file.             |
```

A description of all parameters of the parameter file can be found in
{ref}`userguide/parameters:List of Parameters`
