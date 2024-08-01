
SetFactory("OpenCASCADE");

// Define parameters
a = 5;        // Edge length

// Create two boxes
Box(1) = {0, 0, 0, a, a, a};
Box(2) = {0, 0, a, a, a, a};

Coherence;

Physical Surface("BC_Inlet") = {5};
Physical Surface("BC_Outlet") = {11};
Physical Surface("BC_Wall") = {1,2,3,4,7,8,9,10};

//+
MeshSize {8, 5, 1, 2, 3, 9, 10, 4, 7, 11, 12, 6} = 2.5;

//
Mesh.Algorithm = 1;
//
Mesh.Algorithm3D = 10;
//
Mesh.SubdivisionAlgorithm = 2;
//
Mesh.OptimizeNetgen = 1;

Mesh 3;
// Save all elements even if they are not part of a physical group, required to output volume elements
Mesh.SaveAll = 1;
// Save as ASCII format, Version 4
Mesh.Binary = 0;
Mesh.MshFileVersion = 4.1;

Save "box.msh";