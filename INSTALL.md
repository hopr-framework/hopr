# HOPR INSTALLATION PROCEDURE
## Prerequisites

HOPR supports Linux-based systems only, requires a x86_64 compliant platform and has been tested on the following platforms

- Linux Mint 17 or newer
- Red Hat Enterprise Linux 7.6 or newer
- SUSE Linux Enterprise Server 11 SP3 or newer
- Ubuntu 12.04 or newer

### Compilers
HOPR requires a C and a Fortran 2003 compliant compiler, compilers tested with HOPR include

- GNU Compiler Collection 4.6 or newer
- Intel C/Fortran Compiler 12 or newer (recommended)
- CRAY Compiler Environment 8.1 or newer

HOPR furthermore requires CMake 3.5.2+ as a build system.

### Libraries
The following libraries are required, if not mentioned otherwise, including their development headers. Libraries marked with a star (*) can alternatively be provided by HOPR.

- BLAS/LAPACK* (or compatible, e.g. ATLAS, MKL)
- CGNS*
- HDF5*
- libc6
- zlib
- Python 2.7 or newer (optional)

If not present on your system, HOPR can automatically download and compile these libraries

- HDF5 (1.12.0 if OpenMPI 4.0.0+ is detected, 1.10.6 otherwise)
- LAPACK (0.3.17)/OpenBLAS (3.10.0)
- CGNS (3.4.1)

## Compiling HOPR
HOPR supports CMake as a build system, which should be available on most systems. Ensure that your environment variables `CC` and `FC` (as well as their corresponding MPI counterparts `MPICC`and `MPIFC` if compiling with MPI support) point to the correct compiler. 

For compiling HOPR, create a new sub-directory, e.g. "build" . Inside that directory execute
 
   ccmake ..

Here you can specify library paths and options. If no preinstallied libraries for HDF5 and CGNS are found these libraries will be downloaded and built automatically. Press `c` to configure and `g` to create the Makefiles. Finally compile HOPR by typing `make`.

### Libraries
In case you want to use a precompiled HDF5 or CGNS version on your system, set the option `LIBS_BUILD_HDF5=OFF`, then the path to HDF5 can be specified by the environment variable `$HDF5_DIR` if HDF5 has been built with CMake or `$HDF5_ROOT` if built using Automake. Note that `$HDF5_DIR` should contain the path to the CMake subdirectory of HDF5, i.e. `$HDF5_ROOT/share/cmake/hdf5`.

## Testing HOPR
After compiling, you can test HOPR by going to the `tutorials` directory and running the script `executeall.sh`, which will run HOPR for all tutorial cases. For further information check the [HOPR website](http://www.hopr-project.org/).
