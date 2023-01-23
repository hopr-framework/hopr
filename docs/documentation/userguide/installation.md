# Installation

The following chapter describes the installation procedure on a Linux machine requiring root access.
This includes the installation of required prerequisites, e.g., setting up HDF5.
Please note that high-performance clusters usually have a module environment, where you have to load the appropriate modules
instead of compiling them yourself.

## Prerequisites
**HOPR** supports Linux-based systems only, requires a x86_64 compliant platform and has been tested on the following platforms

- Linux Mint 17 or newer
- Red Hat Enterprise Linux 7.6 or newer
- SUSE Linux Enterprise Server 11 SP3 or newer
- Ubuntu 14.04 LTS, 16.04 LTS and 18.04 LTS, 20.04 LTS 20.10, 21.04 and 22.04 LTS

For **tested combinations** of prerequisites (HDF5, OpenMPI, CMake etc.) and known problems that may occur, visit
Chapter {ref}`userguide/appendix:Appendix`.

The suggested packages in this section can be replaced by self compiled versions. The required packages for the Ubuntu Linux
distributions are listed in {numref}`tab:installation_prereqs_ubuntu`.
Under Ubuntu, they can be obtained using the apt environment:

    sudo apt-get install git

```{table} Debian/Ubuntu packages. x: required, o: optional, -: not available
---
name: tab:installation_prereqs_ubuntu
---
|      Package     | Ubuntu 14.04 | Ubuntu 16.04 | Ubuntu 18.04 | Ubuntu 20.04 |
| :--------------: | :----------: | :----------: | :----------: | :----------: |
|        git       |       x      |       x      |       x      |       x      |
|       cmake      |       x      |       x      |       x      |       x      |
| cmake-curses-gui |       o      |       o      |       o      |       o      |
|    liblapack3    |       x      |       x      |       x      |       x      |
|   liblapack-dev  |       x      |       x      |       x      |       x      |
|     gfortran     |       x      |       x      |       x      |       x      |
|        g++       |       x      |       x      |       x      |       x      |
|  mpi-default-dev |       x      |       x      |       x      |       x      |
|    zlib1g-dev    |       -      |       x      |       x      |       x      |
|  exuberant-ctags |       o      |       o      |       o      |       o      |
```
On some systems it may be necessary to increase the size of the stack (part of the memory used to store information about active
subroutines) in order to execute **HOPR** correctly. This is done using the command

    ulimit -s unlimited

from the command line. For convenience, you can add this line to your `.bashrc`.

### Compilers
**HOPR** requires a C and a Fortran 2003 compliant compiler, compilers tested with **HOPR** include

- GNU Compiler Collection 4.6 or newer
- Intel C/Fortran Compiler 12 or newer (recommended)
- CRAY Compiler Environment 8.1 or newer

**HOPR** furthermore requires CMake 3.5.2+ as a build system.

## Required Libraries
The following libraries are required, if not mentioned otherwise, including their development headers. Libraries marked with a star
(*) can alternatively be provided by HOPR.

- BLAS/LAPACK* (or compatible, e.g. ATLAS, MKL)
- CGNS*
- HDF5*
- libc6
- zlib
- Python 2.7 or newer (optional)

If not present on your system, **HOPR** can automatically download and compile these libraries

- HDF5 (1.12.0 if OpenMPI 4.0.0+ is detected, 1.10.6 otherwise)
- LAPACK (0.3.17)/OpenBLAS (3.10.0)
- CGNS (3.4.1)

For a list of tested library version combinations, see Chapter {ref}`userguide/appendix:Appendix`.

### Installing/setting up GCC

Additional packages are required starting at specific versions of the GCC compiler suite.

|    GCC Version   | Ubuntu 20.04 (and older) |
| :--------------: |       :----------:       |
|       9.3.0      |        libmpfr-dev       |
|                  |        libmpc-dev        |

### Installing/setting up HOPR
HOPR supports CMake as a build system, which should be available on most systems. Ensure that your environment variables `CC` and
`FC` (as well as their corresponding MPI counterparts `MPICC`and `MPIFC` if compiling with MPI support) point to the correct compiler.

For compiling HOPR, create a new sub-directory, e.g. "build" . Inside that directory execute

   ccmake ..

Here you can specify library paths and options. If no preinstallied libraries for HDF5 and CGNS are found these libraries will be
downloaded and built automatically. Press `c` to configure and `g` to create the Makefiles. Finally compile HOPR by typing `make`.

If the user does not need the cgns library (i.e. HOPR mesh is not built via a cgns input meshfile), the cmake option `LIBS_USE_CGNS` can be set to `OFF`, which skips the installation of the cgns library. Note that the cmake tests that depend on CGNS will not be executed. 

(sec:hdf5-installation)=
### Installing/setting up HDF5

An available installation of HDF5 can be utilized with **HOPR**. This requires properly setup environment variables and the
compilation of HDF5 during the **HOPR** compilation has to be turned off (`LIBS_BUILD_HDF5 = OFF`). If this option is enabled,
HDF5 will be downloaded and compiled. However, this means that every time a clean compilation of **HOPR** is performed, HDF5 will
be recompiled. It is preferred to either install HDF5 on your system locally or utilize the packages provided on your cluster.

The recommended HDF5 version to use with **HOPR** is **hdf5-1_12_0**. In the following a manual installation of HDF5 is described,
if HDF5 is already available on your system you can skip to the next section {ref}`sec:setting-env-vars`.

#### Manual HDF5 installation

First, download HDF5 from [HDFGroup (external website)](https://portal.hdfgroup.org/display/support/Downloads) and extract it

    tar xvf hdf5-version.tar.gz

Then create a build folder

    cd hdf-version && mkdir -p build

and configure HDF5 to install into "/opt/hdf5/1.X.X" (your choice, should be writable)

    cmake -DBUILD_TESTING=OFF -DHDF5_BUILD_FORTRAN=ON -DHDF5_BUILD_CPP_LIB=OFF -DHDF5_BUILD_EXAMPLES=OFF -DHDF5_ENABLE_PARALLEL=ON -DHDF5_BUILD_HL_LIB=ON -DHDF5_BUILD_TOOLS=ON -DHDF5_ENABLE_F2003=ON -DBUILD_SHARED_LIBS=OFF -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/opt/hdf5/1.X.X ..

Make and install (if you chosen a folder required root access)

    make && make install

(sec:setting-env-vars)=
#### Setting environment variables

Depending whether HDF5 was installed using *configure* or *CMake*, different settings for the HDF5_DIR variable are required

* Configure

        export HDF5_DIR = /opt/hdf5/1.X.X

* CMake

        export HDF5_DIR = /opt/hdf5/1.X.X/shared/cmake/XXX

If your CMake version is above 3.9.X, CMake uses a new `findPackage` routine, requiring that `HDF5_ROOT` is set

    export HDF5_ROOT=/opt/hdf5/1.X.X

For convenience, you can add these lines to your `.bashrc`.

**IMPORTANT:** Note that `HDF5_ROOT` must be cleared or set to the correct path when using `LIBS_BUILD_HDF5 = ON` to prevent cmake from compiling
hopr and cgns with different HDF5 versions. Otherwise, an error might occur, see {ref}`sec:hdf5-root-problem`.

## Troubleshooting
Sometimes errors occur during installation, for which standard fixes may apply.

(sec:hdf5-root-problem)=
### Wrongly set HDF5_ROOT variable

**Requirements:** The cmake options `LIBS_BUILD_HDF5 = ON` and `LIBS_BUILD_CGNS = ON` have been set.

The output error might look like this during compilation

    [ 6%] Building C object src/CMakeFiles/cgns_static.dir/cgns_internals.c.o
    /hdf5/hdf5-1.12.2/include/H5public.h:68:10: fatal error: mpi.h: No such file or directory
    68 | #include <mpi.h>
    | ^~~~~~~
    compilation terminated.

or the build test might fail with the following message

    WRITING THE DEBUGMESH...
       #Elements          113
       WRITE DATA TO CGNS FILE... SPHERE_CURVED_Debugmesh.cgnsWarning! ***HDF5 library version mismatched error***
    ...
    ...
    ...
    #16  0x149524759d8f in __libc_start_call_main
         at ../sysdeps/nptl/libc_start_call_main.h:58
    #17  0x149524759e3f in __libc_start_main_impl
         at ../csu/libc-start.c:392
    #18  0x4047d4 in ???
    #19  0xffffffffffffffff in ???


The cause of the problem is that `export HDF5_ROOT=/opt/hdf5/vX.X.X/...` sets the `HDF5_ROOT` environment variable, which leads
to HDF5 being built with possibly a different version (or compiler settings) for HOPR and CGNS.
The variable is also exposed in cmake

    HDF5_ROOT                        /opt/hdf5/1.X.X

Note that it does not matter if the correct path is exported via `export HDF5_DIR=...` if the variable **HDF5_ROOT** is also set as
CGNS automatically searches for the latter.

**To fix this problem**, set `export HDF5_ROOT=` in the installation terminal.

### Pre-compiled HDF5 via Spack and/or cmake
**Requirements:** The cmake options `LIBS_BUILD_HDF5 = OFF` and `LIBS_BUILD_CGNS = ON` have been set. Furthermore, it is not clear
whether the pre-installed HDF5 library is installed using Spack **AND/OR** built via cmake.

The output error might look like this during compilation

    CMake Error at CMakeLists.txt:210 (add_executable):
    add_executable cannot create imported target "h5dump" because another
    target with the same name already exists.

**To fix this problem**, pre-compile HDF5 using **configure** instead of **cmake** **AND/OR** do not use Spack. Otherwise compile
HDF5 using `LIBS_BUILD_HDF5 = ON` and do not forget to clear the `HDF5_ROOT` variable, see {ref}`sec:hdf5-root-problem`.

## Testing HOPR
After compiling, tests are automatically run for each parameterfile provided in sub-directories of the `tutorials` directory. The runs are pre-built by cmake in the `build/buildTests` directory and executed there.
