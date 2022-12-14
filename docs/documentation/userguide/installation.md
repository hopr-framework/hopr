# Installation

The following chapter describes the installation procedure on a Linux machine requiring root access.
This includes the installation of required prerequisites, setting up MPI and HDF5.
Please note that high-performance clusters usually have a module environment, where you have to load the appropriate modules
instead of compiling them yourself.

## Prerequisites
**HOPR** has been tested on various Linux distributions. This includes Ubuntu 14.04 LTS, 16.04 LTS and 18.04 LTS, 20.04 LTS
20.10 and 21.04.
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

## Required Libraries

For a list of tested library version combinations, see Chapter {ref}`userguide/appendix:Appendix`.

### Installing/setting up GCC

Additional packages are required starting at specific versions of the GCC compiler suite.

|    GCC Version   | Ubuntu 20.04 (and older) |
| :--------------: |       :----------:       |
|       9.3.0      |        libmpfr-dev       |
|                  |        libmpc-dev        |


(sec:hdf5-installation)=
### Installing/setting up HDF5

An available installation of HDF5 can be utilized with HOPR. This requires properly setup environment variables and the
compilation of HDF5 during the HOPR compilation has to be turned off (`LIBS_BUILD_HDF5 = OFF`). If this option is enabled,
HDF5 will be downloaded and compiled. However, this means that every time a clean compilation of HOPR is performed, HDF5 will
be recompiled. It is preferred to either install HDF5 on your system locally or utilize the packages provided on your cluster.

The recommended HDF5 version to use with HOPR is **1.10.0-patch1**. In the following a manual installation of HDF5 is described,
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

        export HDF5_DIR = /opt/hdf5/1.X.X/

* CMake

        export HDF5_DIR = /opt/hdf5/1.X.X/shared/cmake/XXX

If your CMake version is above 3.9.X, CMake uses a new findPackage routine, requiring that **HDF5_ROOT** is set

    export HDF5_ROOT=/opt/hdf5/1.10.0-patch1/mpi/

For convenience, you can add these lines to your `.bashrc`.

(sec:optaining-the-source)=
