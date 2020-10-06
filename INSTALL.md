# HOPR INSTALLATION PROCEDURE


## Prerequisites

HOPR supports Linux-based systems only requires a x86_64
compliant platform and has been tested on the following platforms:

- Ubuntu 12.04 or newer
- Linux Mint 17 or newer
- SUSE Linux Enterprise Server 11 SP3 and newer


### Compilers

HOPR requires a C and a Fortran 2003 compliant compiler,
compilers tested with HOPR include

- GNU Compiler Collection 4.6 or newer
- Intel C/Fortran Compiler 12 or newer (recommended)
- CRAY Compiler Environment 8.1 or newer

HOPR furthermore requires CMake 3.0+ as a build system.

### Libraries

The following libraries are required, if not mentioned
otherwise, including their development headers:

- libc6
- zlib
- BLAS/LAPACK (or compatible, e.g. ATLAS, MKL)
- Python 2.7 or newer (optional)

If not present on your system, HOPR will automatically
download and compile these libraries

- HDF5 (versions <1.10 need to be compiled with Fortran2003)
- CGNS (not needed if the option `HOPR_USE_CGNS=OFF` is set in cmake)

## Prerequisites for Mac OSX

For OSX, there are a few steps to install the necessary packages. The first is to get the GNU compiler suite:

1. Install *Xcode* from the App Store (this is a fairly large download). After it is downloaded and installed, launch it and agree to the Xcode license to finalize the basic installation. Note, Xcode contains many packages already, like git and LAPACK.
2. Install the *command lines tools*. To do so, open a terminal and enter:

        sudo xcode-select --install

3. Xcode comes with gcc/g++ functionality but not Fortran. So, next, install *gfortran*. There are many ways to do this. An easy way is provided by the maintainers of gfortran who offer [Apple-style installers for macOS](https://github.com/fxcoudert/gfortran-for-macOS/releases). To verify the installation of gfortran type:

        gfortran --version

     This should return the expected compiler version. 
 
The installation of other necessary packages is eased greatly with the *Macports* tool (alternatively, *homebrew* could also be used albeit with slightly different syntax). Macports provides macOS with a Synaptic Package Manager type environment which facilitates installation and updates of software libraries.

1. Install [Macports](https://www.macports.org/install.php).
2. After installation, it is recommended to run a *self-update* to ensure that the ports available are all current:


        sudo port -v selfupdate

      This should be done periodically to keep the Macports system up-to-date.

3. Use Macports to install the remaining packages listed in the table below, which are obtained through the port environment:

        sudo port install cmake

     The port environment will also install any necessary supporting packages that are required.

  | Package |
  |:-------:|
  | cmake   |
  | ctags   |
  | hdf5    |
  | mpich   |
  | openmpi |

Table: Remaining packages to be installed with Macports.

A list of ports that are installed as well as their version is provided with the command

    port installed

## Compiling HOPR

HOPR supports CMake as a build system, which should be
available on most systems. The previously available
custom Makefile suport has been removed.
For compiling HOPR, create a new sub-directory,
e.g. "build" . Inside that directory execute
 
   CC=<C-Compiler> FC=<Fortran-Compiler>  ccmake ../

Here you can specify library paths and options. If no
preinstallied libraries for HDF5 and CGNS are found these
libraries will be downloaded and built automatically. 
**Note** that CGNS is only necessary if you plan to input cgns meshes, else, 
you can also compile without it by setting `HOPR_USE_CGNS=OFF`.
Press <c> to configure and <g> to create the Makefiles.
Finally compile HOPR by typing `make`.

### Libraries

In case you want to use a precompiled HDF5 or CGNS version
on your system, set the option `HOPR_BUILD_HDF5=OFF`, then 
the path to HDF5 can be specified by the environment variable
`$HDF5_DIR` if HDF5 has been built with CMake or `$HDF5_ROOT`
if built using Automake. Note that `$HDF5_DIR` should contain
the path to the CMake subdirectory of HDF5,
i.e. $HDF5_ROOT/share/cmake/hdf5 .

## Testing HOPR

After compiling, you can test HOPR by going to the
`tutorials` directory and running the script `executeall.sh`,
which will run HOPR for all tutorial cases (some examples need CGNS). For further
information check the [HOPR website](http://www.hopr-project.org/).

