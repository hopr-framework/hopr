# HOPR

[![Slack](https://img.shields.io/badge/chat-slack-e01e5a)](https://join.slack.com/t/hopr-framework/shared_invite/zt-tvhyqwwj-7r_neuU~VWHt6AKyzf6oKg)
[![License: GPL-3.0](https://img.shields.io/badge/License-GPLv3-success.svg)](https://opensource.org/licenses/GPL-3.0)


           _____     _____    _______________    _______________   _______________
          /    /)   /    /)  /    _____     /)  /    _____     /) /    _____     /)
         /    //   /    //  /    /)___/    //  /    /)___/    // /    /)___/    //
        /    //___/    //  /    //   /    //  /    //___/    // /    //___/    //
       /    _____     //  /    //   /    //  /    __________// /    __      __//
      /    /)___/    //  /    //   /    //  /    /)_________) /    /)_|    |__)
     /    //   /    //  /    //___/    //  /    //           /    //  |    |_
    /____//   /____//  /______________//  /____//           /____//   |_____/)
    )____)    )____)   )______________)   )____)            )____)    )_____)
                                                    xX
                                  .xXXXXXXXx.       X
                                .XXXXXXXXXXXXXXx  .XXXXx
                              .XXXXXXXXXXXXXXXXXXXXXXXXXx
                            .XXXXXXXXXXXXXXXXXXXXXXX`
                           .XX``XXXXXXXXXXXXXXXXX`
                          XX`   `XXXXX`     .X`
                         XX      XXX`      .`
                       ,X`      XXX`
                     ,xX`     .XX`
                  ,xxX`      XX`
                            XXx

HOPR (High Order Preprocessor) is an open-source software for
the generation of three-dimensional unstructured high-order meshes.
These meshes are needed by high-order numerical methods like
Discontinuous Galerkin, Spectral Element Methods or pFEM,
in order to retain their accuracy if the computational domain
includes curved boundaries.

HOPR has been developed by the [**Numerics Research Group (NRG)**][nrg]
lead by Prof. Claus-Dieter Munz at the Institute of Aerodynamics
and Gasdynamics at the University of Stuttgart, Germany.

This is a scientific project. If you use HOPR for publications or
presentations in science, please support the project by citing
our publications given in [REFERENCE.md](REFERENCE.md).

## Installation / Documentation

See the full documentation including usage instructions and tutorial for HOPR is the [User Guide][hopr].

For installation instruction see the user guide section [Chapter 2][install].

In case you have question regarding HOPR, want to report bugs
or contribute to the project you can use the mailing list
<hopr-project@listserv.uni-stuttgart.de>.
You can also subscribe to the mailing list [here][list].

## Used libraries

HOPR uses several external libraries as well as auxiliary functions from open source projects, including:

* [cmake](https://www.cmake.org)
* [LAPACK](http://www.netlib.org/lapack/)
* [MPI](https://www.open-mpi.org/)
* [HDF5](https://www.hdfgroup.org/)

## License
HOPR is Copyright (C) 2017, Prof. Claus-Dieter Munz and is
released under the terms of the
GNU General Public License v3.0 (GPL v3.0). For the complete
license terms see the included license file [LICENSE](LICENSE).
In addition to GPL v3.0 licensing selected parts of HOPR are
available under other licenses,for more information refer to
[MULTILICENSING.md](MULTILICENSING.md)


[hopr]: https://hopr.readthedocs.io/en/latest/
[install]: https://hopr.readthedocs.io/en/latest/userguide/installation.html
[list]: https://listserv.uni-stuttgart.de/mailman/listinfo/hopr-project
[nrg]: https://www.iag.uni-stuttgart.de/en/working-groups/numerical-methods/
