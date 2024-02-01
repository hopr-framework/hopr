# Building the AppImage Executable

Navigate to the HOPR repository and create a build directory

    mkdir build && cd build

and compile HOPR using the following cmake flags

    cmake .. -DCMAKE_INSTALL_PREFIX=/usr

and then

    make install DESTDIR=AppDir

Then create an AppImage (and subsequent paths) directory in the build folder

    mkdir -p AppDir/usr/share/icons/

and copy the HOPR logo into the icons directory

    cp ../docs/Meshformat/pics/HOPR_logo.png AppDir/usr/share/icons/hopr.png

A desktop file should already exist in the top-level directory containing

    [Desktop Entry]
    Type=Application
    Name=hopr
    Exec=hopr
    Comment=Tool create .h5 hopr meshes to be read by piclas, flexi, fluxo, etc.
    Icon=hopr
    Categories=Development;
    Terminal=true

Next, download the AppImage executable and run

    ./linuxdeploy-x86_64.AppImage --appdir AppDir --output appimage --desktop-file=../hopr.desktop

If an error is encountered, see the Section {ref}`developerguide/appimage:Troubleshooting` section for a possible solution.
The executable should be created in the top-level directory, e.g.,

    hopr-2de94ad-x86_64.AppImage

# Troubleshooting

This section collects typical errors that are encountered when trying to build the AppImage.

## dlopen(): error loading libfuse.so.2

If the error

    dlopen(): error loading libfuse.so.2

    AppImages require FUSE to run.
    You might still be able to extract the contents of this AppImage
    if you run it with the --appimage-extract option.
    See https://github.com/AppImage/AppImageKit/wiki/FUSE
    for more information

is encountered, this might be due to a missing fuse installation.
On Debian/Ubuntu systems, simply run

    sudo apt install libfuse2

