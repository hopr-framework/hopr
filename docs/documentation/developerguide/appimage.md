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

The executable should be created in the top-level directory, e.g.,

    hopr-2de94ad-x86_64.AppImage

