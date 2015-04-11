Setup
=====

1. Run `compile.sh` script.
    The script will fetch Vim source code and compile it.
2. Run `build.sh` script **with root privileges**.
    The script will create and install Vim Deb package using `checkinstall`.
3. Run `init.sh` script.
    The script will install `Vundle` plugin manager, install all the plugins
    and Python dependencies to user directory.
