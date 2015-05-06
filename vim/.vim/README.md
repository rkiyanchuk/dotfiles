Setup
=====

1. Run `compile.sh` script.
    The script will fetch Vim source code and compile it.
2. Run `install.sh` script **with root privileges**.
    The script will package Vim into Deb package using `checkinstall` and 
    install it.
3. Run `init.sh` script.
    The script will install `Vundle` plugin manager, install all the plugins
    and Python dependencies to user directory.

```
$ bash compile.sh
$ sudo bash install.sh
$ bash init.sh
```
