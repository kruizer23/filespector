# FileCruncher
FileCruncher is a GUI tool for quickly finding all text occurences in multiple files in a directory, matching a specific file extension pattern.

## Dependencies

The packages **find**, **sort** and **grep** are needed to run FileCruncher.

## Building

The [Lazarus IDE](https://www.lazarus-ide.org/) was used to develop FileCruncher. To install Lazarus, you can download DEB and RPM packages from [SourceForge](https://sourceforge.net/projects/lazarus/files/). However, it is recommended to install the version of Lazarus that is present in the package repositories of your GNU/Linux distribution. Here are the Lazarus installation instructions for most popular GNU/Linux distributions:

**Debian**: `sudo apt install make gdb fpc fpc-source lazarus`

**Ubuntu**: `sudo apt install make gdb fpc fpc-source lazarus lcl`

**Fedora**: `sudo dnf install make gdb fpc fpc-src lazarus`

**openSUSE**: `sudo zypper install make gdb fpc fpc-src lazarus`

Once Lazarus in installed, you can build FileCruncher from the terminal using the following command:

**`make clean all`**

## Installation

After successfully building FileCruncher, you can install it from the terminal using the following command:

**`sudo make install`**

To remove FileCruncher, you can run:

**`sudo make uninstall`**

## Development

For development work on FileCruncher, it is recommended to work in the Lazarus IDE directly. Start Lazarus by selecting it from the application menu of your desktop environment. Next, select *Project → Open Project* from the program menu. Browse to the `./src` directory and select the `filecruncher.lpi` file. Build, run and debug FileCruncher by simply clicking the green play-button in the toolbar or by selecting *Run → Run* from the program menu.







