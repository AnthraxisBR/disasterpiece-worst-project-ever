# THE FINAL DISASTERPIECE OF PROGRAMMING


This is a ERP project, this project is a disasterpiece of programming, it's a mess, it's a disaster, it's a nightmare, it's a disasterpiece.

# Webserver

This project run a webserver written in COBOL.

To build the webserver, you need to have the following dependencies installed:
    
```bash
wget https://sourceforge.net/projects/gnucobol/files/gnucobol/3.2/gnucobol-3.2.tar.gz/download -O cobol-compiler.tar.gz
tar -xvf cobol-compiler.tar.gz
cd gnucobol-3.2
make
sudo make install
sudo ldconfig
```

Start the webserver with the following command:
```bash
cd webserver
cobc -x -free -o webserver webserver.cbl
./webserver
```

# Cage Container

You can use the script `cagenerator.sh` to run this project.
Read the file and look for `#UPDATE HERE` to change to do the necessary changes.

This will to their best to create a container, it is what it is, a disasterpiece, good luck.

```bash
cd cage-container
mv ../webserver/webserver .
./cagenerator.sh
```

After built, tou can generate a image with the following command:
```bash
export ROOTFS=/var/cage-container
mksquashfs /var/cage-container $ROOTFS -comp xz -e proc
```

The cage container provide a container manager tool written in lua, you can use the following command to start the container:
```bash
