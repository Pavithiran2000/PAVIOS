#!/bin/sh

# This script assembles the PAVIOS bootloader, kernel 
# with NASM, and then creates floppy and CD images (on Linux)

# Only the root user can mount the floppy disk image as a virtual
# drive (loopback mounting), in order to copy across the files

# (If you need to blank the floppy image: 'mkdosfs disk_imgs/pavios.flp')


if test "`whoami`" != "root" ; then
	echo "You must be logged in as root to build (for loopback mounting)"
	echo "Enter 'su' or 'sudo bash' to switch to root"
	exit
fi


if [ ! -e disk_imgs/pavios.flp ]
then
	echo ">>> Creating new PAVIOS floppy image..."
	mkdosfs -C disk_imgs/pavios.flp 1440 || exit
fi


echo ">>> Assembling bootloader..."

nasm -O0 -w+orphan-labels -f bin -o bootloader.bin bootloader.asm || exit


echo ">>> Assembling PAVIOS kernel..."


nasm -O0 -w+orphan-labels -f bin -o kernel.bin kernel.asm || exit


echo ">>> Adding bootloader to floppy image..."

dd status=noxfer conv=notrunc if=bootloader.bin of=disk_imgs/pavios.flp || exit


echo ">>> Copying PAVIOS kernel..."

rm -rf tmp-loop

mkdir tmp-loop && mount -o loop -t vfat disk_imgs/pavios.flp tmp-loop && cp kernel.bin tmp-loop/

sleep 0.2

echo ">>> Unmounting loopback floppy..."

umount tmp-loop || exit

rm -rf tmp-loop


echo ">>> Creating CD-ROM ISO image..."

rm -f disk_imgs/pavios.iso
mkisofs -quiet -V 'PAVIOS' -input-charset iso8859-1 -o disk_imgs/pavios.iso -b pavios.flp disk_imgs/ || exit

echo '>>> Done!'

