#!/bin/sh

# Start Arch Linux Live instance appending parameter `cow_spacesize=1G` to the
# kernel options.

# pacman -Syu git

set -euo pipefail


TEST=true  # If set, scripts are expected to run in VirtualBox.
FS_TYPE='ext4'
HOSTNAME='zion'
TZ='America/Los_Angeles'  # Timezone obtained by running `tzselect`.


is_ntp_synced () {
    timedatectl status | grep 'synchronized:' | cut -d' ' -f 3
}

sync_ntp_time () {
    timedatectl set-ntp true

    while [ "$(is_ntp_synced)" -ne 'yes' ]; do
        sleep 1
    done
}

partition () {
    # I have 2 drives which are reserved entirely for / and /home partitions.
    ROOT_DEVICE_ID='ata-INTEL_SSDSC2BW120A4_CVDA336001ZK1207GN'
    HOME_DEVICE_ID='ata-Samsung_SSD_850_EVO_250GB_S21NNSAG845105P'

    if [ $TEST ]; then
        # Replace UUIDs with test drives from VirtualBox.
        ROOT_DEVICE_ID='ata-VBOX_HARDDISK_VB62f2f07e-e28239ea'
        HOME_DEVICE_ID='ata-VBOX_HARDDISK_VBd3e3eef6-9c639a48'
    fi

    ROOT_DEVICE=$(readlink -f ${ROOT_DEVICE_ID})
    HOME_DEVICE=$(readlink -f ${HOME_DEVICE_ID})

    # Partition device for / (root).
    parted "/dev/${ROOT_DEVICE}" mklabel msdos
    parted "/dev/${ROOT_DEVICE}" mkpart primary ${FS_TYPE} 1MiB 100%
    parted "/dev/${ROOT_DEVICE}" set 1 boot on
    # Partition device for /home.
    parted "/dev/${HOME_DEVICE}" mklabel msdos
    parted "/dev/${HOME_DEVICE}" mkpart primary ${FS_TYPE} 1MiB 100%

    mkfs.ext4 /dev/${ROOT_DEVICE}1
    mkfs.ext4 /dev/${HOME_DEVICE}1
}


sync_ntp_time
partition

mount -U ${ROOT_DEVICE} /mnt
mkdir /mnt/home
mount -U ${HOME_DEVICE} /mnt/home

genfstab -U /mnt >> /mnt/etc/fstab
pacstrap /mnt base base-devel


# CHROOT
# ======

cp chroot.sh /mnt/chroot.sh
arch-chroot /mnt ./chroot.sh
