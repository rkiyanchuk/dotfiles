#!/bin/sh

# Configure locales.
sed -i 's/#en_US.UTF/en_US.UTF-8 UTF-8/g' /etc/locale.gen
sed -i 's/#uk_UA.UTF/uk_UA.UTF-8 UTF-8/g' /etc/locale.gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf
locale-gen

# Sync time.
ln -s "/usr/share/zoneinfo/${TZ}" /etc/localtime
hwclock --systohc --utc

# Configure HTTPS mirrors.
pacman -S reflector
reflector --sort score -p https --country 'United States' --save /etc/pacman.d/mirrorlist

# Install bootloader.
pacman -S grub intel-ucode
grub-install --recheck /dev/sda
grub-mkconfig -o /boot/grub/grub.cfg

# Configure hostname
hostnamectl set-hostname "${HOSTNAME}"
sed -i "/localhost.localdomain/ s/$/ ${HOSTNAME}/" /etc/hosts

# Configure networking
pacman -S linux-firmware  # Install iwlwifi firmware for Intel WiFi chip.
#systemctl start dhcpcd@enp0s3.service
#systemctl enable dhcpcd@enp0s3.service

reboot

