#!/usr/bin/sbcl --script

;;;; Basic Arch Linux installer (conceptual, educational use only!)
;;;; Run from Arch ISO live environment.

(defpackage :arch-install
  (:use :cl :sb-ext)
  (:export :main))
(in-package :arch-install)

(defvar *target-disk* "/dev/sda")
(defvar *efi-partition* "1")
(defvar *root-partition* "2")
(defvar *hostname* "arch-desktop")
(defvar *username* "siraadj")
(defvar *root-password* "root")
(defvar *user-password* "siraadj")
(defvar *timezone* "Europe/Moscow")
(defvar *locale* "en_US.UTF-8")

(defun run-command (cmd &key (output t) (error-output t))
  "Execute shell command. cmd is string"
  (let ((exit-code (sb-ext:process-exit-code
                    (sb-ext:run-program "/bin/sh"
                                        (list "-c" cmd)
                                        :output output
                                        :error error-output
                                        :wait t))))
    (unless (zerop exit-code)
      (error "Command failed: ~A (exit code ~A)" cmd exit-code))))

(defun partition-and-format ()
  (format t "Partitioning and formating ~A...~%" *target-disk*)
  ;; Wipe and create GPT
  (run-command (format nil "parted ~A --script mklabel gpt" *target-disk*))
  ;; Create EFI (512M) and root (rest)
  (run-command (format nil "parted ~A --script mkpart ESP fat32 1MiB 513MiB" *target-disk*)) ; 1MiB - part start, 513MiB part end
  (run-command (format nil "parted ~A --script set 1 esp on" *target-disk*))
  (run-command (format nil "parted ~A --script mkpart root ext4 513MiB 100%" *target-disk*)) ; 513MiB - part start, 100% - to disk end
  ;; Format the parts
  (run-command (format nil "mkfs.fat -F32 ~A~A" *target-disk* *efi-partition*))
  (run-command (format nil "mkfs.ext4 ~A~A" *target-disk* *root-partition*))
  ;; Mount
(run-command "mount /dev/sda2 /mnt")
(run-command "mkdir -p /mnt/boot/efi")
(run-command "mount /dev/sda1 /mnt/boot/efi"))

(defun install-base-system ()
  (format t "Istalling base system...~%")
  (run-command "pacstrap -G /mnt base linux linux-firmware sbcl emacs grub efibootmgr networkmanager base-devel git wget curl rsync dosfstools dmidecode"))

(defun install-gnome ()
  (format t "Istalling GNOME...~%")
  (run-command "pacstrap -G /mnt xorg-server xorg-xinit gnome firefox"))

(defun generate-fstab ()
  (format t "Generating fstab...~%")
  (run-command "genfstab -U /mnt >> /mnt/etc/fstab"))

(defun chroot-run (cmd)
  (run-command (format nil "arch-chroot /mnt /bin/sh -c ~s" cmd)))

(defun init-pacman-keyring ()
  (format t "Imitiation pacman keyring...~%")
  (chroot-run "pacman-key --init")
  (chroot-run "pacman-key --populate archlinux"))

(defun configure-system ()
  (format t "Configuring system...~%")
  ;; Timezone
  (chroot-run (format nil "ln -sf /usr/share/zoneinfo/~A /etc/localtime" *timezone*))
  (chroot-run "hwclock --systohc")
  ;; Locale
  (chroot-run (format nil "echo '~A UTF-8' >> /etc/locale.gen" *locale*))
  (chroot-run "locale-gen")
  (chroot-run (format nil "echo 'LANG=~A' > /etc/locale.conf" *locale*))
  ;; Hostname
  (chroot-run (format nil "echo '~A' > /etc/hostname" *hostname*))
  ;; Set root password
  (chroot-run (format nil "echo 'root:~A' | chpasswd" *root-password*))
  ;; Create user
  (chroot-run (format nil "useradd -m -G wheel -s /bin/bash ~A" *username*))
  (chroot-run (format nil "echo '~A:~A' | chpasswd" *username* *user-password*))
  ;; Enable sudo for wheel
  (chroot-run "sed -i 's/# %wheel ALL=(ALL:ALL) ALL/  %wheel ALL=(ALL:ALL) ALL/' /etc/sudoers")
  ;; Init pacman keyring
  (init-pacman-keyring)
  ;; Enable NetworkManager
  (chroot-run "systemctl enable NetworkManager")
  (chroot-run "systemctl enable gdm"))

(defun install-bootloader ()
  (format t "Installing GRUB bootloader...~%")
  (chroot-run "grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=GRUB")
  (chroot-run "grub-mkconfig -o /boot/grub/grub.cfg"))

(defun main ()
  (handler-case
      (progn
	(format t ">> Starting Arch Linux installer (SBCL script)...~%")
	(partition-and-format)
	(install-base-system)
	(install-gnome)
	(generate-fstab)
	(configure-system)
	(install-bootloader)
	(format t "OK Installation complete! Rebooting soon...~%")
	(run-command "umount -R /mnt")
	(format t "Now run: reboot~%"))
    (error (e)
      (format *error-output* "X Error: ~A~%" e))))
;;; Run if executed as script
(main)

