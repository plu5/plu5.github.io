---
layout: post
title: 21 — QEMU VMs
date: 2026-06-09 02:31
modified_date: 2026-06-09 11:45
categories: qemu vm
lang: en
redirect_from: /devlog/21
---

Set up VMs again on a new machine for testing retype builds, so decided to write down the steps, though I have only a naïve set up.

Windows is trivial and is like setting up any VM. macOS is not.

53G total for these two VMs (21+32), which is less than the game Dota 2 (68G ~/.steam/root/steamapps/common/dota 2 beta).

Using QEMU on Arch Linux.

## win10
(1) Download an iso. There are some on archive.org. [archive.org win10 IoT Entreprise LTSC 21H2](https://archive.org/details/en-us_windows_10_iot_enterprise_ltsc_2021_x64_dvd_257ad90f_202301)

(2) Create an image to serve as the disk for the VM
```sh
qemu-img create -f qcow2 win10.img 64G
```
It's not going to take 64G of your disk space straight away, it takes just 196K and will balloon if needed, up to 64G. It's 21G after installation.

(3) Create a shell script with for the QEMU command to launch the VM.
```bash
#!/usr/bin/env bash

# Adjust these paths
VM_IMG=~/vm/win10.img
VM_ISO=~/Downloads/en-us_windows_10_iot_enterprise_ltsc_2021_x64_dvd_257ad90f.iso

args=(
    -enable-kvm
    -m 8G
    -smp 4
    -cpu host
    -drive file="$VM_IMG",format=qcow2
    -cdrom "$VM_CD"
    -boot d
)

qemu-system-x86_64 "${args[@]}"
```
(it's bash rather than sh for the convenience of having the args in an array rather than a long command with backslash at the end of each line)

You can lower the memory (`-m`), but like the disk it won't actually take up 8G, this is only the max it can take. If the operating system sees it has less memory though it may optimise for lower memory use, so will take less (at a cost of running slower).

`boot -d` is telling it to boot the CD. It will boot into the installer, though it may take some time before it shows up; be patient.

(4) Run it.

During installation, don't sign in to your Microsoft account, select "domain join instead" to create just a local user.

After installation, remove or comment out the last two lines of `args` (the cdrom and `-boot d`).

[Note to self: 06-05 06-06]

## SMB sharing
For sharing files, I like using a [Samba server](https://wiki.archlinux.org/title/Samba) for guests that support it, because you can set it up and modify it on the fly without ever needing to restart the guest.

(1) Create a `/etc/samba/smb.conf` if you don't have one already.
```conf
[global]
ntlm auth = true
logging = systemd
map to guest = Bad User

[w]
# Adjust path and user
path = /home/pm/Downloads/builds/retype-1.7.0-prerelease3-windows/
browseable = yes
read only = no
guest ok = yes
force user = pm
```

You can run `termparm` to validate your configuration.

(2) Apply it with `sudo systemctl restart smb`

(3) Accessing SMB shares as guest is disabled by default on Windows 10. You can enable it by running this command in an elevated PowerShell:
```powershell
Set-SmbClientConfiguration -EnableInsecureGuestLogons $true -Force
```
(from [Microsoft Learn](https://support.microsoft.com/en-us/help/4046019))

(4) Now you can `cd \\10.0.2.4\w` to access the share from the command line, or in Explorer press Window key + L and enter `\\10.0.2.4\w`.

On Linux, `chmod +x` executables also affects the guest.

I also needed to install VC++ redistributable for VS 2017 x64 on the guest to be able to run retype (thanks to [gmatian](https://github.com/lucasg/Dependencies/issues/19) for the hint).
[https://aka.ms/vc14/vc_redist.x64.exe](https://aka.ms/vc14/vc_redist.x64.exe)

## QEMU "built-in" SMB
Alternatively, it's also possible to set up the share with QEMU parameters. From [Arch Wiki](https://wiki.archlinux.org/title/QEMU#QEMU's_built-in_SMB_server):

> QEMU's documentation says it has a "built-in" SMB server, but actually it just starts up Samba on the host with an automatically generated smb.conf file located in /tmp/qemu-smb.random_string and makes it accessible to the guest at a different IP address (10.0.2.4 by default).

(1) Add to args:
```sh
-nic user,id=nic0,smb=/home/pm/Downloads/builds/retype-1.7.0-prerelease3-windows/
```
(2) When we launch the VM, it creates a folder like /tmp/qemu-smb.GP3NQ3 with a similar set up to what we did ourselves.
```sh
[pm@pos qemu-smb.GP3NQ3]$ cat smb.conf
[global]
private dir=/tmp/qemu-smb.GP3NQ3
interfaces=127.0.0.1
bind interfaces only=yes
pid directory=/tmp/qemu-smb.GP3NQ3
lock directory=/tmp/qemu-smb.GP3NQ3
state directory=/tmp/qemu-smb.GP3NQ3
cache directory=/tmp/qemu-smb.GP3NQ3
ncalrpc dir=/tmp/qemu-smb.GP3NQ3/ncalrpc
log file=/tmp/qemu-smb.GP3NQ3/log.smbd
smb passwd file=/tmp/qemu-smb.GP3NQ3/smbpasswd
security = user
map to guest = Bad User
load printers = no
printing = bsd
disable spoolss = yes
usershare max shares = 0
[qemu]
path=/home/pm/Downloads/builds/retype-1.7.0-prerelease3-windows/
read only=no
guest ok=yes
force user=pm
```
(3) It will be on `\\10.0.2.4\qemu` on the guest.

It's not as good because you have to restart the guest. Unless it's possible to do in the QEMU console? In any case, this can be useful if you can't get your own SMB configuration to work.
(In my case it was not before running the PowerShell command because of guest access being disabled by default, and this was useful as a sanity check that it wasn't something wrong with my smb.conf)

## macos
(1) Get an iso. Like Windows, there are a few on archive.org.

There's also [LongQT-sea/macos-iso-builder](https://github.com/LongQT-sea/macos-iso-builder) on GitHub for building an iso using GitHub Actions. The releases has a list of workflow runs you can download an iso from (select one, scroll down, and click on the artifact). GitHub servers are fast to download from.

(2) Create an image to serve as the disk for the VM
```sh
qemu-img create -f qcow2 macos.img 64G
```
After creating this file it will be just 196K. After installing macOS Catalina, it's 32G.

(3) Download OpenCore. It needs to be configured to load the installer, so not all images of it will work correctly. I used [LongQT-sea/OpenCore-ISO](https://github.com/LongQT-sea/OpenCore-ISO/releases/latest) (and before that tried others I found in other people's configs that I couldn't get to load the installer, even after modifying `EFI/OC/config.plist`, but I don't really know what I'm doing).

(4) Download `OVMF_CODE.fd` and `OVMF_VARS.fd`. I'm not sure if this is necessary. I downloaded them from [notAperson535/OneClick-macOS-Simple-KVM/firmware](https://github.com/notAperson535/OneClick-macOS-Simple-KVM/tree/master/firmware).

(5) Create a shell script with for the QEMU command to launch the VM.
```bash
#!/usr/bin/env bash

# Adjust these paths
VM_IMG=~/vm/macos.img
VM_ISO=~/Downloads/macOS_Catalina_10.15.7.iso
OC_ISO=~/Downloads/LongQT-OpenCore-v0.7.iso
OVMF_CODE=~/Downloads/OVMF_CODE.fd
OVMF_VARS=~/Downloads/OVMF_VARS.fd

args=(
    -enable-kvm
    -m 8G
    -machine q35,accel=kvm
    -cpu Haswell-noTSX,model=158
    -device isa-applesmc,osk="ourhardworkbythesewordsguardedpleasedontsteal(c)AppleComputerInc"
    -smbios type=2
    -device intel-hda -device hda-output
    -drive if=pflash,format=raw,readonly=on,file="$OVMF_CODE"
    -drive if=pflash,format=raw,file="$OVMF_VARS"
    -vga qxl
    -usb -device usb-ehci,id=ehci -device usb-kbd,bus=ehci.0 -device usb-tablet,bus=ehci.0
    -netdev user,id=net0
    -device vmxnet3,netdev=net0,id=net0,mac=52:54:00:c9:18:27
    -monitor telnet:127.0.0.1:5801,server,nowait
    -device ich9-ahci,id=sata
    -drive if=none,id=OpenCore,media=cdrom,file="$OC_ISO"
    -device ide-cd,bus=sata.0,drive=OpenCore,bootindex=1
    -drive if=none,id=Installer,media=cdrom,file="$VM_ISO"
    -device ide-cd,bus=sata.1,drive=Installer
    -drive if=none,id=SystemDisk,file="$VM_IMG"
    -device ide-hd,bus=sata.2,drive=SystemDisk
)

qemu-system-x86_64 "${args[@]}"
```
I am not sure all of this is necessary, it was hard to for to get it working and this was the setup that worked.

(6) Run it. You should momentarily see the OpenCore GUI with an "Install macOS" option that gets selected automatically. Afterwards it takes a long time, don't assume it's stuck until you've waited at least half an hour.
(It doesn't take that long after installing the OS, it boots pretty fast.)

If it does not boot into the right thing, see the next section.

(7) If it booted properly, you end up on a GUI menu with options, one of which being to install macOS. Don't pick that yet. Pick Disk Utility and format the 64G virtual disk.

(8) Close that, and on the menu from the previous step pick install. This takes ages and needs to restart partway through (boot back again and OpenCore will have another entry other than the installer for the disk which will be selected by default so you don't need to do anything).

Do not log in to iCloud, your account could get blacklisted.

(9) There are steps in [LongQT-sea/OpenCore-ISO](https://github.com/LongQT-sea/OpenCore-ISO) for things to do postinstall, like moving OpenCore to the disk image. I didn't bother and just continue loading it with OpenCore as a CD.

## Boot issues troubleshooting
If booting immediately takes you to a black screen with nothing, restart and press f2 to open UEFI, in Boot Manager select the first DVD-ROM.

If it takes you to UEFI instead of booting OpenCore, same as above.

If it doesn't work, you can boot OpenCore manually in EFI shell: type `map -r`, explore the disks for example `fs0:` or `fs1:` to go to a particular disk and `ls` to see what's in it. Try running `EFI\OC\OpenCore.efi` to boot OpenCore (it must be backslashes, not forwardslashes).

- To get from EFI shell to the UEFI settings, enter `exit`.
- To get from UEFI settings to the EFI shell, select Boot Manager → EFI Internal Shell.

Another pitfall is if you have a screen resolution lower than 1920x1080. It seems to be set to that by default, so part of the screen will clip. You can press Ctrl+Alt+- to zoom out. You can also lower the resolution in UEFI settings: Device Manager → OVMF Platform Configuration → Change Preferred.

[Note to self: 06-07]

## Accessing the SMB share on macos
(1) Open Finder

(2) Menu at the very top of the screen → `Go` → `Connect to Server...`

(3) Enter `smb://10.0.2.2/w` (`w` is the share name)

## Other things you may wish to explore
- Cutting/pasting between guest and host and other guest integration stuff
- GPU passthrough (see [VFIO](https://www.reddit.com/r/VFIO), apparently people even play demanding videogames on VMs nowadays)

{% include fin.html %}
