
VProbe Toolkit
==============

This toolkit provides an interactive, programmer-friendly interface to
VMware's VProbes facility.  VProbes is a safe, dynamic technology for
instrumenting software running in virtual machines, and the
virtualization software stack itself.

You can find more detailed documentation for VProbes in the manuals in
the doc/ directory.

Overview
--------

This toolkit consists the following:

1. Source code for the Emmett compiler (emmett).
2. VProbes documentation (doc).
3. Emmett examples (cookbook).
4. Some sample scripts that monitor interesting events in linux guests
(bin).


Versions
--------

VProbes initially shipped with Workstation 6.5.  An updated version of
the VProbe engine is also available with Workstation 7.x and Fusion 3.

There are distinct tags for each version of Workstation supported.  So
if you are running Workstation 6.5 then please sync to the ws6.5 tag.
Otherwise sync to the ws7.0 tag (works with 7.0, 7.1, and Fusion 3).

If at all possible we recommend updating to the latest version of
Workstation.  The ws6.5 version uses an older version of the Emmett
compiler which is no longer supported. 


Requirements
------------

Currently the VProbe toolkit only runs on linux hosts, i.e. you must be
running workstation on a linux machine.  We are working on porting the
toolkit to Windows and Mac. 

For ws6.5 you will need a recent version of the Haskell compiler.  On
debian-based systems run

    sudo apt-get install ghc

For ws7.x you will need to have java installed on your system.  If you
don't have java, or you wish to compile the "native" Emmett compiler you
will need a recent version of the Ocaml compiler.  On debian-based
systems run

    sudo apt-get install ocaml-nox

We also provide mechanism to re-compile the Emmett compiler into Java
byte codes.  The generated executable is a .jar file which you can run
with any platform that has a recent version of the Java Runtime (JRE).


Getting Started
---------------
If you have java, simply add the bin directory to your path, e.g.

    export PATH=\$PATH:$dir/bin # (sh, bash, zsh)
 or
    setenv PATH \$PATH:$dir/bin # (csh, tcsh)

If you don't have java then first run the install.sh script to compile
the Emmett compiler and then follow the instructions on the screen.  

You are now ready to trye some of the samples in the cookbook directory.

Running VProbes
---------------

Before you use the toolkit you must enable VProbes and verify that it is
working correctly.

First you need to enable VProbes globally for the installation.  To do
this please first suspend or power down all VM's and quit
workstation/Fusion.  Then add the following 

    vprobe.allow = TRUE

to the system-wide configuration file.  The standard location for the
system-wide configuration file is

- Windows

  C:\Documents and Settings\All Users\Application Data\VMware\VMware Workstation\config.ini

- MacOS

  /Library/Application Support/VMware Fusion/config

- Linux

  /etc/vmware/config

If the file does not already exist, then create it and add the one line.

You must then enable VProbes for each VM you want to probe.  To do this
add

    vprobe.enable = TRUE

to the .vmx file of that VM while it is power-off or suspended.  Changes
made to the .vmx file while the VM is running won't take effect and, in
some cases, may be lost.  So please make sure to edit the file while the
VM is either suspended or powered off.

To verify that everything is working you can run the following command

    vmrun vprobeVersion <path to your vmx file>

You should see the following

    VProbes version: 1.0 (enabled)

If you don't see this message then something is wrong and VProbes has
not been enabled correctly for that VM.

You are now ready to start probing a VM.  If you added the toolkit bin
directory to your path you can now run vprobes.  For example to get a
list of probe available in a VM run

    vprobe -p <path to vmx>

The following script prints a line every time the VM sends a packet on
the virtual NIC.

    vprobe -c 'MAC_SendPacket { printf("Sending a packet!\n"); }' <path to vmx>

Support
-------

If you have any questions please visit the [VProbes Community
forum](http://communities.vmware.com/community/developer/forums/vprobes).
 

