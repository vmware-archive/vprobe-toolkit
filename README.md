# VProbe Toolkit

VProbes is a safe, dynamic technology for transparently instrumenting
a powered-on guest operating system, its currently running processes,
and VMware's virtualization software. VProbes provides both dynamically
and statically defined probes. You can find more detailed documentation
for VProbes at the [community website][1].

This Toolkit provides an interactive, programmer-friendly interface to
VProbes. It allows you to write instrumentation scripts using a C-like
high-level language called Emmett which is a replacement for the more
primitive VP language native to VProbes. It includes,

1. Source code for the Emmett compiler (emmett).
2. Example VProbes scripts written in Emmett (cookbook).
3. Some sample scripts that monitor interesting events in Linux
   guests (bin).


## License

See License.txt


## Compatibility

This toolkit is compatible with the following products:

* [VMware Workstation][2] 8
* [VMware Fusion][3] 4.0, 4.0.1, 4.0.2, 4.1.0, 4.1.1

[2]:http://www.vmware.com/products/workstation/
[3]:http://www.vmware.com/products/fusion/

## Build and Install

This toolkit can be built and installed on a MacOS X or a GNU/Linux
based system (such as Ubuntu). There are some dependencies you must
satisfy before building the toolkit.

1. Python (2.5.x .. 2.7.x) - You must have Python installed in your
   system to be able to run the VProbes command-line application.

2. OCaml (>= 3.11.2) - You need the OCaml toolchain to be able to
   build `emmett` (the VProbes compiler).

Once you have the required software packages, building is as simple
as,

    vprobe-toolkit $ ./configure 
    vprobe-toolkit $ make
    vprobe-toolkit $ [sudo] make install


The toolkit files should be installed in the appropriate system
locations.


### Get OCaml on MacOS X

We recommend using the binary available at,

http://caml.inria.fr/pub/distrib/ocaml-3.11/ocaml-3.11.2-intel.dmg

If you use Macports, you can also install OCaml by doing,

    $ port install ocaml

### Getting OCaml on Linux

If you are using a Debian based system you can get OCaml, as follows,

    $ [sudo] apt-get install ocaml-nox

This will vary depending on your Linux distribution and package manager.


## Enable VProbes

Before you use the toolkit you must enable VProbes and verify that it is
working correctly. VProbes must be both enabled in a system wide configuration
file for your installation of VMware Workstation (on Linux) or VMware Fusion
(on MacOS X), and 

First, you need to enable VProbes globally for your installation of VMware
Workstation (on Linux) or VMware Fusion (on Mac OS X).

### MacOS X (>= 10.4)

If you are already running VMware Fusion, please power down all VM's and
quit Fusion. Then add the following line,

    vprobe.allow = TRUE

to the file,

    /Library/Application\ Support/VMware Fusion/config

After this, you must then enable VProbes for each VM you want to probe, by
adding the following line to `.vmx` file of the VM.

    vprobe.enable = TRUE

Changes made to the .vmx file while the VM is running won't take effect and,
in some cases, may be lost.  So please make sure to edit the file while the
VM is either suspended or powered off.

To verify that everything is working you can run the following command

    $ VMRUN="/Applications/VMware Fusion.app/Contents/Library/vmrun"
    $ $VMRUN vprobeVersion </path/to/your/vmx/file>

You should see the following

    VProbes version: 1.0 (enabled)

If you don't see this message then something is wrong and VProbes has
not been enabled correctly for that VM.

### GNU/Linux

If you are already running VMware Workstation, please power down all VM's
and quit Workstation. Then add the following line,

    vprobe.allow = TRUE

to the file,

    /etc/vmware/config

After this, you must then enable VProbes for each VM you want to probe, by
adding the following line to `.vmx` file of the VM.

    vprobe.enable = TRUE

Note, changes made to the .vmx file while the VM is running won't take effect
and, in some cases, may be lost.  So please make sure to edit the file while
the VM is either suspended or powered off.

To verify that everything is working you can run the following command

    $ VMRUN="/usr/bin/vmrun"
    $ $VMRUN vprobeVersion </path/to/your/vmx/file>

You should see the following

    VProbes version: 1.0 (enabled)

If you don't see this message then something is wrong and VProbes has
not been enabled correctly for that VM.

## Ready

You are now ready to start probing a VM. Depending on where you installed
the toolkit, the vprobe application should now be available in your path.

To get a quick listing of all available probes in your VM, you can do:

    vprobe -p <path/to/vmx>

As a quick example, here's a one-liner that prints something every time
the VM sends a packet on the virtual NIC.

    vprobe -c 'MAC_SendPacket { printf("Sending a packet!\n"); }' <path/to/vmx>

## Support

If you have any questions please visit the [VProbes Community
forum](http://communities.vmware.com/community/developer/forums/vprobes).

## Notes for Toolkit Developers

The toolkit uses GNU Make and Autoconf for managing build configuration.
The Autoconf configuration template is: `$(src)/configuration.in`. To
make things convenient, the `configure` script generated by autoconf is
also checked in as part of the source. The assumption is that
`configure.in` will not be modified very often. If it is modified, you
must perform the following steps:

    vprobe-toolkit $ aclocal -I m4 --force
    vprobe-toolkit $ autoreconf -I m4 -f -i

This will generate an updated `configure` script. **Don't forget to
check-in the updated script!**

### Building the MacOS X Installer Package 

The toolkit build system can build an installer package compatible with modern
versions of Mac OS X (>= 10.4). It uses the free PackageMaker application that
comes with Apple's Xcode development suite, so make sure you have Xcode
installed. To build the package, perform the following commands at the root of
the toolkit source tree,

    vprobe-toolkit $ ./configure
    vprobe-toolkit $ make
    vprobe-toolkit $ make -C installer/macos

This will build `vprobe-toolkit.pkg` in `$(src)/installer/macos</code>`.

The PackageMaker metadata documents are stored in
`$(src)/installer/macos/vprobe-toolkit.pmdoc`. If you want to modify the
installer, you must open this document with the PackageMaker application.

    $ /Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker $(src)/installer/macos/vprobe-toolkit.pmdoc

### Building Emmett.jar (Experimental)

You can create a Java version of `emmett` using `ocamljava`. Because
write-once-run-anywhere. Right?

Download [Ocaml-Java](http://ocamljava.x9c.fr/downloads.html) binary tarball,
untar it into a known location.

Once setup, you can build `emmett.jar` by specifying the path to `ocamljava`
installation.

    vprobe-toolkit $ ./configure --with-ocamljava=/path/to/ocamljava/install/dir
    vprobe-toolkit $ make
    vprobe-toolkit $ [sudo] make install


[1]:http://communities.vmware.com/community/vmtn/developer/forums/vprobes
[2]:http://www.vmware.com/products/workstation/
[3]:http://www.vmware.com/products/fusion/
