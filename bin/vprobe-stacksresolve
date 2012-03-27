#!/usr/bin/perl

# Copyright 2007-2011, VMware, Inc. All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 1. Redistributions of source code must retain the above copyright notice,
# this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
# 3. Neither the name of VMware, Inc. nor the names of its contributors may
# be used to endorse or promote products derived from this software with
# specific prior written permission.
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

my $vmm64 = 0;
my $commasep = 0;
sub parse_cmdline() {
     use Getopt::Long;
     Getopt::Long::config('no_ignore_case');
     GetOptions('vmm64' => \$vmm64,
                'commasep' => \$commasep,) or
                die 'could not parse command-line';
}

#
# This script converts call stacks to contain function names instead
# of return addresses:
#   stacks_resolve.pl symtab.txt < vprobe.out > vprobe1.out
# 
sub get_symname {
    my ($addrStr, $prefix) = @_;
    my ($addr, $l, $u, $i);
    $addr = hex($addrStr);

    # vmmstack strips the high-order bits of the symbols for vmm64 to
    # fit more frames in the string. If this was a VMM stack, stick those
    # bits back in.
    if ($prefix eq "VMM" && $vmm64 &&
       ($addr & (1 << 31)) && !($addr & (0xffffffff << 32))) {
        $addr |= (0xffffffff << 32);
    }
    $l = 0, $u = $#symaddr;
    if ($symaddr[$l] > $addr ||  # less than the start
	$symaddr[$u] <= $addr) { # burn the last symbol for now
	return $addrStr;
    }
    # Binary-search for the lower bound
    while ($l + 1 < $u) {
	$i = int(($l + $u) / 2);
	if ($symaddr[$i] <= $addr) {
	    $l = $i;
	} else {
	    $u = $i;
	} 
    }
    ($symaddr[$l] <= $addr && ($l == $#symaddr || $symaddr[$l+1] > $addr)) ||
	die "Bug";
    return $symname[$l];
}

parse_cmdline();
$#ARGV == 0 or die "Usage: \"$0 symtab-file < vprobe.out\"\n";
$numSymbols = 0;
system("sort < $ARGV[0] > /tmp/t.sorted");
open(SYMTAB, "< /tmp/t.sorted") or die "Can't open /tmp/t.sorted\n";
LINE: while (($ln = <SYMTAB>)) {
    chomp($ln);
    @words = split(' ', $ln);
    $#words >= 2 or next LINE;
    $symaddr[$numSymbols] = hex($words[0]);
    ($numSymbols == 0 || $symaddr[$numSymbols] >= $symaddr[$numSymbols - 1]) ||
	die "Sort error: $symaddr[$numSymbols] vs $symaddr[$numSymbols - 1]\n";
    $symname[$numSymbols] = $words[2];
    $numSymbols++;
}
close(SYMTAB);

while (($ln = <STDIN>)) {
    chomp($ln);

    if ($ln =~ /(GUEST|VMM)_.*/) {
        my $pfx = $1;  # remember guest/VMM to decide whether to
                       # sign-extend
        my $aggrSfx, $aggrPfx;
        undef $aggrSfx;
        undef $aggrPfx;
        if ($ln =~ /(.*\[)(.*)(\].*)/) {
            # preserve aggre prefixes and suffixes
            $aggrPfx = $1;
            $ln = $2;
            $aggrSfx = $3;
        }
	$ln =~ s/_\.\.\.//; # get rid of _...
	@retAddrs = split('_', $ln);
        if (defined($aggrPfx)) {
           print $aggrPfx;
        }
	print $retAddrs[0];
	for ($i = 1; $i <= $#retAddrs; $i++) {
	    $symName = get_symname($retAddrs[$i], $pfx);
            if ($commasep) {
	       print "," . $symName;
            } else {
	       print "\t" . $symName . "\n";
            }
	    if ($symName =~ /_NULL_IMPORT_DESCRIPTOR/ ||
		$symName eq "user") {
		last;
	    }
	}
        if (defined($aggrSfx)) {
           print $aggrSfx;
        }
	print "\n";
    }
}
