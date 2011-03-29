#!/usr/bin/perl

# Copyright 2007-2008, VMware, Inc. All rights reserved.
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
# parameters

use Time::HiRes qw(usleep);
use POSIX;

my $epochStartMarker = "^aggr:";
my $epochEndMarker   = "}\n";

# Behave a bit like "tail -f" mode, reading the output interleaved with
# running the state machine.
my $state = "INIT";
my (%avgHash, %minHash, %maxHash, %countHash);
my %opnameToHashRef = (
   "avg" => \%avgHash,
   "count" => \%countHash,
   "min" => \%minHash,
   "max" => \%maxHash
);        # hash analogues of our 4 aggregating variables
my $href; # pointer to the hash we're currently populating
# Stuff that needs to become parameterizable
my $sortHref = \%countHash; # pointer to the hash on whose values we'll sort

if ($ENV{"SYMTAB"}) {
   $symtab = $ENV{"SYMTAB"};
}

sub get_symname {
    my ($addrStr);
    ($addrStr) = @_;
    my ($addr, $l, $u, $i);
    $addr = hex($addrStr);

    # Uncomment and edit this kludge to filter-out user addresses
    #if ($addr < 0xffffffff80000000) {
    #	return "user";
    #}
    # vmmstack strips the high-order bits of the symbols. An alternative
    # explanation for these low symbols is a user xip, see above.
    #if (($addr & (1 << 31)) && !($addr & (0xffffffff << 32))) {
    #    $addr |= (0xffffffff << 32);
    #}
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
    my $ret = $symname[$l];
    if ($addr - $symaddr[$l] > 0) {
        $ret .= sprintf("+%d", $addr - $symaddr[$l]);
    }
    return $ret;
}
if ($symtab) {
   $numSymbols = 0;
   system("sort < $symtab > /tmp/t.sorted");
   open(SYMTAB, "< /tmp/t.sorted") or die "Can't open /tmp/t.sorted\n";
   while (($ln = <SYMTAB>)) {
      chomp($ln);
      @words = split(' ', $ln);
      $#words >= 2 or die "File format error at $ln\n";
      $symaddr[$numSymbols] = hex($words[0]);
      ($numSymbols == 0 || $symaddr[$numSymbols] >= $symaddr[$numSymbols - 1]) ||
         die "Sort error: $symaddr[$numSymbols] vs $symaddr[$numSymbols - 1]\n";
      $symname[$numSymbols] = $words[2];
      $numSymbols++;
   }
   close(SYMTAB);
}

open(AGGROUT, "< $ARGV[0]") || die("could not open given file $ARGV[0]");
seek(AGGROUT, 0, 2); # SEEK_END

for (;;) {
   # sleep for about 1/3rd of a second between checks for more output. The
   # SEEK_CUR whence field doesn't actually change the position, but it
   # clears the EOF condition on the file handle.
   seek(AGGROUT, 0, 1);
   usleep(1001 * 1000);
   # the EOF moved! Let's move back to our last position, and then read
   # to the end of the file ...
   $state = "INIT";
   LINE: while (<AGGROUT>) {
      if ($state eq "INIT" && /^$epochStartMarker/) {
         $state = "CHOMPLINES";
         next LINE;
      }
      if ($state eq "CHOMPLINES") {
         if (/^ *[a-z0-9]*\[(.*)\] == avg ([0-9]+) count ([0-9]+) min ([0-9]+) max ([0-9]+)/) {
	    my $keystr = $1;
            $avgHash{$keystr} = $2;
	    $countHash{$keystr} = $3;
	    $minHash{$keystr} = $4;
	    $maxHash{$keystr} = $5;
	    next LINE;
	 }
         $state = "INIT";
      }
      # ignore garbage
   }
   outputHashes();
   $state = "INIT";
}

sub outputHashes {
   my ($width, $height);
   ($sysname) = POSIX::uname();
   if ($sysname eq "Darwin") {
      $sttyargs = "-ga";
      $rowpat   = "([0-9]*) rows";
      $colpat   = "([0-9]*) columns";
   } else {
      $sttyargs = "-a";
      $rowpat   = "rows ([0-9]*)";
      $colpat   = "columns ([0-9]*)";
   }
   open(STTY, " stty $sttyargs |");
   while (<STTY>) {
      if (/$rowpat/) {
         $height = $1;
      }
      if (/$colpat/) {
         $width = $1;
      }
      #print "W $width H $height\n";
   }

   close(STTY);
   my @outlines;
   @outlines = ();
   system "clear";
   my @exampleKeys;
   @exampleKeys = keys %countHash;
   my @keys   = split(/,/, $exampleKeys[0]);
   $i = 0;
   foreach $key (@keys) {
      printf("key%-23d", $i++);
   }
   printf("%-10.10s%%\n", "count");
   #for ($i = 0; $i < $width - 1; $i++) {
   #   print "-";
   #}
   print "\n";
   # compute a total on the sorted field, so we can output percentages
   my $total = 0;
   foreach $val (values %countHash) {
      $total += $val;
   }
   
   # @outlines contains the individual lines we'd like to insert.
   # @outlineRefs contains pairs of the form:
   #    [ valueInSortedHash, refToOutlineMember ]
   # We use outlineRefs as a sorted index of @outlines.
   my @outlineRefs;
   @outlineRefs = ();
   foreach $key (@exampleKeys) {
      my @keys   = split(/,/, $key);
      my $outline;
      for ($i = 0; $i <= $#keys; $i++) {
	  if ($symtab && $keys[$i] =~ /^0x/) {
              $keys[$i] = get_symname($keys[$i]);
	  }
	  $keys[$i] = sprintf("%-25.25s ", $keys[$i]);
      }
      $outline = join("", @keys);
      # ok, instead of printing all four, people practically only ever care
      # about averages or counts.
      # foreach $h (\%avgHash, \%minHash, \%maxHash, \%countHash) {
      foreach $h (\%countHash) {
         $outline .= sprintf("%-10d", $h->{$key});
         $outline .= sprintf("%.2f%%", (100 * $h->{$key}) / $total);
      }
      $outline .= "\n";
      push @outlines, $outline;
      # XXX: make the sort field paramaterizable
      push @outlineRefs, [ $sortHref->{$key}, \$outlines[$#outlines]];
   }
   my @sortedRefs =
      sort { -$a->[0] <=> -$b->[0] } @outlineRefs;
   for ($i = 0; $i <= $#sortedRefs && $i < $height - 5; $i++) {
       print ${$sortedRefs[$i]->[1]};
   }
   %avgHash = ();
   %minHash = ();
   %maxHash = ();
   %countHash = ();
}
