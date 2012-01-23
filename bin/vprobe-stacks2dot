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

#
# Usage:
#   enable (vprobe VMM10Hz (logstr (gueststack)) (logstr "\n"))
#   resolve symbols: stacks_resolve.pl symtab.txt < vprobe.out > vprobe1.out
#   stacks2dot.pl < vprobe1.out > vprobe.dt
#   dot -Tps -o vprobe.ps vprobe.dt
#

sub get_node {
    my ($name);
    ($name) = @_;

    if (!defined($nodes{$name})) {
	$nodes{$name} = $numNodes;
	$numNodes++;
    }
    return $nodes{$name};
}

print "digraph stacks { size=\"7,10\"\n";
$numNodes = 0;
while (($ln = <STDIN>)) {
    chomp($ln);

    if ($ln =~ /GUEST,.*/ || $ln =~ /VMM,.*/) {
	@calls = split(',', $ln);
	for ($i = 1; $i < $#calls; $i++) {
	    $first = get_node($calls[$i]);
	    $second = get_node($calls[$i+1]);
	    $edge = sprintf("n%d -> n%d", $second, $first);
            $edges{$edge}++;
	}
    }
}

# print the nodes
foreach $key (keys %nodes) {
   printf("n%d [label=\"%s\"];\n", $nodes{$key}, $key);
}
# print the edges
foreach $key (keys %edges) {
   print $key . ";\n";
}
print "}\n";
