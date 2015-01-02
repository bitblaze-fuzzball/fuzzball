#!/usr/bin/perl

use strict;

my @edges;

print "graph: {\n";
while (<>) {
    /^(\d+): (\d+|none|unknown) (\d+|none|unknown) ([?*]) (\d+|none) \((X|-?\d+ -?\d+)\) at (0x[0-9a-f]+) \[(\d*)\]$/
      or die "Parse failure on $_";
    my($n, $lkid, $rkid, $all_seen, $parent, $heur, $addr, $count)
      = ($1, $2, $3, $4, $5, $6, $7, $8);
    push @edges, [$n, $lkid] if $lkid > 0;
    push @edges, [$n, $rkid] if $rkid > 0;
    my $color;
    if ($all_seen eq "*" and $count eq "") {
	# finished internal node
	$color = "lightgrey";
    } elsif ($all_seen eq "*" and $count ne "") {
	# finished query node
	$color = "khaki";
    } elsif ($all_seen eq "?" and $count eq "") {
	# unfinished internal node
	$color = "white";
    } elsif ($all_seen eq "?" and $count ne "") {
	# unfinished query node
	$color = "yellow";
    }
    my $cnt = ($count eq "" ? "" : " [$count]");
    my $h = ($heur eq "1 1" ? "" : " $heur");
    print qq/node: { title: "n$n" label: "$n$cnt$h $addr" color: $color }\n/;
}
for my $e (@edges) {
    my($from, $to) = @$e;
    print qq/edge: { sourcename: "n$from" targetname: "n$to" }\n/;
}
print "}\n";
