#!/usr/bin/perl

# Annote FuzzBALL trace output with identity information about
# function calls. The trace should include at least -trace-setup,
# -trace-syscalls, and -trace-callstack.

use strict;
use IPC::Open2;

my $exe_fname;
my $ldso_fname;
my $is_exe = 0;
my @fd_to_fname;

my %seen_files;

#       0     1      2      3     4     5
# [$addr, $size, $fd_w, $fd_r, $pid, $off];
my @sections;

sub start_section {
    my($addr, $size, $fname) = @_;
    return if $seen_files{$fname}++ > 0;
    open(OD, "-|", "/usr/bin/objdump", "-h", $fname);
    my $text_off;
    while (<OD>) {
	chomp;
	my @f = split(" ");
	if ($f[1] eq ".text") {
	    my($idx, $name, $size, $vma, $lma, $off, $algn) = @f;
	    $off = hex $off;
	    $text_off = $off;
	}
    }
    close OD;
    printf "%08x+%08x: $fname\n", $addr, $size;
    my($fd_w, $fd_r, $pid);
    $pid = open2($fd_r, $fd_w, "/usr/bin/addr2line", "-e", $fname,
		 "-j", ".text", "-f");
    push @sections, [$addr, $size, $fd_w, $fd_r, $pid, $text_off];
}

sub trans_addr {
    my($addr) = @_;
    $addr = hex $addr;
    for my $s (@sections) {
	next if $addr < $s->[0];
	next if $addr >= $s->[0] + $s->[1];
	my $file_off = $addr - $s->[0];
	my $sym_off = $file_off - $s->[5];
	my($fd_w, $fd_r) = @$s[2, 3];
	printf $fd_w "0x%x\n", $sym_off;
	my $line = <$fd_r>;
	chomp $line;
	my $func = $line;
	$line = <$fd_r>;
	chomp $line;
	my $loc = $line;
	$loc =~ /^(.*):(.*)$/ or die;
	my($file, $line) = ($1, $2);
	$file =~ s[^.*/][];
	return "$func $file:$line";
    }
}

while (<>) {
    my $annot = "";
    if (/^Loading executable from (.*)$/) {
	$exe_fname = $1;
	$is_exe = 1;
    } elsif (/^Loading from dynamic linker (.*)$/) {
	$ldso_fname = $1;
	$is_exe = 0;
    } elsif (/^Finished ldso loading/) {
	$is_exe = 1;
    } elsif (/^Loading\s+text segment from ([0-9a-f]+) to ([0-9a-f]+)$/) {
	my($start, $end) = (hex($1), hex($2));
	my $size = $end - $start;
	my $fname = $is_exe ? $exe_fname : $ldso_fname;
	start_section($start, $size, $fname);
    } elsif (/^open\("(.*)".*\) = (\d+)/) {
	my($fname, $fd) = ($1, $2);
	if ($fname =~ /\.so[\d.]*$/) {
	    $fd_to_fname[$fd] = $fname;
	}
    } elsif (/^mmap\(0x[0-9a-f]+, (\d+), .*, (\d+), \d+\) = (\d+) /) {
	my($size, $fd, $addr) = ($1, $2, $3);
	if ($fd_to_fname[$fd]) {
	    start_section($addr, $size, $fd_to_fname[$fd]);
	}
    } elsif (/^( *)Call from (0x[0-9a-f]+) to (0x[0-9a-f]+) \(return to (0x[0-9a-f]+)\)/) {
	my($space, $site, $targ, $ret) = ($1, $2, $3, $4);
	my $targ_name = trans_addr($targ);
	$annot = $space . "$targ_name\n";
    }
    print;
    print $annot if $annot;
}

for my $s (@sections) {
    close $s->[2];
    close $s->[3];
    waitpid($s->[4], 0);
}
