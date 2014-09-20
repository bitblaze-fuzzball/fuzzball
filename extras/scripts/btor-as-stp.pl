#!/usr/bin/perl

# Wrap the SMT solver Boolector so that it appears to have the same
# interface as the solver STP in its default CVC mode. The latter is
# what FuzzBALL's -solver stp-external supports, so this can serve
# as a stopgap until FuzzBALL supports SMTLIB2 natively. Note that
# because of that motivation, it isn't fully general in its support
# of STP options, for instance: it's best at what FuzzBALL expects.

# Enhancement idea: run STP too, check that the sat/unsat result is
# the same.

# Enhancement idea: run Boolector and STP in parallel, return the
# result from whoever finishes first. (A simple kind of portfolio
# solver.)

use strict;

my $btor_bin = "/export/scratch2/mccamant/build/boolector-1.5.118-with-sat-solvers/boolector/boolector";

my $stp_bin = "/home/fac05/mccamant/bitblaze/fuzzball/trunk-gh/stp/stp";

my $cvc_fname;

my @btor_args = ();

for my $i (0 .. $#ARGV) {
    if ($ARGV[$i] eq "-g") {
	my $timeout = $ARGV[++$i];
	push @btor_args, "-t", $timeout;
    } elsif ($ARGV[$i] =~ /^-/) {
	die "Unrecognized STP option $ARGV[$i]";
    } else {
	if (defined($cvc_fname)) {
	    die "Only one input file expected";
	}
	$cvc_fname = $ARGV[$i];
    }
}

die "Input file $cvc_fname unreadable" unless -r $cvc_fname;

my %type;

# Boolector's model output format doesn't distinguish between Booleans
# and 1-bit bitvectors, but STP's does. Pattern match on declarations
# to at least sometimes distinguish the two.
open(CVC, "<", $cvc_fname)
  or die "Failed to open $cvc_fname for reading: $!";
while (<CVC>) {
    if (/\s*(\w+)\s*:\s*(BV|BITVECTOR|BOOLEAN)/) {
	my($var, $type) = ($1, $2);
	if ($type eq "BV" or $type eq "BITVECTOR") {
	    $type{$var} = "bv";
	} else {
	    $type{$var} = "bool";
	}
    }
}
close CVC;

my $btor_fname = $cvc_fname . ".btor.smt2";
open(SMT2, ">", $btor_fname)
  or die "Failed to open $btor_fname for writing: $!";

open(STPCONV, "-|", $stp_bin, "--print-back-SMTLIB2", $cvc_fname)
  or die "Failed to open stp converter: $!";

while (<STPCONV>) {
    print SMT2 $_;
}
close STPCONV;

# Boolector likes to see these, STP doesn't bother printing them
print SMT2 "(check-sat)\n";
print SMT2 "(exit)\n";

close SMT2;

my $log_fname = $cvc_fname . ".btor.log";
open(LOG, ">", $log_fname);

open(BTOR, "-|", $btor_bin, "-v", "-m", $btor_fname);
while (<BTOR>) {
    if (/^\[/) {
	print LOG $_;
    } elsif (/^sat$/) {
	print LOG $_;
	print "Invalid.\n";
    } elsif (/^unsat$/) {
	print LOG $_;
	print "Valid.\n";
    } elsif (/^\|(.*)\| ([01x]*)$/) {
	print LOG $_;
	my($var, $bits) = ($1, $2);
	my $size = length($bits);
	$bits =~ tr/x/0/; # Treat don't-cares as 0
	my($op, $val);
	if ($size == 1 and $type{$var} eq "bool") {
	    $op = "<=>";
	    $val = ($bits eq "1" ? "TRUE" : "FALSE");
	} elsif ($size == 1) {
	    $op = " = ";
	    $val = "0b" . $bits;
	} elsif (($size % 4) == 0) {
	    $op = " = ";
	    if (($size % 8) == 4) {
		# Odd number of nibbles, pad with a leading 0x0 = 0b0000
		$bits = "0000" . $bits;
	    }
	    my $hex = unpack("H*", pack("B*", $bits));
	    if (($size % 8) == 4) {
		# Compensate for leading 0 inserted above
		$hex = substr($hex, 1);
	    }
	    $val = "0x" . $hex;
	}
	print "ASSERT( $var$op$val );\n";
    } else {
	chomp;
	die "Parse failure on <$_>";
    }
}
close BTOR;

close LOG;
