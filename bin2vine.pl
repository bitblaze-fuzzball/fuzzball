#!/usr/bin/perl

#my $root = "/home/testing-ia32";
my $root = "/";

my $load_base = 0x08048000;

my %memory;

use strict;

sub min { $_[0] < $_[1] ? $_[0] : $_[1] }
sub max { $_[0] > $_[1] ? $_[0] : $_[1] }

sub read_elf_header {
    my($fh, $fname) = @_;
    read($fh, my $ident, 16);
    if ($ident ne "\x7fELF\1\1\1\0\0\0\0\0\0\0\0\0") {
	die "$fname does not appear to be a 32-bit LSB ELF file\n";
    }
    my %h;
    read($fh, my $buf, 36);
    @h{"type", "machine", "version", "entry", "phoff", "shoff", "flags",
	 "ehsize", "phentsize", "phnum", "shentsize", "shnum", "shstrndx"}
      = unpack("SSIIIIISSSSSS", $buf);
    die "Unexpected size ELF header" unless $h{"ehsize"} == 16 + 36;
    return %h;
}

sub read_program_headers {
    my($fh, $ehr) = @_;
    die "Unexpected program header format in core"
      unless $ehr->{"phentsize"} == 32;
    seek($fh, $ehr->{"phoff"}, 0);
    my @phs;
    for my $ph_num (1 .. $ehr->{"phnum"}) {
	read($fh, my $buf, 32);
	my %h;
	@h{"type", "offset", "vaddr", "paddr",
	   "filesz", "memsz", "flags", "align"}
	  = unpack("IIIIIIII", $buf);
	$phs[$ph_num] = {%h};
    }
    return @phs;
}

sub write_page {
    my($vaddr_base, $page, $len) = @_;
    for my $line_num (0 .. 255) {
	return if 16 * $line_num >= $len;
	my $line = substr($page, 16 * $line_num, 16);
	my $vaddr = $vaddr_base + 16 * $line_num;
	my $nice = $line;
	$nice =~ tr/ -~/./c;
	$nice =~ s/\\$/./; # Avoid backslash-newline continuations!
	my @bytes = unpack("C16", $line);
	printf "// %08x  %02x %02x %02x %02x %02x %02x %02x %02x  ".
	  "%02x %02x %02x %02x %02x %02x %02x %02x  %s\n",
	    $vaddr, @bytes, $nice;
	#next if $line eq "\0" x 16;
	for my $b (0 .. 15) {
	    printf "mem[0x%08x:reg32_t]:reg8_t = 0x%02x:reg8_t;",
	      $vaddr + $b, $bytes[$b];
	    $memory{$vaddr + $b} = $bytes[$b];
	    my $stop = (16 * $line_num + $b >= $len);
	    print +($b & 1 || $stop) ? "\n" : " ";
	    return if $stop;
	}
    }
}

sub write_segment {
    my($fh, $phr, $virt_off) = @_;
    #return if $phr->{"filesz"} == 0;
    my $type_str = "???";
    if ($phr->{"type"} == 1 and $phr->{"flags"} == 5) {
	$type_str = "text";
    } elsif ($phr->{"type"} == 1 and $phr->{"flags"} == 6) {
	$type_str = "data";
    } elsif ($phr->{"type"} == 2) {
	$type_str = "DYNAMIC";
    } elsif ($phr->{"type"} == 3) {
	$type_str = "INTERP";
    } elsif ($phr->{"type"} == 4) {
	$type_str = "NOTE";
    } elsif ($phr->{"type"} == 6) {
	$type_str = "PHDR";
    } elsif ($phr->{"type"} == 0x6474e550) {
	$type_str = "EH_FRAME";
    } elsif ($phr->{"type"} == 0x6474e552) {
	$type_str = "RELRO";
    }
    printf STDERR "Loading %8s segment from %08x to %08x\n", $type_str,
      $phr->{"vaddr"} + $virt_off,
	$phr->{"vaddr"} + $virt_off + $phr->{"filesz"};
    seek($fh, $phr->{"offset"}, 0);
    my $partial = $phr->{"filesz"} % 0x1000;
    for my $page_num (0 .. int($phr->{"filesz"}/4096) - 1) {
	read($fh, my $page, 4096);
	my $va = $phr->{"vaddr"} + $virt_off + $page_num*4096;
	write_page($va, $page, 4096); # unless $page eq "\0" x 4096;
    }
    if ($partial) {
	read($fh, my $page, $partial);
	my $va = $phr->{"vaddr"} + $virt_off + $phr->{"filesz"} - $partial;
	write_page($va, $page, $partial);
    }
    if ($phr->{"memsz"} > $phr->{"filesz"}) {
	# E.g., a BSS region. Zero fill to avoid uninit-value errors.
	printf STDERR "            Zero filling from %08x to %08x\n",
	  $phr->{"vaddr"} + $virt_off + $phr->{"filesz"},
	    $phr->{"vaddr"} + $virt_off + $phr->{"memsz"};
	my $va = $phr->{"vaddr"} + $virt_off + $phr->{"filesz"};
	my $first_full = (($va + 4095) & ~4095);
	my $remaining = $phr->{"memsz"} - $phr->{"filesz"};
	my $partial1 = min($first_full - $va, $remaining);
	write_page($va, "\0" x $partial1, $partial1);
	$va += $partial1;
	$remaining -= $partial1;
	while ($remaining >= 4096) {
	    write_page($va, "\0" x 4096, 4096);
	    $remaining -= 4096;
	    $va += 4096;
	}
	write_page($va, "\0" x $remaining, $remaining);
	$va += $remaining;
	# ld.so knows that it can use beyond its BSS to the end of the page
	# that it's stored on as the first part of its heap without asking
	# from an allocation from the OS. So 0-fill the rest of the page
	# too to be compatible with this.
	my $last_aligned = (($va + 4095) & ~4095);
	printf STDERR "      Extra zero filling from %08x to %08x\n",
	  $va, $last_aligned;
	my $last_space = $last_aligned - $va;
	write_page($va, "\0" x $last_space, $last_space);
    }
}

sub load_ldso {
    my($dso, $vaddr) = @_;
    open(my $dso_fh, "<$dso") or die "Can't open library file $dso: $!";
    my %dso_eh = read_elf_header($dso_fh, $dso);
    die "Expecting ld.so to be an DSO" unless $dso_eh{"type"} == 3;
    my @dso_phs = read_program_headers($dso_fh, \%dso_eh);
    for my $ph_num (1 .. $dso_eh{"phnum"}) {
	my $phr = $dso_phs[$ph_num];
	#print join(" ", %$phr), "\n";
	if ($phr->{"type"} == 1) { # PT_LOAD
	    write_segment($dso_fh, $phr, $vaddr);
	} elsif ($phr->{"memsz"} != 0) {
	    write_segment($dso_fh, $phr, $vaddr);
	}
    }
    return $vaddr + $dso_eh{"entry"};
}

die "Usage: bin2vine <program> [args]\n" unless @ARGV >= 1;
my @argv = @ARGV;
my $prog = $ARGV[0];

open(my $prog_fh, "<$prog") or die "Can't open program file $prog: $!";
my %prog_eh = read_elf_header($prog_fh, $prog);
die "Expecting program to be an executable" unless $prog_eh{"type"} == 2;
#print join(" ", %prog_eh), "\n";

print <<END;
var mem:mem32l_t;
var R_EBP:reg32_t;
var R_ESP:reg32_t;
var R_ESI:reg32_t;
var R_EDI:reg32_t;
var R_EIP:reg32_t;
var R_EAX:reg32_t;
var R_EBX:reg32_t;
var R_ECX:reg32_t;
var R_EDX:reg32_t;
var EFLAGS:reg32_t;
var R_CF:reg1_t;
var R_PF:reg1_t;
var R_AF:reg1_t;
var R_ZF:reg1_t;
var R_SF:reg1_t;
var R_OF:reg1_t;
var R_CC_OP:reg32_t;
var R_CC_DEP1:reg32_t;
var R_CC_DEP2:reg32_t;
var R_CC_NDEP:reg32_t;
var R_DFLAG:reg32_t;
var R_IDFLAG:reg32_t;
var R_ACFLAG:reg32_t;
var R_EMWARN:reg32_t;
var R_LDT:reg32_t;
var R_GDT:reg32_t;
var R_CS:reg16_t;
var R_DS:reg16_t;
var R_ES:reg16_t;
var R_FS:reg16_t;
var R_GS:reg16_t;
var R_SS:reg16_t;
var R_FTOP:reg32_t;
var R_FPROUND:reg32_t;
var R_FC3210:reg32_t;
var R_SSEROUND:reg32_t;
END

my @prog_phs = read_program_headers($prog_fh, \%prog_eh);

my($ldso_base, $ldso_entry) = (0, -1);

for my $ph_num (1 .. $prog_eh{"phnum"}) {
    my $phr = $prog_phs[$ph_num];
    #print join(" ", %$phr), "\n";
    if ($phr->{"type"} == 1) { # PT_LOAD
	die "Expected R/X segment to start at $load_base" if
	  $phr->{"flags"} == 5 and $phr->{"vaddr"} != $load_base;
	write_segment($prog_fh, $phr, 0);
    } elsif ($phr->{"type"} == 3) { # PT_INTERP
	seek($prog_fh, $phr->{"offset"}, 0);
	read($prog_fh, my $interp, $phr->{"filesz"});
	$ldso_base = 0xb7f00000;
	$ldso_entry = load_ldso("$root/$interp", $ldso_base);
	write_segment($prog_fh, $phr, 0);
    } elsif ($phr->{"memsz"} != 0) {
	write_segment($prog_fh, $phr, 0);
    }
}

my %env;
my %string_locs;
for my $k (qw(DISPLAY EDITOR HOME LANG LOGNAME PAGER PATH PWD SHELL TERM
	      USER USERNAME XAUTHORITY)) {
    $env{$k} = $ENV{$k};
    $string_locs{"$k=$env{$k}\0"} = 0;
}
# Hmm, this means that argv entries with the same value will share storage.
# Is that kosher?
for my $str (@argv, "i686") {
    $string_locs{"$str\0"} = 0;
}
my $stack_top = 0xc0000000;
my $stack_size = 4096;
my $stack_start = $stack_top - $stack_size;
my $stack = "\0" x $stack_size;
my $esp = $stack_top;
for my $str (sort keys %string_locs) {
    $esp -= length $str;
    substr($stack, $esp-$stack_start, length $str) = $str;
    $string_locs{$str} = $esp;
}
$esp &= ~0xf; # 16-byte align
my %auxv =
  (3 => $load_base + $prog_eh{"phoff"},  # AT_PHDR
   4 => $prog_eh{"phentsize"},           # AT_PHENT,
   5 => $prog_eh{"phnum"},               # AT_PHNUM,
   6 => 4096,                            # AT_PAGESZ
   7 => $ldso_base,                      # AT_BASE: dynamic loader, if present
   8 => 0,                               # AT_FLAGS, no flags
   9 => $prog_eh{"entry"},               # AT_ENTRY,
   11 => $<,                             # AT_UID
   12 => $>,                             # AT_EUID
   13 => $(,                             # AT_GID
   14 => $),                             # AT_EGID
   15 => $string_locs{"i686\0"},         # AT_PLATFORM
#   16 => 0xbfebfbff,                     # AT_HWCAP, Core 2 Duo
   16 => 0x0,                            # AT_HWCAP, presumably bare-bones
   17 => 100,                            # AT_CLKTCK
   23 => 0,                              # AT_SECURE
   # Let's see if we can avoid bothering with AT_SYSINFO
  );
$esp -= 8 if keys(%auxv) & 1;
for my $k (sort {$a <=> $b} keys %auxv) {
    $esp -= 8;
    substr($stack, $esp-$stack_start, 8) = pack("II", $k, $auxv{$k});
}
$esp -= 4; # 0 byte marking end of environment
for my $k (reverse sort keys %env) {
    $esp -= 4;
    substr($stack, $esp-$stack_start, 4)
      = pack("I", $string_locs{"$k=$env{$k}\0"});
}
$esp -= 4; # 0 byte marking end of argv
for my $str (reverse @argv) {
    $esp -= 4;
    substr($stack, $esp-$stack_start, 4) = pack("I", $string_locs{"$str\0"});
}
$esp -= 4;
substr($stack, $esp-$stack_start, 4) = pack("I", scalar @argv); # argc

die "Stack was too big" if $esp < $stack_start;
write_page($stack_start, $stack, $stack_size);

printf "R_ESP:reg32_t = 0x%08x:reg32_t;\n", $esp;
printf "R_EDX:reg32_t = 0x%08x:reg32_t;\n", 0; # Nothing for atexit()

my $entry;
if ($ldso_entry != -1) {
    # Run the dynamic linker first
    $entry = $ldso_entry;
} else {
    # Run the main program first
    $entry = $prog_eh{"entry"};
}
printf "R_EIP:reg32_t = 0x%08x:reg32_t;\n", $entry;

# ABI says: other GPRs undefined
# But zero out some that the Vine translations or the code will
# actually read, to avoid spurious uninit value warnings from Vine.

# Callee-saved registers, saved in startup code
printf "R_EBP:reg32_t = 0x%08x:reg32_t;\n", 0;
printf "R_EDI:reg32_t = 0x%08x:reg32_t;\n", 0;
printf "R_ESI:reg32_t = 0x%08x:reg32_t;\n", 0;
printf "R_EBX:reg32_t = 0x%08x:reg32_t;\n", 0;

# LDT, read even when not used
printf "R_LDT:reg32_t = 0x%08x:reg32_t;\n", 0;

# Caller-saved registers, printed for debugging purposes
printf "R_EAX:reg32_t = 0x%08x:reg32_t;\n", 0;
printf "R_ECX:reg32_t = 0x%08x:reg32_t;\n", 0;

my $eflags = 0; # ABI says: unspecified, except DF clear
printf "EFLAGS:reg32_t = 0x%08x:reg32_t;\n", $eflags;

printf "R_CF:reg1_t = 0x%01x:reg1_t;\n", !!($eflags & 0x001);
printf "R_PF:reg1_t = 0x%01x:reg1_t;\n", !!($eflags & 0x004);
printf "R_AF:reg1_t = 0x%01x:reg1_t;\n", !!($eflags & 0x010);
printf "R_ZF:reg1_t = 0x%01x:reg1_t;\n", !!($eflags & 0x040);
printf "R_SF:reg1_t = 0x%01x:reg1_t;\n", !!($eflags & 0x080);
printf "R_OF:reg1_t = 0x%01x:reg1_t;\n", !!($eflags & 0x800);
printf "R_DFLAG:reg32_t = 0x%08x:reg32_t;\n", (-1)**(!!($eflags & 0x400));
		
