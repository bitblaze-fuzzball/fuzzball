#!/usr/bin/perl

my $version = `svnversion`;
chomp $version;

if ($version eq "") {
    $version = "unknown";
}

my $new_line = qq/let svn_version = "$version"\n/;

my $regen;
if (not -e "svn_version.ml") {
    print "svn_version.ml does not exist or is empty, regenerating\n";
    $regen = 1;
} else {
    open(OLD, "<svn_version.ml")
      or die "Failed to open svn_version.ml for reading: $!";
    my $old_line = <OLD>;
    close OLD;
    if ($old_line eq $new_line) {
	print "svn_version.ml is unchanged at $version\n";
	$regen = 0;
    } else {
	print "Updating svn_version.ml to $version\n";
	$regen = 1;
    }
}

if ($regen) {
    open(NEW, ">svn_version.ml") or
      die "Failed to open svn_version.ml for writing: $!";
    print NEW $new_line;
    close NEW;
}
