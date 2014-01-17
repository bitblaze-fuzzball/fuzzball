#!/usr/bin/perl

my $version = `git describe --always --dirty`;
chomp $version;

if ($version eq "") {
    $version = "unknown";
}

my $new_line = qq/let git_version = "$version"\n/;

my $regen;
if (not -e "git_version.ml") {
    print "git_version.ml does not exist or is empty, regenerating\n";
    $regen = 1;
} else {
    open(OLD, "<git_version.ml")
      or die "Failed to open git_version.ml for reading: $!";
    my $old_line = <OLD>;
    close OLD;
    if ($old_line eq $new_line) {
	print "git_version.ml is unchanged at $version\n";
	$regen = 0;
    } else {
	print "Updating git_version.ml to $version\n";
	$regen = 1;
    }
}

if ($regen) {
    open(NEW, ">git_version.ml") or
      die "Failed to open git_version.ml for writing: $!";
    print NEW $new_line;
    close NEW;
}
