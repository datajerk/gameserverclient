#!/usr/bin/env perl -w

use strict;

my @titles = ();
my $max = 0;
my $s = 1;

print "first:\n";

while(<>)
{
	my @a = split(/\"/);

	push @titles,$a[3];

	if(length($a[3]) > $max) {
		$max = length($a[3]);
	}
}

foreach(@titles) {
	if(\$_ == \$titles[-1]) {
		print "last:\n";
	}
	printf("s%03d:\t.asciiz\t\"%-${max}s\"\n",$s++,$_);
}



