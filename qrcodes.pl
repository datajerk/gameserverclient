#!/usr/bin/env perl -w

use strict;

my @titles = ();
my $max = 0;
my $s = 0;

while(<>)
{
	my @a = split(/\"/);
	my $short = $a[5];
	my $qr;

	$short =~ s/.html//;
	$short .= ".qr.wav";
	$short = "http://asciiexpress.net/gameserver/$short";

	$qr = `./qrbytes "$short"`;

	my $width = $qr;
	my $bytes = $qr;

	$width =~ s/\n//g;
	$width =~ s/^width: (\d+).*/$1/;

	$bytes =~ s/\n//g;
	$bytes =~ s/.*bytes: (.*)/$1/;
	$bytes =~ s/0x/\$/g;
	$bytes =~ s/ /,/g;
	$bytes =~ s/,$//;

	$s++;

	printf("qr%03d:\t.byte\t",$s);
	print "$width\n";

	my $i = 0;

	foreach(split(/,/,$bytes)) {
		$i++;
		if(($i-1) % 16 == 0) {
			print "	.byte	";
		}
		print "$_";
		if($i % 16 == 0) {
			print "\n";
		}
		else {
			if($i != split(/,/,$bytes)) {
				print ",";
			}
		}
	}
	print "\n";
	print "\n";
}

print "qrptrl:";

for(my $i=1;$i<=128;$i++) {
	if(($i-1) % 8 == 0) {
		print "	.word	";
	}
	printf("qr%03d",$i);
	if($i % 8 == 0) {
		print "\n";
	}
	else {
		if($i != $s) {
			print ",";
		}
	}
}
print "\n";

print "qrptrh:";

for(my $i=129;$i<=$s;$i++) {
	if(($i-1) % 8 == 0) {
		print "	.word	";
	}
	printf("qr%03d",$i);
	if($i % 8 == 0) {
		print "\n";
	}
	else {
		if($i != $s) {
			print ",";
		}
	}
}
print "\n";

