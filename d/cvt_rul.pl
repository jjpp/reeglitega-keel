#!/usr/bin/perl -w

use strict;

my ($grammar) = (shift @ARGV);
my $identity = 0;
while (<>) {
	chomp();
	s/\/.*$//g;
	next if /^\s*$/;
	$identity = 1 if (/^%ori/);
	next if /^%/;
	next if /^\//;

	my ($upper, $lower, $lc, $rc) = split(' ');
	print "$upper $lower $lc $rc { stem_transform = prerule && stem_grammar = $grammar: stem_transform = postrule }\n";
}

if ($identity) {
	print "0 0 0 0 { stem_grammar = $grammar && stem_transform = prerule: stem_transform = postrule }\n"
}
