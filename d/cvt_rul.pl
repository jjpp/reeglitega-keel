#!/usr/bin/perl -w

use strict;

my ($grammar) = (shift @ARGV);
my $identity = 0;
my $postrule = 'postrule';
while (<>) {
	chomp();
	s/\/.*$//g;
	next if /^\s*$/;
	$identity = 1 if (/^%ori/);
	$postrule = 'plus_degree' if (/^%\+v/);
	$postrule = 'minus_degree' if (/^%-v/);
	next if /^%/;
	next if /^\//;

	my ($upper, $lower, $lc, $rc) = split(' ');
	print "$upper $lower $lc $rc { step = tyved && stem_transform = prerule && stem_grammar = $grammar: stem_transform = $postrule; stop = 1 }\n";
}

if ($identity) {
	print "0 0 0 0 { step = tyved && stem_grammar = $grammar && stem_transform = prerule: stem_transform = $postrule; stop = 1 }\n"
}
