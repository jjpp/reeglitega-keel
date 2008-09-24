#!/usr/bin/perl -w

use strict;

while (<>) {
	chomp();
	chomp();
	next if /^\s*$/;
	next if /^\x1a/;
	my ($num, $lemma, $form, $grammars, $ruleToo) = split(' ');
	my @g = split('_', $grammars);

	my $st0 = ($ruleToo eq '<') ? 'postexc' : 'postrule';
	my $st1 = ($ruleToo eq '>') ? 'postexc' : 'postrule';

	rule($lemma, $form, $num, $g[0], $st0);
	rule($form, $lemma, $num, $g[1], $st1) if (@g > 1);
}

sub rule {
	my ($a, $b, $num, $g, $st) = @_;

	my $precond = "stem_transform = preexc && stem_grammar = $g";
	
	return if $a eq '0';
	$st = 'dead_end' if $b eq '0';

	print "#$a# #$b# 0 0 { type = $num && $precond: stem_transform = $st; stop = 1 }\n";
}

