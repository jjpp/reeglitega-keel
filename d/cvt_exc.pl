#!/usr/bin/perl -w

use strict;

my %seq = ();

while (<>) {
	chomp();
	next if /^\s*$/;
	my ($num, $lemma, $form, $grammars, $ruleToo) = split(' ');
	my @g = split('_', $grammars);

	my $st0 = ($ruleToo eq '<') ? 'postexc' : 'postrule; unset seq';
	my $st1 = ($ruleToo eq '>') ? 'postexc' : 'postrule; unset seq';

	rule($lemma, $form, $num, $g[0], $st0);
	rule($lemma, $form, $num, $g[1], $st1) if (@g > 1);
}

for (keys %seq) {
	print "0 0 0 0 { stem_transform = preexc && stem_grammar = $_ && seq = $seq{$_}: stem_transform = prerule; seq = 0 }\n";
}

sub rule {
	my ($a, $b, $num, $g, $st) = @_;

	if (!defined($seq{$g})) {
#		print "0 0 0 0 { type = $num && stem_transform = preexc && stem_grammar = $g && !defined(seq): seq = 0 }\n";
		$seq{$g} = 0;
	}

	my $precond = "stem_transform = preexc && stem_grammar = $g && seq = " . $seq{$g};
	my $set_seq = "seq = " . (++$seq{$g});

	print "#$a# #$b# 0 0 { type = $num && $precond: $set_seq; stem_transform = $st } { $precond: $set_seq } \n";
}

