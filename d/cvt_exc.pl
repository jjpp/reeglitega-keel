#!/usr/bin/perl -w

use strict;

while (<>) {
	chomp();
	next if /^\s*$/;
	my ($num, $lemma, $form, $grammars, $ruleToo) = split(' ');
	my @g = split('_', $grammars);

	my $st0 = ($ruleToo eq '<') ? 'postexc' : 'postrule';
	my $st1 = ($ruleToo eq '>') ? 'postexc' : 'postrule';

	print "#$lemma# #$form# 0 0 { type = $num && stem_transform = preexc && stem_grammar = $g[0]: stem_transform = $st0 }\n";
	print "#$form# #$lemma# 0 0 { type = $num && stem_transform = preexc && stem_grammar = $g[1]: stem_transform = $st1 }\n" if (@g > 1);
}
