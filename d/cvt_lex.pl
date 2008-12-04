#!/usr/bin/perl

while (<>) {
	chomp();
	my ($w, $t) = split(' ');
	my ($type, $kind) = split('_', $t);

	$rule = $kind =~ /V/ ? 'VERB' : 'NOOMEN';

	if ($rule eq 'VERB') {
		$w =~ s/ma$//;
	}

	print "{$rule} $w 0 0 { step = lemma_valik: type = $type; step = vaike_tyvi; lemma = $w }\n";
}
