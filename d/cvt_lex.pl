#!/usr/bin/perl

# 4 	X - seep, näikse, kuulukse, tunnukse ?
# 5 	H - (koha?)nimi
# 18 	O - järgarv, (ordinaal?)
# 21 	J - sidesõna, konjunktsioon?
# 43 	G - genitiivatribuut (nimetaoline omadusnõna, käändumatu omadussõna)
# 43 	N - numeraal, arvsõna
# 64 	P - pronoomen, asesõna
# 210 	K - sidesõna, konjunktsioon
# 284 	I - hüüdsõna, interjektsioon 
# 2672 	D - adverb, määrsõna
# 6923 	V - verb
# 7152 	A - adjektiiv, omadussõna
# 19738	S - substantiiv, nimisõna
# 

while (<>) {
	chomp();
	my ($w, $t) = split(' ');
	my ($type, $kind) = split('_', $t);

	$rule = $kind =~ /V/ ? 'VERB' : 'NOOMEN';

	$muutetyyp = ($type > 38) ? 'muutumatu' : ($type < 27 ? 'knd' : 'prd');

	if ($kind =~ /^(.)/) {
		$k1 = "; kind = $1";
	}

	if ($kind =~ /^.(.)/) {
		$k2 = "; kind2 = $1";
	}


	if ($rule eq 'VERB') {
		$w =~ s/ma$//;
	}

	print "{$rule} $w 0 0 { step = lemma_valik: type = $type; muutus = $muutetyyp; step = vaike_tyvi; erand = 1; lemma = $w$k1$2 }\n";
}
