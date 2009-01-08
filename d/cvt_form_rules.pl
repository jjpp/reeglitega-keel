#!/usr/bin/perl -w

use strict;

my %codes;
my $mode = '';
my $type = 'XX';
my $clean_stem = 'step = vormid && (!defined(form))';
my @stoppers = ();
my @exception_stoppers = ();
my %required_forms = ();

my %modetab = (
	'noomen' => 'knd',
	'verb' => 'prd',
	'varia' => 'prd',
);

print "0 0 0 0 { step = vormid && !defined(substep): substep = vormierandid }\n";

load_form_codes(shift @ARGV);
process_exceptions(shift @ARGV);
output_exception_stoppers();
print "0 0 0 0 { step = vormid && substep = vormierandid: substep = vormireeglid }\n";
process_rules(shift @ARGV);



for (sort values %required_forms) {
	print $_;
}

for (@stoppers) {
	print $_;
}

exit 0;

sub output_exception_stoppers {
	my %x = map { $_ => 1 } @exception_stoppers;
	for (sort keys %x) {
		print $_;
	}
}

sub process_rules {
	open (R, "<$_[0]") || die "reeglifaili $_[0] ei saa avada?";
	while (<R>) {
		chomp();
		s/$//;

		next if /^\s*$/;

		if (/^tyyp=(.*)$/) {
			$type = sprintf("%02d", $1);
			next;
		}

		my ($form, $rest) = split(':', $_, 2);
		my @rules = split(/[|&]/, $rest);

		gen_rules($form, $rest);
	}
	close R;
}

sub process_exceptions {
	open (E, "<$_[0]") || die "erandifaili $_[0] ei saa avada?";
	while (<E>) {
		chomp;
		s/$//;
		next if /^\s*$/;
		
		my ($tk, $lemma, $form, $formcode, $homocode, $homoform, $exctype) = split(',');
		my ($type, $kinds) = split('_', $tk);

		my $pref = '';
		my $suff = '';

		if ($form =~ /^(.*)\[(.*)\]$/) {
			$pref = $1;
			$suff = $2;
			$form = $1 . $2;
		} else {
			
			$form = "X";
		}

		my $stem = $lemma;
		my $homoclause = '';
		if ($homocode ne '-') {
			$homoclause = '&& stem = ' . $homocode
		}

		my $cond = "$clean_stem && substep = vormierandid && lemma = $lemma && target_form = $formcode $homoclause";

		print "$stem $form 0 # { $cond: unset target_form; form = $formcode; step = para_deriv; unset substep; erand = 1 }\n" unless ($form eq 'X');
		push @exception_stoppers, "0 0 0 0 { $cond: stop = 1 }\n" if ($exctype eq '*');
	}
	close(E);
}


sub gen_rules {
	my ($form, $rest, $negative) = @_;

	
	return if ($rest eq 'X'); # nothing to see here, move on.

	my $rule;
	my $skipnext = 0;

	if ($rest =~ /^([^|&]+)([&|])(.*)$/) {
		$rule = $1;
		$rest = $3;
		$skipnext = $2 eq '|';
	} else {
		$rule = $rest;
		$rest = '';
	}

	if ($rule !~ /^([^\[]+)\[(.*)\]$/) {
		print STDERR "Invalid rule? '$rule'\n";
		return;
	}

	my $stem = $1;
	my $suff = $2;

	my $clause = "stem = $stem && type = $type";

#	$required_forms{$type, $form, $stem}
#		= "0 0 0 0 { $clause && $clean_stem && !defined(target_form): target_form = $form }\n";


	if ($suff eq '') {
		$suff = '0';
	}

	print "0 $suff 0 # { substep = vormireeglid && $clause && $clean_stem && target_form = $form"
		. ($negative ? " && alt_of = $negative " : " && !defined(alt_of)")
		. ": "
		. ($negative ? "unset alt_of; " : "")
		. "unset target_form; "
		. "form = $form; step = para_deriv; unset substep }\n"; 

	if (!$negative && $skipnext) {
		$skipnext = $stem . $form; 
		push @stoppers, "0 0 0 0 { substep = vormireeglid && type = $type && $clean_stem && target_form = $form: alt_of = $skipnext; stop = 1 }\n";
	}

	if ($rest) {
		gen_rules($form, $rest, $skipnext);
	}
}

sub load_form_codes {
	open (F, "<$_[0]") || die ("vormikoodid pole loetavad ($_[0])");
	while (<F>) {
		chomp; chomp;
		
		next unless /^[0..9A-Z@-]/;

		if (/^@(.*)$/) {
			$mode = $modetab{$1} ? '&& muutus = ' . $modetab{$1} : '';
			next;
		}

		my ($abbr, $clearcode, $ekicode, $fscode, $name) = split(',', $_, 5);

		$codes{$ekicode} = $abbr;
		$codes{$abbr} = $ekicode;
		$required_forms{$ekicode}
			= "0 0 0 0 { step = vormid $mode && !defined(target_form): target_form = $ekicode }\n";

#		print "$ekicode -> $fscode, $abbr, $mode\n";

	}
	close (F);
}

__END__
; Piirangud failiformaadile:

; 1. Iga programmi juhtinfot sisaldav rida algab sümbolitega 0...9, A...Z, -, @, 
;  ülejäänud ridu ignoreeritakse.
; 2. Noomeni ja verbi vormikoodid on esitatud eraldi gruppidena, mille algusi
;  tähistavad vastavalt "@noomen" ja "@verb".
; 3. Igas vormikoodide reas on 3 koodivarianti, mida eraldavad komad ja/või
;  tühikud; vormikoodi pikkus võib olla kuni 29 märki.
; 4. Sisekood, mida kasutatakse reegli- ja erandifailides, asub kolmandas veerus
;  ja seda ei tohi muuta.
;
; Lisatud Filosofti koodid (4. veerg) ja vorminimi tekstina (5. veerg)
;
; Lisatud lühitüved liitsõna jaoks (29.01.02)
;
; Vormikoodide kujud:
; vorminimi lühendina, klaarkood, sisekood, FS-kood, vorminimi tekstina

ID,_,--------,, muutumatu sõna (indekl)
??,?,------xx,?, määramatu vorm

; muutumatute eritüved LS-le
Ilyhi,RIC,R------C,, muutumatu sõna lühitüvi
Ivahe,HIC,H------C,, muutumatu sõna vahehäälikuga vorm
Qunik,RQC,Q------C,, seotud / unikaalne tüvi
Fpref,RFC,F------C,, prefiks

@noomen

; singular e ainsus
SgN,0n,------0N,sg n, ainsuse nimetav
SgG,0g,------0G,sg g, ainsuse omastav
SgP,0p,------0P,sg p, ainsuse osastav
SgAdt,0d,------0D,adt, ainsuse suunduv e lühike sisseütlev
SgIll,SSE,------01,sg ill, ainsuse sisseütlev
SgIn,S,------02,sg in, ainsuse seesütlev
SgEl,ST,------03,sg el, ainsuse seestütlev
SgAll,LE,------04,sg all, ainsuse alaleütlev
SgAd,L,------05,sg ad, ainsuse alalütlev
SgAbl,LT,------06,sg abl, ainsuse alaltütlev
SgTr,KS,------0T,sg tr, ainsuse saav
SgTer,NI,------0R,sg ter, ainsuse rajav
SgEs,NA,------0E,sg es, ainsuse olev
SgAb,TA,------0A,sg ab, ainsuse ilmaütlev
SgKom,GA,------0K,sg kom, ainsuse kaasaütlev

; pluural e mitmus
PlN,D,------1N,pl n, mitmuse nimetav
PlG,DE,------1G,pl g, mitmuse omastav
PlP,SID,------1P,pl p, mitmuse osastav
PlIll,DESSE,------11,pl, illmitmuse sisseütlev
PlIn,DES,------12,pl in, mitmuse seesütlev
PlEl,DEST,------13,pl el, mitmuse seestütlev
PlAll,DELE,------14,pl all, mitmuse alaleütlev
PlAd,DEL,------15,pl ad, mitmuse alalütlev
PlAbl,DELT,------16,pl abl, mitmuse alaltütlev
PlTr,DEKS,------1T,pl tr, mitmuse saav
PlTer,DENI,------1R,pl ter, mitmuse rajav
PlEs,DENA,------1E,pl es, mitmuse olev
PlAb,DETA,------1A,pl ab, mitmuse ilmaütlev
PlKom,DEGA,------1K,pl kom, mitmuse kaasaütlev
Rpl,_,------1-,, vokaalmitmuse tüvi
;
; noomeni eritüved LS-le
Nlyhi,RNC,------RC,, noomeni lühitüvi
Nvahe,HNC,------HC,, noomeni vahehäälikuga vorm
;
@verb
; infiniitsed e käändelised vormid:

; infinitiivid ja gerundium
Sup,MA,30------,ma, ma-infinitiiv e ma-tegevusnimi
SupAb,MATA,30-----A,mata, mata-vorm
SupIn,MAS,30-----2,mas, mas-vorm
SupEl,MAST,30-----3,mast, mast-vorm
SupTr,MAKS,30-----T,maks, maks-vorm
SupIps,TAMA,31------,tama, ma-tegevusnime umbisikuline tegumood
Inf,DA,1-------,da, da-infinitiiv e da-tegevusnimi
Ger,DES,2-------,des, des-vorm
; partitsiibid
PtsPrPs,V,400---0N,v, oleviku isikuline kesksõna e v-kesksõna
PtsPrIps,TAV,410---0N,tav, oleviku umbisikuline kesksõna e tav-kesksõna
PtsPtPs,NUD,401--0--,nud, mineviku isikuline kesksõna e nud-kesksõna
PtsPtIps,TUD,411--0--,tud, mineviku umbisikuline kesksõna e tud-kesksõna

; finiitsed e pöördelised vormid:
; indikatiiv, preesens
IndPrSg1,N,-00011--,n, kindla kõneviisi oleviku ainsuse 1.pööre
IndPrSg2,D,-00021--,d, kindla kõneviisi oleviku ainsuse 2.p.
IndPrSg3,B,-00031--,b, kindla kõneviisi oleviku ainsuse 3.p.
IndPrPl1,ME,-00041--,me, kindla kõneviisi oleviku mitmuse 1.p.
IndPrPl2,TE,-00051--,te, kindla kõneviisi oleviku mitmuse 2.p.
IndPrPl3,VAD,-00061--,vad, kindla kõneviisi oleviku mitmuse 3.p.
IndPrPs_,00,-00000--,o, kindla kõneviisi oleviku isikuline tegumood (eitusega)
IndPrIps,TAKSE,-100-1--,takse, kindla kõneviisi oleviku umbisikuline tegumood
IndPrIps_,TA,-100-0--,ta, kindla kõneviisi oleviku umbisikuline tegumood (eitusega)
; indikatiiv, imperfekt
IndIpfSg1,SIN,-02011--,sin, kindla kõneviisi lihtmineviku ainsuse 1.p.
IndIpfSg2,SID2,-02021--,sid, kindla kõneviisi lihtmineviku ainsuse 2.p.
IndIpfSg3,S,-02031--,s, kindla kõneviisi lihtmineviku ainsuse 3.p.
IndIpfPl1,SIME,-02041--,sime, kindla kõneviisi lihtmineviku mitmuse 1.p.
IndIpfPl2,SITE,-02051--,site, kindla kõneviisi lihtmineviku mitmuse 2.p.
IndIpfPl3,SID6,-02061--,sid, kindla kõneviisi lihtmineviku mitmuse 3.p.
IndIpfIps,TI,-120-1--,ti, kindla kõneviisi lihtmineviku umbisikuline tegumood

; konditsionaal, preesens
KndPrSg1,KSIN,-00211--,ksin, tingiva kõneviisi oleviku ainsuse 1.p.
KndPrSg2,KSID2,-00221--,ksid, tingiva kõneviisi oleviku ainsuse 2.p.
KndPrPs,KS,-00200--,ks, tingiva kõneviisi oleviku isikuline tegumood
KndPrPl1,KSIME,-00241--,ksime, tingiva kõneviisi oleviku mitmuse 1.p.
KndPrPl2,KSITE,-00251--,ksite, tingiva kõneviisi oleviku mitmuse 2.p.
KndPrPl3,KSID6,-00261--,ksid, tingiva kõneviisi oleviku mitmuse 3.p.
KndPrIps,TAKS,-102-0--,taks, tingiva kõneviisi oleviku umbisikuline tegumood
; konditsionaal, preteeritum
KndPtSg1,NUKSIN,-01211--,nuksin, tingiva kõneviisi mineviku ainsuse 1.p.
KndPtSg2,NUKSID2,-01221--,nuksid, tingiva kõneviisi mineviku ainsuse 2.p.
KndPtPs,NUKS,-01200--,nuks, tingiva kõneviisi mineviku isikuline tegumood
KndPtPl1,NUKSIME,-01241--,nuksime, tingiva kõneviisi mineviku mitmuse 1.p.
KndPtPl2,NUKSITE,-01251--,nuksite, tingiva kõneviisi mineviku mitmuse 2.p.
KndPtPl3,NUKSID6,-01261--,nuksid, tingiva kõneviisi mineviku mitmuse 3.p.
KndPtIps,TUKS,-112-0--,tuks, tingiva kõneviisi mineviku umbisikuline tegumood

; kvotatiiv: preesens, preteeritum
KvtPrPs,VAT,-00100--,vat, kaudse kõneviisi oleviku isikuline tegumood
KvtPrIps,TAVAT,-101-0--,tavat, kaudse kõneviisi oleviku umbisikuline tegumood
KvtPtPs,NUVAT,-01100--,nuvat, kaudse kõneviisi mineviku isikuline tegumood
KvtPtIps,TUVAT,-111-0--,tuvat, kaudse kõneviisi mineviku umbisikuline tegumood

; imperatiiv: preesens
ImpPrSg2,02,-00320--,o, käskiva kõneviisi oleviku ainsuse 2.p.
ImpPrPl1,GEM,-00340--,gem, käskiva kõneviisi oleviku mitmuse 1.p.
ImpPrPl2,GE,-00350--,ge, käskiva kõneviisi oleviku mitmuse 2.p.
ImpPrPs,GU,-00300--,gu, käskiva kõneviisi oleviku isikuline tegumood
ImpPrIps,TAGU,-103-0--,tagu, käskiva kõneviisi oleviku umbisikuline tegumood

; verbi eritüved LS-le
Vlyhi,RVC,RC------,, verbi lühitüvi 
Vvahe,HVC,HC------,, verbi vahehäälikuga vorm

@varia
; muud vormikoodid (eitussõnad):
Neg,EI,-----2--,neg, eitus
ImpPrSg2N,ÄRA,-00322--,neg o, käskiva kõneviisi oleviku ainsuse 2.p. eitus
ImpPrPl1N,ÄRGEM,-00342--,neg gem, käskiva kõneviisi oleviku mitmuse 1.p. eitus
ImpPrPl2N,ÄRGE,-00352--,neg ge, käskiva kõneviisi oleviku mitmuse 2.p. eitus
ImpPrN,ÄRGU,--0302--,neg gu, käskiva kõneviisi oleviku isikulise tegumoe eitus
;
IndPrPsN,p00,-00002--,neg o, pole
IndPrIpsN,pTA,-100-2--,neg ta, polda
IndIpfPsN,pNUD,-02002--,neg nud, polnud
IndIpfIpsN,pTUD,-120-2--,neg tud, poldud
KvtPrPsN,pVAT,-00102--,neg vat, polevat
KvtPtPsN,pNUVAT,-01102--,neg nuvat, polnuvat
KndPrPsN,pKS,-00202--,neg ks, poleks
KndPtPsN,pNUKS,-01202--,neg nuks, polnuks

