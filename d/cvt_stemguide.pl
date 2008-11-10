#!/usr/bin/perl -w

use strict;

my %stems = ();

while (<>) {
	chomp(); chomp();
	next if /^\s*$/;

	my @w = split(' ');
	my $num = substr($w[0], $[, $[ + 2);
	my $allexc = $w[0] =~ /#$/;
	shift @w;

	print " Tüveteisenduse reeglid tüübi $num jaoks:\n";

	if ($allexc) {
		print "0 0 0 0 { type = $num && stem_transform = postexc: stem_transform = postrule; stop = 1 }\n";
	}

	my $p = undef;

	my ($exc, $ort1, $ort2, $unused);

	while (@w) {
		my $w = shift @w;

		if ($w =~ /^;/) {
			$p = undef;
			next;
		}

		if ($w =~ /^>/) {
			$exc = !$allexc && ($w =~ /#$/);
			$ort1 = $w =~ /\+$/;
			$ort2 = $w =~ /\*$/;
			next;
		}

		if ($w =~ /\(/) {
			$unused = 1;
			$w =~ tr/\(\)//d;
		}

		if (defined($p)) {
			my $g = get_grammar($p, $w);
			print "0 0 0 0 { type = $num && stem = $p && (!defined(stem_transform)) && (!defined(form)) && (!defined(target_form)): "
				. "stem_transform = preexc; stem_grammar = $g; target_stem = $w }\n";
			if ($exc) {
				print "0 0 0 0 { type = $num && stem_grammar = $g && stem_transform = postexc: stem_transform = postrule; stop = 1 }\n";
			}

			if ($ort1 || $ort2) {
				my $ort = ($ort1 ? "ort1" : "ort2");
				print "0 0 0 0 { type = $num && stem_grammar = $g && target_stem = $w && stem_transform = postrule: "
					. "stem_grammar = $ort; stem_transform = preexc; stop = 1 }\n";
			}
		}

		$stems{$w} = 1;
		
		$p = $w;
	}
	print "\n\n";
}


for (keys %stems) {
	print "0 0 0 0 { target_stem = $_ && stem_transform = postrule: unset stem_transform; unset stem_grammar; unset target_stem; stem = $_ }\n";
}

sub get_grammar {
	my ($a, $b) = @_;
	
	$a .= '0';
	$b .= '0';

	my $o;

	while (length($a.$b) > 0) {
		if (substr($a, $[, 1) eq substr($b, $[, 1)) {
			$a = substr($a, $[ + 1);
			$b = substr($b, $[ + 1);
			next;
		} else { 
			return substr($a, $[, 1) . substr($b, $[, 1);
		}
	}

	return '00';
}
