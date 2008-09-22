:start #$#
:class W aeiouõäöüy
:class E aeu
:class I aei
:class U iuü
:class Y aiõ
:class X eouäöü
:class A aeiou
:class O õäöü
:class C bcdfghjklmnpqrsšzžtvwx
:class Q kptfš
:class G gbd
:class K kpt
:class Z gbdlmnrvshjzž
:class H lmnrvshjzž
:class V lmnrvwž
:class M lmnrv
:class L lmnr
:class N lnr
:class R nr
:class J lrvj
:class T gbdkptlmnrvshj
:class B gbdjfšzž
:class D ntd
:class F fš
:class S sh
:class Ü ie
:class Ä ns
:class Õ mjntv
:class Ö dtslkng
:class P gbdkptfh

 mh? $ vallatu 0 0 { true: type = 00; stem = a0 }
0 0 0 0 { type = 00 && stem_transform = postexc: stem_transform = postrule }
0 0 0 0 { type = 00 && stem = a0 && !defined(stem_transform): stem_transform = preexc; stem_grammar = ab; target_stem = b0; seq = 0 }
 
 $ vallatu 0 0 { true: type = 01; stem = a0 }
0 0 0 0 { type = 00 && stem_transform = postexc: stem_transform = prerule }
0 0 0 0 { type = 01 && stem = a0 && !defined(stem_transform): stem_transform = preexc; stem_grammar = 0r; target_stem = a0r; seq = 0 }

$ õpik 0 0 { true: type = 02; stem = a0 }
0 0 0 0 { type = 02 && stem = a0 && !defined(stem_transform): stem_transform = preexc; stem_grammar = ab; target_stem = b0; seq = 0 }
0 0 0 0 { type = 02 && stem = b0 && !defined(stem_transform): stem_transform = preexc; stem_grammar = bc; target_stem = c0; seq = 0 }
0 0 0 0 { type = 02 && stem = b0 && !defined(stem_transform): stem_transform = preexc; stem_grammar = 0r; target_stem = b0r; seq = 0 }
 0 0 0 0 { type = 02 && stem_transform = preexc: stem_transform = prerule }

 $ vaher 0 0 { true: type = 03; stem = an }
0 0 0 0 { type = 03 && stem_transform = postexc: stem_transform = postrule }
0 0 0 0 { type = 03 && stem = an && !defined(stem_transform): stem_transform = preexc; stem_grammar = nt; target_stem = at; seq = 0 }
0 0 0 0 { type = 03 && stem = at && !defined(stem_transform): stem_transform = preexc; stem_grammar = ab; target_stem = bt; seq = 0 }

 0 0 0 0 { stem_transform = postexc: stem_transform = prerule }

0 0 0 0 { target_stem = a0 && stem_transform = postrule: unset stem_transform; unset stem_grammar; unset target_stem; stem = a0 }
0 0 0 0 { target_stem = a0r && stem_transform = postrule: unset stem_transform; unset stem_grammar; unset target_stem; stem = a0r }
0 0 0 0 { target_stem = an && stem_transform = postrule: unset stem_transform; unset stem_grammar; unset target_stem; stem = an }
0 0 0 0 { target_stem = at && stem_transform = postrule: unset stem_transform; unset stem_grammar; unset target_stem; stem = at }
0 0 0 0 { target_stem = b0 && stem_transform = postrule: unset stem_transform; unset stem_grammar; unset target_stem; stem = b0 }
0 0 0 0 { target_stem = b0r && stem_transform = postrule: unset stem_transform; unset stem_grammar; unset target_stem; stem = b0r }
0 0 0 0 { target_stem = bn && stem_transform = postrule: unset stem_transform; unset stem_grammar; unset target_stem; stem = bn }
0 0 0 0 { target_stem = bt && stem_transform = postrule: unset stem_transform; unset stem_grammar; unset target_stem; stem = bt }

:include d/erand_nttn.r
:include d/erand_0gg0.r
:include d/erand_0rr0.r
:include d/erand_0vv0.r
:include d/erand_abba.r
:include d/erand_bccb.r

:include d/reegel_0g.r
:include d/reegel_g0.r
:include d/reegel_0r.r
:include d/reegel_r0.r
:include d/reegel_0v.r
:include d/reegel_v0.r
:include d/reegel_ab.r
:include d/reegel_ba.r
:include d/reegel_bc.r
:include d/reegel_cb.r
:include d/reegel_nt.r
:include d/reegel_tn.r




