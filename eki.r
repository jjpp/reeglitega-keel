:start #{START}#
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

{START} {VERB} 0 0
{START} {NOOMEN} 0 0

 {NOOMEN} mina 0 0 { true: type = 00; stem = a0 }
 {NOOMEN} vallatu 0 0 { true: type = 01; stem = a0 }
{NOOMEN} õpik 0 0 { true: type = 02; stem = a0 }
 {NOOMEN} 'andur 0 0 { true: type = 02; stem = a0 }
 {NOOMEN} pagas 0 0 { true: type = 02; stem = a0 }
 {NOOMEN} vaher 0 0 { true: type = 03; stem = an }
 {NOOMEN} ase 0 0 { true: type = 04; stem = a0 }
 {NOOMEN} süda 0 0 { true: type = 04; stem = a0 }
 {NOOMEN} liige 0 0 { true: type = 05; stem = an }
 {NOOMEN} mõte 0 0 { true: type = 06; stem = an }
 {NOOMEN} perse 0 0 { true: type = 06; stem = an }
 {NOOMEN} hammas 0 0 { true: type = 07; stem = an }
 {NOOMEN} taevas 0 0 { true: type = 07; stem = an }
 {NOOMEN} tütar 0 0 { true: type = 08; stem = an }
 {NOOMEN} katus 0 0 { true: type = 09; stem = a0 }
 {NOOMEN} soolane 0 0 { true: type = 10; stem = a0 }
 {NOOMEN} harjutus 0 0 { true: type = 11; stem = a0 }
 {NOOMEN} oluline 0 0 { true: type = 12; stem = a0 }
 {NOOMEN} s'uur 0 0 { true: type = 13; stem = at }
 {NOOMEN} 'uus 0 0 { true: type = 14; stem = at }
 {NOOMEN} käsi 0 0 { true: type = 15; stem = at }
 {NOOMEN} kõne 0 0 { true: type = 16; stem = a0 }
 {NOOMEN} saba 0 0 { true: type = 17; stem = a0 }
 {NOOMEN} sõda 0 0 { true: type = 18; stem = at }
 {NOOMEN} seminar 0 0 { true: type = 19; stem = a0 }
 {NOOMEN} nimi 0 0 { true: type = 20; stem = a0 }
 {NOOMEN} lumi 0 0 { true: type = 20; stem = a0 }
 {NOOMEN} jõgi 0 0 { true: type = 21; stem = at }
 {NOOMEN} s'epp 0 0 { true: type = 22; stem = at }
 {NOOMEN} m'ets 0 0 { true: type = 22; stem = at }
 {NOOMEN} h'ein 0 0 { true: type = 23; stem = at }
 {NOOMEN} padi 0 0 { true: type = 24; stem = an }
 {NOOMEN} õnnel'ik 0 0 { true: type = 25; stem = at }
 {NOOMEN} id'ee 0 0 { true: type = 26; stem = a0 }
 {NOOMEN} s'uu 0 0 { true: type = 26; stem = a0 }
 
 {VERB} ela 0 0 { true: type = 27; stem = a0 }
 {VERB} l'eppi 0 0 { true: type = 28; stem = at }
 {VERB} h'üppa 0 0 { true: type = 29; stem = at }
 {VERB} r'iidle 0 0 { true: type = 30; stem = at }
 {VERB} rabele 0 0 { true: type = 31; stem = a0 }
 {VERB} s'eis 0 0 { true: type = 32; stem = at }
 {VERB} n'aer 0 0 { true: type = 33; stem = at }
 {VERB} s'ööt 0 0 { true: type = 34; stem = at }
 {VERB} n'ut 0 0 { true: type = 35; stem = at }
 {VERB} tule 0 0 { true: type = 36; stem = an }
 {VERB} v'õi 0 0 { true: type = 37; stem = at }
 {VERB} s'öö 0 0 { true: type = 38; stem = at }


:include d/stemguide.r

:include d/erand_nttn.r
:include d/erand_0gg0.r
:include d/erand_0rr0.r
:include d/erand_0vv0.r
:include d/erand_abba.r
:include d/erand_bccb.r
:include d/erand_ort1.r
:include d/erand_ort2.r

0 0 0 0 { stem_transform = preexc: stem_transform = prerule; stop = 1 }

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
:include d/reegel_ort1.r
:include d/reegel_ort2.r

:include degrees.r

:include d/vormid.r
