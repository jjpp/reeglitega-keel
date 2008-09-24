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

$ mina 0 0 { true: type = 00; stem = a0 }
$ vallatu 0 0 { true: type = 01; stem = a0 }
$ õpik 0 0 { true: type = 02; stem = a0 }
$ vaher 0 0 { true: type = 03; stem = an }
$ ase 0 0 { true: type = 04; stem = a0 }
$ liige 0 0 { true: type = 05; stem = an }
$ mõte 0 0 { true: type = 06; stem = an }
$ perse 0 0 { true: type = 06; stem = an }
$ hammas 0 0 { true: type = 07; stem = an }
$ taevas 0 0 { true: type = 07; stem = an }
$ tütar 0 0 { true: type = 08; stem = an }
$ katus 0 0 { true: type = 09; stem = a0 }
$ soolane 0 0 { true: type = 10; stem = a0 }
$ harjutus 0 0 { true: type = 11; stem = a0 }
$ oluline 0 0 { true: type = 12; stem = a0 }
$ s'uur 0 0 { true: type = 13; stem = at }
$ 'uus 0 0 { true: type = 14; stem = at }
$ käsi 0 0 { true: type = 15; stem = at }
$ kõne 0 0 { true: type = 16; stem = a0 }
$ saba 0 0 { true: type = 17; stem = a0 }
$ sõda 0 0 { true: type = 18; stem = at }
$ seminar 0 0 { true: type = 19; stem = a0 }
$ nimi 0 0 { true: type = 20; stem = a0 }
$ lumi 0 0 { true: type = 20; stem = a0 }
$ jõgi 0 0 { true: type = 21; stem = at }
$ s'epp 0 0 { true: type = 22; stem = at }
$ m'ets 0 0 { true: type = 22; stem = at }
$ h'ein 0 0 { true: type = 23; stem = at }
$ padi 0 0 { true: type = 24; stem = an }
$ õnnel'ik 0 0 { true: type = 25; stem = at }
$ id'ee 0 0 { true: type = 26; stem = a0 }
$ s'uu 0 0 { true: type = 26; stem = a0 }



:include d/stemguide.r

:include d/erand_nttn.r
:include d/erand_0gg0.r
:include d/erand_0rr0.r
:include d/erand_0vv0.r
:include d/erand_abba.r
:include d/erand_bccb.r
:include d/erand_ort1.r
:include d/erand_ort2.r

0 0 0 0 { stem_transform = preexc: stem_transform = prerule }

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
