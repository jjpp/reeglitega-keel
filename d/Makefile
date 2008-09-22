#rule_or1.rul
#rule_or2.rul

ERANDID=erand_0gg0.r erand_0rr0.r erand_0vv0.r erand_abba.r erand_bccb.r erand_nttn.r
REEGLID=reegel_0g.r reegel_0r.r reegel_0v.r reegel_ab.r reegel_ba.r reegel_bc.r reegel_cb.r reegel_g0.r reegel_nt.r reegel_r0.r reegel_tn.r reegel_v0.r 

all:	 $(REEGLID) $(ERANDID)

ETOOL=Makefile cvt_exc.pl
RTOOL=Makefile cvt_rul.pl
ICONV=iconv -flatin1 -tutf8 | sed -e 's/þ/ž/g' -e 's/ð/š/g' 


reegel_0g.r:	../ekidata/rules_0g.rul $(RTOOL)
	cat ../ekidata/rules_0g.rul | $(ICONV) | ./cvt_rul.pl 0g > reegel_0g.r

reegel_0r.r:	../ekidata/rules_0r.rul $(RTOOL)
	cat ../ekidata/rules_0r.rul | $(ICONV) | ./cvt_rul.pl 0r > reegel_0r.r

reegel_0v.r:	../ekidata/rules_0v.rul $(RTOOL)
	cat ../ekidata/rules_0v.rul | $(ICONV) | ./cvt_rul.pl 0v > reegel_0v.r

reegel_ab.r:	../ekidata/rules_ab.rul $(RTOOL)
	cat ../ekidata/rules_ab.rul | $(ICONV) | ./cvt_rul.pl ab > reegel_ab.r

reegel_ba.r:	../ekidata/rules_ba.rul $(RTOOL)
	cat ../ekidata/rules_ba.rul | $(ICONV) | ./cvt_rul.pl ba > reegel_ba.r

reegel_bc.r:	../ekidata/rules_bc.rul $(RTOOL)
	cat ../ekidata/rules_bc.rul | $(ICONV) | ./cvt_rul.pl bc > reegel_bc.r

reegel_cb.r:	../ekidata/rules_cb.rul $(RTOOL)
	cat ../ekidata/rules_cb.rul | $(ICONV) | ./cvt_rul.pl cb > reegel_cb.r

reegel_g0.r:	../ekidata/rules_g0.rul $(RTOOL)
	cat ../ekidata/rules_g0.rul | $(ICONV) | ./cvt_rul.pl g0 > reegel_g0.r

reegel_nt.r:	../ekidata/rules_nt.rul $(RTOOL)
	cat ../ekidata/rules_nt.rul | $(ICONV) | ./cvt_rul.pl nt > reegel_nt.r

reegel_r0.r:	../ekidata/rules_r0.rul $(RTOOL)
	cat ../ekidata/rules_r0.rul | $(ICONV) | ./cvt_rul.pl r0 > reegel_r0.r

reegel_tn.r:	../ekidata/rules_tn.rul $(RTOOL)
	cat ../ekidata/rules_tn.rul | $(ICONV) | ./cvt_rul.pl tn > reegel_tn.r

reegel_v0.r:	../ekidata/rules_v0.rul $(RTOOL)
	cat ../ekidata/rules_v0.rul | $(ICONV) | ./cvt_rul.pl v0 > reegel_v0.r

erand_0gg0.r:	../ekidata/0g_g0.exc $(ETOOL)
	cat ../ekidata/0g_g0.exc | $(ICONV) | ./cvt_exc.pl > erand_0gg0.r

erand_0rr0.r:	../ekidata/0r_r0.exc $(ETOOL)
	cat ../ekidata/0r_r0.exc | $(ICONV) | ./cvt_exc.pl > erand_0rr0.r

erand_0vv0.r:	../ekidata/0v_v0.exc $(ETOOL)
	cat ../ekidata/0v_v0.exc | $(ICONV) | ./cvt_exc.pl > erand_0vv0.r

erand_abba.r:	../ekidata/ab_ba.exc $(ETOOL)
	cat ../ekidata/ab_ba.exc | $(ICONV) | ./cvt_exc.pl > erand_abba.r

erand_bccb.r:	../ekidata/bc_cb.exc $(ETOOL)
	cat ../ekidata/bc_cb.exc | $(ICONV) | ./cvt_exc.pl > erand_bccb.r

erand_nttn.r:	../ekidata/nt_tn.exc $(ETOOL)
	cat ../ekidata/nt_tn.exc | $(ICONV) | ./cvt_exc.pl > erand_nttn.r


