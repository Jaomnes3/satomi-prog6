      parameter (l1=10000,l2=16,l3=6)
C
      common /d1/
     $    ts, r,   e,  ew,    ftbl,   fn, fwn, mass, massf,
     $    tp, rhop, rhof, g, gw, tno, tnomax, ft, fwt, inat,
     $    te, mu,  nu, muw,           fb,  fwb, cgn,
     $    dt, t, cf, cfw,   k1max,    nfl, nfs, cgs, cycle,
C
C
     $    iwall, xw, ewnx,ewtx,ewbx, vwx,
     $           yw, ewny,ewty,ewby, vwy,
     $           zw, ewnz,ewtz,ewbz, vwz,
C
     $    ip,   x, vx, vbx, omgx, fx,mx,
     $    nf,   y, vy, vby, omgy, fy,my,
     $    nfno, z, vz, vbz, omgz, fz,mz,
C
     $    upx, dpartx, parttl, kn, vn, vwn, knmx,
     $    upy, dparty, part,   ks, vt, vwt, ksmx,
     $    upz, dpartz, boxmax,     vb, vwb,
     $    ang
C
      common /sdv/
     $    step,   sumfn, sumvx,vcnt, cx, vol,   
     $    outcnt, sumft, sumvy,      cy, rang,  
     $            sumfb, sumvz,      cz, v,ct
C
      integer 
     $    ip,     px, upx, tno,    parttl(31,12,30),
     $    iwall,  py, upy, tnomax, part(31,12,30,16),
     $    nf(l1), pz, upz, boxmax, outcnt, ftbl(l1,l2),
     $    nfl(l1), nfs(l1)
C
      integer  ct, cycle
C
      real*8
     $    ts, r(l1), e, ew, fn(l1,l2), fwn(l1,l3), mass(l1), knmx,
     $    tp,        g, gw, ft(l1,l2), fwt(l1,l3), inat(l1), ksmx,
     $    te, mu, nu,  muw, fb(l1,l2), fwb(l1,l3), cgn(l1),
     $    dt, t,  cf,  cfw,  k1max,                cgs(l1),
     $    massf(l1),  rhop,   rhof,
C
     $    xw(l3), ewnx(l3), ewtx(l3), ewbx(l3), vwx(l3),
     $    yw(l3), ewny(l3), ewty(l3), ewby(l3), vwy(l3),
     $    zw(l3), ewnz(l3), ewtz(l3), ewbz(l3), vwz(l3),
C
     $    x(l1), vx(l1), vbx(l1), omgx(l1), fx(l1), mx(l1),
     $    y(l1), vy(l1), vby(l1), omgy(l1), fy(l1), my(l1),
     $    z(l1), vz(l1), vbz(l1), omgz(l1), fz(l1), mz(l1),
C
     $    dpartx, kn, vn(l1,l2), vwn(l1,l3),
     $    dparty, ks, vt(l1,l2), vwt(l1,l3),
     $    dpartz,     vb(l1,l2), vwb(l1,l3)
C
      real*8
     $    xo, yo, ang





















