      subroutine pre

c      implicit real*8(a-h,o-z)

      include 'dem.h'

      integer wp
      real*8  ab,bc,cd,de,ef,fg,pai
     &       ,xmax,ymax,zmax,vxmax,vymax,vzmax,omgxmax,omgymax,omgzmax

C      npoint=0

      xmax=0.0
      ymax=0.0
      zmax=0.0
      vxmax=0.0
      vymax=0.0
      vzmax=0.0
      omgxmax=0.0
      omgymax=0.0
      omgzmax=0.0

      open( unit = 1, err = 100, status='old', file = 'infile.dat' )

      read(1,*) ts, tp, te, dt
      read(1,*) rhop, rhof, mu
      read(1,*) e,  ew, nu
      read(1,*) ang, cf, cfw, muw

C      write(*,*) "T etc. = OK"

      read(1,*) iwall
      do 10 i = 1, iwall
         read(1,*)      j
         read(1,*)   xw(i),   yw(i),   zw(i)
         read(1,*) ewnx(i), ewny(i), ewnz(i)
         read(1,*) ewtx(i), ewty(i), ewtz(i)
         read(1,*) ewbx(i), ewby(i), ewbz(i)
 10   continue

C      write(*,*) "Wall = OK"

      read(1,*)    upx,    upy,    upz
      read(1,*) dpartx, dparty, dpartz
      read(1,*) ip

C      write(*,*) "Partition =OK"

      NLP=0

      do 20 i = 1, ip
        read(1,*)      j , nf(i), nfl(i), nfs(i), r(i), massf(i)
        read(1,*)    x(i),    y(i),    z(i)  
        read(1,*)   vx(i),   vy(i),   vz(i) 
        read(1,*)  vbx(i),  vby(i),  vbz(i)
        read(1,*) omgx(i), omgy(i), omgz(i)

          xmax=max(x(i),xmax)
          ymax=max(y(i),ymax)
          zmax=max(z(i),zmax)
          vxmax=max(abs(vx(i)),vxmax)
          vymax=max(abs(vy(i)),vymax)
          vzmax=max(abs(vz(i)),vzmax)
          omgxmax=max(abs(omgx(i)),omgxmax)
          omgymax=max(abs(omgy(i)),omgymax)
          omgzmax=max(abs(omgz(i)),omgzmax)
          IF(R(I).EQ.0.001) THEN
          NLP=NLP+1
          ENDIF
 20   continue

      write(*,*) "Particle = OK"
      write(*,*) 'xmax=',xmax
      write(*,*) 'ymax=',ymax
      write(*,*) 'zmax=',zmax
      write(*,*) 'vxmax=',vxmax
      write(*,*) 'vymax=',vymax
      write(*,*) 'vzmax=',vzmax
      write(*,*) 'omgxmax=',omgxmax
      write(*,*) 'omgymax=',omgymax
      write(*,*) 'omgzmax=',omgzmax
      WRITE(*,*) 'NLP = ', NLP
      write(*,*) 'ip=',ip

 200  read(1,*,end=300) wp,i,j,l,ab,bc,cd
     $     ,de,ef,fg

C      npoint=npoint+1
C      write(*,*) npoint

      if(wp.eq.1)then
         ftbl(i,j) = l
         fn(i,j) = ab
         ft(i,j) = bc
         fb(i,j) = cd
         vn(i,j) = de
         vt(i,j) = ef
         vb(i,j) = fg
      else
         fwn(i,j) = ab
         fwt(i,j) = bc
         fwb(i,j) = cd
         vwn(i,j) = de
         vwt(i,j) = ef
         vwb(i,j) = fg
      endif
      goto 200

C      write(*,*) "Contact = OK"

 300  close(1)

      pai = 4.0*atan(1.0)

      DO 30 i = 1 , ip
        mass(i) = (4./3.)*pai*(r(i)**3)*rhop
        inat(i) = 0.4*mass(i)*(r(i)**2)
 30   CONTINUE

      tnomax = 16

      return

 100  write(9,*) 'open error infile.dat (unit=1)'

      stop

      end








