      subroutine pp( i, j, px )

c      implicit real*8(a-h,o-z)

      include 'dem.h'

      integer tnoj

      real*8   dx,d,     enx,eny,enz, dvx,
     &         dy,d2,    etx,ety,etz, dvy,
     &         dz,limit, ebx,eby,ebz, dvz,
     &         dvn, fpn,dpn, fs, ds,  fpx, bn,
     &         dvt, fpt,dpt, fricf, fpy, bt,
     &         dvb, fpb,dpb, bdt,   fpz, bb

      real*8  vs,dvs,fps,bf,k1,cn,cs

      do 10 tno = 1,tnomax
         if ( ftbl(i,tno). eq .j ) goto 11
 10   continue

      do 20 tno = 1,tnomax
         if ( ftbl(i,tno). eq .0 ) goto 12
 20   continue

      write(2,*) 'contact error'
      write(2,*) 't = ',t
      write(2,*) ( ftbl(i,tno),tno=1,16 )
      ct = 99999
      goto 998

 12   continue

      do 30 tnoj = 1,tnomax
         if ( ftbl(j,tnoj). eq .i ) goto 13
 30   continue

      ftbl(i,tno) = j
      fn(i,tno) = 0.
      ft(i,tno) = 0.
      fb(i,tno) = 0.
      vn(i,tno) = 0.
      vt(i,tno) = 0.
      vb(i,tno) = 0.
      goto 11

 13   continue

      ftbl(i,tno) = j
      fn(i,tno) = fn(j,tnoj)
      ft(i,tno) = ft(j,tnoj)
      fb(i,tno) = fb(j,tnoj)
      vn(i,tno) = vn(j,tnoj)         
      vt(i,tno) = vt(j,tnoj)
      vb(i,tno) = vb(j,tnoj)
      ftbl(j,tnoj) = 0
      fn(j,tnoj) = 0.
      ft(j,tnoj) = 0.
      fb(j,tnoj) = 0.
      vn(j,tnoj) = 0.
      vt(j,tnoj) = 0.
      vb(j,tnoj) = 0.

 11   continue

**************************************************

      if (px .eq. upx-1) then
         if (x(i) .lt. dpartx) then
            xikeep = x(i)
            x(i) = x(i) + dpartx * float(upx-1)
         endif
      endif

      if (px .eq. upx-1) then
         if (x(j) .lt. dpartx) then
            xjkeep = x(j)
            x(j) = x(j) + dpartx * float(upx-1)
         endif
      endif

**************************************************

      dx = ( x(j) - x(i) )
      dy = ( y(j) - y(i) )
      dz = ( z(j) - z(i) )
      d  = sqrt( dx**2 + dy**2 + dz**2 )
      d2 = sqrt( dx**2 + dy**2 )

       if ( d .gt. r(i)+r(j)) then
         ftbl(i,tno) = 0
         fn(i,tno) = 0.
         ft(i,tno) = 0.
         fb(i,tno) = 0.
         vn(i,tno) = 0.
         vt(i,tno) = 0.
         vb(i,tno) = 0.
         goto 994
      endif

**************************************************

      if(nf(i).eq.nf(j)) then
        goto 995
      endif

      if(nf(i).lt.nf(j)) then
        nfmin = nf(i)
        nfmax = nf(j)
C        nf(j) = nfmin
        do 992 ii=1,ip
          if(nf(ii).eq.nfmax) then
            nf(ii)=nfmin
          endif
  992   continue
      else
        nfmin = nf(j)
        nfmax = nf(i)
C        nf(i) = nfmin
        do 993 ii=1,ip
          if(nf(ii).eq.nfmax) then
            nf(ii)=nfmin
          endif
  993   continue
      endif

      massf(nfmin) = massf(nfmin)+massf(nfmax)
      nfl  (nfmin) = nfl(nfmin)+nfl(nfmax)
      nfs  (nfmin) = nfs(nfmin)+nfs(nfmax)
      massf(nfmax) = 0.0
      nfl  (nfmax) = 0
      nfs  (nfmax) = 0

  995 continue

**************************************************

      xc = ( x(i)*r(j) + x(j)*r(i) )/(r(i)+r(j))
      yc = ( y(i)*r(j) + y(j)*r(i) )/(r(i)+r(j))    
      zc = ( z(i)*r(j) + z(j)*r(i) )/(r(i)+r(j))

      enx =  dx/(d+1d-30)
      eny =  dy/(d+1d-30)
      enz =  dz/(d+1d-30)
      etx = -dy/(d2+1d-30)
      ety =  dx/(d2+1d-30)
      etz =  0.0 
      ebx =  dx*dz/(d*d2+1d-30)
      eby =  dy*dz/(d*d2+1d-30)
      ebz = -d2 / (d+1d-30)

      dvx = ( vx(j) - vx(i) - omgy(i) * ( zc - z(i) )
     $                      + omgy(j) * ( zc - z(j) )
     $                      + omgz(i) * ( yc - y(i) )
     $                      - omgz(j) * ( yc - y(j) )) * dt
      dvy = ( vy(j) - vy(i) - omgz(i) * ( xc - x(i) )
     $                      + omgz(j) * ( xc - x(j) )
     $                      + omgx(i) * ( zc - z(i) )
     $                      - omgx(j) * ( zc - z(j) )) * dt
      dvz = ( vz(j) - vz(i) - omgx(i) * ( yc - y(i) )
     $                      + omgx(j) * ( yc - y(j) )
     $                      + omgy(i) * ( xc - x(i) )
     $                      - omgy(j) * ( xc - x(j) )) * dt

      dvn =     dvx * enx +   dvy * eny +   dvz * enz
      dvt =     dvx * etx +   dvy * ety +   dvz * etz
      dvb =     dvx * ebx +   dvy * eby +   dvz * ebz

      vn(i,tno) = vn(i,tno) + dvn
      vt(i,tno) = vt(i,tno) + dvt
      vb(i,tno) = vb(i,tno) + dvb

      k1 = -(e/(1-nu**2))*(r(i)*r(j)/(r(i)+r(j)))**.5
     &                *(abs(vn(i,tno)))**.5
      fpn = k1 * dvn 
      fn(i,tno) = fn(i,tno) + fpn
      if( fn(i,tno) .lt. 0. )then
         ftbl(i,tno) = 0
         fn(i,tno) = 0.
         ft(i,tno) = 0.
         fb(i,tno) = 0.
         vn(i,tno) = 0.
         vt(i,tno) = 0.
         vb(i,tno) = 0.
C         vx(i) = 0.
C         vy(i) = 0.
C         vz(i) = 0.
C         fx(i) = 0.
C         fy(i) = 0.
C         fz(i) = 0.
         goto 999
      else
         fpt = dvt * .2 * k1
         fpb = dvb * .2 * k1
         ft(i,tno) = ft(i,tno) + fpt
         fb(i,tno) = fb(i,tno) + fpb
         fs = sqrt( ft(i,tno)**2 + fb(i,tno)**2 )
         ds = sqrt( vt(i,tno)**2 + vb(i,tno)**2 )

         fricf = mu * abs(fn(i,tno))

         if( abs(fs) .lt. fricf ) then
               bt = -fricf*vt(i,tno)/(ds+1d-30)
               bb = -fricf*vb(i,tno)/(ds+1d-30)
               cn = 2. * sqrt(abs(k1 * (mass(i)+mass(j))/2))
               cs = 2. * sqrt(abs(k1/5. *(mass(i)+mass(j))/2))
         else
            bt = ft(i,tno)*fricf/(fs+1d-30)
            bb = fb(i,tno)*fricf/(fs+1d-30)
            ft(i,tno) = 0.
            fb(i,tno) = 0.
            cn = 2. * sqrt(abs(k1 * (mass(i)+mass(j))/2))
            cs = 0.
            vt(i,tno) = 0.
            vb(i,tno) = 0.
         endif
         dpn = -cn * dvn / dt
         dpt = -cs * dvt / dt
         dpb = -cs * dvb / dt
      endif

      k1max = min( k1max, k1 )

***************** cohesion force *************************

      if( dvn .gt. 0.0 ) then
         if( fn(i,tno) .gt. cf ) then                          !ïtíÖÇ»Çµ
            bn = fn(i,tno) - cf
         else
            bn = 0.
            dpn = 0.
         endif
      else
         bn = fn(i,tno)
      endif

**********************************************************

      goto 996

  994 continue

**************** van der Waals force *****************

C      aa = 1.5e-12                                             ! van der Waals forceÇ»Çµ
C
C      dd = d-r(i)-r(j)
C        if(dd.lt.4.0e-8) then
C          dd=4.0e-8
C        endif
C
C      bn = -1.0*aa*r(i)*r(j)/(6.0*(r(i)+r(j))*dd)
C
      bn = 0.0
      bt = 0.0
      bb = 0.0
      dpn = 0.0
      dpt = 0.0
      dpb = 0.0

*****************************************************

  996 continue

      fpx = (bn+dpn)*enx+(bt+dpt)*etx
     &                         +(bb+dpb)*ebx
      fpy = (bn+dpn)*eny+(bt+dpt)*ety
     &                         +(bb+dpb)*eby
      fpz = (bn+dpn)*enz+(bt+dpt)*etz
     &                         +(bb+dpb)*ebz


      fx(i) = fx(i) - fpx
      fy(i) = fy(i) - fpy
      fz(i) = fz(i) - fpz

      mx(i) = mx(i) + fpy * (zc - z(i)) - fpz * (yc - y(i))
      my(i) = my(i) + fpz * (xc - x(i)) - fpx * (zc - z(i))
      mz(i) = mz(i) + fpx * (yc - y(i)) - fpy * (xc - x(i))

      fx(j) = fx(j) + fpx
      fy(j) = fy(j) + fpy
      fz(j) = fz(j) + fpz

C      if(cycle.ge.94) then
C      if((i.eq.1853).and.(j.eq.3543)) then
C      WRITE(77,*) 'ppÇÃíÜ','FZ(çáåv) =',FZ(3543)
C      WRITE(77,*) 'fpz =',fpz
C      WRITE(77,*) 'bn=',bn,'bt=',bt,'bb=',bb
C      WRITE(77,*) 'dpn=',dpn,'dpt=',dpt,'dpb=',dpb
C      endif
C      endif

      mx(j) = mx(j) - fpy * (zc - z(j)) + fpz * (yc - y(j))
      my(j) = my(j) - fpz * (xc - x(j)) + fpx * (zc - z(j))
      mz(j) = mz(j) - fpx * (yc - y(j)) + fpy * (xc - x(j))

 999  continue
C      if (xikeep .ne. 0.0) x(i) = xikeep
C      if (xjkeep .ne. 0.0) x(j) = xjkeep

      xikeep = 0.0
      xjkeep = 0.0

      return

 998  stop

      end





















