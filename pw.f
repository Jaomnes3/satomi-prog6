      subroutine pw( i, j )

c      implicit real*8(a-h,o-z)

      include 'dem.h'

      real*8   dx,dy,dz,d,ax,bx,
     &         dvn, fpn, dpn, fs, ds, fpx,dpx, bn,
     &         dvt, fpt, dpt, fricf,  fpy,dpy, bt,
     &         dvb, fpb, dpb, bdt,    fpz,dpz, bb

      real*8   vs,dvs,fps,cn,cs,k1

      dx = xw(j) - x(i)
      dy = yw(j) - y(i)
      dz = zw(j) - z(i)

      d = abs( dx * ewnx(j) + dy * ewny(j) + dz * ewnz(j) )

      if ( r(i) .le. d ) then
         fwn(i,j) = 0.
         fwt(i,j) = 0.
         fwb(i,j) = 0.
         vwn(i,j) = 0.
         vwt(i,j) = 0.
         vwb(i,j) = 0.
         goto 999
      endif

      xc = x(i) + d * ewnx(j)
      yc = y(i) + d * ewny(j)
      zc = z(i) + d * ewnz(j)

      dwx = (vwx(j)-vx(i)-omgy(i)*(zc-z(i))+omgz(i)*(yc-y(i)))*dt
      dwy = (vwy(j)-vy(i)-omgz(i)*(xc-x(i))+omgx(i)*(zc-z(i)))*dt
      dwz = (vwz(j)-vz(i)-omgx(i)*(yc-y(i))+omgy(i)*(xc-x(i)))*dt

      dvn =     dwx * ewnx(j) +   dwy * ewny(j) +   dwz * ewnz(j)
      dvt =     dwx * ewtx(j) +   dwy * ewty(j) +   dwz * ewtz(j)
      dvb =     dwx * ewbx(J) +   dwy * ewby(j) +   dwz * ewbz(j)

      vwn(i,j) = vwn(i,j) + dvn
      vwt(i,j) = vwt(i,j) + dvt
      vwb(i,j) = vwb(i,j) + dvb

      k1=-2.*(r(i)**.5)/((1.-nu**2.)*(e+ew)/(e*ew))*
     &                            (abs(vwn(i,j)))**.5 
      fpn = dvn * k1   
      fwn(i,j) = fwn(i,j) + fpn
      if( fwn(i,j) .lt. 0. )then
         fwn(i,j) = 0.
         fwt(i,j) = 0.
         fwb(i,j) = 0.
         vwn(i,j) = 0.
         vwt(i,j) = 0.
         vwb(i,j) = 0.
         vz(i) = 0.
         fz(i) = 0.
         goto 999
      else
         fpt = dvt * k1/5.
         fpb = dvb * k1/5.
         fwt(i,j) = fwt(i,j) + fpt
         fwb(i,j) = fwb(i,j) + fpb
         fs  = sqrt(fwt(i,j)**2 + fwb(i,j)**2)
         ds  = sqrt(vwt(i,j)**2 + vwb(i,j)**2)
         fricf = muw * fwn(i,j) 
         if( fs .lt. fricf ) then
               bt = -fricf*vwt(i,j)/(ds+1d-30)
               bb = -fricf*vwb(i,j)/(ds+1d-30)
               cn = 2. * sqrt( abs(k1 *mass(i)))
               cs = 2. * sqrt( abs(k1/5. *mass(i)))
         else
            bt = fwt(i,j)*fricf/(fs+1d-30)-vwt(i,j)*cfw/(ds+1d-30)
            bb = fwb(i,j)*fricf/(fs+1d-30)-vwb(i,j)*cfw/(ds+1d-30)
            fwt(i,j) = 0.
            fwb(i,j) =0.
            cn = 2. * sqrt( abs(k1 *mass(i)))
            cs = 0.
            vwt(i,j) = 0.
            vwb(i,j) = 0.
         endif
         dpn = -cn * dvn / dt
         dpt = -cs * dvt / dt
         dpb = -cs * dvb / dt
      endif

**************** cohesion force **********************
C      if( dvn .gt. 0.0 ) then
C         if( fwn(i,j) .gt. cf ) then
C            bn = fwn(i,j) - cf
C         else
C            bn = 0.0
C            dpn = 0.0
C         endif
C      else
         bn = fwn(i,j)
C      endif
*****************************************************

      fpx = (bn+dpn)*ewnx(j)
     $     +(bt+dpt)*ewtx(j)
     $     +(bb+dpb)*ewbx(j)
      fpy = (bn+dpn)*ewny(j)
     $     +(bt+dpt)*ewty(j)
     $     +(bb+dpb)*ewby(j)
      fpz = (bn+dpn)*ewnz(j)
     $     +(bt+dpt)*ewtz(j)
     $     +(bb+dpb)*ewbz(j)

      fx(i) = fx(i) - fpx
      fy(i) = fy(i) - fpy 
      fz(i) = fz(i) - fpz

      if(j.eq.5) then
       if(fz(i).lt.0.) then
       fz(i) = 0.   
       vz(i) = 0.
C       write(42,*) outcnt,i
C       write(42,*) cycle,i
       endif
      endif 

      mx(i) = mx(i) + fpy*(zc-z(i)) - fpz*(yc-y(i))
      my(i) = my(i) + fpz*(xc-x(i)) - fpx*(zc-z(i)) 
      mz(i) = mz(i) + fpx*(yc-y(i)) - fpy*(xc-x(i))

      k1max = min( k1max, k1 )

 999  return

      end

