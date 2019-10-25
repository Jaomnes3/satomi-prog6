      subroutine move

c      implicit real*8(a-h,o-z)

      include 'dem.h'

      real*8  bound

      acc = 1.000
      vlim = 13.1
      omglim = 400.0

      do 95  i= 1, ip
        vbx(i) = vx(i)
        vby(i) = vy(i)
        vbz(i) = vz(i)
 95   continue

      R1zpmax=0.0
      R2zpmax=0.0
      mR1zip=0
      mR2zip=0
	vxmax=0.0
	mvxip=0
      vzmax=0.0
      mvzip=0
      vzmin=0.0
      nvzip=0

      do 100 i = 1, ip

        vx(i)  =  vx(i) + fx(i)/(mass(i)*acc)*dt
          if(abs(vx(i)).gt.vlim) then
            vx(i)=vlim*vx(i)/abs(vx(i))
          endif
        vy(i)  =  vy(i) + fy(i)/(mass(i)*acc)*dt
          if(abs(vy(i)).gt.vlim) then
            vy(i)=vlim*vy(i)/abs(vy(i))
          endif
        vz(i)  =  vz(i) + fz(i)/(mass(i)*acc)*dt
          if(abs(vz(i)).gt.vlim) then
            vz(i)=vlim*vz(i)/abs(vz(i))
          endif
        omgx(i) = omgx(i) + mx(i)/(inat(i)*acc)*dt
          if(abs(omgx(i)).gt.omglim) then
            omgx(i)=omglim*omgx(i)/abs(omgx(i))
          endif
        omgy(i) = omgy(i) + my(i)/(inat(i)*acc)*dt
          if(abs(omgy(i)).gt.omglim) then
            omgy(i)=omglim*omgy(i)/abs(omgy(i))
          endif
        omgz(i) = omgz(i) + mz(i)/(inat(i)*acc)*dt
          if(abs(omgz(i)).gt.omglim) then
            omgz(i)=omglim*omgz(i)/abs(omgz(i))
          endif

        x(i) = x(i) + vx(i)*dt
        y(i) = y(i) + vy(i)*dt
        z(i) = z(i) + vz(i)*dt

      
C      WRITE(77,*) 'moveÇÃíÜ','FZ(çáåv) =',FZ(3543)
C      WRITE(77,*) 'VZ =',VZ(3543),' mass =',mass(3543)
C      WRITE(77,*) 'VZ(ëùï™) =', fz(3543)/(mass(3543)*acc)*dt
C      endif

****** PERIODIC BOUNDARY CONDITION  ******

      bound = dpartx * float(upx-1)

      if (x(i) .lt. 0.0) then
         x(i) = x(i) + bound
      endif

      if (x(i) .ge. bound) then
         x(i) = x(i) - bound
      endif

*******************************************

        if(r(i).gt.0.0015) then
        if(z(i).gt.R1zpmax) then
        R1zpmax=z(i)
        mR1zip=i
        endif
        else
        if(z(i).gt.R2zpmax) then
        R2zpmax=z(i)
        mR2zip=i
        endif
        endif
        if(vx(i).gt.vxmax) then
        vxmax=vx(i)
        mvxip=i
        endif
        if(vz(i).gt.vzmax) then
        vzmax=vz(i)
        mvzip=i
        endif
        if(vz(i).lt.vzmin) then
        vzmin=vz(i)
        nvzip=i
        endif

 100  continue

        if(mod(cycle,100).lt.1) then
        write(*,*) 'i=',mR1zip
        write(*,*) 'R1zmax=',R1zpmax
        write(*,*) 'i=',mR2zip
        write(*,*) 'R2zmax=',R2zpmax
	  write(*,*) 'i=',mvxip
        write(*,*) 'vxmax=',vxmax
        write(*,*) 'i=',mvzip
        write(*,*) 'vzmax=',vzmax
        write(*,*) 'i=',nvzip
        write(*,*) 'vzmin=',vzmin
        endif

      return

      end

