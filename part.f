      subroutine partition

c      implicit real*8(a-h,o-z)

      include 'dem.h'
      integer tno2

      do 10 px = 1, upx
      do 10 py = 1, upy
      do 10 pz = 1, upz
         parttl( px, py, pz ) = 0
         do 11 tno = 1, 16
            part(px, py, pz, tno)= 0
 11      continue
 10   continue

      tno = 0
      do 30 j = 1,ip

         px = int( x(j) / dpartx ) + 1
         py = int( y(j) / dparty ) + 1
         pz = int( z(j) / dpartz ) + 1

         tno = parttl( px, py, pz ) + 1
         parttl( px, py, pz ) = tno
         part( px, py, pz, tno ) =  j


*****  PERIODIC BOUNDARY CONDITION  *****
         if (px .eq. 1) then
            px = upx
            tno2 = parttl(px, py, pz) + 1
            parttl(px, py, pz) = tno2
            part(px, py, pz, tno2) = j
         endif
*****************************************

c 20   continue
c
c      IF((y(j).lt.0.).or.(y(j).gt.0.0012).or.
c     &(z(j).lt.0.).or.(z(j).gt.0.0036)) THEN
c       vbx(j) = 0.
c       vby(j) = 0.
c       vbz(j) = 0.
c       write(41,*) 'ryuusitobidasi'
c       write(41,*) 'j=',j
c       write(41,*) 'cycle=', outcnt
c       write(41,*) 'cycle=', cycle
c       endif

 30   continue

      return

      end











