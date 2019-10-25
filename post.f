      subroutine post

c      implicit real*8(a-h,o-z)

      include 'dem.h'

      CHARACTER kot*7,kot1*4,kot2*5,kot3*6,kot4*7

      R1=0.002
      R2=0.001
C
      SL0=0.12
      SU0=0.131

      open(unit = 10,err = 6060,status = 'old',file = 'count')
      close(unit = 10, status = 'delete' )
 6060  open(unit = 10,err = 9000,status = 'new',file = 'count')

      write(10,*) cycle
      close(10)

      open(unit = 11,err = 9000,status = 'old',file = 'count')
      read(11,*) kot
      close(11)

******************************************************
      if(cycle.lt.10000) then
      kot1=kot
      OPEN (UNIT=1,ERR=9001,STATUS='NEW',FILE='000'//kot1//'.dat')
      OPEN (UNIT=2,ERR=9001,STATUS='NEW',FILE='p000'//kot1//'.dat')
      goto 3000
      endif

      if((cycle.ge.10000).and.(cycle.lt.100000)) then
      kot2=kot
      OPEN (UNIT=1,ERR=9001,STATUS='NEW',FILE='00'//kot2//'.dat')
      OPEN (UNIT=2,ERR=9001,STATUS='NEW',FILE='p00'//kot2//'.dat')
      goto 3000
      endif

      if((cycle.ge.100000).and.(cycle.lt.1000000)) then
      kot3=kot
      OPEN (UNIT=1,ERR=9001,STATUS='NEW',FILE='0'//kot3//'.dat')
      OPEN (UNIT=2,ERR=9001,STATUS='NEW',FILE='p0'//kot3//'.dat')
      goto 3000
      endif

      if(cycle.ge.1000000) then
      kot4=kot
      OPEN (UNIT=1,ERR=9001,STATUS='NEW',FILE=kot4//'.dat')
      OPEN (UNIT=2,ERR=9001,STATUS='NEW',FILE='p'//kot4//'.dat')
      goto 3000
      endif
******************************************************

 3000  continue

      write(1,1000) tp, t, te, dt
      write(1,1000) rhop, rhof, mu
      write(1,1000) e,  ew, nu
      write(1,1000) ang,cf,cfw,muw

      write(1,1010) iwall
      do 10 i = 1, iwall
         write(1,1010)      i
         write(1,1000)   xw(i),   yw(i),   zw(i)  
         write(1,1000) ewnx(i), ewny(i), ewnz(i)  
         write(1,1000) ewtx(i), ewty(i), ewtz(i)  
         write(1,1000) ewbx(i), ewby(i), ewbz(i)  
 10   continue
      write(1,1010)    upx,    upy,    upz            
      write(1,1000) dpartx, dparty, dpartz

      write(1,1010) ip
      do 20 i = 1, ip
         write(1,1100)   i, nf(i), nfl(i), nfs(i), r(i), massf(i)
         write(1,1000)    x(i),    y(i),    z(i)
         write(1,1000)   vx(i),   vy(i),   vz(i) 
         write(1,1000)  vbx(i),  vby(i),  vbz(i)
         write(1,1000) omgx(i), omgy(i), omgz(i) 


      IF((Y(I).GT.0.018).AND.(Y(I).LT.0.030)) THEN
       IF((I.EQ.4056).OR.(I.EQ.4757).OR.(I.EQ.4776).OR.(I.EQ.3199))
     & GOTO 20
        IF (R(I).gt.(R1+R2)/2.) THEN
          WRITE(2,*) I, ',', X(I)/SL0, ',', Z(I)/SL0, ', '
        ELSE
          WRITE(2,*) I, ',', X(I)/SL0, ', ', ',', Z(I)/SL0
        ENDIF
      ENDIF         

                  
 20   continue

      do 30 i = 1,ip
         do 40 j = 1,tnomax
            if( fn(i,j) .ne. 0. ) then
              write(1,1020) i,j,ftbl(i,j),fn(i,j),ft(i,j),fb(i,j)
     $                                   ,vn(i,j),vt(i,j),vb(i,j)
            endif
 40      continue
         do 50 j = 1,iwall
            if( fwn(i,j) .ne. 0. ) then
              write(1,1030) i,j,fwn(i,j),fwt(i,j),fwb(i,j)
     $                         ,vwn(i,j),vwt(i,j),vwb(i,j)
            endif
 50      continue
 30   continue


      close( 1 )

      write(51,*) 'k1max=', -k1max

      return

 3    format( f8.5,4e16.7 )
 1000 format( 4e16.7 )
 1010 format( 3i16 ) 
 1020 format( ' 1   ',3i6,6e14.5 )
 1030 format( ' 0   ',2i6,'    0',6e14.5 )
 1100 format( 4I8,2e16.7 )
 9000 write(22,*) 'open error count'
 9001 write(21,*) 'open error kot'
      stop
      end

