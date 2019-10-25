***************************************************
*******    PROGRAM MAINPRO(INPUT,OUTPUT)   ********
***************************************************

      PROGRAM SETLLING

c      implicit real*8(a-h,o-z)                          !óLå¯êîéö15åÖ ()ì‡ÇÕìKópïœêîÇÃêÊì™ï∂éö

      PARAMETER(LX=10+8,LY=4+8,LZ=10+8,LN=10000)         !ÉÅÉbÉVÉÖêîÅAó±éqêî

      real*8
     & VOID(LX,LY,LZ),                                   !ãÛåÑó¶
     & VOLP(LX,LY,LZ)                                    !ÉÅÉbÉVÉÖíÜÇÃó±éqÇÃëÃêœ

      real*8
     & DP(LN)                                            !ó±åa

      INTEGER TNOA,TNOAK

      INTEGER PARTATL(LX-8,LY-8,LZ-8),                   !ÉÅÉbÉVÉÖÉpÅ[ÉgÉeÅ[ÉuÉã
     &        PARTA(LX-8,LY-8,LZ-8,256)

      real*8 MUF,TA,DELUW                                       

      CHARACTER kota*7,kota1*4,kota2*5,kota3*6,kota4*7      

*************************
****  ó±éqÇÃïœêîêÈåæ ****
*************************

      include 'dem.h'

*************************
********  íËêî **********
*************************

      CYCLE =1                                            !åvéZäJénÉTÉCÉNÉã

      SU0 = 0.131                                         !ë„ï\ë¨ìx(èIññíæç~ë¨ìx) Uo(cm/s)
      SL0 = 0.12                                          !ë„ï\í∑Ç≥ Lo(cm)

      UW0 = 0.0                                           !è„ï«ïîÇÃë¨ìx UW0(cm/s)

      dt = 0.1e-5                                         !óLéüå≥éûä‘çèÇ›(sec)

      TWFIN = dt*5000001.0                                !åvéZèIóπéûä‘
      NCYCLE = 10000                                      !ÉfÅ[É^èëÇ´èoÇµä‘äu
      TA = 0.0                                            !åvéZéûä‘ÇÃçáåvíl

      muf = 0.01002                                       !ó¨ëÃîSìx
      RE = SL0 * SU0 / 0.01                               !ÉåÉCÉmÉãÉYêî

      DELX = 0.12/10.0                                     !ñ≥éüå≥ÉÅÉbÉVÉÖïù
      DELY = 0.12/10.0
      DELZ = 0.12/10.0

      CZ = 1.0                                            !âìêSóÕÇ…ÇÊÇÈî{ó¶å

      RADJ = 0.0015                                       !ëÂè¨ó±éqîªíË

      PI=4.0*ATAN(1.0)                                    !ÉŒ

******************************
**** ÉfÅ[É^ÇÃì«Ç›çûÇ›ê›íË ****
******************************

      SW01 = 0.0                                          !0.0ÇÃèÍçáÇÕÉfÅ[É^ì«Ç›çûÇ›ÇµÇ»Ç¢

******************************

      IBAR = LX-8                                         !ÉÅÉbÉVÉÖì‡ì_êî
      JBAR = LY-8
      KBAR = LZ-8

      UI=0.0                                              !èâä˙0ê›íËÇ…ópÇ¢ÇÈílÇ
      VI=0.0
      WI=0.0

      DPARTAX=1.0
      DPARTAY=1.0
      DPARTAZ=1.0

      VOLC=DELX*DELY*DELZ                                 !ÉÅÉbÉVÉÖëÃêœ

      IMAX=IBAR+8
      JMAX=JBAR+8
      KMAX=KBAR+8
      IM1=IMAX-1
      JM1=JMAX-1
      KM1=KMAX-1
      IM2=IMAX-2
      JM2=JMAX-2
      KM2=KMAX-2
      IM3=IMAX-3
      JM3=JMAX-3
      KM3=KMAX-3
      IM4=IMAX-4
      JM4=JMAX-4
      KM4=KMAX-4
      IM5=IMAX-5
      JM5=JMAX-5
      KM5=KMAX-5
      IM6=IMAX-6
      JM6=JMAX-6
      KM6=KMAX-6
      IM7=IMAX-7
      JM7=JMAX-7
      KM7=KMAX-7

      RDX=1.0/DELX                                        !ãtêî
      RDY=1.0/DELY
      RDZ=1.0/DELZ

      NPARTAX=IBAR/DPARTAX
      NPARTAY=JBAR/DPARTAY
      NPARTAZ=KBAR/DPARTAZ

************************
**** ó¨ëÃÇÃèâä˙ê›íË ****                                  !ó¨ëÃÇÃèâä˙ê›íË
************************

      DO 560 K=1,KMAX
      DO 560 J=1,JMAX
      DO 560 I=1,IMAX
        VOLP(I,J,K)= 0.0
        VOID(I,J,K)= 1.0
  560 CONTINUE 

********************************
***** ó±éqÉfÅ[É^ÇÃì«Ç›çûÇ› *****
********************************

      CALL PRE

********************************

      DO 275 I=1,IP
        DP(I) = 2.0*R(I)                                  !ó±éqÇÃóLéüå≥íºåa
 275  CONTINUE

*******************************

      IF(SW01.NE.0.0) GO TO 750                           !1ÉTÉCÉNÉãñ⁄ÇÃèÍçáÇÃê›íËÉRÉRÇ©ÇÁ

************************************
****** ÉpÅ[ÉeÉBÉVÉáÉìÇÃèâä˙âª ******
************************************

      DO 7140 IP1=1,NPARTAX
      DO 7140 JP1=1,NPARTAY
      DO 7140 KP1=1,NPARTAZ
          PARTATL(IP1,JP1,KP1)=0
        DO 7140 NPP1=1,256
          PARTA(IP1,JP1,KP1,NPP1)=0
 7140 CONTINUE

********************************************
****** ÉÅÉbÉVÉÖíÜÇÃó±éqå¬êîÇÃÉJÉEÉìÉg ******
********************************************

      DO 7103 N=1,IP
        XPP=X(N)
        YPP=Y(N)
        ZPP=Z(N)
        II=INT(XPP/(DELX*DPARTAX))+1
        JJ=INT(YPP/(DELY*DPARTAY))+1
        KK=INT(ZPP/(DELZ*DPARTAZ))+1
        TNOA=PARTATL(II,JJ,KK)+1
        IF(TNOA.GT.256) GO TO 7103
        PARTATL(II,JJ,KK)=TNOA
        PARTA(II,JJ,KK,TNOA)=N 
 7103 CONTINUE

**************************
****** ãÛåÑó¶ÇÃåvéZ ******
**************************

      DO 7125 N=1,IP
        X1=X(N)-DELX*INT(X(N)/DELX)
        X2=DELX*(INT(X(N)/DELX)+1)-X(N)
        Y1=Y(N)-DELY*INT(Y(N)/DELY)
        Y2=DELY*(INT(Y(N)/DELY)+1)-Y(N)
        Z1=Z(N)-DELZ*INT(Z(N)/DELZ)
        Z2=DELZ*(INT(Z(N)/DELZ)+1)-Z(N)
        IX=INT(X(N)/DELX)+5
        JY=INT(Y(N)/DELY)+5
        KZ=INT(Z(N)/DELZ)+5
        XX1=DP(N)/2-X1
        XX2=DP(N)/2-X2
        YY1=DP(N)/2-Y1
        YY2=DP(N)/2-Y2
        ZZ1=DP(N)/2-Z1
        ZZ2=DP(N)/2-Z2
        XX1=MAX(XX1,0.0)
        XX2=MAX(XX2,0.0)
        YY1=MAX(YY1,0.0)
        YY2=MAX(YY2,0.0)
        ZZ1=MAX(ZZ1,0.0)
        ZZ2=MAX(ZZ2,0.0)
        VOLPX1=PI*XX1**2*(DP(N)+X1)/3.0
        VOLPX2=PI*XX2**2*(DP(N)+X2)/3.0
        VOLPY1=PI*YY1**2*(DP(N)+Y1)/3.0
        VOLPY2=PI*YY2**2*(DP(N)+Y2)/3.0
        VOLPZ1=PI*ZZ1**2*(DP(N)+Z1)/3.0
        VOLPZ2=PI*ZZ2**2*(DP(N)+Z2)/3.0
        VOLP(IX-1,JY,KZ)=VOLP(IX-1,JY,KZ)+VOLPX1
        VOLP(IX,JY-1,KZ)=VOLP(IX,JY-1,KZ)+VOLPY1
        VOLP(IX,JY,KZ-1)=VOLP(IX,JY,KZ-1)+VOLPZ1
        VOLP(IX,JY,KZ)=VOLP(IX,JY,KZ)+(PI*DP(N)**3)/6.0
     &  -(VOLPX1+VOLPY1+VOLPZ1+VOLPX2+VOLPY2+VOLPZ2)
        VOLP(IX+1,JY,KZ)=VOLP(IX+1,JY,KZ)+VOLPX2
        VOLP(IX,JY+1,KZ)=VOLP(IX,JY+1,KZ)+VOLPY2
        VOLP(IX,JY,KZ+1)=VOLP(IX,JY,KZ+1)+VOLPZ2
 7125 CONTINUE

      DO 7130 K=5,KM4
      DO 7130 J=5,JM4
      DO 7130 I=5,IM4
        VOIDD =(VOLC-VOLP(I,J,K))/VOLC
        VOID(I,J,K)=VOIDD
 7130 CONTINUE

**************************
**** ãÛåÑó¶ÇÃã´äEèåè ****
**************************

**** íÍïî ****
      DO 7113 J=5,JM4
      DO 7113 I=5,IM4
        VOID(I,J,4)=VOID(I,J,5)
        VOID(I,J,3)=VOID(I,J,6)
        VOID(I,J,2)=VOID(I,J,7)
        VOID(I,J,1)=VOID(I,J,8)
 7113 CONTINUE

**** ç∂ë§ ****
      DO 7132 K=5,KM4
      DO 7132 J=5,JM4
        VOID(1,J,K)=VOID(IM7,J,K)
        VOID(2,J,K)=VOID(IM6,J,K)
        VOID(3,J,K)=VOID(IM5,J,K)
        VOID(4,J,K)=VOID(IM4,J,K)
 7132 CONTINUE

**** âEë§ ****
      DO 7133 K=5,KM4
      DO 7133 J=5,JM4
        VOID(IMAX,J,K)=VOID(8,J,K)
        VOID(IM1,J,K)=VOID(7,J,K)
        VOID(IM2,J,K)=VOID(6,J,K)
        VOID(IM3,J,K)=VOID(5,J,K)
 7133 CONTINUE

**** è„ïî ****
      DO 7134 J=5,JM4
      DO 7134 I=5,IM4
        VOID(I,J,KMAX)=VOID(I,J,KM7)
        VOID(I,J,KM1)=VOID(I,J,KM6)
        VOID(I,J,KM2)=VOID(I,J,KM5)
        VOID(I,J,KM3)=VOID(I,J,KM4)
 7134 CONTINUE

**** éËëOë§ ****
      DO 7117 K=5,KM4
      DO 7117 I=5,IM4
        VOID(I,1,K)=VOID(I,8,K)
        VOID(I,2,K)=VOID(I,7,K)
        VOID(I,3,K)=VOID(I,6,K)
        VOID(I,4,K)=VOID(I,5,K)
 7117 CONTINUE

**** âúë§ ****
      DO 7127 K=5,KM4
      DO 7127 I=5,IM4
        VOID(I,JMAX,K)=VOID(I,JM7,K)
        VOID(I,JM1,K)=VOID(I,JM6,K)
        VOID(I,JM2,K)=VOID(I,JM5,K)
        VOID(I,JM3,K)=VOID(I,JM4,K)
 7127 CONTINUE

******************************
****** ÇπÇÒífó¨ÇÍÇÃê›íË ******
******************************

******************************

      GOTO 920                                               !1ÉTÉCÉNÉãñ⁄èÍçáÇÃê›íËÉRÉRÇ‹Ç≈

  750 CONTINUE

*****************************************
***** ó¨ëÃà¯åpÇ¨ÉfÅ[É^ÇÃì«Ç›çûÇ› ********                    !à¯åpÇ¨ÉfÅ[É^ì«Ç›çûÇ›
*****************************************

      open( unit = 1, err = 9906, status='old',
     &file = 'airinfile.dat' )

      READ(1,*) I

      DO 900 I=1,IMAX
      DO 900 J=1,JMAX
      DO 900 K=1,KMAX
        READ(1,*) VOID(I,J,K)
  900 CONTINUE

      close( 1 )

      goto 9907

 9906 write(9,*) 'open error airinfile.dat (unit=1)'

 9907 continue

**************************************************

  920 CONTINUE                                               !Ç±Ç±Ç‹Ç≈èâä˙ê›íË

****************************
***** ÉTÉCÉNÉãÉXÉ^Å[Ég *****
****************************

 1000 CONTINUE


********************************
***** DEMÇ…ÇÊÇÈó±éqÇÃåvéZ  *****                                 !ó±éqÇÃåvéZ
********************************

      outcnt = 0

**** CLEAR ALL PARTICLE PARAMETERS ****

c      DO 1507 I=1,IP
c          NF(I)=I
c          MASSF(I)=MASS(I)
c        IF(R(I).GT.RADJ) THEN
c          nfl(I) = 1
c          nfs(I) = 0
c        ELSE
c          nfl(I) = 0
c          nfs(I) = 1
c        ENDIF
c 1507 CONTINUE

********************************

**************************************
***** ó±éqï˚íˆéÆíÜÇÃçRóÕçÄÇÃåvéZ *****
**************************************

      DO 20 iq = 1, IP

        ISUP=INT(X(iq)/DELX)+5
        JSUP=INT(Y(iq)/DELY)+5
        KSUP=INT(Z(iq)/DELZ)+5
        VOIDD=VOID(ISUP,JSUP,KSUP)

C        faip=3.757-5.376/VOIDD+2.619/(VOIDD**2)
        faip=1.0

        usp=0.0
        vsp=0.0                                            !ó¨ëÃë¨ìx
        wsp=0.0

        upxp=vx(iq)-usp
        vpyp=vy(iq)-vsp
        wpzp=vz(iq)-wsp

****** ãÖëäìñíºåa ********

        rrr=(3.0*massf(nf(iq))/(4.0*pi*rhop))**0.3333333

        if(r(iq).gt.radj) then
          rpp=2.0*rrr/(2.0*float(nfl(nf(iq)))+float(nfs(nf(iq))))
        else
          rpp=    rrr/(2.0*float(nfl(nf(iq)))+float(nfs(nf(iq))))
        endif

        dpp=2.0*rpp

****************************

        FX(iq) = -3.0*PI*muf*dpp*upxp*faip

        FY(iq) = -3.0*PI*muf*dpp*vpyp*faip

        FZ(iq) = -3.0*PI*muf*dpp*wpzp*faip
     &           -980.7*CZ*mass(iq)*(rhop-rhof)/rhop

 20   CONTINUE


****** ãÖëäìñåaåvéZópÉfÅ[É^ÇÃèâä˙âª ****

      DO 507 I=1,IP
          NF(I)=I
          MASSF(I)=MASS(I)
        IF(R(I).GT.RADJ) THEN
          nfl(I) = 1
          nfs(I) = 0
        ELSE
          nfl(I) = 0
          nfs(I) = 1
        ENDIF
  507 CONTINUE

**************************
***** ó±éqÇÃê⁄êGåvéZ *****
**************************

      CALL PARTITION

      CALL CALCULATION

      CALL MOVE

**************************************
***** ó¨ëÃï˚íˆéÆíÜÇÃçRóÕçÄÇÃåvéZ *****
**************************************

      DO 6100 IP1=1,NPARTAX
      DO 6100 JP1=1,NPARTAY
      DO 6100 KP1=1,NPARTAZ
        PARTATL(IP1,JP1,KP1)=0
        DO 6100 NPP1=1,256
          PARTA(IP1,JP1,KP1,NPP1)=0
 6100 CONTINUE

***** ÉÅÉbÉVÉÖì‡ÇÃó±éqêîÇÃÉJÉEÉìÉg *****

      DO 6151 N=1,IP
        XPP=X(N)
        YPP=Y(N)
        ZPP=Z(N)
        II=INT(XPP/DELX)+1
        JJ=INT(YPP/DELY)+1
        KK=INT(ZPP/DELZ)+1
        if(N.eq.1060) then
C       write(*,*) cycle,X(N)
        endif
        TNOA=PARTATL(II,JJ,KK)+1
        IF(TNOA.GT.256) GO TO 6151
        PARTATL(II,JJ,KK)=TNOA
        PARTA(II,JJ,KK,TNOA)=N 
 6151 CONTINUE

      DO 6153 K=1,KMAX
      DO 6153 J=1,JMAX
      DO 6153 I=1,IMAX
        VOLP(I,J,K)=0.0
 6153 CONTINUE

***** ãÛåÑó¶ÇÃåvéZ *****

      DO 6700 N=1,IP
        X1=X(N)-DELX*INT(X(N)/DELX)
        X2=DELX*(INT(X(N)/DELX)+1)-X(N)
        Y1=Y(N)-DELY*INT(Y(N)/DELY)
        Y2=DELY*(INT(Y(N)/DELY)+1)-Y(N)
        Z1=Z(N)-DELZ*INT(Z(N)/DELZ)
        Z2=DELZ*(INT(Z(N)/DELZ)+1)-Z(N)
        IX=INT(X(N)/DELX)+5
        JY=INT(Y(N)/DELY)+5
        KZ=INT(Z(N)/DELZ)+5
        XX1=DP(N)/2-X1
        XX2=DP(N)/2-X2
        YY1=DP(N)/2-Y1
        YY2=DP(N)/2-Y2
        ZZ1=DP(N)/2-Z1
        ZZ2=DP(N)/2-Z2
        XX1=MAX(XX1,0.0)
        XX2=MAX(XX2,0.0)
        YY1=MAX(YY1,0.0)
        YY2=MAX(YY2,0.0)
        ZZ1=MAX(ZZ1,0.0)
        ZZ2=MAX(ZZ2,0.0)
        VOLPX1=PI*XX1**2*(DP(N)+X1)/3.0
        VOLPX2=PI*XX2**2*(DP(N)+X2)/3.0
        VOLPY1=PI*YY1**2*(DP(N)+Y1)/3.0
        VOLPY2=PI*YY2**2*(DP(N)+Y2)/3.0
        VOLPZ1=PI*ZZ1**2*(DP(N)+Z1)/3.0
        VOLPZ2=PI*ZZ2**2*(DP(N)+Z2)/3.0
        VOLP(IX-1,JY,KZ)=VOLP(IX-1,JY,KZ)+VOLPX1
        VOLP(IX,JY-1,KZ)=VOLP(IX,JY-1,KZ)+VOLPY1
        VOLP(IX,JY,KZ-1)=VOLP(IX,JY,KZ-1)+VOLPZ1
        VOLP(IX,JY,KZ)=VOLP(IX,JY,KZ)+(PI*DP(N)**3)/6.0
     &    -(VOLPX1+VOLPY1+VOLPZ1+VOLPX2+VOLPY2+VOLPZ2)
        VOLP(IX+1,JY,KZ)=VOLP(IX+1,JY,KZ)+VOLPX2
        VOLP(IX,JY+1,KZ)=VOLP(IX,JY+1,KZ)+VOLPY2
        VOLP(IX,JY,KZ+1)=VOLP(IX,JY,KZ+1)+VOLPZ2
 6700 CONTINUE

      DO 6800 K=5,KM4
      DO 6800 J=5,JM4
      DO 6800 I=5,IM4
        VOIDD =(VOLC-VOLP(I,J,K))/VOLC
        VOID(I,J,K)=VOIDD
 6800 CONTINUE

***** ãÛåÑó¶ÇÃã´äEèåèê›íË *****

**** íÍïî ****
      DO 7181 J=5,JM4
      DO 7181 I=5,IM4
        VOID(I,J,4)=VOID(I,J,5)
        VOID(I,J,3)=VOID(I,J,6)
        VOID(I,J,2)=VOID(I,J,7)
        VOID(I,J,1)=VOID(I,J,8)
 7181 CONTINUE

**** ç∂ë§ ****
      DO 7182 K=5,KM4
      DO 7182 J=5,JM4
        VOID(1,J,K)=VOID(IM7,J,K)
        VOID(2,J,K)=VOID(IM6,J,K)
        VOID(3,J,K)=VOID(IM5,J,K)
        VOID(4,J,K)=VOID(IM4,J,K)
 7182 CONTINUE

**** âEë§ ****
      DO 7183 K=5,KM4
      DO 7183 J=5,JM4
        VOID(IMAX,J,K)=VOID(8,J,K)
        VOID(IM1,J,K)=VOID(7,J,K)
        VOID(IM2,J,K)=VOID(6,J,K)
        VOID(IM3,J,K)=VOID(5,J,K)
 7183 CONTINUE

**** è„ïî ****
      DO 7184 J=5,JM4
      DO 7184 I=5,IM4
        VOID(I,J,KMAX)=VOID(I,J,KM7)
        VOID(I,J,KM1)=VOID(I,J,KM6)
        VOID(I,J,KM2)=VOID(I,J,KM5)
        VOID(I,J,KM3)=VOID(I,J,KM4)
 7184 CONTINUE

**** éËëOë§ ****
      DO 7185 K=5,KM4
      DO 7185 I=5,IM4
        VOID(I,1,K)=VOID(I,8,K)
        VOID(I,2,K)=VOID(I,7,K)
        VOID(I,3,K)=VOID(I,6,K)
        VOID(I,4,K)=VOID(I,5,K)
 7185 CONTINUE

**** âúë§ ****
      DO 7186 K=5,KM4
      DO 7186 I=5,IM4
        VOID(I,JMAX,K)=VOID(I,JM7,K)
        VOID(I,JM1,K)=VOID(I,JM6,K)
        VOID(I,JM2,K)=VOID(I,JM5,K)
        VOID(I,JM3,K)=VOID(I,JM4,K)
 7186 CONTINUE


************************************************************


****************************
***** ÉfÅ[É^ÇÃèëÇ´èoÇµ *****                                 !ÉfÅ[É^èëÇ´èoÇµ
****************************

      IF(MOD(CYCLE,NCYCLE).LT.1) THEN

***** ó±éqÉfÅ[É^ÇÃèëÇ´èoÇµ *****

      call post

***** ó¨ëÃÉfÅ[É^ÇÃèëÇ´èoÇµ *****

      open(unit = 10,err = 6060,status = 'old',file = 'acount')
      close(unit = 10, status = 'delete' )
 6060 open(unit = 10,err = 9904,status = 'new',file = 'acount')

      write(10,*) cycle
      close(10)

      open(unit = 11,err = 9904,status = 'old',file = 'acount')
      read(11,*) kota
      close(11)

      if(cycle.lt.10000) then
      kota1=kota
      OPEN (UNIT=1,ERR=9904,STATUS='NEW',FILE='a000'//kota1//'.dat')
      goto 3000
      endif

      if((cycle.ge.10000).and.(cycle.lt.100000)) then
      kota2=kota
      OPEN (UNIT=1,ERR=9904,STATUS='NEW',FILE='a00'//kota2//'.dat')
      goto 3000
      endif

      if((cycle.ge.100000).and.(cycle.lt.1000000)) then
      kota3=kota
      OPEN (UNIT=1,ERR=9904,STATUS='NEW',FILE='a0'//kota3//'.dat')
      goto 3000
      endif

      if(cycle.ge.1000000) then
      kota4=kota
      OPEN (UNIT=1,ERR=9904,STATUS='NEW',FILE='a'//kota4//'.dat')
      goto 3000
      endif

******************************

 3000 continue

      WRITE(1,93) CYCLE
      DO 5203 I=1,IMAX
      DO 5203 J=1,JMAX
      DO 5203 K=1,KMAX
        WRITE(1,88) VOID (I,J,K)
 5203 CONTINUE

      close( 1 )

      goto 9905
 9904 write(9,*) 'open error airfile01.dat (unit=1)'
 9905 continue

      ENDIF

***** âÊñ èoóÕ *****

      if(mod(cycle,100).lt.1) then
      WRITE(*,*)'CYCLE = ',CYCLE
      WRITE(*,*)
      endif

**********************
***** éûä‘ÇÃëOêi *****
**********************

      TA=TA+dt                                             !åvéZéûä‘èIóπÇÃîªíË
      IF(TA.GT.TWFIN) GO TO 5100

      CYCLE=CYCLE+1

      GO TO 1000

***** åvéZèIóπÉfÅ[É^ÇÃèëÇ´èoÇµ *****

 5100 CONTINUE

C      close( 77 )

      TA=TA-dt
      WRITE(60,110) TA,CYCLE+1
      WRITE(60,*) 'END KEKKA'

  999 CONTINUE

 6500 STOP

***** èëéÆê›íË *****

   48 FORMAT(4(1PE14.6))
   50 FORMAT(1H ,5X,'IBAR= ',I3/6X,'JBAR= ',I3/6X,
     &'KBAR= ',I3/6X,
     &'DELX= ',1PE12.5/6X,'DELY= ',E12.5/6X,'DELZ= ',1PE12.5/6X,
     &'DELT= ',E12.5/8X,'RE= ',E12.5/8X,'UI= ',E12.5/8X,
     &'VI= ',E12.5/8X,'WI= ',E12.5/7X,'OMG= ',E12.5/5X,
     &'TWFIN= ',E12.5)
   77 FORMAT(2X,'CYCLE= ',I6,4X,'ITER= ',I7,4X,'RRMAX= ',1PE12.5
     &,4X,'EPSI= ',E12.5,5X)
   78 FORMAT(3(1PE14.6))
   88 FORMAT(2(1PE14.6))
   93 FORMAT(I7)
  110 FORMAT(2X,'TIME=  ',1PE12.5,4X,'CYCLE= ',I6)


      END 

