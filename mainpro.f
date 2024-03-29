***************************************************
*******    PROGRAM MAINPRO(INPUT,OUTPUT)   ********
***************************************************

      PROGRAM SETLLING

c      implicit real*8(a-h,o-z)                          !有効数字15桁 ()内は適用変数の先頭文字

      PARAMETER(LX=10+8,LY=4+8,LZ=10+8,LN=10000)         !メッシュ数、粒子数

      real*8
     & VOID(LX,LY,LZ),                                   !空隙率
     & VOLP(LX,LY,LZ)                                    !メッシュ中の粒子の体積

      real*8
     & DP(LN)                                            !粒径

      INTEGER TNOA,TNOAK

      INTEGER PARTATL(LX-8,LY-8,LZ-8),                   !メッシュパートテーブル
     &        PARTA(LX-8,LY-8,LZ-8,256)

      real*8 MUF,TA,DELUW                                       

      CHARACTER kota*7,kota1*4,kota2*5,kota3*6,kota4*7      

*************************
****  粒子の変数宣言 ****
*************************

      include 'dem.h'

*************************
********  定数 **********
*************************

      CYCLE =1                                            !計算開始サイクル

      SU0 = 0.131                                         !代表速度(終末沈降速度) Uo(cm/s)
      SL0 = 0.12                                          !代表長さ Lo(cm)

      UW0 = 0.0                                           !上壁部の速度 UW0(cm/s)

      dt = 0.1e-5                                         !有次元時間刻み(sec)

      TWFIN = dt*5000001.0                                !計算終了時間
      NCYCLE = 10000                                      !データ書き出し間隔
      TA = 0.0                                            !計算時間の合計値

      muf = 0.01002                                       !流体粘度
      RE = SL0 * SU0 / 0.01                               !レイノルズ数

      DELX = 0.12/10.0                                     !無次元メッシュ幅
      DELY = 0.12/10.0
      DELZ = 0.12/10.0

      CZ = 1.0                                            !遠心力による倍率�

      RADJ = 0.0015                                       !大小粒子判定

      PI=4.0*ATAN(1.0)                                    !π

******************************
**** データの読み込み設定 ****
******************************

      SW01 = 0.0                                          !0.0の場合はデータ読み込みしない

******************************

      IBAR = LX-8                                         !メッシュ内点数
      JBAR = LY-8
      KBAR = LZ-8

      UI=0.0                                              !初期0設定に用いる値�
      VI=0.0
      WI=0.0

      DPARTAX=1.0
      DPARTAY=1.0
      DPARTAZ=1.0

      VOLC=DELX*DELY*DELZ                                 !メッシュ体積

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

      RDX=1.0/DELX                                        !逆数
      RDY=1.0/DELY
      RDZ=1.0/DELZ

      NPARTAX=IBAR/DPARTAX
      NPARTAY=JBAR/DPARTAY
      NPARTAZ=KBAR/DPARTAZ

************************
**** 流体の初期設定 ****                                  !流体の初期設定
************************

      DO 560 K=1,KMAX
      DO 560 J=1,JMAX
      DO 560 I=1,IMAX
        VOLP(I,J,K)= 0.0
        VOID(I,J,K)= 1.0
  560 CONTINUE 

********************************
***** 粒子データの読み込み *****
********************************

      CALL PRE

********************************

      DO 275 I=1,IP
        DP(I) = 2.0*R(I)                                  !粒子の有次元直径
 275  CONTINUE

*******************************

      IF(SW01.NE.0.0) GO TO 750                           !1サイクル目の場合の設定ココから

************************************
****** パーティションの初期化 ******
************************************

      DO 7140 IP1=1,NPARTAX
      DO 7140 JP1=1,NPARTAY
      DO 7140 KP1=1,NPARTAZ
          PARTATL(IP1,JP1,KP1)=0
        DO 7140 NPP1=1,256
          PARTA(IP1,JP1,KP1,NPP1)=0
 7140 CONTINUE

********************************************
****** メッシュ中の粒子個数のカウント ******
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
****** 空隙率の計算 ******
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
**** 空隙率の境界条件 ****
**************************

**** 底部 ****
      DO 7113 J=5,JM4
      DO 7113 I=5,IM4
        VOID(I,J,4)=VOID(I,J,5)
        VOID(I,J,3)=VOID(I,J,6)
        VOID(I,J,2)=VOID(I,J,7)
        VOID(I,J,1)=VOID(I,J,8)
 7113 CONTINUE

**** 左側 ****
      DO 7132 K=5,KM4
      DO 7132 J=5,JM4
        VOID(1,J,K)=VOID(IM7,J,K)
        VOID(2,J,K)=VOID(IM6,J,K)
        VOID(3,J,K)=VOID(IM5,J,K)
        VOID(4,J,K)=VOID(IM4,J,K)
 7132 CONTINUE

**** 右側 ****
      DO 7133 K=5,KM4
      DO 7133 J=5,JM4
        VOID(IMAX,J,K)=VOID(8,J,K)
        VOID(IM1,J,K)=VOID(7,J,K)
        VOID(IM2,J,K)=VOID(6,J,K)
        VOID(IM3,J,K)=VOID(5,J,K)
 7133 CONTINUE

**** 上部 ****
      DO 7134 J=5,JM4
      DO 7134 I=5,IM4
        VOID(I,J,KMAX)=VOID(I,J,KM7)
        VOID(I,J,KM1)=VOID(I,J,KM6)
        VOID(I,J,KM2)=VOID(I,J,KM5)
        VOID(I,J,KM3)=VOID(I,J,KM4)
 7134 CONTINUE

**** 手前側 ****
      DO 7117 K=5,KM4
      DO 7117 I=5,IM4
        VOID(I,1,K)=VOID(I,8,K)
        VOID(I,2,K)=VOID(I,7,K)
        VOID(I,3,K)=VOID(I,6,K)
        VOID(I,4,K)=VOID(I,5,K)
 7117 CONTINUE

**** 奥側 ****
      DO 7127 K=5,KM4
      DO 7127 I=5,IM4
        VOID(I,JMAX,K)=VOID(I,JM7,K)
        VOID(I,JM1,K)=VOID(I,JM6,K)
        VOID(I,JM2,K)=VOID(I,JM5,K)
        VOID(I,JM3,K)=VOID(I,JM4,K)
 7127 CONTINUE

******************************
****** せん断流れの設定 ******
******************************

******************************

      GOTO 920                                               !1サイクル目場合の設定ココまで

  750 CONTINUE

*****************************************
***** 流体引継ぎデータの読み込み ********                    !引継ぎデータ読み込み
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

  920 CONTINUE                                               !ここまで初期設定

****************************
***** サイクルスタート *****
****************************

 1000 CONTINUE


********************************
***** DEMによる粒子の計算  *****                                 !粒子の計算
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
***** 粒子方程式中の抗力項の計算 *****
**************************************

      DO 20 iq = 1, IP

        ISUP=INT(X(iq)/DELX)+5
        JSUP=INT(Y(iq)/DELY)+5
        KSUP=INT(Z(iq)/DELZ)+5
        VOIDD=VOID(ISUP,JSUP,KSUP)

C        faip=3.757-5.376/VOIDD+2.619/(VOIDD**2)
        faip=1.0

        usp=0.0
        vsp=0.0                                            !流体速度
        wsp=0.0

        upxp=vx(iq)-usp
        vpyp=vy(iq)-vsp
        wpzp=vz(iq)-wsp

****** 球相当直径 ********

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


****** 球相当径計算用データの初期化 ****

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
***** 粒子の接触計算 *****
**************************

      CALL PARTITION

      CALL CALCULATION

      CALL MOVE

**************************************
***** 流体方程式中の抗力項の計算 *****
**************************************

      DO 6100 IP1=1,NPARTAX
      DO 6100 JP1=1,NPARTAY
      DO 6100 KP1=1,NPARTAZ
        PARTATL(IP1,JP1,KP1)=0
        DO 6100 NPP1=1,256
          PARTA(IP1,JP1,KP1,NPP1)=0
 6100 CONTINUE

***** メッシュ内の粒子数のカウント *****

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

***** 空隙率の計算 *****

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

***** 空隙率の境界条件設定 *****

**** 底部 ****
      DO 7181 J=5,JM4
      DO 7181 I=5,IM4
        VOID(I,J,4)=VOID(I,J,5)
        VOID(I,J,3)=VOID(I,J,6)
        VOID(I,J,2)=VOID(I,J,7)
        VOID(I,J,1)=VOID(I,J,8)
 7181 CONTINUE

**** 左側 ****
      DO 7182 K=5,KM4
      DO 7182 J=5,JM4
        VOID(1,J,K)=VOID(IM7,J,K)
        VOID(2,J,K)=VOID(IM6,J,K)
        VOID(3,J,K)=VOID(IM5,J,K)
        VOID(4,J,K)=VOID(IM4,J,K)
 7182 CONTINUE

**** 右側 ****
      DO 7183 K=5,KM4
      DO 7183 J=5,JM4
        VOID(IMAX,J,K)=VOID(8,J,K)
        VOID(IM1,J,K)=VOID(7,J,K)
        VOID(IM2,J,K)=VOID(6,J,K)
        VOID(IM3,J,K)=VOID(5,J,K)
 7183 CONTINUE

**** 上部 ****
      DO 7184 J=5,JM4
      DO 7184 I=5,IM4
        VOID(I,J,KMAX)=VOID(I,J,KM7)
        VOID(I,J,KM1)=VOID(I,J,KM6)
        VOID(I,J,KM2)=VOID(I,J,KM5)
        VOID(I,J,KM3)=VOID(I,J,KM4)
 7184 CONTINUE

**** 手前側 ****
      DO 7185 K=5,KM4
      DO 7185 I=5,IM4
        VOID(I,1,K)=VOID(I,8,K)
        VOID(I,2,K)=VOID(I,7,K)
        VOID(I,3,K)=VOID(I,6,K)
        VOID(I,4,K)=VOID(I,5,K)
 7185 CONTINUE

**** 奥側 ****
      DO 7186 K=5,KM4
      DO 7186 I=5,IM4
        VOID(I,JMAX,K)=VOID(I,JM7,K)
        VOID(I,JM1,K)=VOID(I,JM6,K)
        VOID(I,JM2,K)=VOID(I,JM5,K)
        VOID(I,JM3,K)=VOID(I,JM4,K)
 7186 CONTINUE


************************************************************


****************************
***** データの書き出し *****                                 !データ書き出し
****************************

      IF(MOD(CYCLE,NCYCLE).LT.1) THEN

***** 粒子データの書き出し *****

      call post

***** 流体データの書き出し *****

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

***** 画面出力 *****

      if(mod(cycle,100).lt.1) then
      WRITE(*,*)'CYCLE = ',CYCLE
      WRITE(*,*)
      endif

**********************
***** 時間の前進 *****
**********************

      TA=TA+dt                                             !計算時間終了の判定
      IF(TA.GT.TWFIN) GO TO 5100

      CYCLE=CYCLE+1

      GO TO 1000

***** 計算終了データの書き出し *****

 5100 CONTINUE

C      close( 77 )

      TA=TA-dt
      WRITE(60,110) TA,CYCLE+1
      WRITE(60,*) 'END KEKKA'

  999 CONTINUE

 6500 STOP

***** 書式設定 *****

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

