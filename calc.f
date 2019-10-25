      subroutine calculation

c      implicit real*8(a-h,o-z)

      include 'dem.h'
      integer no1,no2,tnoi, tnoj, zo

      do 1 py =  1, upy
      do 1 px =  1, upx-1
      do 1 pz =  1, upz
         tnoi = parttl( px, py, pz )
         do 2 no1 = 1, tnoi-1
         do 2 no2 = no1+1, tnoi
            i = part( px, py, pz, no1)
            j = part( px, py, pz, no2)
            call pp ( i, j, px )
 2       continue
 1    continue


      do 11 py = 1, upy
      do 11 px = 1, upx-1
      do 11 pz = 1, upz
         tnoi = parttl( px, py, pz )
         do 12 no1 = 1, tnoi
            tnoj = parttl( px+1, py, pz )
            do 13 no2 = 1, tnoj
               i = part( px,    py,   pz, no1)
               j = part( px+1,  py,   pz, no2)
               call pp ( i, j, px )
 13         continue
 12      continue
 11   continue


      do 21 py = 1, upy-1
      do 21 px = 1, upx-1
      do 21 pz = 1, upz 
         tnoi = parttl( px, py, pz )
         do 22 no1 = 1, tnoi
            tnoj = parttl( px, py+1, pz )
            do 23 no2 = 1, tnoj
               i = part( px,  py,   pz, no1)
               j = part( px,  py+1, pz, no2)
               call pp ( i, j, px )
 23         continue
 22      continue
 21   continue


      do 31 py = 1, upy
      do 31 px = 1, upx-1
      do 31 pz = 1, upz-1
         tnoi = parttl( px, py, pz )
         do 32 no1 = 1, tnoi
            tnoj = parttl( px, py, pz+1 )
            do 33 no2 = 1, tnoj
               i = part( px,  py, pz  , no1)
               j = part( px,  py, pz+1, no2)
               call pp ( i, j, px )
 33         continue
 32      continue
 31   continue


** x,y
      do 41 py = 1, upy-1
      do 41 px = 1, upx-1
      do 41 pz = 1, upz
         tnoi = parttl( px, py, pz )
         do 42 no1 = 1, tnoi
            tnoj = parttl( px+1, py+1, pz )
            do 43 no2 = 1, tnoj
               i = part( px,    py,   pz, no1)
               j = part( px+1,  py+1, pz, no2)
               call pp ( i, j, px )
 43         continue
 42      continue
 41   continue


** x,-y
      do 46 py = 1+1,upy
      do 46 px = 1,  upx-1
      do 46 pz = 1,  upz
         tnoi = parttl( px, py, pz )
         do 47 no1 = 1, tnoi
            tnoj = parttl( px+1, py-1, pz )
            do 48 no2 = 1, tnoj
               i = part( px,    py,   pz, no1)
               j = part( px+1,  py-1, pz, no2)
               call pp ( i, j, px )
 48         continue
 47      continue
 46   continue


** y,z
      do 51 py = 1, upy-1
      do 51 px = 1, upx-1
      do 51 pz = 1, upz-1
         tnoi = parttl( px, py, pz )
         do 52 no1 = 1, tnoi
            tnoj = parttl( px, py+1, pz+1 )
            do 53 no2 = 1, tnoj
               i = part( px,  py,   pz,   no1)
               j = part( px,  py+1, pz+1, no2)
               call pp ( i, j, px )
 53         continue
 52      continue
 51   continue


** y,-z
      do 56 py = 1,  upy-1
      do 56 px = 1,  upx-1
      do 56 pz = 1+1,upz
         tnoi = parttl( px, py, pz )
         do 57 no1 = 1, tnoi
            tnoj = parttl( px, py+1, pz-1 )
            do 58 no2 = 1, tnoj
               i = part( px,  py,   pz,   no1)
               j = part( px,  py+1, pz-1, no2)
               call pp ( i, j, px )
 58         continue
 57      continue
 56   continue


** z,x
      do 61 py = 1, upy
      do 61 px = 1, upx-1
      do 61 pz = 1, upz-1
         tnoi = parttl( px, py, pz )
         do 62 no1 = 1, tnoi
            tnoj = parttl( px+1, py, pz+1 )
            do 63 no2 = 1, tnoj
               i = part( px,   py, pz  , no1)
               j = part( px+1, py, pz+1, no2)
               call pp ( i, j, px )
 63         continue
 62      continue
 61   continue


** -z,x
      do 66 py = 1,  upy
      do 66 px = 1,  upx-1
      do 66 pz = 1+1,upz
         tnoi = parttl( px, py, pz )
         do 67 no1 = 1, tnoi
            tnoj = parttl( px+1, py, pz-1 )
            do 68 no2 = 1, tnoj
               i = part( px,   py, pz  , no1)
               j = part( px+1, py, pz-1, no2)
               call pp ( i, j, px )
 68         continue
 67      continue
 66   continue


** x,y,z
      do 71 px = 1,  upx-1
      do 71 py = 1,  upy-1
      do 71 pz = 1,  upz-1
         tnoi = parttl( px, py, pz )
         do 72 no1 = 1, tnoi
            tnoj = parttl( px+1, py+1, pz+1 )
            do 73 no2 = 1, tnoj
               i = part( px,   py,   pz  , no1)
               j = part( px+1, py+1, pz+1, no2)
               call pp ( i, j, px )
 73         continue
 72      continue
 71   continue


** x,-y,z
      do 81 px = 1,  upx-1
      do 81 py = 1+1,upy
      do 81 pz = 1,  upz-1
         tnoi = parttl( px, py, pz )
         do 82 no1 = 1, tnoi
            tnoj = parttl( px+1, py-1, pz+1 )
            do 83 no2 = 1, tnoj
               i = part( px,   py,   pz  , no1)
               j = part( px+1, py-1, pz+1, no2)
               call pp ( i, j, px )
 83         continue
 82      continue
 81   continue


** x,y,-z
      do 91 px = 1,  upx-1
      do 91 py = 1,  upy-1
      do 91 pz = 1+1,upz
         tnoi = parttl( px, py, pz )
         do 92 no1 = 1, tnoi
            tnoj = parttl( px+1, py+1, pz-1 )
            do 93 no2 = 1, tnoj
               i = part( px,   py,   pz  , no1)
               j = part( px+1, py+1, pz-1, no2)
               call pp ( i, j, px )
 93         continue
 92      continue
 91   continue


** x,-y,-z
      do 101 px = 1,  upx-1
      do 101 py = 1+1,upy
      do 101 pz = 1+1,upz
         tnoi = parttl( px, py, pz )
         do 102 no1 = 1, tnoi
            tnoj = parttl( px+1, py-1, pz-1 )
            do 103 no2 = 1, tnoj
               i = part( px,   py,   pz  , no1)
               j = part( px+1, py-1, pz-1, no2)
               call pp ( i, j, px )
 103         continue
 102      continue
 101   continue


*********************************************************

C      j = 1                                !ç∂ï«
C      px = 1
C      do 111 py = 1, upy
C      do 111 pz = 1, upz
C         tnoi = parttl( px, py, pz )
C         do 112 no1 = 1, tnoi
C            i = part( px, py, pz, no1)
C            call pw( i, j )
C 112     continue
C 111  continue
C
C      j = 2                                !âEï«
C      px = upx
C      do 131 py = 1, upy
C      do 131 pz = 1, upz
C         tnoi = parttl( px, py, pz )
C         do 132 no1 = 1, tnoi
C            i = part( px, py, pz, no1)
C            call pw( i, j )
C 132     continue
C 131  continue

      j = 3                                !éËëOï«
      py = 1
      do 141 px = 1, upx-1
      do 141 pz = 1, upz
         tnoi = parttl( px, py, pz )
         do 142 no1 = 1, tnoi
            i = part( px, py, pz, no1)
            call pw( i, j )
 142     continue
 141  continue


      j = 4                                !âúï«
      py = upy
      do 151 px = 1, upx-1
      do 151 pz = 1, upz
         tnoi = parttl( px, py, pz )
         do 152 no1 = 1, tnoi
            i = part( px, py, pz, no1)
            call pw( i, j )
 152     continue
 151  continue


      j = 5                                !â∫ï«
      pz = 1
      do 171 py = 1, upy
      do 171 px = 1, upx-1
         tnoi = parttl( px, py, pz )
         do 172 no1 = 1, tnoi
            i = part( px, py, pz, no1)
            call pw( i, j )
 172     continue
 171  continue


      j = 6                                !è„ï«
      pz = upz     
      do 121 py = 1, upy
      do 121 px = 1, upx-1
         tnoi = parttl( px, py, pz )
         do 122 no1 = 1, tnoi
            i = part( px, py, pz, no1)
            call pw( i, j )
 122     continue
 121  continue


      return
      end

