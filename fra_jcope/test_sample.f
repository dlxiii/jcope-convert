c===============================================================
c
c     name: test_sample.f
c
c     read data files 
c
c     2006.10-  Miyazawa, Y.
c     2004.01-  Miyazawa, Y.
c     2003.12-  Maeda, T. Kawai, K. 
c
c===============================================================
c
      program main
c------------------------------
      parameter( im=122, jm=86, km=47 )
c------------------------------
      common /domain/
     & z(im,jm,km),zz(im,jm,km),dz(im,jm,km)
      common /coord/
     & dxlon(im),dylat(jm),xlon(im),ylat(jm),xlonu(im),ylatu(jm)
     &,xlonv(im),ylatv(jm)
      common /orgdata2d/
     & el(im,jm)
      common /orgdata3d/
     & u(im,jm,km),v(im,jm,km),t(im,jm,km),s(im,jm,km)
c
      parameter( klev=28 )
      common /zlevel/
     & h0(klev)
      data h0/
     & -0., -5., -10., -50., -75., -100., -150., -200., -250., -300.
     &, -400., -500., -600., -700., -800., -900., -1000., -1200.
     &, -1400., -1600., -1800., -2000., -2500, -3000., -3500., -4000.
     &, -5000., -6000. /
      common /data3d/
     & ulev(im,jm,klev),vlev(im,jm,klev)
     &,tlev(im,jm,klev),slev(im,jm,klev)
      character*3 namemonth(12)
      data namemonth/
     & 'JAN','FEB','MAR','APR','MAY','JUN'
     &,'JUL','AUG','SEP','OCT','NOV','DEC'/
c
      character*80 datadir,depthfile,outdir
      character*40 ctlname,dataname
      character*2 cmonth,cday
      character*4 
     & cyear,stepday,fildscH,fildscEL,fildscU,fildscV,fildscT,fildscS
c
c------- SETTING START -------
      datadir='./'
      isyy = 2015
      ismm = 01
      isdd = 01
      ieyy = isyy
      iemm = ismm
      iedd = isdd
      xlons = 132 -1./24.
      ylats = 29  -1./24.
      intday=1
      stepday=' 1DY'
      depthfile='./basic.dat'
c------- SETTING END -------
c
c-------- READ START ---------
      write(6,*) '******* READ START *******' 
      tbias = 10.
      sbias = 35.
      valmis = -99.9999
c
      open(10,file=depthfile,form='unformatted')
      read(10) fildscH,Z,ZZ,DZ,ichflg
      close(10)
      write(6,*) 'read ',fildscH,ichflg
c
      timejde = julday(ieyy,iemm,iedd)
      iyy = isyy
      imm = ismm
      idd = isdd
      timejd = julday(iyy,imm,idd)
c
      numfilmx = int(timejde -timejd)/intday +1
c
      do 250 numfil=1,numfilmx
c
         call caldat(int(timejd+0.001),iyy,imm,idd)
         write(cmonth,'(i2.2)') imm
         write(cday,'(i2.2)') idd
         write(cyear,'(i4.4)') iyy
c-----------------------------------------------------------
             write(6,*) '### read START (EL_) '
             open(10,file=datadir(1:lnblnk(datadir))
     &             //'EL_'//cyear//cmonth//cday
     &             ,form='unformatted')
             read(10) fildscEL,iyf,imf,idf,el,mall
             close(10)
             write(6,*) '### read END (EL_) ',fildscEL,iyf,imf,idf,mall
c-----------------------------------------------------------
             open(10,file=datadir(1:lnblnk(datadir))
     &             //'U_'//cyear//cmonth//cday
     &          ,form='unformatted')
             read(10) 
     &        fildscU,iyf,imf,idf
     &       ,(((U(i,j,k),i=1,im),j=1,jm),k=1,km-1),mall
             close(10)
             write(6,*) '### read(U_) ',fildscU,iyf,imf,idf,mall
c-----------------------------------------------------------
             open(10,file=datadir(1:lnblnk(datadir))
     &             //'V_'//cyear//cmonth//cday
     &          ,form='unformatted')
             read(10) 
     &        fildscV,iyf,imf,idf
     &       ,(((V(i,j,k),i=1,im),j=1,jm),k=1,km-1),mall
             close(10)
             write(6,*) '### read(V_) ',fildscV,iyf,imf,idf,mall
c-----------------------------------------------------------
             open(10,file=datadir(1:lnblnk(datadir))
     &             //'T_'//cyear//cmonth//cday
     &          ,form='unformatted')
             read(10) 
     &        fildscT,iyf,imf,idf
     &       ,(((T(i,j,k),i=1,im),j=1,jm),k=1,km-1),mall
             close(10)
             write(6,*) '### read(T_) ',fildscT,iyf,imf,idf,mall
c-----------------------------------------------------------
             open(10,file=datadir(1:lnblnk(datadir))
     &             //'S_'//cyear//cmonth//cday
     &          ,form='unformatted')
             read(10) 
     &        fildscS,iyf,imf,idf
     &       ,(((S(i,j,k),i=1,im),j=1,jm),k=1,km-1),mall
             close(10)
             write(6,*) '### read(S_) ',fildscS,iyf,imf,idf,mall
c-----------------------------------------------------------

         write(6,*) '### NUM,yymmdd = ',numfil,iyy,imm,idd
         timejd = timejd +intday
c
  250 continue
c
      write(6,*) '******* READ END *******' 
c------- READ END -------

c------- Make GrADS data START -------
      write(6,*) '******* Make GrADS data START *******' 
c
      do i=1,im
      do j=1,jm
      if( abs(z(i,j,km)).le.1. ) then
        el(i,j) = valmis
      end if
      enddo
      enddo 
      do k=1,klev
            call gsigtoz3
     &    ( zz,u,ulev,h0(k),im,jm,km,klev,k,valmis )
            call gsigtoz3
     &    ( zz,v,vlev,h0(k),im,jm,km,klev,k,valmis )
            call gsigtoz3
     &    ( zz,t,tlev,h0(k),im,jm,km,klev,k,valmis )
            call gsigtoz3
     &    ( zz,s,slev,h0(k),im,jm,km,klev,k,valmis )
      enddo
      do i=1,im
         do j=1,jm
            do k=1,klev
               if( abs(z(i,j,km)).le.1. ) then
                  ulev(i,j,k) = valmis
                  vlev(i,j,k) = valmis
                  tlev(i,j,k) = valmis
                  slev(i,j,k) = valmis
               else
                  tlev(i,j,k) = tlev(i,j,k) +tbias
                  if (tlev(i,j,k).lt.-20.0) tlev(i,j,k)=valmis
                  slev(i,j,k) = slev(i,j,k) +sbias
                  if (slev(i,j,k).lt.-20.0) slev(i,j,k)=valmis
               end if
            enddo
         enddo
      enddo 
c
      call degset( dxlon,dylat,xlon,ylat,xlonu,ylatu,xlonv,ylatv
     &            ,xlons,ylats,im,jm )
c
      write(ctlname,'(''h.ctl'')')
      write(dataname,'(''h.dat'')')
      call gs2dt0var
     &( z(1,1,km),im,jm,1
     & ,iedd,namemonth(iemm),ieyy
     & ,xlon,ylat
     & ,ctlname,dataname,stepday,valmis)
c
      write(ctlname,'(''el.ctl'')')
      write(dataname,'(''el.dat'')')
      call gs2dt0var
     &( el,im,jm,1
     & ,iedd,namemonth(iemm),ieyy
     & ,xlon,ylat
     & ,ctlname,dataname,stepday,valmis)
      write(ctlname,'(''t.ctl'')')
      write(dataname,'(''t.dat'')')
      call gs3dt0var
     &( tlev,im,jm,klev,1
     & ,iedd,namemonth(iemm),ieyy
     & ,xlon,ylat,h0
     & ,ctlname,dataname,stepday,valmis)
c
      write(ctlname,'(''s.ctl'')')
      write(dataname,'(''s.dat'')')
      call gs3dt0var
     &( slev,im,jm,klev,1
     & ,iedd,namemonth(iemm),ieyy
     & ,xlon,ylat,h0
     & ,ctlname,dataname,stepday,valmis)
c
      write(ctlname,'(''u.ctl'')')
      write(dataname,'(''u.dat'')')
      call gs3dt0var
     &( ulev,im,jm,klev,1
     & ,iedd,namemonth(iemm),ieyy
     & ,xlonu,ylatu,h0
     & ,ctlname,dataname,stepday,valmis)
c
      write(ctlname,'(''v.ctl'')')
      write(dataname,'(''v.dat'')')
      call gs3dt0var
     &( vlev,im,jm,klev,1
     & ,iedd,namemonth(iemm),ieyy
     & ,xlonv,ylatv,h0
     & ,ctlname,dataname,stepday,valmis)
c
      write(6,*) '******* Make GrADS data END *******' 
c------- Make GrADS data END -------
c
      write(6,*) '##### normal end #####'
c
      stop
      end
*
******************************************************************************
*
      subroutine degset
     &(dxlon,dylat,xlon,ylat,xlonu,ylatu,xlonv,ylatv,xlons,ylats,im,jm)
      dimension dxlon(im),dylat(jm),xlon(im),ylat(jm)
      dimension xlonu(im),ylatu(jm),xlonv(im),ylatv(jm)
c
      do i=1,im
      dxlon(i) = 1./12.
      enddo
c
      xlon(1) = xlons
      do i=2,im
         xlon(i) = xlon(i-1) +( dxlon(i-1)+dxlon(i) )/2.
      enddo
      xlonu(1) = xlons -dxlon(1)/2.
      do i=2,im
         xlonu(i) = xlonu(i-1) +dxlon(i-1)
      enddo
      xlonv(1) = xlons
      do i=2,im
         xlonv(i) = xlonv(i-1) +( dxlon(i-1)+dxlon(i) )/2.
      enddo
      do i=1,im
        write(6,*) 'in degset i,xlonu = ',i,xlonu(i)
      end do
c
      do j=1,jm
      dylat(j) = 1./12.
      enddo
c
      ylat(1) = ylats
      do j=2,jm
         ylat(j) = ylat(j-1) +( dylat(j-1)+dylat(j) )/2.
      enddo
      ylatu(1) = ylats
      do j=2,jm
         ylatu(j) = ylatu(j-1) +( dylat(j-1)+dylat(j) )/2.
      enddo
      ylatv(1) = ylats -dylat(1)/2.
      do j=2,jm
         ylatv(j) = ylatv(j-1) +dylat(j-1)
      enddo
      do j=1,jm
        write(6,*) 'in degset j,ylatv = ',j,ylatv(j)
      end do
c
      return
      end
*
******************************************************************************
*
      SUBROUTINE GSIGTOZ3(ZZ,T,TLEV,ZLEV,IM,JM,KM
     &                   ,klev,kk,valmis)
C-------------------------------------------------------------------
C     THIS ROUTINE LINERLY INTERPOLATES TLEV AT THE LEVEL,
C     ZLEV, FROM T LOCATED ON SIGMA LEVELS, ZZ.
C     ZLEV AND ZZ ARE NEGATIVE QUANTITIES.
C     A SEARCH IS MADE TO FIND ZZ(I,J,K) AND ZZ(I,J,K+1) WHICH
C     BRACKETS ZLEV; THEN THE INTERPOLATION IS MADE.
C     IN THE REGION < 0 and > ZZ(I,J,1) AND,  IN THE REGION,
C     < ZZ(I,J,KM-1) and > -1, DATA IS EXTRAPOLATED.
C     NOTE THAT A NEW MASK ,FSM, IS CREATED.
C
C     THE VALUES OF H SUPPLIED TO THIS SUBROUTINE SHOULD BE
C     APPROPRIATE TO THE VARIABLE T. FOR EXAMPLE, IF T IS THE
C     X-COMPONENT OF VELOCITY, H SHOULD THE AVERAGE OF DEPTH
C     AT I AND I-1.
C-------------------------------------------------------------------
      DIMENSION ZZ(IM,JM,KM),T(IM,JM,KM),TLEV(IM,JM,KLEV)
c
      do j=1,jm
      do i=1,im
         tlev(i,j,kk) = valmis
      enddo
      enddo
ccc      i = 227
ccc      j = 300
ccc      write(0,*) 'gsigoz3 zlev ',zlev
ccc      do k=1,km
ccc        write(0,*) 'zz t ',zz(i,j,k),t(i,j,k)
ccc      enddo
C
      DO 200 J=1,JM 
      DO 200 I=1,IM
      IF(ZLEV.GT.ZZ(I,J,1) .and. t(i,j,1).gt.valmis+1. .and .
     &   t(i,j,2).gt.valmis+1. ) THEN
            K=1
            TLEV(I,J,kk)=T(I,J,K)+(ZLEV-ZZ(I,J,K))
     1         *(T(I,J,K+1)-T(I,J,K))/(ZZ(I,J,K+1)-ZZ(I,J,K))
ccc            if( i.eq.227 .and. j.eq.300 ) then
ccc              write(0,*) 'gsigoz3 zlev>zz(1)'
ccc              write(0,*) 'kk k k+1 ',kk,k,k+1
ccc              write(0,*) 'zlev zz(k) zz(k+1) '
ccc     &          ,zlev,zz(i,j,k),zz(i,j,k+1)
ccc              write(0,*) 'tlev t(k) t(k+1) '
ccc     &          ,tlev(i,j,kk),t(i,j,k),t(i,j,k+1)
ccc            end if
            GO TO 200
      ENDIF
      K=1
  100 CONTINUE
C
      IF(ZLEV.LE.ZZ(I,J,K).AND.ZLEV.GE.ZZ(I,J,K+1) .and. 
     &   t(i,j,k).gt.valmis+1. .and. t(i,j,k+1).gt.valmis+1. ) THEN
              TLEV(I,J,kk)=T(I,J,K)+(ZLEV-ZZ(I,J,K))
     1           *(T(I,J,K+1)-T(I,J,K))/(ZZ(I,J,K+1)-ZZ(I,J,K))
              GO TO 200
ccc            if( i.eq.227 .and. j.eq.300 ) then
ccc              write(0,*) 'gsigoz3 zz(k)>zlev>zz(k+1)'
ccc              write(0,*) 'k kk k+1 ',k,kk,k+1
ccc              write(0,*) 'zz(k) zlev zz(k+1) '
ccc     &          ,zz(i,j,k),zlev,zz(i,j,k+1)
ccc              write(0,*) 't(k) tlev t(k+1) '
ccc     &          ,t(i,j,k),tlev(i,j,kk),t(i,j,k+1)
ccc            end if
      ELSE
              K=K+1
              IF(K.LT.(KM-1)) GO TO 100
      ENDIF
      IF(ZLEV.LE.ZZ(I,J,K).AND.ZLEV.GE.ZZ(I,J,KM) .and. 
     &   t(i,j,k).gt.valmis+1. .and. t(i,j,k-1).gt.valmis+1. ) THEN
                 TLEV(I,J,kk)=T(I,J,K-1)+(ZLEV-ZZ(I,J,K-1))
     1             *(T(I,J,K)-T(I,J,K-1))/(ZZ(I,J,K)-ZZ(I,J,K-1))
ccc            if( i.eq.227 .and. j.eq.300 ) then
ccc              write(0,*) 'gsigoz3 zz(k)>zlev>zz(km)'
ccc              write(0,*) 'k-1 k kk ',k-1,k,kk
ccc              write(0,*) 'zz(k-1) zz(k) zlev '
ccc     &          ,zz(i,j,k-1),zz(i,j,k),zlev
ccc              write(0,*) 't(k-1) t(k) tlev '
ccc     &          ,t(i,j,k-1),t(i,j,k),tlev(i,j,kk)
ccc            end if
      ENDIF
  200 CONTINUE
      RETURN
      END
*
******************************************************************************
*
      INTEGER FUNCTION JULDAY(IYYY,MONTH,DD)
C
C ********** SUBROUTINE DESCRIPTION:
C
C FINDS THE JULIAN DAY FROM A DATE.
C
C ********** ORIGINAL AUTHOR AND DATE:
C
C PRESS,FLANNERY,TEUKOLSKY,VETTERLING 1986.
C NUMERICAL RECIPES
C
C ********** REVISION HISTORY:
C
C
C ********** ARGUMENT DEFINITIONS:
C
      INTEGER IYYY,MONTH,DD
C
C NAME   IN/OUT DESCRIPTION
C
C IYYY     I    YEAR
C MONTH    I    MONTH (1 TO 12)
C DD       I    DAY OF MONTH
C JULDAY   O    JULIAN DAY
C
C ********** COMMON BLOCKS:
C
C NONE
C
C ********** LOCAL PARAMETER DEFINITIONS:
C
      INTEGER IGREG
      PARAMETER (IGREG = 15 + 31*(10 + 12*1582))
C
C ********** LOCAL VARIABLE DEFINITIONS:
C
      INTEGER JY,JM,JA
C
C NAME   DESCRIPTION
C
C
C ********** OTHER ROUTINES AND FUNCTIONS CALLED:
C
C INT    - INTRINSIC TRUNCATE
C
C---+67--1----+----2----+----3----+----4----+----5----+----6----+----7--
C
      IF (IYYY .LT. 0) IYYY = IYYY + 1
      IF (MONTH .GT. 2) THEN
        JY = IYYY
        JM = MONTH + 1
      ELSE
        JY = IYYY - 1
        JM = MONTH + 13
      ENDIF
      JULDAY = INT(365.25*JY) + INT(30.6001*JM) + DD + 1720995
      IF (DD + 31*(MONTH + 12*IYYY) .GE. IGREG) THEN
        JA = INT(0.01*JY)
        JULDAY = JULDAY + 2 - JA + INT(0.25*JA)
      ENDIF
      RETURN
      END
*
******************************************************************************
*
      SUBROUTINE CALDAT(JULIAN,IYYY,MONTH,DD)
C
C ********** SUBROUTINE DESCRIPTION:
C
C GIVEN THE JULIAN DAY, RETURNS THE YEAR, MONTH AND DAY OF MONTH.
C
C ********** ORIGINAL AUTHOR AND DATE:
C
C PRESS,FLANNERY,TEUKOLSKY,VETTERLING 1986.
C NUMERICAL RECIPES
C
C ********** REVISION HISTORY:
C
C
C ********** ARGUMENT DEFINITIONS:
C
      INTEGER JULIAN,IYYY,MONTH,DD
C
C NAME   IN/OUT DESCRIPTION
C
C JULIAN   I    THE JULIAN DAY
C IYYY     O    THE YEAR
C MONTH    O    THE MONTH (1 TO 12)
C DD       O    THE DAY OF THE MONTH
C
C ********** COMMON BLOCKS:
C
C NONE
C
C ********** LOCAL PARAMETER DEFINITIONS:
C
      INTEGER IGREG
      PARAMETER (IGREG=2299161)
C
C ********** LOCAL VARIABLE DEFINITIONS:
C
      INTEGER JALPHA,JA,JB,JC,JD,JE
C
C NAME   DESCRIPTION
C
C
C ********** OTHER ROUTINES AND FUNCTIONS CALLED:
C
C
C---+67--1----+----2----+----3----+----4----+----5----+----6----+----7--
C
C
      IF (JULIAN .GE. IGREG) THEN
        JALPHA = INT(((JULIAN - 1867216) - 0.25)/36524.25)
        JA = JULIAN + 1 + JALPHA - INT(0.25*JALPHA)
      ELSE
        JA = JULIAN
      ENDIF
      JB = JA + 1524
      JC = INT(6680. + ((JB - 2439870) - 122.1)/365.25)
      JD = 365*JC + INT(0.25*JC)
      JE = INT((JB - JD)/30.6001)
      DD = JB - JD - INT(30.6001*JE)
      MONTH = JE - 1
      IF (MONTH .GT. 12) MONTH = MONTH - 12
      IYYY = JC - 4715
      IF (MONTH .GT. 2) IYYY = IYYY - 1
      IF (IYYY .LE. 0) IYYY = IYYY - 1
      RETURN
      END
*
******************************************************************************
*
      subroutine gs2dt0var
     &( val,i0,j0,itmax,iday,cmonth,iyear
     & ,xlon,ylat
     & ,ctlname,dataname,tinc,valmis)
c
      real*4 val(i0,j0,itmax),xlon(i0),ylat(j0)
      character*16 ctlname,dataname
      character*5 tinc
      character*3 cmonth
c
      numval = 1
      kb = 1
c
c SUN/PC
c      open(50,file=dataname,form='unformatted'
c     &    ,access='direct',recl=i0*j0*4*numval)
c IRIX
      open(50,file=dataname,form='unformatted'
     &    ,access='direct',recl=i0*j0*numval)
c
      irec = 1
      do i=1,itmax
         write(50,rec=irec) ((val(j,k,i),j=1,i0),k=1,j0)
         irec = irec + 1
      enddo
      close(50)
c
      open(60,file=ctlname,form='formatted')
      write(60,1010) 'dset ',dataname
      write(60,1000) 'options big_endian'
      write(60,1000) 'title val'
      if( valmis.gt.-1.e+3) then
          write(60,1020) 'undef ',valmis
      else
          write(60,1000) 'undef -1.e+21'
      end if
      write(60,2000) 'xdef ',i0,' levels '
     & ,(xlon(i), i=1,i0)
      write(60,2000) 'ydef ',j0,' levels '
     & ,(ylat(i), i=1,j0)
      write(60,3000) 'zdef ',kb,' linear 0 1.'
      write(60,4100) 'tdef ',itmax,' linear ',iday,cmonth,iyear,tinc
      write(60,1000) 'vars 1'
      write(60,1000) 'val 1 0 val'
      write(60,1000) 'endvars'
      close(60)
c
      return
c
 1000 format(a)
 1010 format(a,a16)
 1020 format(a,f8.4)
 2000 format(a,i4,a,/,6(1x,f12.6))
 3000 format(a,i4,a)
ccc 4100 format(a,i4,a,i4,1x,a)
 4100 format(a,i4,a,i2,a3,i4,1x,a)
      end
*
************************************************************************
*
      subroutine gs3dt0var
     &( val,i0,j0,k0,itmax,iday,cmonth,iyear
     & ,xlon,ylat,zz     
     & ,ctlname,dataname,tinc,valmis)
c
      real*4 val(i0,j0,k0,itmax),xlon(i0),ylat(j0),zz(k0)
      character*16 ctlname,dataname
      character*5 tinc
      character*3 cmonth
c
      numval = 1
c
c SUN/PC
c      open(50,file=dataname,form='unformatted'
c     &    ,access='direct',recl=i0*j0*k0*4*numval)
c IRIX
      write(6,*) 'open ',dataname
      open(50,file=dataname,form='unformatted'
     &    ,access='direct',recl=i0*j0*k0*numval)
c
      irec = 1
      do 100 m=1,itmax
         write(50,rec=irec)
     &   (((val(j,k,i,m),j=1,i0),k=1,j0),i=k0,1,-1)
         irec = irec + 1
  100 continue 
      close(50)
c
      open(60,file=ctlname,form='formatted')
      write(60,1010) 'dset ',dataname
      write(60,1000) 'title val'
      write(60,1000) 'options big_endian'
      write(60,1020) 'undef ',valmis
      if( i0.gt.1 ) then
        write(60,2000) 'xdef ',i0,' levels '
     & ,(xlon(i), i=1,i0)
      else
        write(60,2001) 'xdef ',i0,' linear ',xlon(1),1.0
      end if
      if( j0.gt.1 ) then
        write(60,2000) 'ydef ',j0,' levels '
     & ,(ylat(i), i=1,j0)
      else
        write(60,2001) 'ydef ',j0,' linear ',ylat(1),1.0
      end if
      if( k0.gt.1 ) then
        write(60,2100) 'zdef ',k0,' levels '
     & ,(zz(k), k=k0,1,-1)
      else
        write(60,2002) 'zdef ',k0,' linear ',zz(1),1.0
      end if
      write(60,4100) 'tdef ',itmax,' linear ',iday,cmonth,iyear,tinc
      write(60,1000) 'vars 1'
      write(60,3000) 'val ',k0,' 0 val'
      write(60,1000) 'endvars'
      close(60)
c
      return
c
 1000 format(a)
 1010 format(a,a16)
 1020 format(a,f8.4)
 2000 format(a,i4,a,/,6(1x,f10.6))
 2001 format(a,i4,a,6(1x,f10.6))
 2002 format(a,i4,a,6(1x,f10.1))
 2100 format(a,i4,a,/,6(1x,f10.3))
 3000 format(a,i4,a)
 4100 format(a,i4,a,i2,a3,i4,1x,a)
      end

