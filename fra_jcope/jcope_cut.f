c===============================================================
c
c     name: jcope_cut.f
c
c     read data files & make data for generalized sigma data
c
c     2006.11-  Miyazawa, Y.
c     2004.01-  Miyazawa, Y.
c     2003.12-  Kawai, K. 
c     (ORG 2001.12-  Maeda, T. (gs.nest46.v2.f))
c
c===============================================================
c
      program main
c
c------------------------------
      parameter( im=864+2, jm=618+2, km=47 )
c------------------------------
      common /domain/ z(im,jm,km),zz(im,jm,km),dz(im,jm,km)
     &               ,fsm(im,jm),dum(im,jm),dvm(im,jm),d(im,jm)
     &               ,dx(im,jm),dy(im,jm)
      common /coord/
     & dxlon(im),dylat(jm)
     &,xlon(im),ylat(jm)
     &,xlonu(im),ylatu(jm)
     &,xlonv(im),ylatv(jm)
      common /orgdata2d/ el(im,jm),wtsurf(im,jm),wssurf(im,jm)
     &                ,uab(im,jm),vab(im,jm),psi(im,jm)
      common /orgdata3d/ u(im,jm,km),v(im,jm,km),t(im,jm,km),s(im,jm,km)
c
      character*80 datadir,depthfile,outdir,outdepthfile,logfile
      character*40 ctlname,dataname
c
      character cyear*4,cmonth*2,cday*2
      character*4 fildscH,fildscEL,fildscU,fildscV,fildscT,fildscS
c
c------- SETTING START -------
      datadir =   '/shio/zhangr/nest80/nest80ani2.hc_mirc_fra_wod_new/'
      depthfile = '/shio/zhangr/nest80/nest80ani2.hc_mirc_fra_wod_new/basic.dat'
      outdir = './'
      outdepthfile =
     & './basic.dat'
      logfile =
     & './jcope_cut.prn'
c
c      write(*,*) 'isyy,ismm,isdd'
c      read (*,*) isyy,ismm,isdd
c      ieyy=isyy
c      iemm=ismm
c      iedd=isdd
      isyy=2014
      ismm=12
      isdd=24
      ieyy=2018
      iemm=12
      iedd=09
c
      xlonmin = 132.
      xlonmax = 142.
      ylatmin = 29.
      ylatmax = 36.
c
      intday = 1
c------- SETTING END -------
c
c------- GET START --------
      open(99,file=logfile,form='formatted')
      write(99,*) '******* GET START *******' 
      call getregion
     &( xusampmax,xusampmin,ixlonumaxnum,ixlonuminnum
     & ,xlonmin,xlonmax
     & ,xlon,xlonu,xlonv,dxlon,im
     & ,ylatmin,ylatmax
     & ,yvsampmax,yvsampmin,iylatvmaxnum,iylatvminnum
     & ,ylat,ylatu,ylatv,dylat,jm )
      write(99,*) '******* GET END *******' 
      write(99,*) 'new isize: ',ixlonumaxnum-ixlonuminnum+1
      write(99,*) 'new jsize: ',iylatvmaxnum-iylatvminnum+1
c------- GET END -------
c
c-------- READ/WRITE START ---------
      write(99,*) '******* READ/WRITE START *******' 
c
      open(10,file=depthfile,form='unformatted')
      read(10) fildscH,z,zz,dz,ichflg
      close(10)
      write(99,*) 'read ',fildscH,ichflg
c
      open(10,file=outdepthfile,form='unformatted')
      write(10) fildscH
     &     ,(((z(i,j,k)
     &      ,i=ixlonuminnum,ixlonumaxnum),j=iylatvminnum,iylatvmaxnum)
     &      ,k=1,km)
     &     ,(((zz(i,j,k)
     &      ,i=ixlonuminnum,ixlonumaxnum),j=iylatvminnum,iylatvmaxnum)
     &      ,k=1,km)
     &     ,(((dz(i,j,k)
     &      ,i=ixlonuminnum,ixlonumaxnum),j=iylatvminnum,iylatvmaxnum)
     &      ,k=1,km)
     &      ,123456
      close(10)
      write(99,*) 'write basic data '
c
      timejde = julday(ieyy,iemm,iedd)
      iyy = isyy
      imm = ismm
      idd = isdd
      timejd = julday(iyy,imm,idd)
c
      numfilmx = int(timejde -timejd)/intday +1
      write(99,*) '### numfilmx = ',numfilmx 
c
      do 250 numfil=1,numfilmx
c
             write(cyear,'(i4.4)') iyy
             write(cmonth,'(i2.2)') imm
             write(cday,'(i2.2)') idd
c-----------------------------------------------------------
             open(10,file=datadir(1:lnblnk(datadir))
     &             //'EL_'//cyear//cmonth//cday
     &          ,form='unformatted')
             read(10) fildscEL,iyf,imf,idf,el,mall
             close(10)
             write(99,*) '### read(EL_) ',fildscEL,iyf,imf,idf,mall
c-----------------------------------------------------------
             open(10,file=datadir(1:lnblnk(datadir))
     &             //'U_'//cyear//cmonth//cday
     &          ,form='unformatted')
             read(10) 
     &        fildscU,iyf,imf,idf
     &       ,(((U(i,j,k),i=1,im),j=1,jm),k=1,km-1),mall
             close(10)
             write(99,*) '### read(U_) ',fildscU,iyf,imf,idf,mall
c-----------------------------------------------------------
             open(10,file=datadir(1:lnblnk(datadir))
     &             //'V_'//cyear//cmonth//cday
     &          ,form='unformatted')
             read(10) 
     &        fildscV,iyf,imf,idf
     &       ,(((V(i,j,k),i=1,im),j=1,jm),k=1,km-1),mall
             close(10)
             write(99,*) '### read(V_) ',fildscV,iyf,imf,idf,mall
c-----------------------------------------------------------
             open(10,file=datadir(1:lnblnk(datadir))
     &             //'T_'//cyear//cmonth//cday
     &          ,form='unformatted')
             read(10) 
     &        fildscT,iyf,imf,idf
     &       ,(((T(i,j,k),i=1,im),j=1,jm),k=1,km-1),mall
             close(10)
             write(99,*) '### read(T_) ',fildscT,iyf,imf,idf,mall
c-----------------------------------------------------------
             open(10,file=datadir(1:lnblnk(datadir))
     &             //'S_'//cyear//cmonth//cday
     &          ,form='unformatted')
             read(10) 
     &        fildscS,iyf,imf,idf
     &       ,(((S(i,j,k),i=1,im),j=1,jm),k=1,km-1),mall
             close(10)
             write(99,*) '### read(S_) ',fildscS,iyf,imf,idf,mall
c
c-----------------------------------------------------------
             open(50,file=outdir(1:lnblnk(outdir))
     &              //'EL_'//cyear//cmonth//cday
     &       ,form='unformatted')
             write(50) fildscEL,iyf,imf,idf
     &      ,((el(i,j),i=ixlonuminnum,ixlonumaxnum)
     &      ,j=iylatvminnum,iylatvmaxnum),mall
             close(50)
             write(99,*) '### write(EL_) ',fildscEL,iyf,imf,idf,mall
c-----------------------------------------------------------
             open(50,file=outdir(1:lnblnk(outdir))
     &              //'U_'//cyear//cmonth//cday
     &       ,form='unformatted')
             write(50) fildscU,iyf,imf,idf
     &      ,(((U(i,j,k),i=ixlonuminnum,ixlonumaxnum)
     &      ,j=iylatvminnum,iylatvmaxnum),k=1,km-1),mall
             close(50)
             write(99,*) '### write(U_) ',fildscU,iyf,imf,idf,mall
c-----------------------------------------------------------
             open(50,file=outdir(1:lnblnk(outdir))
     &             //'V_'//cyear//cmonth//cday
     &       ,form='unformatted')
             write(50) fildscV,iyf,imf,idf
     &      ,(((V(i,j,k),i=ixlonuminnum,ixlonumaxnum)
     &      ,j=iylatvminnum,iylatvmaxnum),k=1,km-1),mall
             close(50)
             write(99,*) '### write(V_) ',fildscV,iyf,imf,idf,mall
c-----------------------------------------------------------
             open(50,file=outdir(1:lnblnk(outdir))
     &             //'T_'//cyear//cmonth//cday
     &       ,form='unformatted')
             write(50) fildscT,iyf,imf,idf
     &      ,(((T(i,j,k),i=ixlonuminnum,ixlonumaxnum)
     &      ,j=iylatvminnum,iylatvmaxnum),k=1,km-1),mall
             close(50)
             write(99,*) '### write(T_) ',fildscT,iyf,imf,idf,mall
c-----------------------------------------------------------
             open(50,file=outdir(1:lnblnk(outdir))
     &             //'S_'//cyear//cmonth//cday
     &       ,form='unformatted')
             write(50) fildscS,iyf,imf,idf
     &      ,(((S(i,j,k),i=ixlonuminnum,ixlonumaxnum)
     &      ,j=iylatvminnum,iylatvmaxnum),k=1,km-1),mall
             close(50)
             write(99,*) '### write(S_) ',fildscS,iyf,imf,idf,mall
c
             timejd = timejd +intday
             call caldat(int(timejd),iyy,imm,idd)
c
             call flush(99)
c
 250  continue
      write(99,*) '******* READ/WRITE END *******' 
c------- READ/WRITE END ------- 
c
      write(0,*) '##### normal end #####'
      write(99,*) '##### normal end #####'
c
      stop
      end
*
******************************************************************************
*
      subroutine getregion
     &( xusampmax,xusampmin,ixlonumaxnum,ixlonuminnum
     & ,xlonmin,xlonmax
     & ,xlon,xlonu,xlonv,dxlon,im
     & ,ylatmin,ylatmax
     & ,yvsampmax,yvsampmin,iylatvmaxnum,iylatvminnum
     & ,ylat,ylatu,ylatv,dylat,jm )
      dimension 
     & xlon(im),xlonu(im),xlonv(im),dxlon(im)
     &,ylat(jm),ylatu(jm),ylatv(jm),dylat(jm)
c
      xlons = 108.
      ylats =  10.5
c
      do i=1,im
      dxlon(i) = 1./12.
      enddo
      xlon(1) = xlons -dxlon(1)/2.
      do i=2,im
         xlon(i) = xlon(i-1) +( dxlon(i-1)+dxlon(i) )/2.
        write(99,*) 'CAL(xlon i) ',xlon(i),i
      enddo

      xlonu(1) = xlons -dxlon(1)
      do i=2,im
         xlonu(i) = xlonu(i-1) +dxlon(i-1)
      enddo

      iflg = 0
      ixlonumaxnum = 0
      ixlonuminnum = 0

      do i=1,im
      if ( xlon(i) .gt. xlonmax .AND. iflg .eq. 0) then
         ixlonumaxnum = i
         xusampmax = xlon(i)
         iflg = 1
      end if

      if ( xlon(i) .le. xlonmin ) then
         ixlonuminnum = i
         xusampmin = xlon(i)
      end if
      enddo
      if( ixlonuminnum.eq.0 .or. ixlonumaxnum.eq.0 ) then
        write(99,*) '$$$ error: xlonmin xlonmax ',xlonmin,xlonmax
        stop
      else
        write(99,*) 'CAL(xlonMAX,xlonMIN) ',xusampmax,xusampmin
        write(99,*) 'CAL(xlonMAX,xlonMIN) ',ixlonumaxnum,ixlonuminnum
        write(99,*) 'Please set xlons = ', nint(xlonu(ixlonuminnum+1)) 
     &                                   ,' -1./24.'        
      end if

      xlonv(1) = xlons -dxlon(1)/2.
      do i=2,im
         xlonv(i) = xlonv(i-1) +( dxlon(i-1)+dxlon(i) )/2.
      enddo

      do j=1,jm
      dylat(j) = 1./12.
      enddo
      ylat(1) = ylats -dylat(1)/2.
      do j=2,jm
         ylat(j) = ylat(j-1) +( dylat(j-1)+dylat(j) )/2.
      enddo

      ylatu(1) = ylats -dylat(1)/2.
      do j=2,jm
         ylatu(j) = ylatu(j-1) +( dylat(j-1)+dylat(j) )/2.
      enddo

      ylatv(1) = ylats -dylat(1)
      do j=2,jm
         ylatv(j) = ylatv(j-1) +dylat(j-1)
      enddo

      iflg = 0
      iylatvminnum = 0
      iylatvmaxnum = 0
      do j=1,jm
      if ( ylat(j) .gt. ylatmax .AND. iflg .eq. 0) then
         iylatvmaxnum = j
         yvsampmax = ylat(j)
         iflg = 1
      end if
      if ( ylat(j) .le. ylatmin ) then
         iylatvminnum = j
         yvsampmin = ylat(j)
      end if
      enddo
      if( iylatvminnum.eq.0 .or. iylatvmaxnum.eq.0 ) then
        write(99,*) '$$$ error: ylatmin ylatmax ',ylatmin,ylatmax
        stop
      else
        write(99,*) 'CAL(ylatMAX,ylatMIN) ',yvsampmax,yvsampmin
        write(99,*) 'CAL(ylatMAX,ylatMIN) ',iylatvmaxnum,iylatvminnum
        write(99,*) 'Please set ylats = ', nint(ylatv(iylatvminnum+1)) 
     &                                   ,' -1./24.'      
      end if
c
      return
      end
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

