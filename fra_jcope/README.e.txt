The media stores following files.

1. README.e.txt (this file)

2. JCOPE data (5 types; '********' denotes YearMonthDay)
 2.1 EL_********: Sea Surface Height
 2.2 T_******** : Potential Temperature
 2.3 S_******** : Salinity
 2.4 U_******** : Zonal velocity
 2.5 V_******** : Meridional velocity

3. basic.dat: topography and coordinate 

Data format is fortran binary with single precision and big-endian.

1. size of array
      paramater( im=122,jm=86,km=47 )
im: number of zonal direction, jm: number of meridional direction, km: number of vertical levels

2. topography and coordinate

reading format:
    open(10,file='basic.dat',form='unformatted')
    read(10) fildsc,z,zz,dz,ichflg

bottom depth of a cell(i,j): z(i,j,km) 
depth of a cell edge:  z(i,j,k)   
depth of a cell center: zz(i,j,k); depth of velocity, temperature, salinity
height of a cell (i,j,k): dz(i,j,k) = z(i,j,k)-z(i,j,k+1)
file description (a4): fildsc
read flag(=123456,i4): ichflg

3. data

The JCOPE data is consists of following 5 file types:
T_: Temperature - 10. (deg.C)
S_: Salinity - 35. (psu)
U_: zonal velocity (m/s)
V_: meridional velocity (m/s)
EL_: sea surface height  (m)

Variables were averaged in 1 day. For example, T_20021105 were averaged from 
the temperature sampled with a time step of 8 minutes during a period from 
0:00, Nov 4, 2002 to 0:00, Nov 5, 2002.

A reading format of 'EL_********' is as follows:
             open(10,file=datadir(1:lnblnk(datadir))
     &             //'EL_'//cyear//cmonth//cday ; e.g., EL_20021105
     &             ,form='unformatted')
             read(10) fildscEL,iyf,imf,idf,el,mall

file description (a4): fildscEL
year (i4): iyf
month (i4): imf
day (i4): idf
sea surface height (r4): el(im,jm)
read flag (=180) (i4): mall

A reading format of 'T(S,U,V)_********' is as follows:
             open(10,file=datadir(1:lnblnk(datadir))
     &             //'T_'//cyear//cmonth//cday ; e.g., T_20021105
     &             ,form='unformatted')
             read(10) fildscT,iyf,imf,idf,(((T(i,j,k),i=1,im),j=1,jm),k=1,km-1),mall

file description (a4): fildscT
year (i4): iyf
month (i4): imf
day (i4): idf
sea surface height (r4): T(im,jm,km) ; variable at level=km is not used.
read flag (=180) (i4): mall

+++++

A sample program 'test_sample.f' can be compiled using 'ifort' command.

$ifort -convert big_endian test_sample.f
A loadmodule 'a.out' will be created.
Following lines in program 'test_sample.f' may be modified if you use this program.

1.       datadir='./'
2.       isyy = 1992
         ismm = 11
         isdd = 02
         ieyy = 1992
         iemm = 11
         iedd = 02 
3.       depthfile='./basic.dat'

If you use 'GrADS' for visualizing variables, you had better check a following matter.

in subroutine gs2dt0var

c SUN/PC
c      open(50,file=dataname,form='unformatted'
c     &    ,access='direct',recl=i0*j0*4*numval)
c IRIX
      open(50,file=dataname,form='unformatted'
     &    ,access='direct',recl=i0*j0*numval)

also, in subroutine gs3dt0var
c SUN/PC
c      open(50,file=dataname,form='unformatted'
c     &    ,access='direct',recl=i0*j0*4*numval)
c IRIX
      open(50,file=dataname,form='unformatted'
     &    ,access='direct',recl=i0*j0*numval)

The sample program simply puts all variables on the same position
 in GrADS output; users can superimpose velocities on temperature/
salinity using u.ctl, v.ctl and t.ctl. 

However, the model (POM) uses staggered grid.
The sample program specifies following variables: 
xlon(i): longitude of i-th cell's center (position of EL, T, S)
ylat(j): latitude of j-th cell's center (position of EL, T, S)
xlonu(i): longitude of i-th cell's western edge (position of U)
ylatu(j): latitude of j-th cell's western edge (position of U)
xlonv(i): longitude of i-th cell's southern edge (position of V)
ylatv(j): latitude of j-th cell's southern edge (position of V)

(request)

If you refer the JCOPE data in documents, you would be requested that you also refer our documents:

Miyazawa, Y., R. Zhang, X. Guo, H. Tamura, D. Ambe, J.-S. Lee, A. Okuno, H. Yoshinari, T. Setou, and K. Komatsu, 2009: Water mass variability in the western North Pacific detected in a 15-year eddy resolving ocean reanalysis, J. Oceanogr., in press. 

E-mail: miyazawa@jamstec.go.jp
http://www.jamstec.go.jp/frcgc/jcope/
