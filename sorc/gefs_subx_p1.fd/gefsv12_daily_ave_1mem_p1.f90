program daily_ave_acc
! main program: daily_ave_acc 
! Author: Wei Li :2016-12-20
! Wei Li: 2020-05-07: Modified to use getarg to get input data, made the readin
! forecast hours flexible 

! REF: Yali Mao, Bo Cui
! Purpose: calculate daily mean and accumulation (00Z,06Z,12Z,18Z), (0Z-24Z) 
! 
! usage: daily_ave_acc.exe yyyymmdd
!
!   input file: ncep/cmc/fnmoc ensemble forecast                                          
!
!   output file: daily mean and accumulation

! programs called:
!   baopenr          grib i/o
!   baopenw          grib i/o
!   baclose          grib i/o
!   getgb2           grib reader
!   putgb2           grib writer
!   init_parm        define grid definition and product definition
!   printinfr        print grib2 data information
!   change_template4 change data values for specified Product Definition Template

! exit states:
!   cond =   0 - successful run
!   cond =   1 - I/O abort
!
! attributes:
!   language: fortran 90
!
!$$$

use grib_mod
use params
implicit none

type(gribfield) :: gfld,gfldo

!type pdt_t0 
!    integer :: npdt0   ! Product Definition Template Number
!    integer :: icat0   ! Parameter Category by Product Discipline 
!    integer :: iprm0   ! Parameter Number by Product Discipline and Parameter Category
!    integer :: igp0    ! Type of Generating Process
!    integer :: iffs0   ! Type of first fixed surface 
!    integer :: isf0    ! Scale factor of first fixed surface
!    integer :: isv0    !Scaled value of first fixed surface
!end type pdt_t0
!
!! PDT parameters in the input GRIB2 file (template 4 number, category, parameter, type of level)
!type(pdt_t0), parameter :: &
!in pgrb2a
!    pdt_hgt500   = pdt_t0(1, 3, 5, 4, 100, 0, 50000), &
!    pdt_hgt200   = pdt_t0(1, 3, 5, 4, 100, 0, 20000), &
!    pdt_ugrd200  = pdt_t0(1, 2, 2, 4, 100, 0, 20000), &
!    pdt_ugrd850  = pdt_t0(1, 2, 2, 4, 100, 0, 85000), &
!    pdt_vgrd200  = pdt_t0(1, 2, 3, 4, 100, 0, 20000), &
!    pdt_vgrd850  = pdt_t0(1, 2, 3, 4, 100, 0, 85000), &
!    pdt_t2m      = pdt_t0(1, 0, 0, 4, 103, 0, 2), &
!    pdt_apcp     = pdt_t0(11,1, 8, 4,   1, 0, 0), &
!    pdt_ulwrf_top= pdt_t0(11,5,193,4,   8, 0, 0), &
!now in pgrb2a
!    pdt_tsfc     = pdt_t0(1, 0, 0, 4, 1,   0, 0)
!    pdt_prate     = pdt_t0(11,1, 7, 4,   1, 0, 0), &

integer :: nfield
parameter(nfield=11)

type pdt_t
    integer,dimension(nfield):: npdt  ! Product Definition Template Number
    integer,dimension(nfield):: icat  ! Parameter Category by Product Discipline
    integer,dimension(nfield):: iprm  ! Parameter Number by Product Discipline and Parameter Category
    integer,dimension(nfield):: igp   ! Type of Generating Process
    integer,dimension(nfield):: iffs  ! Type of first fixed surface 
    integer,dimension(nfield):: isf   ! Scale factor of first fixed surface
    integer,dimension(nfield):: isv   !Scaled value of first fixed surface
end type pdt_t

type(pdt_t) :: pdt

data pdt%npdt/1,  1,  1,  1,  1,  1,  1, 11, 11,  1,  11/  ! Product Definition Template Number
data pdt%icat/3,  3,  2,  2,  2,  2,  0,  1,  5,  0,   1/  ! Parameter Category by Product Discipline
data pdt%iprm/5,  5,  2,  2,  3,  3,  0,  8,193,  0,   7/  ! Parameter Number by Product Discipline and Parameter Category
data pdt%igp /4,  4,  4,  4,  4,  4,  4,  4,  4,  4,   4/  ! Type of Generating Process
data pdt%iffs/100,100,100,100,100,100,103,1,  8,  1,   1/  ! Type of first fixed surface 
data pdt%isf /0,  0,  0,  0,  0,  0,  0,  0,  0,  0,   0/  ! Scale factor of first fixed surface
data pdt%isv/50000,20000,20000,85000,20000,85000,2,0,0,0,0/  !Scaled value of first fixed surface

integer :: n_time
parameter(n_time=141)
integer :: j,jpdtn,jgdtn
integer,dimension(200) :: jids,jpdt,jgdt
logical :: unpack=.true.
integer :: jdisc      ! discipline#, table 0.0(met:0 hydro:1 land:2)
integer :: day,month,year,hour,fhour,nfhmax
character(len=256) :: datafile(n_time),outfile
character(len=5) :: ens_mem,fhmax
integer :: unit, nx,ny,iret,jret,i,k,ifh,nfi,maxgrd,ifd,ivar,ind,icount,ens_id
character(len=3) ::sfh,smn
character(len=2) ::sdy
character(len=4) ::syr
character(len=8) :: pabbrev
character(len=30) :: labbrev
character(len=30) :: labbrev_short

real, allocatable :: var(:,:) 
real, allocatable :: var_save(:,:) ! (maxgrd,nfd,nen)
real, allocatable :: var_save_daily(:,:) ! (maxgrd,35,nen)

call getarg(1,ens_mem)
call getarg(2,fhmax)
read(fhmax,'(i3)') nfhmax

do ivar=1,nfield !number of required fields

icount=0

   nfi=0
   do_ifh_loop: do ifh=0,nfhmax,6  !foercast hours

     nfi=nfi+1
     write(sfh,'(i3.3)') ifh
     datafile(nfi)=trim(ens_mem)//'.t00z.subx.1p0.f'//trim(sfh)
!        print *, datafile(nfi)
     unit = nfi

     call BAOPENR(unit, datafile(nfi), iret)

     if(iret /= 0 ) then
       write(*,*) "there is no GEFS forecast",datafile(nfi)
     end if

     j = 0
     jids=-9999
     jids=-9999;jpdt=-9999; jgdt=-9999
     jdisc=-1; jgdtn=-1
     
     jpdtn = pdt%npdt(ivar)  !template version num. 
     jpdt(1)  = pdt%icat(ivar)
     jpdt(2)  = pdt%iprm(ivar)
     jpdt(3)  = pdt%igp(ivar)
     jpdt(10) = pdt%iffs(ivar)
     jpdt(11) = pdt%isf(ivar)
     jpdt(12) = pdt%isv(ivar)

! for template 4.11, no varuable in f00
     if (jpdtn == 11 .and. ifh == 0)then 
          cycle do_ifh_loop
     end if
!
     
     call getgb2(unit,0,j,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,unpack,j,gfld,iret)
     if (iret /= 0) then
        write(*,*) "reading file iret=", iret
        stop
     end if

!     print *, gfld%ipdtmpl
     maxgrd=gfld%ngrdpts
     nx = gfld%igdtmpl(8)
     ny = gfld%igdtmpl(9)
! assign gfldo same arribute for gfld
     gfldo=gfld
!        print *, size(gfld%fld(:)) !'N->S'

     if(.not. allocated(var)) allocate(var(nx,ny))
     var(:,:) = reshape(gfld%fld(:),[nx,ny])

     if(.not. allocated(var_save)) allocate(var_save(maxgrd,n_time))
     var_save(:,nfi) = gfld%fld
!        print *, gfld%fld 
!        print *, maxval(var_save(:,nfi)) 

!        write(*,*) "nx,ny=",nx,ny
! date
     year  = gfld%idsect(6)
     month = gfld%idsect(7)
     day   = gfld%idsect(8)
     hour  = gfld%idsect(9)
     write(syr,'(i4.4)') year
     write(sdy,'(i2.2)') day
     if (month.eq.1) then
         smn='jan'
     end if
     if (month.eq.2) then
         smn='feb'
     end if
     if (month.eq.3) then
         smn='mar'
     end if
     if (month.eq.4) then
         smn='apr'
     end if
     if (month.eq.5) then
         smn='may'
     end if
     if (month.eq.6) then
         smn='jun'
     end if
     if (month.eq.7) then
         smn='jul'
     end if
     if (month.eq.8) then
         smn='aug'
     end if
     if (month.eq.9) then
         smn='sep'
     end if
     if (month.eq.10) then
         smn='oct'
     end if
     if (month.eq.11) then
         smn='nov'
     end if
     if (month.eq.12) then
         smn='dec'
     end if
!- forecast hour
     fhour=gfld%ipdtmpl(9)
     ens_id=gfld%ipdtmpl(17)
     pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),gfld%ipdtmpl(2))
     call prlevel(gfld%ipdtnum,gfld%ipdtmpl,labbrev)
!     print *, "after prlevel"

     deallocate(var)
!WWLL: 4/15/2020 comments of for dell
!     call gf_free(gfld)
     call BACLOSE(unit, iret)


 enddo do_ifh_loop !ifh

!open files for output grib2
     labbrev_short=trim(labbrev)
     print *,pabbrev,trim(labbrev_short(1:4))
     outfile=trim(pabbrev)//trim(labbrev_short(2:4))//'_'//ens_mem(4:5)//'.grb2'
     if(pabbrev=='TMP'.and.trim(labbrev).eq.'2 m above ground') then
        outfile='tas_2m_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='APCP') then
        outfile='pr_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='PRATE') then
        outfile='prate_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='TMP'.and.trim(labbrev_short(1:4)).eq.' Sur') then
        outfile='ts_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='HGT'.and.trim(labbrev_short(1:4)).eq.' 500') then
        outfile='zg_500_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='HGT'.and.trim(labbrev_short(1:4)).eq.' 200') then
        outfile='zg_200_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='UGRD'.and.trim(labbrev_short(1:4)).eq.' 200') then
        outfile='ua_200_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='UGRD'.and.trim(labbrev_short(1:4)).eq.' 850') then
        outfile='ua_850_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='VGRD'.and.trim(labbrev_short(1:4)).eq.' 200') then
        outfile='va_200_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='VGRD'.and.trim(labbrev_short(1:4)).eq.' 850') then
        outfile='va_850_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='ULWRF'.and.trim(labbrev_short(1:4)).eq.' Nom') then
        outfile='rlut_toa_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if

     print *,outfile
     call baopenwa(200,outfile,iret) ! for add more than 1 members

     if(.not. allocated(var_save_daily)) allocate(var_save_daily(maxgrd,35))
     gfldo%fld=-999999.

!calculate the daily ave/acc
     do ind=1,35   !35 day forecast
      icount=icount+1

!daily avaerage 00z,06z,12z,18z
      if(pabbrev=='HGT'.or.pabbrev=='UGRD'.or.pabbrev=='VGRD') then
        var_save_daily(:,ind)=(var_save(:,1+(ind-1)*4)+var_save(:,2+(ind-1)*4)+var_save(:,3+(ind-1)*4)+var_save(:,ind*4))/4.
      end if

!20200506
      if(pabbrev=='TMP') then
        if (ind==1) then
            print *,'AVG(00,06,12,18Z) var:',pabbrev,trim(labbrev)
        end if
        var_save_daily(:,ind)=(var_save(:,1+(ind-1)*4)+var_save(:,2+(ind-1)*4)+var_save(:,3+(ind-1)*4)+var_save(:,ind*4))/4.    
      end if
!daily accumulation 0-24z
      if(pabbrev=='APCP') then
        var_save_daily(:,ind)=sum(var_save(:,2+(ind-1)*4:ind*4+1),dim=2)         
      end if
      if(pabbrev=='ULWRF'.or.pabbrev=='PRATE') then
        var_save_daily(:,ind)=(var_save(:,2+(ind-1)*4)+var_save(:,3+(ind-1)*4)+var_save(:,ind*4)+var_save(:,ind*4+1))/4.    
      end if
!reasign gfldo%fld as daily
     gfldo%fld=var_save_daily(:,ind)

!reassign the ipdtmpl
     gfldo%ipdtmpl(17)=ens_id !!!! ensemble member: iens=0 ->CTL
     gfldo%ipdtmpl(9)=ind !forecast time
     gfldo%ipdtmpl(8)=2   !change hours to day (time unit)
!change template from 4.11 to 4.1
     if (jpdtn == 11 )then 
        gfldo%ipdtnum=1
     end if
     call putgb2(200,gfldo,jret)
!     call printinfr(gfldo,icount)

    end do !ind
    call gf_free(gfldo)
    write(*,'(1i5,a10,a20,6f15.2)') ivar,pabbrev,trim(labbrev),var_save(100,1:5),var_save_daily(100,1)

deallocate(var_save)
deallocate(var_save_daily)
end do !ivar

end program daily_ave_acc
