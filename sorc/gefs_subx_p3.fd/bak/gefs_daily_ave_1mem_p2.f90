program daily_ave_acc
! main program: daily_ave_acc 
! Author: Wei Li :2016-12-20
! REF: Yali Mao, Bo Cui
! Purpose: calculate daily mean and accumulation (00Z,06Z,12Z,18Z), (12Z-12Z) 
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
!    pdt_ugrd100  = pdt_t0(1, 2, 2, 4, 100, 0, 10000)
!    pdt_vgrd100  = pdt_t0(1, 2, 3, 4, 100, 0, 10000), &
!    pdt_ugrd10m  = pdt_t0(1, 2, 2, 4, 103, 0, 10), &
!    pdt_vgrd10m  = pdt_t0(1, 2, 3, 4, 103, 0, 10), &
!    pdt_tmax2m   = pdt_t0(11,0, 4, 4, 103, 0, 2), &
!    pdt_tmin2m   = pdt_t0(11,0, 5, 4, 103, 0, 2), &
!    pdt_shtfl    = pdt_t0(11,0,11, 4,   1, 0, 0), &
!    pdt_lhtfl    = pdt_t0(11,0,10, 4,   1, 0, 0), &
!    pdt_prmsl    = pdt_t0(1, 3, 1, 4, 101, 0, 0), &
!    pdt_weasd    = pdt_t0(1, 1,13, 4,   1, 0, 0), &
!    pdt_ulwrf    = pdt_t0(11,5,193,4,   1, 0, 0), &
!    pdt_dlwrf    = pdt_t0(11,5,192,4,   1, 0, 0), &
!    pdt_uswrf    = pdt_t0(11,4,193,4,   1, 0, 0), &
!    pdt_dswrf    = pdt_t0(11,4,192,4,   1, 0, 0), &
!    pdt_snod     = pdt_t0( 1,1,11, 4,   1, 0, 0), &
!    pdt_soilw1   = pdt_t0(1, 0,192,4, 106, 2, 0), &
!    pdt_cape     = pdt_t0(1, 7, 6, 4, 108, 0,18000) 
!now in pgrb2a
!    pdt_spfh850  = pdt_t0(1, 1, 0, 4, 100, 0, 85000)
!    pdt_vvel500  = pdt_t0(1, 2, 8, 4, 100, 0, 50000)
!    pdt_dpt      = pdt_t0(1, 0, 6, 4, 103, 0, 2)
!    pdt_soilw2   = pdt_t0(1, 0,192,4,106,  2, 10) 
!    pdt_soilw3   = pdt_t0(1, 0,192,4,106,  2, 40)
!    pdt_soilw4   = pdt_t0(1, 0,192,4,106,  2, 100)
!    pdt_uflx     = pdt_t0(11,2, 17,4,  1,  0, 0)
!    pdt_vflx     = pdt_t0(11,2, 18,4,  1,  0, 0)
!    pdt_snowc    = pdt_t0(1, 1, 42,4,  1,  0, 0)
!    pdt_icec     = pdt_t0(1, 2,  0,4,  1,  0, 0)

integer :: nfield
parameter(nfield=27) !not include SNOWC and ICEC

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

!  columns:   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
data pdt%npdt/1,  1,  1,  1,  11, 11, 11, 11, 1,  1, 11, 11, 11, 11,  1,  1,  1,  1,  1,  1,  1,  1,  1, 11, 11, 1, 1/   ! Product Definition Template Number
data pdt%icat/2,  2,  2,  2,   0,  0,  0,  0, 3,  1,  5,  5,  4,  4,  1,  0,  7,  1,  2,  0,  0,  0,  0,  2,  2, 1, 2/  ! Parameter Category by Product Discipline
data pdt%iprm/2,  3,  2,  3,  4,  5, 11, 10,  1, 13,193,192,193,192, 11,192,  6,  0,  8,  6,192,192,192, 17, 18,42, 0/     ! Parameter Number by Product Discipline and Parameter Category
data pdt%igp /4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4, 4, 4/    ! Type of Generating Process
data pdt%iffs/100,100,103,103,103,103,1,  1,101,  1,  1,  1,  1,  1,  1,106,108,100,100,103,106,106,106,  1,  1, 1, 1/   ! Type of first fixed surface 
data pdt%isf /0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  2,  2,  2,  0,  0, 0, 0/   ! Scale factor of first fixed surface
data pdt%isv/10000,10000,10,10,2, 2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,18000,85000,50000,2,10,40,100,0,  0, 0, 0/    !Scaled value of first fixed surface

integer :: n_time
parameter(n_time=141)
integer :: j,jpdtn,jgdtn
integer,dimension(200) :: jids,jpdt,jgdt
logical :: unpack=.true.
integer :: jdisc      ! discipline#, table 0.0(met:0 hydro:1 land:2)
integer :: day,month,year,hour,fhour
character(len=256) :: datafile(n_time),outfile,file_dir,out_dir
character(len=8) :: file_date
character(len=5) :: ens_mem
integer :: unit, nx,ny,iret,jret,i,k,ifh,nfi,maxgrd,ifd,ivar,ind,icount,ens_id
character(len=3) ::sfh,smn
character(len=2) ::sdy
character(len=4) ::syr
character(len=8) :: pabbrev
character(len=30) :: labbrev
character(len=30) :: labbrev_short

real, allocatable :: sdes_save_daily(:,:),netr_save_daily(:,:),mrso_save_daily(:,:) 
real, allocatable :: weasd_save_daily(:,:),snod_save_daily(:,:) 
real, allocatable :: soilw1_save_daily(:,:),soilw2_save_daily(:,:),soilw3_save_daily(:,:),soilw4_save_daily(:,:) 
real, allocatable :: ulwrf_save_daily(:,:),uswrf_save_daily(:,:),dlwrf_save_daily(:,:),dswrf_save_daily(:,:) 
real, allocatable :: var(:,:) ! 
real, allocatable :: var_save(:,:) ! 
real, allocatable :: var_save_daily(:,:) ! (maxgrd,35,nen)

!datafile(1)='/gpfs/hps/emc/ensemble/noscrub/Wei.Li/SCRIPT/SubX/gefs.20141001/00/gep_APCP.2014100100.grb2'

call GET_COMMAND_ARGUMENT(1, file_date)
call get_environment_variable("file_dir", file_dir)
call get_environment_variable("out_dir", out_dir)
call get_environment_variable("ens_mem", ens_mem)
print *,'file dir:',trim(file_dir)


do ivar=1,nfield !number of required fields
!do ivar=1,3 !number of required fields

icount=0
!do iens=1,n_ensem !iensemble member
!do iens=1,1 !ensemble member

   nfi=0
   do ifh=0,840,6  !foercast hours

     nfi=nfi+1
     if (ifh.le.99) then
        write(sfh,'(i2.2)') ifh
     else
        write(sfh,'(i3.3)') ifh
     endif

!search in either pgrb2a and pgrb2b
        datafile(nfi)=trim(file_dir)//'/gefs.'//trim(file_date)//'/00/pgrb2a/'//trim(ens_mem)//'.t00z.pgrb2af'//trim(sfh)
!        datafile(nfi)=trim(file_dir)//'/gefs.'//trim(file_date)//'/pgrb2a/'//trim(ens_mem)//'.t00z.pgrb2af'//trim(sfh)

     unit = 100

     call BAOPENR(unit, datafile(nfi), iret)

     if(iret /= 0 ) then
       write(*,*) "there is no GEFS forecast",datafile(nfi)

     else
     !  write(*,*) "check file= ", trim(datafile(nfi))
     end if

     !if(allocated(var)) deallocate(var)
     !if(allocated(var_save)) deallocate(var_save)

     j = 0
     jids=-9999;jpdt=-9999; jgdt=-9999
     jdisc=-1; jgdtn=-1

     jpdtn = pdt%npdt(ivar)  !template version num. 
     jpdt(1)  = pdt%icat(ivar)
     jpdt(2)  = pdt%iprm(ivar)
     jpdt(3)  = pdt%igp(ivar)
     jpdt(10) = pdt%iffs(ivar)
     jpdt(11) = pdt%isf(ivar)
     jpdt(12) = pdt%isv(ivar)
!
!     call init_parm(ipdtn,ipdt,igdtn,igdt,idisc,iids)
! for template 4.11, no varuable in f00
     if (jpdtn == 11 .and. ifh == 0 )then 
         goto 201
     end if
     call getgb2(unit,0,j,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,unpack,j,gfld,iret)
     if (iret /= 0) then
        write(*,*) "reading file iret=", iret
        stop
     end if

     !   print *, gfld%ipdtmpl
     maxgrd=gfld%ngrdpts
     nx = gfld%igdtmpl(8)
     ny = gfld%igdtmpl(9)
! assign gfldo same arribute for gfld
     gfldo=gfld
     !   print *, size(gfld%fld(:)) !'N->S'

     if(.not. allocated(var)) allocate(var(nx,ny))
     var(:,:) = reshape(gfld%fld(:),[nx,ny])

     if(.not. allocated(var_save)) allocate(var_save(maxgrd,n_time))
     var_save(:,nfi) = gfld%fld
     !   print *, gfld%fld 
     !   print *, maxval(var_save(:,nfi)) 

     !   write(*,*) "nx,ny=",nx,ny
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
     !write(*,*), 'yymmddhh:', year,month,day,hour,fhour

     pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),gfld%ipdtmpl(2))
     call prlevel(gfld%ipdtnum,gfld%ipdtmpl,labbrev)

     deallocate(var)
     call gf_free(gfld)
     call BACLOSE(unit, iret)


 201 end do !ifh
     !write(*,*) "total files for one member:",nfi : should be 141

!open files for output grib2
     labbrev_short=trim(labbrev)
     print *,pabbrev//trim(labbrev_short(1:4))
!     outfile=trim(out_dir)//trim(pabbrev)//trim(labbrev_short(2:4))//'_'//ens_mem//'.grb2'
     if(pabbrev=='SPFH'.and.trim(labbrev_short(1:4)).eq.' 850') then
outfile=trim(out_dir)//'huss_850_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='VVEL'.and.trim(labbrev_short(1:4)).eq.' 500') then
outfile=trim(out_dir)//'wap_500_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='UGRD'.and.trim(labbrev_short(1:4)).eq.' 100') then
outfile=trim(out_dir)//'ua_100_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='VGRD'.and.trim(labbrev_short(1:4)).eq.' 100') then
outfile=trim(out_dir)//'va_100_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='UGRD'.and.trim(labbrev).eq.'10 m above ground') then
        outfile=trim(out_dir)//'ua_10m_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='VGRD'.and.trim(labbrev).eq.'10 m above ground') then
        outfile=trim(out_dir)//'va_10m_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='TMAX'.and.trim(labbrev).eq.'2 m above ground') then
        outfile=trim(out_dir)//'tasmax_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='TMIN'.and.trim(labbrev).eq.'2 m above ground') then
        outfile=trim(out_dir)//'tasmin_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='DPT'.and.trim(labbrev).eq.'2 m above ground') then
        outfile=trim(out_dir)//'tdps_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='SHTFL') then
        outfile=trim(out_dir)//'hfss_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='LHTFL') then
        outfile=trim(out_dir)//'hfls_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='PRMSL') then
        outfile=trim(out_dir)//'psl_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='WEASD') then
        outfile=trim(out_dir)//'swe_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='SNOD') then
        outfile=trim(out_dir)//'snod_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='CAPE') then
        outfile=trim(out_dir)//'cape_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='SOILW'.and.trim(labbrev).eq.' 0 - .10 m DBLY') then
        outfile=trim(out_dir)//'soilw1_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='SOILW'.and.trim(labbrev).eq.'.10 - .40 m DBLY') then
        outfile=trim(out_dir)//'soilw2_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='SOILW'.and.trim(labbrev).eq.'.40 -  1 m DBLY') then
        outfile=trim(out_dir)//'soilw3_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='SOILW'.and.trim(labbrev).eq.' 1 -  2 m DBLY') then
        outfile=trim(out_dir)//'soilw4_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='ULWRF') then
        outfile=trim(out_dir)//'ulwrf_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='USWRF') then
        outfile=trim(out_dir)//'uswrf_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='DLWRF') then
        outfile=trim(out_dir)//'dlwrf_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='DSWRF') then
        outfile=trim(out_dir)//'dswrf_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='UFLX'.and.trim(labbrev_short(1:4)).eq.' Sur') then
        outfile=trim(out_dir)//'stx_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='VFLX'.and.trim(labbrev_short(1:4)).eq.' Sur') then
        outfile=trim(out_dir)//'sty_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='UNKNOWN'.and.trim(labbrev_short(1:4)).eq.' Sur') then
        outfile=trim(out_dir)//'snc_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if
     if(pabbrev=='ICEC'.and.trim(labbrev_short(1:4)).eq.' Sur') then
        outfile=trim(out_dir)//'sic_sfc_GEFS_'//sdy//smn//syr//'_00z_d01_d35_m'//ens_mem(4:5)//'.grb2'
     end if

     print *,outfile
     call baopenwa(200,outfile,iret) ! for add more than 1 members

     if(.not. allocated(var_save_daily)) allocate(var_save_daily(maxgrd,35))
     gfldo%fld=-999999.

!calculate the daily ave/acc
     do ind=1,35   !35 day forecast
      icount=icount+1
!daily avaerage 00z,06z,12z,18z
      if(pabbrev=='HGT'.or.pabbrev=='UGRD'.or.pabbrev=='VGRD'.or.pabbrev=='VVEL'.or.pabbrev=='PRMSL'.or.pabbrev=='WEASD'.or.pabbrev=='DPT'.or.pabbrev=='SOILW'.or.pabbrev=='UNKNOWN'.or.pabbrev=='ICEC'.or.pabbrev=='SNOD'.or.pabbrev=='SPFH'.or.pabbrev=='CAPE') then
!        if (ind==1) then
!            print *,'AVG(00,06,12,18Z) var:',pabbrev,trim(labbrev)
!        end if
        var_save_daily(:,ind)=(var_save(:,1+(ind-1)*4)+var_save(:,2+(ind-1)*4)+var_save(:,3+(ind-1)*4)+var_save(:,ind*4))/4.         
      end if
!WWLL
!           if(pabbrev=='SOILW'.and.ind==1) then
!              print *, var_save
!           end if
!WWLL
!daily max 00z,06z,12z,18z
      if(pabbrev=='TMAX'.and.trim(labbrev).eq.'2 m above ground') then
!        if (ind==1) then
!            print *,'MAX(00,06,12,18Z) var:',pabbrev,trim(labbrev)
!        end if
        var_save_daily(:,ind)=maxval(var_save(:,2+(ind-1)*4:ind*4+1),dim=2)         
      end if
!daily min 00z,06z,12z,18z
      if(pabbrev=='TMIN'.and.trim(labbrev).eq.'2 m above ground') then
!        if (ind==1) then
!            print *,'MIN(00,06,12,18Z) var:',pabbrev,trim(labbrev)
!        end if
       var_save_daily(:,ind)=minval(var_save(:,2+(ind-1)*4:ind*4+1),dim=2)         
      end if
!daily accumulation 0-24z
      if(pabbrev=='ULWRF'.or.pabbrev=='DLWRF'.or.pabbrev=='USWRF'.or.pabbrev=='DSWRF'.or.pabbrev=='SHTFL'.or.pabbrev=='LHTFL'.or.pabbrev=='UFLX'.or.pabbrev=='VFLX') then
!        if (ind==1) then
!            print *,'ACC(06,12,18,24Z) var:',pabbrev,trim(labbrev)
!            print *, 2+(ind-1)*4,ind*4+1
!        end if
!Changed 20170505
!        var_save_daily(:,ind)=sum(var_save(:,2+(ind-1)*4:ind*4+1),dim=2)         
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
!    write(*,'(1i5,a10,a20,6f15.2)') ivar,pabbrev,trim(labbrev),var_save(10000,1:5),var_save_daily(10000,1)
!    write(*,'(1i5,a10,a20,7f15.2)') ivar,pabbrev,trim(labbrev),var_save(10000,136:141),var_save_daily(10000,5)

!end do !iens

!gfldo%fld=var_save_daily(:,ind)

deallocate(var_save)
deallocate(var_save_daily)
end do !ivar

end program daily_ave_acc
