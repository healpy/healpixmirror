program process_mask

  use healpix_types
  use extension, only: getArgument, nArguments
  use fitstools, only: getsize_fits, input_map, output_map
  use head_fits, only: add_card, write_minimal_header
  use pix_tools, only: convert_ring2nest, convert_nest2ring, nside2npix
  USE paramfile_io, only: paramfile_handle, parse_init, parse_int, &
         parse_string, parse_double, parse_summarize, parse_check_unused, parse_finish !, concatnl, scan_directories
  use misc_utils, only: assert_alloc, fatal_error, wall_clock_time !, brag_openmp, string
!  use newmask
  use mask_tools
  implicit none

  integer(I4B) :: nbordpix, status, n_args
  integer(I4B) :: nside, npix, order, np_th
  integer(I4B), allocatable, dimension(:) :: mask
  real(DP), allocatable, dimension(:,:) :: map, map2
  character(len=filenamelen) :: file_mask, file_bord, file_dist, file_filled, file_dist2
  character(len=80), dimension(1:200) :: header
  real(DP):: surface_deg, surf_pix_deg
  
  character(len=*), parameter :: code = 'process_mask'
  real(kind=SP)                 :: clock_time, time0, time1, t0, t1
  real(kind=SP)                 :: ptime, ptime0, ptime1, pt0, pt1
  type(paramfile_handle)     :: handle
  character(len=FILENAMELEN) :: parafile

!------------------------------------------------
  call wall_clock_time(time0)
  call cpu_time(ptime0)
!   file_mask = '/tmp/mask_kp0.fits'
!   file_bord = '!/tmp/mask_border.fits'
!   file_dist = '!/tmp/mask_dist_th.fits'
!   file_newmask = '!/tmp/mask_kp0_th.fits'
!   surface_deg = 4.d0

  n_args = nArguments()
  parafile = ''

  if (n_args > 1) then
     stop 'Expected 0 or 1 argument'
  endif
  if (n_args == 1) then
     call getArgument(1, parafile)
  endif
  handle = parse_init(parafile)
  
  file_mask = parse_string(handle,'file_mask',descr='Input mask file',filestatus='old')
  !file_bord = parse_string(handle,'file_bord',descr='Output border file',filestatus='new',default="''")
  file_dist = parse_string(handle,'file_dist',descr='Output distance file',filestatus='new',default="''")
  file_dist2 = parse_string(handle,'file_dist2',descr='Output distance file 2',filestatus='new',default="''")
  file_filled = parse_string(handle,'file_filled',descr='Output filled mask',filestatus='new',default="''")
  surface_deg = parse_double(handle,'surface_deg',descr='[Deg^2] largest surface of hole to be ignored',default=0.d0)

  !call parse_check_unused(handle, code=code)
  call parse_summarize(handle,code=code)
  call parse_finish(handle)
  !call brag_openmp()

  ! input mask
  npix = getsize_fits(file_mask, nside=nside, ordering=order)
  print*, trim(file_mask)
  print*, npix, nside, order
  allocate(map(0:npix-1,1:1),stat=status)
  call assert_alloc(status,code,'map')
  call input_map(file_mask, map, npix, 1)

  surf_pix_deg = fourpi/npix * rad2deg**2
  np_th = NINT(surface_deg/surf_pix_deg)
  print*,'hole threshold:',np_th

  if (order == 1) then
     print*,'Converting RING -> NESTED!!'
     call convert_ring2nest(nside, map)
     print*,'conversion done'
  endif
  allocate(mask(0:npix-1),stat=status)
  call assert_alloc(status,code,'mask')
  mask = NINT(map(:,1))
  print*,'SUM:',sum(mask)

  !----------------------------------------------------------------------
  ! generate new mask by filling small holes
  !----------------------------------------------------------------------
  call fill_holes_nest(nside, np_th, mask, mask)
  print*,'SUM:',sum(mask)

  if (trim(file_filled) /= '') then
     ! output new mask
     map(:,1) = mask
     if (order == 1) call convert_nest2ring(nside, map)
     
     print*, trim(file_filled)
     !header(:) = ' '
     call write_minimal_header(header,'MAP',&
          &   nside=nside, &
          &   order=order, &
          &   creator=code, &
          &   version=healpix_version)
     call add_card(header,'HISTORY','Hole filler')
     call add_card(header,'HISTORY',trim(file_mask))
     call add_card(header,'SURFMIN',surface_deg,'[Deg^2]')
     call add_card(header,'NP_MIN',np_th,'pixels')
     call output_map(map, header, file_filled)
  endif

  !----------------------------------------------------------------------
  ! compute distance to nearest hole on new mask
  !----------------------------------------------------------------------


  if (trim(file_dist) /= '') then

     allocate(map2(0:npix-1,1:1),stat=status)
     call wall_clock_time(t0)
     call cpu_time(pt0)
     map2 = 0.0_dp
     call assert_alloc(status,code,'map2')
     call dist2holes_nest(nside, mask, map2(:,1))
     !call dist2holes_nest_new(nside, mask, map2(:,1), algo=1, nslow=64, dimtest=1)
     !call dist2holes_nest_new(nside, mask, map2(:,1), algo=1, nslow=16, dimtest=1)
     call wall_clock_time(t1)
     call cpu_time(pt1)
     print*,'times=',t1-t0,pt1-pt0
     print*,minval(map2(:,1)),maxval(map2(:,1))

     if (order == 1) call convert_nest2ring(nside, map2)

     print*, trim(file_dist)
     ! header(:) = ' '
     call write_minimal_header(header,'MAP',&
          &   nside=nside, &
          &   order=order, &
          &   creator=code, &
          &   version=healpix_version)
     call add_card(header,'HISTORY','Distance to closest hole for')
     call add_card(header,'HISTORY',trim(file_filled))
     !call add_card(header,'HISTORY','new code')
     call add_card(header,'SURFMIN',surface_deg,'[Deg^2]')
     call add_card(header,'NP_MIN',np_th,'pixels')
     call output_map(map2, header, file_dist)
  endif


!   if (trim(file_dist2) /= '') then
!      call wall_clock_time(t0)
!      call cpu_time(pt0)
!      map = 0.0_DP
! !     call dist2holes_nest_old(nside, mask, map(:,1))
! !     call dist2holes_nest_new(nside, mask, map(:,1), algo=1, nslow=16, dimtest=4)
! !     call dist2holes_nest_new(nside, mask, map(:,1), algo=-1, nslow=32)
! !     call dist2holes_nest_new(nside, mask, map(:,1), algo=0, nslow=16)
!      call dist2holes_nest_new(nside, mask, map(:,1), algo=-1)

!      call wall_clock_time(t1)
!      call cpu_time(pt1)
!      print*,'times=',t1-t0,pt1-pt0
!      print*,minval(map(:,1)),maxval(map(:,1))
!      if (allocated(map2)) then
!         print*,'----------------------------------'
!         print*,minval(map2(:,1)-map(:,1)),maxval(map2(:,1)-map(:,1))
!         print*,'----------------------------------'
!      endif

!      if (order == 1) call convert_nest2ring(nside, map)

!      print*, trim(file_dist2)
!      header(:) = ' '
!      call add_card(header,'HISTORY','Distance to closest hole for')
!      call add_card(header,'HISTORY',trim(file_filled))
!      call add_card(header,'HISTORY','old code')
!      call add_card(header,'NSIDE',nside)
!      call add_card(header,'SURFMIN',surface_deg,'[Deg^2]')
!      call add_card(header,'NP_MIN',np_th,'pixels')
!      if (order == 1) call add_card(header,'ORDERING','RING')
!      if (order == 2) call add_card(header,'ORDERING','NESTED')
!      call output_map(map, header, trim(file_dist2))
!   endif

  call wall_clock_time(time1)
  call cpu_time(ptime1)

  print*,time1-time0,ptime1-ptime0

stop
end program process_mask

