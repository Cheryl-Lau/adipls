      subroutine set_rotation(x, nn0)
c
c  sets angular velocity into common/comgrp/, for calculation of
c  rotational splitting. 
c  This is intended as a user modifiable routine. The present version
c  simply reads in the x and the angular velocity from a file and 
c  interpolates to the computational mesh which must be provided in
c  the argument x(n), n = 1, ..., nn.
c  The argument icontr may be used to control the action of the routine
c  if needed.
c
c  The file name is read from standard input. The file is assumed to be
c  in ASCII format, and structured as
c
c       r/R   omega
c
c  Note that in the present version the angular velocity is read in
c  only in the first call of set_rotation.
c
c  Original version: 15/2/06
c

      include 'adipls.c.d.incl'  ! parameter (nnmax = 20000)
      implicit double precision (a-h, o-z)
      character(len=280) :: omega_file
      integer :: n, nn, nnr, nn0, ierr, init_rot, inter
!      dimension x(1)
      real, dimension(1) :: x
!      dimension xr(nnmax), omegar(nnmax)
      real, dimension(11) :: xr, omegar  
      real, dimension(nnmax) :: omgrtp
    
!  common for storing angular velocity, to be passed to rotker 
!  omgrtp array of length 1 - omgrtp(1)
      common/comgrp/ isprtp, irotcp, omgrtp
!  for now, store omgrtp as array of length nnmax in common
c
c  common defining standard input and output
      common/cstdio/ istdin, istdou, istdpr, istder
c
      save
c
      data init_rot /0/
c
      write(*,*) 'Subroutine set_rotation called' 
c


      write(istdou,'(/'' Entering set_rotation'')')

      if(istdpr.gt.0.and.istdpr.ne.istdou)
     *  write(istdpr,'(/'' Entering set_rotation'')')

      if(init_rot.eq.0) then
!        read(istdin,'(a)') omega_file
!        if(istdpr.gt.0) write(istdpr,'(/'' Read rotation from '',a)')
!     *    omega_file
!        close(99)
c
	write(*,*) 'init_rot=0 begin set_rotation'

      end if 

        omega_file = 'set_angular_velocity.txt' 

      ! Count number of points to get nn, added by me 
!!        open(99,file=omega_file,status='old')
!!	write(*,*) 'opening omega_file to read nn'
!!	nn = 0 
!!	do
!!	  read(99,'(2f4.1)',iostat=ierr)
!!	  write(*,*) 'iostat value=', ierr
!	  if (ierr.ne.0) stop  
!!	  nn = nn + 1 
!!        end do 
!!	if (nn.gt.nnmax) then 
!!	  write(*,*) 'number of points exceeded nnmax'
!	  stop 
!!	close(99)
!!	write(*,*) 'nn=', nn

      ! nn refers to total number of mesh points in model 

      ! Read the data 
	open(99,file=omega_file, status='old') 
	write(*,*) 'opening omega_file to read data'

        call skpcom(99)  ! (what is this)
c
        read_omega_file_loop: do n=1,11

	  read(99,'(2f4.1)',iostat=ierr,end=20) xr(n), omegar(n)
!	  write(*,*) 'iostat value=', ierr
	  if (ierr.lt.0) then 
	    write(*,*) 'End of file error - attempted to read beyond' 
	    write(*,*) 'the last record of stream file'
!	    stop 
	  else if (ierr.gt.0) then 
	    write(*,*) 'omega_file not found'
!	    stop
	  else if (ierr.eq.0) then  
	    write(*,'(2f4.1)') xr(n), omegar(n)  ! checking 
	  end if 
c
        end do read_omega_file_loop 

  20    continue      
c
        nnr=n-1           
c
!        n=1
!   10   read(99,'(2f4.1)',end=20) xr(n), omegar(n)
!        n=n+1
!        go to 10
c
!   20   nnr=n-1
        close(99)
c
	init_rot=1
        if(istdpr.gt.0) write(istdpr,'(/'' Rotation rate read at '',i5,
     *    '' points in s/r set_rotation''/)') nnr
c
	write(*,'(/'' Rotation rate read at '',i5,
     *    '' points in s/r set_rotation''/)') nnr


c
c  interpolate to mesh in x
c
      write(*,*) 'Begin interpolating to mesh'
      do n=1,nn
	write(*,*) 'calling lir'
!	call lir(x(n),xr,omgrtp(n),omegar,1,1,nnr,n,inter)
	call lir(x(n),xr,omgrtp(n),omegar,nn0,11,nnr,n,1)
!       call lir(z,zi,y,yi,ii,id,nt,l,inter)
      end do

      write(*,*) 'Show me omgrtp after calling lir in set_rotation'
      test_print: do n=1,5
	write(*,*) omgrtp(n)
      end do test_print

      write(*,*) 'Exiting set_rotation'
      write(istdou,'(/'' Exiting set_rotation'')')
      if(istdpr.gt.0.and.istdpr.ne.istdou)
     *  write(istdpr,'(/'' Exiting set_rotation'')')

      return

      end

