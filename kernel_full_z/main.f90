!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! note to change the (NUM_CFZ) after changing the number of kernels you will compute
! here we try to output the kernel data into a single file instead of antedones
!
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      program main
      use nrtype
      use str_mod
	implicit none
	integer i,j,k,l,m,n
	integer  len, wid, maxtime
	real*8  p,CFL,C311,C322,vp,vs
	real*8, allocatable, dimension (:,:,:,:)  ::  Cmtx,temp
	character(len=3) ::namesub
	character(len=80) :: FILENAME,kerneldir,kernelname 
	common  /basic/ CFL,p

!(1)--------------- Input basic data -------------------
  open(10,file=trim(INPUTDIR)//'input_z.dat')    
  read(10,*) vp
  read(10,*) vs
  read(10,*) 
  read(10,*) 
  read(10,*) CFL
  read(10,*) 
  read(10,*)
  read(10,*)
  read(10,*)
  read(10,*) len
  read(10,*)
  read(10,*) wid
  read(10,*)
  read(10,*)
  read(10,*) maxtime
  read(10,*) kerneldir
  read(10,*) kernelname
  close(10)
	p=vs/vp

  allocate( Cmtx(0:len, 0:wid,0:maxtime,1:NUM_CFZ) )
   allocate( temp(0:len, 0:wid,0:maxtime,1:NUM_CFZ) )
!----------------初始化、计算积分核-----------------
      print*,'computing kernels...'

      do k=0,maxtime
       write(*,'("+",a, i8)') 'k=',k
       do j=0, wid
        do i=0, len
	   Cmtx(i,j,k,1)=C311(i,j,k,0,0,0)  !!strike
!           Cmtx(i,j,k,1)=C322(i,j,k,0,0,0) !! dip
	  end do
	 end do
	end do
!         temp=Cmtx
!         do j=0, wid
!         do i=0, len
!         do k=4,maxtime-5
!               Cmtx(i,j,k,1)=(temp(i,j,k-2,1)+temp(i,j,k-1,1)+&
!               &temp(i,j,k,1)+temp(i,j,k+1,1)+temp(i,j,k+2,1))/5d0
!         enddo
!         enddo
!         enddo
      
!---------------------------------------------------------------
      print*,' writing kernels.....'
      call system(trim('mkdir -p '//kerneldir))
   open(29,file=trim(kerneldir)//trim(kernelname),form='binary')
       write(29) Cmtx(:,:,:,1)
   close(29)
!  Mon Mar 26 15:31:57 CST 2007 to output the data into a single file

!      do i = 0, len
!      call str_get_name(i,3,namesub)
!      call system('mkdir -p '//trim(FULLSELF)//trim(namesub))
!         do j = 0, wid
!           write(*,'("+",2(a,i8))') 'i=',i,',   j=', j
!           do m = 0, wid
!             call str_get_selffile(i,j,m,FULLSELF,FILENAME)
!~~~~ output in binary
!             open(29, file=trim(FILENAME),form='binary')
!             do k = 0, maxtime
!                write(29) Cmtx(i,j-m,k,1:NUM_CFZ)
!             end do
!~~~~ output in ascii
!             open(29, file=trim(FILENAME))
!             do k = 0, maxtime
!                write(29,'(e20.10)') Cmtx(i,j-m,k,1:NUM_CFZ)
!             end do
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
!
!             close(29)
!           end do
!         end do
!      end do
!!----------------------------------------------------------------            

	end

