MODULE str_mod
implicit none
private ::NUM_bit
integer ,parameter :: NUM_bit =3
character,parameter ::  DIVIDER='/'
contains
subroutine str_get_selffile(i,j,m,DIRNAME,FILENAME)
integer,intent(in) :: i,j,m
character(len=*),intent(in) :: DIRNAME
character(len=80),intent(out) :: FILENAME

character(len=NUM_bit) :: name1,name2,name3
character(len=80) :: subname 
call str_get_name(i,3,name1)
call str_get_name(j,2,name2)
call str_get_name(m,2,name3)
subname = trim(DIRNAME)//trim(name1)
FILENAME=trim(subname)//DIVIDER//'C_'//trim(name1)//'_'//trim(name2)//'_'//trim(name3)//'.dat'
end subroutine  str_get_selffile

subroutine str_get_cofile(i,l,j,m,DIRNAME,FILENAME)
integer,intent(in) :: i,l,j,m
character(len=*),intent(in) :: DIRNAME
character(len=80),intent(out) :: FILENAME

character(len=NUM_bit) :: name1,name2,name3,name4
character(len=80) :: subname 
call str_get_name(i,2,name1)
call str_get_name(l,2,name2)
call str_get_name(j,2,name3)
call str_get_name(m,2,name4)
subname = trim(DIRNAME)//trim(name1)//'_'//name2
FILENAME=trim(subname)//DIVIDER//'C_'//trim(name1)//'_'//trim(name2)//'_'//trim(name3)//'_'//trim(name4)//'.dat'
end subroutine  str_get_cofile


subroutine str_get_name(x,x_mask,strname)
! SUB_NAME   :: get_name
! intent-in  :: x,x_bit
! intent-out :: xname
! USAGE :: to change a 3-bit integer into a string
! NOTICE:: if x is actually 2-bit then we should use trim in the calling proc
integer,intent(in) :: x,x_mask
character(len=NUM_bit),intent(out) :: strname

integer :: numrec(3)
  numrec(1) = abs(x)/100 
  numrec(2) = (abs(x)-numrec(1)*100)/10
  numrec(3) = x-numrec(1)*100-numrec(2)*10
  strname  = char(numrec(1)+48)//char(numrec(2)+48)//char(numrec(3)+48)
  if (x_mask==2) then 
     strname  = char(numrec(2)+48)//char(numrec(3)+48)
  end if 
end subroutine str_get_name

subroutine str_get_pattern(pmode,times,PATTERN)
character(len=*),intent(in) :: pmode
integer ,intent(in) :: times
character(len=100000),intent(out) :: PATTERN
integer :: k
    PATTERN=')';
    do k=1,times
       PATTERN=pmode//','//PATTERN 
    end do
    PATTERN='('//PATTERN
end subroutine str_get_pattern
end module str_mod
