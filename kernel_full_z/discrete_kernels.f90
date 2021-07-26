
!c 2002年5月5日
!c 为了与Madariaga et al. (1998)的例子比较而做了修改，只保留了需要的积分核。
!c 程序里已经采用了相应的归一化因子。      

!c################################################################
!c    求积分核C_311^ijklmn
!c################################################################

      real*8 function C311(i,j,k,l,m,n)

	implicit none 
	integer i,j,k,l,m,n
	real*8  Sp11,Sp22,Tp11,A,c,p,vp,vs,CFL
        common  /basic/ CFL, p

	c=2.*p**3
	vp=1./p
	vs=1.0
	C311= c*Sp11(i,j,k,l,m,n,vp)-2.*Sp11(i,j,k,l,m,n,vs)      &
     	 -Sp22(i,j,k,l,m,n,vs)-c*Tp11(i,j,k,l,m,n,vp)      &
          +2.*Tp11(i,j,k,l,m,n,vs)-A(i,j,k,l,m,n,vs)  !!!! 5月14日改正



	return
	end
!c################################################################

      real*8 function C322(i,j,k,l,m,n)

	implicit none 
	integer i,j,k,l,m,n
	real*8  Sp11,Sp22,Tp22,A,c,p,vp,vs,CFL
      common  /basic/ CFL, p

	c=2.*p**3
	vp=1./p
	vs=1.0
	C322= c*Sp22(i,j,k,l,m,n,vp)-2.*Sp22(i,j,k,l,m,n,vs)      &
     	 -Sp11(i,j,k,l,m,n,vs)-c*Tp22(i,j,k,l,m,n,vp)      &
          +2.*Tp22(i,j,k,l,m,n,vs)-A(i,j,k,l,m,n,vs)  !!!! 5月14日改正

	return
	end
!c################################################################

	real*8 function Sp11(i,j,k,l,m,n,c)
	
	implicit none
      integer i,j,k,l,m,n,t1,t2
	real*8  c,Sp011,x11,x12,x21,x22

	x11=l-0.5
	x12=l+0.5
	x21=m-0.5
	x22=m+0.5
	t1=n-1
	t2=n
	Sp11=Sp011(i,j,k,x11,x21,t1,c)-Sp011(i,j,k,x11,x22,t1,c)      &
         -Sp011(i,j,k,x12,x21,t1,c)+Sp011(i,j,k,x12,x22,t1,c)      &
         -Sp011(i,j,k,x11,x21,t2,c)+Sp011(i,j,k,x11,x22,t2,c)      &
         +Sp011(i,j,k,x12,x21,t2,c)-Sp011(i,j,k,x12,x22,t2,c)
      
	return
	end
!c--------------------------------------------------------------------
	real*8 function Sp22(i,j,k,l,m,n,c)
	
	implicit none
      integer i,j,k,l,m,n,t1,t2
	real*8  c,Sp022,x11,x12,x21,x22

	x11=l-0.5
	x12=l+0.5
	x21=m-0.5
	x22=m+0.5
	t1=n-1
	t2=n
	Sp22=Sp022(i,j,k,x11,x21,t1,c)-Sp022(i,j,k,x11,x22,t1,c)      &
         -Sp022(i,j,k,x12,x21,t1,c)+Sp022(i,j,k,x12,x22,t1,c)      &
         -Sp022(i,j,k,x11,x21,t2,c)+Sp022(i,j,k,x11,x22,t2,c)      &
         +Sp022(i,j,k,x12,x21,t2,c)-Sp022(i,j,k,x12,x22,t2,c)
      
	return
	end
!c--------------------------------------------------------------------
	real*8 function Tp11(i,j,k,l,m,n,c)
	
	implicit none
      integer i,j,k,l,m,n,t1,t2
	real*8  c,Tp011,x11,x12,x21,x22

	x11=l-0.5
	x12=l+0.5
	x21=m-0.5
	x22=m+0.5
	t1=n-1
	t2=n
	Tp11=Tp011(i,j,k,x11,x21,t1,c)-Tp011(i,j,k,x11,x22,t1,c)      &
         -Tp011(i,j,k,x12,x21,t1,c)+Tp011(i,j,k,x12,x22,t1,c)      &
         -Tp011(i,j,k,x11,x21,t2,c)+Tp011(i,j,k,x11,x22,t2,c)      &
         +Tp011(i,j,k,x12,x21,t2,c)-Tp011(i,j,k,x12,x22,t2,c)
      
	return
	end
!c--------------------------------------------------------------------
	real*8 function Tp22(i,j,k,l,m,n,c)
	
	implicit none
      integer i,j,k,l,m,n,t1,t2
	real*8  c,Tp022,x11,x12,x21,x22

	x11=l-0.5
	x12=l+0.5
	x21=m-0.5
	x22=m+0.5
	t1=n-1
	t2=n
	Tp22=Tp022(i,j,k,x11,x21,t1,c)-Tp022(i,j,k,x11,x22,t1,c)      &
         -Tp022(i,j,k,x12,x21,t1,c)+Tp022(i,j,k,x12,x22,t1,c)      &
         -Tp022(i,j,k,x11,x21,t2,c)+Tp022(i,j,k,x11,x22,t2,c)      &
         +Tp022(i,j,k,x12,x21,t2,c)-Tp022(i,j,k,x12,x22,t2,c)
      
	return
	end
!c--------------------------------------------------------------------
	real*8 function A(i,j,k,l,m,n,c)
	
	implicit none
      integer i,j,k,l,m,n,t1,t2
	real*8  c,A0,x11,x12,x21,x22

	x11=l-0.5
	x12=l+0.5
	x21=m-0.5
	x22=m+0.5
	t1=n-1
	t2=n
	A=A0(i,j,k,x11,x21,t1,c)-A0(i,j,k,x11,x22,t1,c)      &
      -A0(i,j,k,x12,x21,t1,c)+A0(i,j,k,x12,x22,t1,c)      &
      -A0(i,j,k,x11,x21,t2,c)+A0(i,j,k,x11,x22,t2,c)      &
      +A0(i,j,k,x12,x21,t2,c)-A0(i,j,k,x12,x22,t2,c)
      
	return
	end
!c####################################################################

	real*8 function Sp011(i,j,k,K1,K2,n,c)
      
	implicit none
	integer cs,i,j,k,n
	real*8  c,K1,K2,CFL,p
	real*8  Y_zeta,X_zeta,kapa_i,Z

      common  /basic/ CFL, p
	common  /kernel/ Y_zeta,X_zeta,Z

      call cases_judge(cs,1,i,j,k,K1,K2,n,c)
	if(cs.eq.1) then
	  Sp011=0.
	else if(cs.eq.2) then
	  Sp011=(i-K1)/Y_zeta**2*(X_zeta+p*CFL*c*(k-n)*(j-K2)/Z)
	else if(cs.eq.3) then
	  Sp011=2.*(i-K1)/Y_zeta**2*X_zeta
	end if
           
      return
	end	 
!c-------------------------------------------------------------
	real*8 function Sp022(i,j,k,K1,K2,n,c)
      
	implicit none
	integer cs,i,j,k,n
	real*8  c,K1,K2,CFL,p
	real*8  Y_zeta,X_zeta,kapa_i,Z

      common  /basic/ CFL, p
	common  /kernel/ Y_zeta,X_zeta,Z
      
      call cases_judge(cs,2,i,j,k,K1,K2,n,c)
	if(cs.eq.1) then
	  Sp022=0.
	else if(cs.eq.2) then
	  Sp022=(j-K2)/Y_zeta**2*(X_zeta+p*CFL*c*(k-n)*(i-K1)/Z)
	else if(cs.eq.3) then
	  Sp022=2.*(j-K2)/Y_zeta**2*X_zeta
	end if
           
      return
	end	 
!c-------------------------------------------------------------
	real*8 function Tp011(i,j,k,K1,K2,n,c)
      
	implicit none
	integer cs,i,j,k,n
	real*8  c,K1,K2,CFL,p,Y2,ct2,ct3
	real*8  Y_zeta,X_zeta,kapa_i,Z

      common  /basic/ CFL, p
	common  /kernel/ Y_zeta,X_zeta,Z

      call cases_judge(cs,1,i,j,k,K1,K2,n,c)
	ct2=(p*CFL*c*(k-n))**2
	ct3=p*CFL*c*(k-n)*ct2
	Y2=Y_zeta**2
	if(cs.eq.1) then
	  Tp011=0.
	else if(cs.eq.2) then
	  Tp011=1./3.*(i-K1)/Y2*(X_zeta*(1.+2.*ct2/Y2)      &
             +ct3*(j-K2)/Z*(1./Z**2+2./Y2))
	else if(cs.eq.3) then
	  Tp011=2./3.*(i-K1)/Y2*X_zeta*(1.+2.*ct2/Y2)
	end if
           
      return
	end	 

!c-------------------------------------------------------------
	real*8 function Tp022(i,j,k,K1,K2,n,c)
      
	implicit none
	integer cs,i,j,k,n
	real*8  c,K1,K2,CFL,p,Y2,ct2,ct3
	real*8  Y_zeta,X_zeta,kapa_i,Z

      common  /basic/ CFL, p
	common  /kernel/ Y_zeta,X_zeta,Z

      call cases_judge(cs,2,i,j,k,K1,K2,n,c)
	ct2=(p*CFL*c*(k-n))**2
	ct3=p*CFL*c*(k-n)*ct2
	Y2=Y_zeta**2
	if(cs.eq.1) then
	  Tp022=0.
	else if(cs.eq.2) then
	  Tp022=1./3.*(j-K2)/Y2*(X_zeta*(1.+2.*ct2/Y2)      &
             +ct3*(i-K1)/Z*(1./Z**2+2./Y2))
	else if(cs.eq.3) then
	  Tp022=2./3.*(j-K2)/Y2*X_zeta*(1.+2.*ct2/Y2)
	end if
           
      return
	end	 

!c---------------------------------------------------------------------
	real*8 function A0(i,j,k,K1,K2,n,c)
      
	implicit none
	integer i,j,k,n,cs,zeta
	real*8  c,H,K1,K2,ct2,CFL,p,pi,X1,X2,theta,X12,X22
	real*8  d1,d2,sum1,sum2,diff,u,phi
      common  /basic/ CFL, p
      
      ct2=(p*CFL*c*(k-n))**2
	d1=i-K1
	d2=j-K2
      X12=ct2-d1**2
	X22=ct2-d2**2
	pi=4.0*datan(dble(1.0))
      diff=X12-d2**2
	phi=H(diff)*(H(d1)*H(-d2)+H(-d1)*H(d2)-H(-d1)*H(-d2))
	u=pi*(2.*H(d1)*H(d2)+phi/2.)  

	zeta=1
      call cases_judge(cs,zeta,i,j,k,K1,K2,n,c)
	if(cs.eq.1) then
	  sum1=0.0
	else if(cs.eq.2) then
	  X1=dsqrt(X12)
        sum1=-datan(X1/d1)-H(d1)*H(d2)*datan(d2/d1) 
	else if(cs.eq.3) then
	  X1=dsqrt(X12)
	  sum1=-2.*H(d2)*datan(X1/d1)  
	end if

      zeta=2
      call cases_judge(cs,zeta,i,j,k,K1,K2,n,c)
	if(cs.eq.1) then
	  sum2=0.0
	else if(cs.eq.2) then
	  X2=dsqrt(X22)
        sum2=-datan(X2/d2)-H(d1)*H(d2)*datan(d1/d2)
	else if(cs.eq.3) then
	  X2=dsqrt(X22)
	  sum2=-2.*H(d1)*datan(X2/d2)
	end if
      A0=H(c*(k-n))*(u+sum1+sum2)

      return
	end	 
!c---------------------------------------------------------------------
	real*8 function A0_bak(i,j,k,K1,K2,n,c)
      
	implicit none
	integer i,j,k,n,cs
	real*8  c,H,K1,K2,ct2,CFL,p,pi
	real*8  d1,d2,sums(2),X1,X2,a1,a2,a3
      common  /basic/ CFL, p
	      
	pi=4.0*datan(dble(1.0))
      ct2=(p*CFL*c*(k-n))**2
	d1=i-K1
	d2=j-K2

      call cases_judge(cs,1,i,j,k,K1,K2,n,c)
	if(cs.eq.1) then
	  sums(1)=0.0
	else if(cs.eq.2) then
        X1=dsqrt(ct2-d1**2)
	  X2=dsqrt(ct2-d2**2)
	  a1=datan(X1/d1) 
	  a2=datan(d2/d1)
	  a3=datan(X2/d2)
        sums(1)=-(a1+a2)*H(d1)*H(d2)+(pi/2.-a1-a3)*H(-d1)*H(d2)      &
               -(pi/2.+a1+a3)*H(-d1)*H(-d2)
	else if(cs.eq.3) then
        X1=dsqrt(ct2-d1**2)
	  sums(1)=-2.*datan(X1/d1)*H(d2)
	end if

      call cases_judge(cs,2,i,j,k,K1,K2,n,c)
	if(cs.eq.1) then
	  sums(2)=0.0
	else if(cs.eq.2) then
        X1=dsqrt(ct2-d1**2)
	  X2=dsqrt(ct2-d2**2)
	  a1=datan(X2/d2) 
	  a2=datan(d1/d2)
	  a3=datan(X1/d1)
        sums(2)=-(a1+a2)*H(d1)*H(d2)+(pi/2.-a1-a3)*H(d1)*H(-d2)
     	else if(cs.eq.3) then
	  X2=dsqrt(ct2-d2**2)
	  sums(2)=-2.*datan(X2/d2)*H(d1)
	end if

      A0_bak=H(c*(k-n))*(2.*pi*H(d1)*H(d2)+sums(1)+sums(2))

      return
	end	 

!c#################################################################

      real*8 function H(x)
	
	implicit none
      real*8 x

	if(x.gt.0.) then
	   H=1.0
	else if(x.le.0.) then
	   H=0.0
!c	else if (x.eq.0.0) then
!c	   H=0.5
	end if

	return
	end
!c-------------------------------------------------------------------

	subroutine cases_judge(cs,zeta,i,j,k,k1,k2,n,c)
!c------------------------------------------------------------
!c  此函数的作用是：1 计算所有基本积分都需要的变量；
!c                  2 根据场点和源点的坐标判断属于哪一种情形。
!c------------------------------------------------------------

	implicit none
	integer cs,zeta,zeta_bar,i,j,k,n
	real*8  x(2),KK(2),k1,k2,c,ct2,Y2,CFL
	real*8  Y_zeta,X_zeta,Z,diff,p

      common  /basic/ CFL, p
	common  /kernel/ Y_zeta,X_zeta,Z

	zeta_bar=3-zeta
	x(1)=i
	x(2)=j
	KK(1)=k1
	KK(2)=k2
      Y2=(x(zeta)-KK(zeta))**2
      Y_zeta=dsqrt(Y2)
	ct2=(p*CFL*c*(k-n))**2	  

	if(ct2.le.Y2) then
	   cs=1
      else
	   X_zeta=dsqrt(ct2-Y2)
	   diff=x(zeta_bar)-KK(zeta_bar)
	   Z=dsqrt(Y2+diff**2)
	   if(diff.le.-X_zeta) then
	      cs=1
	   else if(Z**2.lt.ct2) then
	      cs=2
	   else if(diff.ge.X_zeta) then
	      cs=3
	   end if
	end if
      return
	end

!c##################################################################
