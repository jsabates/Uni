c programa implementa el metode de Halley per trobar zeros de funcions continues

      program halley
      implicit none

      double precision x,x0,x1,f,df,df2,e,delta,gap
      integer n

      parameter(delta=1.d-14)

c funcio de la que busquem els zeros
c      f(x)=dcos(x)-x
c	df(x)=-dsin(x)-1
c	f(x)=1.+dsin(x)
c	df(x)=dcos(x)
c	f(x)=datan(x)
c	df(x)=1.d0/(1.d0+x**2)
c	df2(x)=-2.d0*x/(1.d0+x**2)**2

	f(x)=5.d0*x*dexp(-x)-1
	df(x)=5.d0*(1.d0-x)*dexp(-x)
	df2(x)=-5.d0*(2.d0-x)*dexp(-x)

c funcio de la que busquem els zeros
c ens demana per pantalla el valor inicial

	write(*,*) 'entra el valor inicial'
	 read(*,*) x0

c STATUS INDICA SI UN DOCUMENT NOU O NO
	open(1
