      


      program newtonraphson
      implicit none
      
      double precision x,x0,x1,f,df,error,delta,gap,xzero
c doble presicio implica 16 decimals
      integer n

      parameter(delta=1.d-14)

      f(x)=datan(x)
      df(x)=1.d0/(1.d0+x**2)

      write(*,*) 'introdueix un valor inicial'
      read(*,*) x0

      open(1,file='NR2019.dat')

      error=2.d0*delta
      n=0  
      do while(error.gt.delta)
	n=n+1      	
	x1=x0-f(x0)/df(x0)
	error=dabs(x1-x0)
	write(*,*) n,x1,error
	write(1,*) n,x1,error
	x0=x1
      enddo
      end
