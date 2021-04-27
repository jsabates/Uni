      


      program logistic
      implicit none
      
      double precision x,x0,x1,r
c doble presicio implica 16 decimals
      integer n

      r=3.7d0

      write(*,*) 'introdueix un valor inicial'
      read(*,*) x0

      open(1,file='logistic.dat')

      do n=1,1000    	
	x1=r*x0*(1.d0-x0)
	write(1,*) n,x1
	x0=x1
      enddo
      end
