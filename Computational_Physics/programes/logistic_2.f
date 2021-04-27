      


      program logistic
      implicit none
      
      double precision x,x0,x00,x1,r
c doble presicio implica 16 decimals
      integer n,i


      write(*,*) 'introdueix un valor inicial'
      read(*,*) x00

      open(1,file='logistic2.dat')
      
      do i=2000,3900
	x0=x00
	r=dble(i)*1.d-3
        do n=1,1000000
	  x1=r*x0*(1.d0-x0)
	  if(mod(n,1000).eq.0) write(1,*) r,x1
c agafem els multiples de 1000
	  if(mod(n,1001).eq.0) write(1,*) r,x1
	  x0=x1
        enddo
      enddo
      end
