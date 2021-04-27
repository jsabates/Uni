      PROGRAM trapecis2
       IMPLICIT NONE
       DOUBLE PRECISION x,y,Into,pi,a,b,h,Int0,Into1,func,func2
       integer n,i,idum
      external func2,func

      pi=4.d0*datan(1.d0)

      a=-1.d0
      b=1.0

      open(1,file='trapecis.dat',status='unknown')

      do n=2,1000
       call trapecis(n,Into,a,b,func)
       Int0=Into
       write(1,*) n,Int0hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
      enddo

      close(1)
      stop
      end

      subroutine trapecis(n,Into,a,b,func)
      double precision Into,h,x,a,b,func
      integer n,i

       x=a
       Into=func(a)+func(b)
       h=(b-a)/dble(n)

       do i=1,n-1
        x=x+h
        Into=Into+2.d0+func(x)
       enddo
       Into=Into*h/2.d0

       return
      end


      double precision function func(x)
      double precision x

c     func=(dsin(x))**2
      func=dsqrt(1.d0-x**2)
      return
      end

      double precision function func2(x)
      double precision x

      func2=dcos(x)

      return
      end
