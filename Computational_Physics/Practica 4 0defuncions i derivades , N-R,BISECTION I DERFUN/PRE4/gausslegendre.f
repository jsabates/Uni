      PROGRAM main
      IMPLICIT NONE

      external f
      double precision f,int,legendre
      integer n

      do n=2,10
       call gausslegendre(f,n,int)
       write(*,*) n, int
      enddo

      stop
      end

      double precision function f(x)
       double precision x
       f=x**4

       end

       subroutine gausslegendre(f,n,integral)
       implicit none
       double precision x,x0,x1,e,delta,xi,y,legendre,zeros,weights,f
       double precision integral
       integer n,j,i,m
       dimension zeros(1:1000)
       dimension weights(1:1000)
       parameter(delta=1.d-12)

       m=100*n
1      y=1.d0
       j=0
       do i=m,-m,-1
        xi=dble(i)/dble(m)
c        write(*,*) y,legendre(xi,n)
        if ((legendre(xi,n)*y).lt.0.d0)then
            j=j+1
            zeros(j)=xi
            y=legendre(xi,n)
        else
         y=legendre(xi,n)
        endif
       enddo

       if(j.ne.n)then
        write(*,*) j,n,'increasing number of sampled points'
        m=2*m
        goto 1

       endif

       do i=1,n
        write(*,*) 'initial zeros',zeros(i)
       enddo

       do i=1,n
        x0=zeros(i)
        do while(.true.)
         x1=x0-legendre(x0,n)*(1.d0-x0**2)
     + /(dble(n)*(legendre(x0,n-1)-x0*legendre(x0,n)))
        e=dabs(x1-x0)
        if(e.lt.delta) goto 2
        x0=x1
       end do
2      zeros(i)=x1
      end do

      do i=1,n
       weights(i)=2.d0*(1.d0-zeros(i)**2)
     +  /(dble(n)*legendre(zeros(i),n-1))**2
       end do

       integral=0.d0
       do i=1,n
        integral=integral+f(zeros(i))*weights(i)
       end do

       return
       end

      function legendre (x,n)
      double precision x,xi,legendre,legendre0,legendre1
      integer n,i

      if(n.eq.0)then
       legendre=1.d0
       else if(n.eq.1)then
       legendre=x
       else
        legendre0=1.d0
        legendre1=x
           do i=2,n
            xi=dble(i)
            legendre=((2.d0*xi-1.d0)*x*legendre1-(xi-1.d0)*legendre0/n)
            legendre0=legendre1
            legendre1=legendre
            enddo
      END IF

      end
