      program pre6
      implicit none
      external I1,I2,I3,I4,I5,p      
      double precision I1,I2,I3,I4,I5,I6,x,fun,a,b,sigma1,integral1,
     +suma1,p,pi,x1,f1,x2,f2,integral2,sigma2,suma2,xnums(1050000),M,
     +x12,x34,x5,f3,f4,f5,integral3,integral4,integral5,suma3,suma4,
     +suma5,sigma3,sigma4,sigma5,xgaus(1050000),teoric1,teoric2,suma6,
     +sigma6,f6,integral6,x3,x4
      integer i,N,iseed,ndat
      pi=dacos(-1.d0)
100   format(I10,2x,F10.6,2x,F10.6,2x,F10.6,2x,F10.6,2x,E12.6,
     +2x,E12.6)   
200   format(I10,2x,F10.6,2x,F10.6,2x,F10.6,2x,F10.6,2x,F10.6,
     +2x,F10.6) 
300   format(6x,A,6x,A,5x,A,5x,A,4x,A,3x,A,2x,A)
400   format(6x,A,7x,A,4x,A,5x,A,4x,A,4x,A,4x,A)
c                         APARTAT 1 a)

      open(1,file='P6-apartata.dat')  
      teoric1=(pi**3.d0)/2.d0
      teoric2=1.d0/288.d0*pi*((120.d0*(pi**2))-677.d0)
      b=pi
      a=-pi
      write(1,300) 'N','integral1','sigma1','integral2','sigma2',
     +  'int1-teoric1','int2-teoric2' 
      do N=2500,150000,2500
       suma1=0.d0
       suma2=0.d0
       f1=0.d0
       f2=0.d0
       do i=1,N
        x12=a+(((b-a)*i)/dble(N))
	f1=f1+((b-a)*I1(x12))	
        f2=f2+((b-a)*I2(x12))	
	suma1=suma1+(I1(x12)*(b-a))**2.d0 !per calcular després la sigma
        suma2=suma2+(I2(x12)*(b-a))**2.d0
       enddo
       integral1=(f1/dble(N))
       integral2=(f2/dble(N))
       sigma1=dsqrt(((suma1/dble(N))-(integral1**2.d0))/dble(N)) 
       sigma2=dsqrt(((suma2/dble(N))-(integral2**2.d0))/dble(N))    
       write(1,100) N,integral1,sigma1,integral2,sigma2,
     +  dabs(integral1-teoric1),dabs(integral2-teoric2)  			
      enddo
      close(1)


C                  APARTAT 1 b)

      call acceptrebuig(1050000,xnums,-pi,pi,0.4d0,p)


c                  APARTAT 1 c)

      call subgauss(1050000,0.d0,1.d0,xgaus)


c                   APARTAT 1 d)
 
      open(2,file='P6-apartatd.dat')
      a=-pi
      b=pi
      write(2,400) 'N','integral1','sigma1','integral2','sigma2',
     +  'integral3','sigma3' 
      do N=5000,1050000,5000
       f3=0.d0
       f4=0.d0
       f5=0.d0
       suma3=0.d0
       suma4=0.d0
       suma5=0.d0
       do i=1,N
        x34=xnums(i)
        x5=xgaus(i)
        f3=f3+(I3(x34)/p(x34))	
        f4=f4+(I4(x34)/p(x34))
        f5=f5+(I5(x5)*dsqrt(2.d0*pi)*dexp(x5**2.d0/2.d0))	
	suma3=suma3+(I3(x34)/p(x34))**2.d0
        suma4=suma4+(I4(x34)/p(x34))**2.d0
        suma5=suma5+(I5(x5)*dsqrt(2.d0*pi)*dexp(x5**2.d0/2.d0))
       enddo
       integral3=(f3/dble(N))
       integral4=(f4/dble(N))
       integral5=(f5/dble(N))
       sigma3=dsqrt(((suma3/dble(N))-(integral3**2.d0))/dble(N)) 
       sigma4=dsqrt(((suma4/dble(N))-(integral4**2.d0))/dble(N)) 
       sigma5=dsqrt(((suma5/dble(N))-(integral5**2.d0))/dble(N))   
       write(2,200) N,integral3,sigma3,integral4,sigma4,integral5,sigma5
      enddo

    
       
c                           APARTAT 2
500   format(5x,A,7x,A,5x,A)
600   format(I10,2x,F10.6,2x,F10.6)
      open(3,file='P6-apartat2.dat')
      write(3,500) 'N','integral6','error6' 
      do N=1500,210000,1500
       f6=0.d0
       suma6=0.d0
       do i=1,N
        x1=xgaus(i)
        x2=xgaus(i+1)
        x3=xgaus(i+2)
        x4=xgaus(i+3)
        x5=xgaus(i+4)
        f6=f6+(I6(x1,x2,x3,x4,x5))
c*dsqrt(2.d0*pi)*dexp(x5**2.d0/2.d0))	
        suma6=suma6+(I6(x1,x2,x3,x4,x5))
c*dsqrt(2.d0*pi)*dexp(x5**2.d0/2.d0))
       enddo
       integral6=(f6/dble(N))
       sigma6=dsqrt(((suma6/dble(N))-(integral6**2.d0))/dble(N))   
       write(3,600) N,integral6,sigma6
      enddo
      close(3)

      call system('gnuplot -p scriptP6.plt')
      end



c-------------------------------------------------------------------------------------
c                                SUBRUTINES
c-------------------------------------------------------------------------------------

c                           SUBRUTINA ACCEPTREBUIG         
                   
      subroutine acceptrebuig(ndat,xnums,a,b,M,fun)
      implicit none
      external fun
      double precision x,p,xnums(ndat),a,b,M,fun,x1,x2,mitja,
     +var,des,xnum
      integer ndat,i,iseed
      
100   format(E12.5,2x,E12.5)   
400   format(A,2x,E12.5,2x,A,2x,E12.5,2x,A,2x,E12.5) 
      iseed=16622152
      call srand(iseed)

      i=1
      do while (i.le.ndat)
       x1=rand()
       x2=rand() 
       x=((b-a)*x1)+a
       p=M*x2
       if(fun(x).ge.p) then
	xnums(i)=x
        i=i+1
       endif
      enddo

      return
      end subroutine
C----------------------------------------------------------------------------

c                           SUBRUTINA DISTRIBUCIÓ GAUSSIANA

      subroutine subgauss(ndat,xmu,xsigma,xgaus)
      implicit none
      double precision xmu,xsigma,xgaus(ndat),pi,xg(ndat),random,
     +theta(ndat),r(ndat)
      integer ndat,i,iseed
      parameter (pi=dacos(-1.d0))
         
      iseed=16622152
      call srand(iseed)
      do i=1,ndat
       theta(i)=2.d0*pi*rand()
       r(i)=dsqrt(-2.d0*dlog(1.d0-rand()))
      enddo
      xg=xsigma*r*dcos(theta)+xmu
      xgaus=xg

      return
      
      end subroutine


c--------------------------------------------------------------------------------------
c                                   FUNCIONS
c--------------------------------------------------------------------------------------

c                                   FUNCIÓ P(X)

      double precision function p(x)
      double precision pi,x
      parameter (pi=dacos(-1.d0))
      p=(5.d0/4.d0)*dexp(-dabs(x))*(dsin(x)**2)/(1.d0-dexp(-pi))
      end function

c--------------------------------------------------------------------------------------

c                                   FUNCIÓ I1

      double precision function I1(x)
      double precision x
      parameter (pi=dacos(-1.d0))
      I1=dsqrt((pi**2.d0)-(x**2.d0))
      end function

c--------------------------------------------------------------------------------------

c                                   FUNCIÓ I2
      double precision function I2(x)
      double precision pi,x
      parameter (pi=dacos(-1.d0))
      I2=(x-(3*x**2)*dsin(x)+x**3)*(dcos(x)**2)*(dsin(x))
      end function

c--------------------------------------------------------------------------------------

c                                   FUNCIÓ I3
      double precision function I3(x)
      double precision pi,x
      parameter (pi=dacos(-1.d0))
      I3=(1/dsqrt(8*pi))*dexp(-dabs(x))*(x**2)*(dsin(x)**2)
      end function

c--------------------------------------------------------------------------------------

c                                   FUNCIÓ I4
      double precision function I4(x)
      double precision pi,x
      parameter (pi=dacos(-1.d0))
      I4=dexp(-(x**2)/2)*(dcos(x)**2)
      end function

c--------------------------------------------------------------------------------------

c                                   FUNCIÓ I5
      double precision function I5(x)
      double precision pi,x
      parameter (pi=dacos(-1.d0))
      I5=dexp(-(x**2))*(dsin(x)**6)*(x**2)
      end function

c----------------------------------------------------------------------------------------

c                                    FUNCIÓ I6

      double precision function I6(x1,x2,x3,x4,x5)
      double precision x1,x2,x3,x4,x5
      g=(dexp(x1*dcos(x2+x3)))*(((x3**2.d0)*(x4**2.d0))+
     +(dcos(x3+x4)*x5*dsin(x5)))
      I6=g*dexp(-(x1**2.d0+x2**2.d0+x3**2.d0+x4**2.d0+x5**2.d0))
      end function






