      PROGRAM MAIN
      IMPLICIT NONE

      EXTERNAL funint,simpson,integral
      INTEGER Ninter,m,k,i,r
      DOUBLE PRECISION L,w0,xi,phi0(3),g,PI,funint,phi,h,x
      DOUBLE PRECISION trapezis,simpson,integral,tab(100),integralc
42    FORMAT (e24.18,2x,e24.18,2x,e24.18,2x,e24.18,2x,e24.18,2x,
     + e24.18,2x,e24.18)
987   FORMAT(10X,A,20X,A,13X,A,8X,A,7X,A,15X,A,7X,A)
      L=4.0d0
      g=9.807d0
      PI=DACOS(-1.D0)
      phi0(1)=0
      phi0(2)=PI-0.1d0
      phi0(3)=PI-0.001d0
      w0=dsqrt(g/L)
      xi=(dsin(phi0(1)/2))**2

C A)*******************************************************************
      OPEN(1,FILE='P3-1819-res1.dat')
      WRITE(1,987)'h','Ttrap (phi=0)','Tsimpson (phi=0)',
     = 'Ttrap (phi=PI-0,1)','Tsimpson (phi=PI-0,1)',
     + 'Ttrap (phi=PI-0,001)','Tsimpson (phi=PI-0,001)'
      WRITE(1,*)''
      DO k=2,20
       h=((PI/2.d0)/2**k)
       WRITE(1,42) h,(4.d0/w0)*trapezis(0.0d0,PI/2,2**k,funint,phi0(1)),
     + (4/w0)*simpson(0.0d0,PI/2,k,funint,phi0(1)),
     + (4/w0)*trapezis(0.0d0,PI/2,2**k,funint,phi0(2)),
     + (4/w0)*simpson(0.0d0,PI/2,k,funint,phi0(2)),
     + (4/w0)*trapezis(0.0d0,PI/2,2**k,funint,phi0(3)),
     + (4/w0)*simpson(0.0d0,PI/2,k,funint,phi0(3))

      END DO
      CLOSE(1)

C B)*******************************************************************
      OPEN(2,FILE='P3-1819-error1.dat')
      DO k=2,20

       h=((PI/2.d0)/2**k)
       WRITE(2,42) h,(4.d0/w0)*trapezis(0.0d0,PI/2,2**20,funint,phi0(1))
     + -(4.d0/w0)*trapezis(0.0d0,PI/2,2**k,funint,phi0(1)),
     + (4/w0)*simpson(0.0d0,PI/2,20,funint,phi0(1))
     + -(4/w0)*simpson(0.0d0,PI/2,k,funint,phi0(1)),
     + (4/w0)*trapezis(0.0d0,PI/2,2**20,funint,phi0(2))
     + -(4/w0)*trapezis(0.0d0,PI/2,2**k,funint,phi0(2)),
     + (4/w0)*simpson(0.0d0,PI/2,20,funint,phi0(2))
     + -(4/w0)*simpson(0.0d0,PI/2,k,funint,phi0(2)),
     + (4/w0)*trapezis(0.0d0,PI/2,2**20,funint,phi0(3))
     + -(4/w0)*trapezis(0.0d0,PI/2,2**k,funint,phi0(3)),
     + (4/w0)*simpson(0.0d0,PI/2,20,funint,phi0(3))
     + -(4/w0)*simpson(0.0d0,PI/2,k,funint,phi0(3))

      END DO
      CLOSE(2)
C C)*******************************************************************

      OPEN(3,FILE='P3-1819-T.dat')
      h=((PI/2.d0)/2**10)
      integralc=(4/w0)*(PI/2)*(1+((1/2)**2)*xi+(3/8)**2 *xi**2)
      DO r=1,100
       tab(r)=((PI-0.01-0.01)/100)*r +0.01
       WRITE(3,*) h,integral(tab(r)),integralc
      END DO
      CLOSE(3)

      CALL SYSTEM('gnuplot -p scriptP3.plt')

C generem una grafica del valor de la integral vs que???
      END

      DOUBLE PRECISION FUNCTION funint(phi,phi0)
      DOUBLE PRECISION L,xi,w0,g,phi,phi0,PI

      xi=(dsin(phi0/2))**2
      funint=1/dsqrt(1-(xi*(dsin(phi)**2)))
      END FUNCTION

      DOUBLE PRECISION FUNCTION integral(x)
      EXTERNAL funint
      DOUBLE PRECISION L,w0,xi,phi0(3),g,PI,funint,phi,h,x
      DOUBLE PRECISION trapezis,simpson
      L=4.0d0
      g=9.807d0
      PI=DACOS(-1.D0)
      w0=dsqrt(g/L)
      integral=dble((4.0d0/w0)*simpson(0.0d0,PI/2,10,funint,x))
      END FUNCTION


      DOUBLE PRECISION FUNCTION trapezis(a,b,Ninter,fcn,phi0)
      DOUBLE PRECISION h,x,a,b,fcn,PI,funint,phi,phi0,xi
      INTEGER Ninter

      h=(b-a)/Ninter
      trapezis=0.d0
      DO k=1,Ninter-1
       x=a+k*h
       trapezis=trapezis+2.d0*fcn(x,phi0)
      END DO
      trapezis=trapezis*h/2.d0
      END FUNCTION

      DOUBLE PRECISION FUNCTION simpson(a,b,m,fcn,phi0)
      DOUBLE PRECISION h,x,a,b,fcn,PI,funint,phi,phi0,xi,tab(100)
      INTEGER m

      DO i=1,100
      tab(i)=((PI-0.01-0.01)/100)*100 +0.01
      END DO
      Ninter=2**m
      h=(b-a)/Ninter
      simpson=0.d0
      DO k=1,Ninter-1
       x=a+k*h
       IF ( MOD (k,2).eq.0) THEN
        simpson=simpson+(2.d0*fcn(x,phi0))
       ELSE
        simpson=simpson+(4.d0*(fcn(x,phi0)))
       END IF
      END DO
      simpson=(h/3)*simpson
      END FUNCTION

