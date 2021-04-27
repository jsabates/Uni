      PROGRAM MAIN
      IMPLICIT NONE

      EXTERNAL fcn,funlon,funmas,funmasx
      INTEGER Ninter,k,i
      DOUBLE PRECISION tab1(2001),tab2(2001),x,a,b,m,funlon,R,t,PI
      DOUBLE PRECISION trapezis,f0,h,fcn,simpson,L,rho,funmas,funmasx
987   FORMAT(12X,A,3X,A)
123   FORMAT(A,2X,F10.4,5X,F10.4)
1234  FORMAT(A,3X,F10.4,5X,F10.4)
42    FORMAT(15X,A,20X,A,15X,A)
43    FORMAT(E11.4,2X,F10.4,2X,F10.4)

C*********APARTAT 0 **************************************************
C*************************** Estrategia 1 ****************************
      x=0
      i=1
      DO k=0,200000000
       IF ( MOD (k, 100000).eq.0) THEN
       tab1(i)=x
       WRITE(1,*)tab1(i)
       i=i+1
       END IF
       x=x+0.01
      END DO

C*********************** ESTRATEGIA 2 **********************************

      x=0
      OPEN(1,FILE="PROVA.dat")
      DO k=0,2000
       x=1000*k
       tab2(k+1)=x
       WRITE(1,*)tab1(k+1),tab2(k+1)
      END DO
      CLOSE (1)

C******* APARTAT 1 Y APARTAT 2 ****************************************************

      R=0.042325d0
      L=0.1432d0
      rho=8.42d0
      OPEN(1,FILE='P3-1819-res1.dat')
      WRITE(1,987) 'Longitud (m) ','Massa (kg) '
      WRITE(1,*)'   '
      WRITE(1,123)'Trapezis',trapezis(-R,R,2**20,funlon),
     + trapezis(-L/3,L/3,2**20,funmas)
      WRITE(1,1234)'Simpson',simpson(-R,R,20,funlon),
     + simpson(-L/3,L/3,20,funmas)
      CLOSE(1)

C**************APARTAT 3 ********************************************
      a=-R
      b=R

      OPEN(2,FILE='P3-1819-res2.dat')
      WRITE(2,*)'LONGITUD'
      WRITE(2,*)''
      WRITE(2,42)'h','Trapezis','Simpson'
      WRITE(2,*)' '
      DO k=2,20
       WRITE(2,*)((b-a)/2**k),dabs(trapezis(-R,R,2**k,funlon)-
     + trapezis(-R,R,2**20,funlon)),dabs(simpson(-R,R,k,funlon)-
     + simpson(-R,R,20,funlon))
      END DO
      CLOSE(2)

      OPEN(3,FILE='P3-1819-res3.dat')
      WRITE(3,*)'MASSA'
      WRITE(3,*)''
      WRITE(3,42)'h','Trapezis','Simpson'
      WRITE(3,*)' '
      DO k=2,20
       WRITE(3,*)((2*L/3)/2**k),dabs(trapezis(-L/3,L/3,2**k,funmas)-
     + trapezis(-L/3,L/3,2**20,funmas)),dabs(simpson(-L/3,L/3,k,funmas)-
     + simpson(-L/3,L/3,20,funmas))
      END DO
      CLOSE(3)

      OPEN(4,FILE='P3-1819-res4.dat')
      PI=DACOS(-1.D0)
      WRITE(4,*)'MASSA en funcio de t'
      WRITE(4,*)''
      WRITE(4,42)'h','Trapezis','Simpson'
      WRITE(4,*)' '
      DO k=2,20
       WRITE(4,*)(PI/2**k),dabs(trapezis(-PI/2,PI/2,2**k,funmasx)-
     + trapezis(-PI/2,PI/2,2**20,funmasx)),
     + dabs(simpson(-PI/2,PI/2,k,funmasx)-
     + simpson(-PI/2,PI/2,20,funmasx))
      END DO
      CLOSE(4)
      CALL SYSTEM('gnuplot -p scriptP3.plt')

C***************** APARTAT 4 ****************************************

      END

      DOUBLE PRECISION FUNCTION funlon(x)
      DOUBLE PRECISION x,R
      R=0.042325d0
      funlon=dsqrt(1+(-x/(R*dsqrt(1-(x/R)**2)))**2)
      END FUNCTION

      DOUBLE PRECISION FUNCTION funmas(x)
      DOUBLE PRECISION x,R,L,rho
      R=0.042325d0
      L=0.1432d0
      rho=8.42d0
      funmas=rho*(dsqrt(1-(3*x/L)**2))*(1-(3*x/L))**3
      END FUNCTION

      DOUBLE PRECISION FUNCTION funmasx(t)
      DOUBLE PRECISION t,R,L,rho,PI
      PI=DACOS(-1.D0)
      R=0.042325d0
      L=0.1432d0
      rho=8.42d0
      funmasx=rho*(dsqrt(1-(dsin(t))**2))*(1-(dsin(t)))**3
      END FUNCTION


      DOUBLE PRECISION FUNCTION trapezis(a,b,Ninter,fcn)
      DOUBLE PRECISION h,x,a,b,funlon,R,fcn,funmas,L,rho,PI,t
      INTEGER Ninter
      h=(b-a)/Ninter
      trapezis=0.d0
      DO k=1,Ninter-1
       x=a+k*h
       trapezis=trapezis+2.d0*fcn(x)
      END DO
      trapezis=trapezis*h/2.d0
      END FUNCTION

      DOUBLE PRECISION FUNCTION simpson(a,b,m,fcn)
      DOUBLE PRECISION h,x,a,b,funlon,funmas,R,fcn,L,rho,PI,t
      INTEGER m
      Ninter=2**m
      h=(b-a)/Ninter
      simpson=0.d0
      DO k=1,Ninter-1
       x=a+k*h
       IF ( MOD (k,2).eq.0) THEN
        simpson=simpson+(2*fcn(x))
       ELSE
        simpson=simpson+(4*(fcn(x)))
       END IF
      END DO
      simpson=(h/3)*simpson
      END FUNCTION

