      PROGRAM jordisabates
      IMPLICIT NONE

      EXTERNAL p
      DOUBLE PRECISION theta,l,alpha,idum,p,a,b,g
      DOUBLE PRECISION rx1,ry1,rx0,ry0,dist
      INTEGER i,ntraj,j
C**NO UTILITZO EL RAN2(IDUM) UTILITZO UN ALTRE METODE PER GENERAR NOMBRES ALEATORIS

C**amb el metode de accept rebuig que utilitzo el programa es molt lent

      ntraj=10000
      OPEN(1,FILE='levy2.dat')

C** DADES INICIALS
      alpha=4.5d0
      DO j=1,ntraj
        rx0=0.d0
        ry0=0.d0
C** inicialitzem el bucle segons la formula
        DO i=1,1000
          CALL levystep(theta,l,alpha)
          rx1=rx0+l*dcos(theta)
          ry1=ry0+l*dsin(theta)
          rx0=rx1
          ry0=ry1
        END DO
        dist=dsqrt((rx0**2.d0)+(ry0**2.d0)) !distancia al punt inicial
        write(1,*) dist
      END DO
      write(1,*)
      write(1,*)

C** DADES INICIALS
      alpha=3.5d0
      DO j=1,ntraj
        rx0=0.d0
        ry0=0.d0
C** inicialitzem el bucle segons la formula
        DO i=1,1000
          CALL levystep(theta,l,alpha)
          rx1=rx0+l*dcos(theta)
          ry1=ry0+l*dsin(theta)
          rx0=rx1
          ry0=ry1
        END DO
        dist=dsqrt((rx0**2.d0)+(ry0**2.d0)) !distancia al punt inicial
        write(1,*) dist
      END DO

      write(1,*)
      write(1,*)

C** DADES INICIALS
      alpha=2.5d0
      DO j=1,ntraj
        rx0=0.d0
        ry0=0.d0
C** inicialitzem el bucle segons la formula
        DO i=1,1000
          CALL levystep(theta,l,alpha)
          rx1=rx0+l*dcos(theta)
          ry1=ry0+l*dsin(theta)
          rx0=rx1
          ry0=ry1
        END DO
        dist=dsqrt((rx0**2.d0)+(ry0**2.d0)) !distancia al punt inicial
        write(1,*) dist
      END DO

      write(1,*)
      write(1,*)

C** DADES INICIALS
      alpha=1.5d0
      DO j=1,ntraj
        rx0=0.d0
        ry0=0.d0
C** inicialitzem el bucle segons la formula
        DO i=1,1000
          CALL levystep(theta,l,alpha)
          rx1=rx0+l*dcos(theta)
          ry1=ry0+l*dsin(theta)
          rx0=rx1
          ry0=ry1
        END DO
        dist=dsqrt((rx0**2.d0)+(ry0**2.d0)) !distancia al punt inicial
        write(1,*) dist
      END DO

      write(1,*)
      write(1,*)

      CLOSE(1)

C      CALL SYSTEM('gnuplot -p script.plt')

      END

      SUBROUTINE levystep(theta,l,alpha)
      IMPLICIT NONE
      EXTERNAL p,ran2
      DOUBLE PRECISION theta,l,alpha,PI,p,x1,x2,x,a,b,M,g,ran2,idum
      INTEGER i,n
      REAL :: r

      a=1.d0 !interval a-b del metode accepta rebuig
      b=2.d0
      M=alpha-1.d0 !constant per acotar la funcio

      PI=dacos(-1.d0)
      CALL random_seed
      CALL random_number(r)
      theta=2.d0*PI*r !generem thetas random

      i=0
      DO WHILE(i.lt.1)
      CALL random_seed
      CALL random_number(x1)
      CALL random_seed
      CALL random_number(x2)

      x=(b-a)*x1 + a !seguim el metode acceptrebuig
      g=M*x2
      IF (p(x,alpha).ge.g) THEN
        l=x
        i=i+1
      END IF
      END DO

      END SUBROUTINE


      DOUBLE PRECISION FUNCTION p(l,alpha)
      DOUBLE PRECISION l,alpha
      p=(alpha-1.d0)/(l**alpha)
      END FUNCTION

