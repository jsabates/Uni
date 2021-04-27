      PROGRAM jordisabates
      IMPLICIT NONE

      EXTERNAL p
      DOUBLE PRECISION theta,l,alpha,idum,p,a,b,M,g
      DOUBLE PRECISION rx1,ry1,rx0,ry0
      INTEGER i
C**NO UTILITZO EL RAN2(IDUM) UTILITZO UN ALTRE METODE PER GENERAR NOMBRES ALEATORIS
C**utilitzo el metode de accept rebuig el qual em retarda molt el calcul
C**al final hi ha una subrotina per generar nombres aleatoris respecte la distribucio gaussiana amb box muller
c*pero no m'ha donat temps a fer el canvi de metode

C** DADES INICIALS
      alpha=4.5d0
      rx0=0.d0
      ry0=0.d0
      write(1,*) rx0,ry0
C** inicialitzem el bucle segons la formula
      DO i=1,1000
        CALL levystep(theta,l,alpha)
        rx1=rx0+l*dcos(theta)
        ry1=ry0+l*dsin(theta)
        rx0=rx1
        ry0=ry1
        write(1,*) rx0,ry0
      END DO
      write(1,*)
      write(1,*)

C** DADES INICIALS
      alpha=3.5d0
      rx0=0.d0
      ry0=0.d0
      write(1,*) rx0,ry0
C** inicialitzem el bucle segons la formula
      DO i=1,1000
        CALL levystep(theta,l,alpha)
        rx1=rx0+l*dcos(theta)
        ry1=ry0+l*dsin(theta)
        rx0=rx1
        ry0=ry1
        write(1,*) rx0,ry0
      END DO
      write(1,*)
      write(1,*)

C** DADES INICIALS
      alpha=2.5d0
      rx0=0.d0
      ry0=0.d0
      write(1,*) rx0,ry0
C** inicialitzem el bucle segons la formula
      DO i=1,1000
        CALL levystep(theta,l,alpha)
        rx1=rx0+l*dcos(theta)
        ry1=ry0+l*dsin(theta)
        rx0=rx1
        ry0=ry1
        write(1,*) rx0,ry0
      END DO
      write(1,*)
      write(1,*)


      CLOSE(1)

      CALL SYSTEM('gnuplot -p script.plt')

      END

      SUBROUTINE levystep(theta,l,alpha)
      IMPLICIT NONE
      EXTERNAL p,ran2
      DOUBLE PRECISION theta,l,alpha,PI,p,x1,x2,x,a,b,M,g,ran2,idum
      INTEGER i,n
      REAL :: r

      a=1.d0
      b=2.d0
      M=alpha-1.d0

      PI=dacos(-1.d0)
      CALL random_seed
      CALL random_number(r)
      theta=2.d0*PI*r

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


      subroutine subgaussians(ndat,xmu,xsigma,xgaus)
	implicit none
	double precision x1(ndat),x2(ndat),xmu,xsigma,xgaus(ndat),pi
     +,theta(ndat),r(ndat)
	integer iseed,ndat,i
	pi=dacos(-1.d0)

	iseed=16861202
	call srand(iseed)

	do i=1,ndat
	  theta(i)=2.d0*pi*rand()  !genera un valor aleatori per l'angle entre 0 i 2pi
	  r(i)=dsqrt(-2.d0*dlog(1.d0-rand()))   !genera un valor per al radi, segons el metode de box-muller
	enddo

cºººx1 i x2 son valors aleatoris generats segons una gaussiana amb variança xsigma**2 i valor mitja xmu

	x1=xsigma*r*dcos(theta)+xmu
c	x2=xsigma*r*dsin(theta)+xmu   !nomes necessito ndat termes i amb x1 ja en tinc prou

	xgaus=x1

	end subroutine
