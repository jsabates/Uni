      PROGRAM MAIN
      IMPLICIT NONE

      EXTERNAL difusio
      DOUBLE PRECISION difusio,Lx,h,error,Tin,ht,k,alfa,tmitj,nx,kk(2)
      DOUBLE PRECISION T0in(0:50),T0out(0:50),Tt(0:50,0:6000)
      DOUBLE PRECISION r(50),psi(50),a(50),b(50),c(50),betha,bethal(3)
      INTEGER i,n,j,imax
      common/dades/betha,k

C ** apartat 1 *********************************************************
C ** DADES INICIALS
C**   j=index x
      !T0in==matriu de dades
      !T0out==matriu de "resultats"

      Lx=1.5d0
      nx=50.d0
      h=Lx/nx
      ht=0.75d0
      error=1.d-5
      Tin=22.d0
      imax=49
      bethal(1)=0.00004d0 !llista amb totes les betas
      bethal(2)=0.0003d0
      bethal(3)=0.00025d0
      k=2.2d-5
      OPEN(1,FILE='apartat1.dat')
      DO i=1,3
        betha=bethal(i)
        CALL RELAX (T0in,T0out,Lx,h,error,Tin,0.d0)

        DO j=0,50
          WRITE(1,*) j*h,T0out(j)
          !WRITE(*,*) j*h,T0out(j)
        END DO
        WRITE(1,*)
        WRITE(1,*)
      END DO

      CLOSE(1)


C***********************************************************************
C ** apartat 2a *********************************************************
      OPEN (2,FILE='apartat2.dat')
      betha=0.0002d0
      CALL RELAX (T0in,T0out,Lx,h,error,Tin,0.d0)
      k=2.2d-5
      alfa=k*ht/h**2.d0
      a=-alfa
      a(1)=0.d0
      b=1.d0+2.d0*alfa
      c=-alfa
      c(49)=0.d0

      DO j=1,49
        r(j)=T0out(j)
      END DO

      r(1)=T0out(1)+alfa*T0out(0)
      r(49)=T0out(49)+alfa*T0out(50)

      WRITE(2,*) '0',r(2),r(14),r(42)  !punts demanats x

      DO i=1,6000
        CALL tridiag(a,b,c,r,psi,imax)
        WRITE(2,*) dble(i*ht),psi(2),psi(14),psi(42)

        r=psi
        r(1)=psi(1)+alfa*T0out(0)
        r(49)=psi(49)+alfa*T0out(50)
      END DO

      CLOSE(2)

C ** apartat 2b*********************************************************
      kk(1)=2.2d-5
      kk(2)=1.29d-4
      betha=0.00015d0
      OPEN (3,FILE='apartat2b.dat')

      DO n=1,2

        k=kk(n) !HEM DE TORNAR A DEFINIR ELS PARAMETRES dinici
        alfa=k*ht/h**2.d0
        a=-alfa
        a(1)=0.d0
        b=1.d0+2.d0*alfa
        c=-alfa
        c(49)=0.d0
        CALL RELAX (T0in,T0out,Lx,h,error,Tin,0.d0)

        DO j=1,49
          r(j)=T0out(j)
        END DO
        r(1)=T0out(1)+alfa*T0out(0)
        r(49)=T0out(49)+alfa*T0out(50)

        tmitj=(T0out(0)+sum(r)+T0out(50))/(dble(nx))
        WRITE(3,*) '0',tmitj

        DO i=1,6000
          CALL tridiag(a,b,c,r,psi,imax)

          tmitj=(T0out(0)+sum(psi)+T0out(50))/(dble(nx))
          WRITE(3,*) dble(i*ht),tmitj

          r=psi
          r(1)=psi(1)+alfa*T0out(0)
          r(49)=psi(49)+alfa*T0out(50)
        END DO

        WRITE(3,*)
        WRITE(3,*)
      END DO
      CLOSE(3)

C** animacio **********************************************************
      OPEN (5,FILE='gif.dat')
      k=1.29d-4
      betha=0.002d0
      CALL RELAX (T0in,T0out,Lx,h,error,Tin,0.d0)
      DO j=0,50  !escribim lestat d'equilibri
        WRITE(5,*) j*h,T0out(j)
      END DO
      WRITE(5,*)
      WRITE(5,*)


      alfa=k*ht/h**2.d0
      a=-alfa
      a(1)=0.d0
      b=1.d0+2.d0*alfa
      c=-alfa
      c(49)=0.d0

      DO j=1,49
        r(j)=T0out(j)
      END DO

      r(1)=T0out(1)+alfa*T0out(0)
      r(49)=T0out(49)+alfa*T0out(50)

      DO i=1,6000
        CALL tridiag(a,b,c,r,psi,imax)

        IF (mod(i,60).eq.0.d0) THEN
          WRITE(5,*) '0.00',T0out(0)
          DO j=1,nx-1
            WRITE(5,*) dble(j*h),psi(j)
          END DO
          WRITE(5,*) '1.5',T0out(nx)
          WRITE(5,*)
          WRITE(5,*)
        END IF

        r=psi
        r(1)=psi(1)+alfa*T0out(0)
        r(49)=psi(49)+alfa*T0out(50)
      END DO

      CLOSE(5)

      CALL SYSTEM('gnuplot -p scriptP10.plt')
      END



      SUBROUTINE RELAX(T1,T2,Lx,h,error,Tin,w)
      IMPLICIT NONE
      INTEGER i,j,nm,q
      EXTERNAL difusio
      DOUBLE PRECISION Lx,h,error,difusio,Tin,w,erro
      DOUBLE PRECISION T1(0:50),T2(0:50),betha,k
      common/dades/betha,k

        nm=100000 !nombre maxim d'iteracions en cas de no convergir
        q=0

C *********metode relaxacio *******************************
C ** GENEREM LES MATRIUS DE LES TEMPERATURES
        DO j=0,50
          IF (j.eq.0)then
            T1(j)=22.d0
            T2(j)=22.d0
          ELSE IF (j.eq.(Lx/h)) then
            T1(j)=280.d0
            T2(j)=280.d0
          ELSE
            T1(j)=Tin
            T2(j)=Tin
          END IF
        END DO
C **formula
        erro=1.d0 !donem un valor inicial al error per iniciar el bucle
        DO while (erro.ge.error.and.q.le.nm)
          erro=0.d0
          DO j=1,50-1
            T2(j)=0.5d0*(T1(j+1)+T1(j-1)+((h/2.d0)**2)*
     +       (1.d0/k)*(-betha)*T1(j))
	      erro=erro+dabs(T1(j)-T2(j))!comparem el valor del mitg de les dos matrius
          END DO
          !write(*,*) erro
          T1=T2 !cambiem la matriu dels resultats per la de les dades
          q=q+1 !contador
        END DO
      END SUBROUTINE

      SUBROUTINE CRANK(T0,Tt,npasos,h,ht,k,error)
      IMPLICIT NONE
      INTEGER npasos,j,t,nm,q
      DOUBLE PRECISION T0(0:140),Tt(0:140,0:10000)
      DOUBLE PRECISION ht,error,erro,k,h,r

        nm=10000 !nombre maxim d'iteracions en cas de no convergir
        q=0

C ** GENEREM LA MATRIU DE TEMPS ****************************************
      !j == index x
      !t == index temps

        DO t=0,10000
          DO j=0,140
            IF (t.EQ.0) then
              Tt(j,t)=T0(j)
            ELSE IF (j.EQ.0) then
              Tt(j,t)=15.d0
            ELSE IF (j.EQ.140) then
              Tt(j,t)=55.d0
            ELSE
              Tt(j,t)=0.d0
            END IF
          END DO
        END DO

C **formula
       erro=1.d0
        DO WHILE (q.lt.nm.AND.erro.ge.error)
          !erro=0.d0
          r=(k*ht)/(2.d0*h**2)
          DO t=0,10000-1
            DO j=1,139
              Tt(j,t+1)=(r*Tt(j+1,t)+(1.d0-2.d0*r)*Tt(j,t)+r*Tt(j-1,t)+
     +        r*Tt(j-1,t+1)+r*Tt(j+1,t+1))/(1.d0+2.d0*r)
              !erro=erro+dabs(Tt(j,t)-(r*Tt(j+1,t)+(1.d0-2.d0*r)*
     +        !Tt(j,t)+r*Tt(j-1,t)+
     +        !r*Tt(j-1,t+1)+r*Tt(j+1,t+1))/(1.d0+2.d0*r))
              !write(*,*)erro
            END DO
          END DO
C          erro=dabs((r*Tt(71,5000)+(1.d0-2.d0*r)*Tt(70,5000)+r*
c     +     Tt(69,5000)+
c     +     r*Tt(69,5001)+r*Tt(71,5001))/(1.d0+2.d0*r)-Tt(70,5000))
          q=q+1
        END DO
      END SUBROUTINE

      SUBROUTINE TRIDIAG(A,B,C,R,PSI,IMAX)
        IMPLICIT double precision (A-H,K,O-Z)
        IMPLICIT INTEGER (I-J , L-N)
        double precision  BET
        double precision  GAM(4001)
        double precision A(IMAX),B(IMAX),C(IMAX),R(IMAX),PSI(IMAX)

        IF(B(1).EQ.0.) PAUSE
        BET=B(1)
        PSI(1)=R(1)/BET
        DO 11 J=2,IMAX
        GAM(J)=C(J-1)/BET
        BET=B(J)-A(J)*GAM(J)
        IF(BET.EQ.0) PAUSE
        PSI(J)=(R(J)-A(J)*PSI(J-1))/BET
11      CONTINUE

        DO 12 J=IMAX-1,1,-1
        PSI(J)=PSI(J)-GAM(J+1)*PSI(J+1)
12      CONTINUE

       RETURN
       END SUBROUTINE




