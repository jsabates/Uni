      PROGRAM JUNY2018
      IMPLICIT NONE


      DOUBLE PRECISION sigmau,TIME,T,a,b,N,DELTAH
      DOUBLE PRECISION , DIMENSION (0:51,0:51) :: S
      INTEGER M,L
      INTEGER i,j,k

!********APARTAT A **************************************************
!******* DADES INICIALS

      L=50 !COSTAT DE LA CAIXA
      T=2.3d0 !temperatura inicial
      N=L*L !nombre d'espins
      TIME=0.d0 !temps inicial
      M=0 !magnetitzacio inicial

      DO i=0,51
        DO j=0,51
          S(i,j) = 0 !definim les vores dels espins =0
        END DO
      END DO

!****definim la matriu d'espins triats a l'atzar

      DO i=1,50
        DO j=1,50
          CALL RANDOM_NUMBER (a) !valor aleatori entre 0 i 1
          IF (a.ge.0.5d0) then
            b=1 !valor que prendra el sigma
          ELSE IF (a.lt.0.5d0) then
            b=-1
          ENDIF
          S(i,j)=b !definim la matriu de sigmes aleatories
        END DO
      END DO

!********** Calculem M inicial**********************************************
      DO i=1,50
        DO j=1,50
          M=M+S(i,j) !calculem la magnetitzacio inicial total
        END DO
      END DO
      WRITE(*,*) 'la magnetitzacio total inical es de ',M
C************************************************************************

C ****** guardem les cordenades dels nodes amb espi +1 al temps 0
      OPEN(42,FILE='time0.dat')
      DO i=1,50
        DO j=1,50
           IF (S(i,j).EQ.1.d0) THEN
             WRITE(42,*) i,j
           END IF
        END DO
      END DO
      CLOSE(42)
C **********************************************************************

C***** inicialitzem el bucle que cridara la subroutina metropolis per diferents temps
      OPEN(555,FILE='magnetitzacio.dat')

      DO WHILE (TIME.LT.100.d0)

      CALL METROPOLIS(S,L,T,M,TIME)

      IF (int(TIME).EQ.1) THEN  !guardem la posicio dels espins +1 pel temps=1
        OPEN(10,FILE='time1.dat')
        DO i=1,50
          DO j=1,50
             IF (S(i,j).EQ.1.d0) THEN
               WRITE(10,*) i,j
             END IF
          END DO
        END DO
        CLOSE(10)
      END IF

      IF (int(TIME).EQ.10) THEN  !guardem la posicio dels espins +1 pel temps=10
        OPEN(100,FILE='time10.dat')
        DO i=1,50
          DO j=1,50
             IF (S(i,j).EQ.1.d0) THEN
               WRITE(100,*) i,j
             END IF
          END DO
        END DO
        CLOSE(100)
      END IF

      write(555,*) TIME,M
      END DO

      CLOSE(555)
C ****** guardem les cordenades dels nodes amb espi +1 al temps 100

      OPEN(1000,FILE='time100.dat')
      DO i=1,50
        DO j=1,50
          IF (S(i,j).EQ.1.d0) THEN
            WRITE(1000,*) i,j
          END IF
        END DO
      END DO
      CLOSE(1000)

C***********************************************************************
C*****calculem la magnetitzacio total final

      M=0
      DO i=1,50
        DO j=1,50
          M=M+S(i,j)
        END DO
      END DO
      WRITE(*,*) 'la magnetitzacio total final es de ',M


!********APARTAT B **************************************************
!******* DADES INICIALS

      L=50 !COSTAT DE LA CAIXA
      T=3.5d0 !temperatura inicial
      N=L*L !nombre d'espins
      TIME=0.d0 !temps inicial
      M=0 !magnetitzacio inicial

      DO i=0,51
        DO j=0,51
          S(i,j) = 0 !definim les vores dels espins =0
        END DO
      END DO

!****definim la matriu d'espins triats a l'atzar

      DO i=1,50
        DO j=1,50
          S(i,j)=1 !definim la matriu de sigmes aleatories
        END DO
      END DO

!********** Calculem M inicial**********************************************
      DO i=1,50
        DO j=1,50
          M=M+S(i,j) !calculem la magnetitzacio inicial total
        END DO
      END DO
      WRITE(*,*) 'la magnetitzacio total Inicial BBBB es de ',M
C************************************************************************

C ****** guardem les cordenades dels nodes amb espi +1 al temps 0
      OPEN(24,FILE='time0B.dat')
      DO i=1,50
        DO j=1,50
           IF (S(i,j).EQ.1.d0) THEN
             WRITE(24,*) i,j
           END IF
        END DO
      END DO
      CLOSE(24)
C **********************************************************************

C***** inicialitzem el bucle que cridara la subroutina metropolis per diferents temps
      OPEN(666,FILE='magnetitzacioB.dat')

      DO WHILE (TIME.LT.100.d0)

      CALL METROPOLIS(S,L,T,M,TIME)

      IF (int(TIME).EQ.1) THEN  !guardem la posicio dels espins +1 pel temps=1
        OPEN(20,FILE='time1B.dat')
        DO i=1,50
          DO j=1,50
             IF (S(i,j).EQ.1.d0) THEN
               WRITE(20,*) i,j
             END IF
          END DO
        END DO
        CLOSE(20)
      END IF

      IF (int(TIME).EQ.10) THEN  !guardem la posicio dels espins +1 pel temps=10
        OPEN(200,FILE='time10B.dat')
        DO i=1,50
          DO j=1,50
             IF (S(i,j).EQ.1.d0) THEN
               WRITE(200,*) i,j
             END IF
          END DO
        END DO
        CLOSE(200)
      END IF

      write(666,*) TIME,M
      END DO

      CLOSE(666)
C ****** guardem les cordenades dels nodes amb espi +1 al temps 100

      OPEN(2000,FILE='time100B.dat')
      DO i=1,50
        DO j=1,50
          IF (S(i,j).EQ.1.d0) THEN
            WRITE(2000,*) i,j
          END IF
        END DO
      END DO
      CLOSE(2000)

C***********************************************************************
C*****calculem la magnetitzacio total final

      M=0
      DO i=1,50
        DO j=1,50
          M=M+S(i,j)
        END DO
      END DO
      WRITE(*,*) 'la magnetitzacio total final BBB es de ',M



      CALL SYSTEM('gnuplot -p script.plt')

      END




      SUBROUTINE METROPOLIS(S,L,T,M,TIME)
      IMPLICIT NONE
      INTEGER M,L,u,v,j,k
      DOUBLE PRECISION T,TIME,N,DELTAH,prob
      DOUBLE PRECISION , DIMENSION (L,L) :: S
      integer :: i
      integer :: a(50) = (/ (i, i = 1, 50) /)
      real :: r

      N=L*L !nombre de espins totals

      CALL random_seed
      CALL random_number(r)
      u=a(int(r*size(a)) + 1) !GENERADOR DE NOMBRES ALEATORIS ENTRE 1 I 50

      CALL random_seed
      CALL random_number(r)
      v=a(int(r*size(a)) + 1) !escollim un espin al atzar

      DELTAH=2.d0*S(u,v)*(S(u,v+1)+S(u,v-1)+S(u+1,v)+S(u-1,v)) !calculem el delta de h per lespin seleccionat
C******IMPLEMENTACIO METODE ***********************************
      IF (DELTAH.LT.0.d0) then  !si deltah es mes petit que 0 acceptem el canvi
        S(u,v)=-S(u,v)
      ELSE IF (DELTAH.GE.0.d0) then !si deltah es mes gran o igual a 0 nomes acceptem el canvi si deltah = 0 o T es molt gran
        prob=EXP(-DELTAH/T)
        IF (prob.eq.1.d0) then
          S(u,v)=-S(u,v)
        END IF
      END IF
C**** CALCULEM LA MAGNETITZACIO INSTANTANEA
      M=0
      DO k=1,50
        DO j=1,50
          M=M+S(k,j)
        END DO
      END DO

      TIME=TIME+1/N !incrementem un pas de temps

      END SUBROUTINE
