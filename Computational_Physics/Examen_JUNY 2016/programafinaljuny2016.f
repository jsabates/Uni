      PROGRAM percolacio1
      IMPLICIT NONE

      INTEGER resultat,prob,x,y,xx,yy,i,j,contador,L,yeah,w
      INTEGER,DIMENSION(29,14) :: matriu
***DADES INICIALS
      L=10

***apartat a*********************************************************
      OPEN(1,FILE='percolacio1.dat')
      CALL PERCOLACIO(6,L,matriu,y)
!      write(*,*) 'hem arribat fins la fila',y
      write(1,*)
      write(1,*)
      CALL PERCOLACIO(12,L,matriu,y)
!      write(*,*) 'hem arribat fins la fila',y

c      do i=10,1,-1
c      write(1,'(21(2I4))') (matriu(j,i),j=1,21)
c      end do
      close(1)

***apartat b*********************************************************
      open(2,FILE='percolacio2.dat')
      yeah=0 !contador de repeticions que arrivem a la generacio L
      DO i=1,10000
        CALL PERCOLACIO(6,L,matriu,y)
        IF (y.EQ.0) then
          yeah=yeah+1
        END IF

      END DO

      write(2,*) 'prob 1 fraccio',(yeah/10000.d0),'sobre',yeah
      write(2,*)
      write(2,*)

      yeah=0 !contador de repeticions que arrivem a la generacio L
      DO i=1,10000
      CALL PERCOLACIO(12,L,matriu,y)
      IF (y.EQ.0) then
        yeah=yeah+1
      END IF

      END DO

      write(2,*) 'prob 2 fraccio',(yeah/10000.d0),'sobre',yeah

      close(2)

***apartat c ********************************************************
      OPEN(3,FILE='percolacio3.dat')

      DO L=8,14,2
        write(*,*) L
        DO w=0,20
          yeah=0
          DO i=1,10000

            CALL PERCOLACIO(w,L,matriu,y)
            IF (y.EQ.0) then
              yeah=yeah+1
            END IF

          END DO

          write(3,*) (w/20.d0),yeah/10000.d0

        END DO
        write(3,*)
        write(3,*)
      END DO

      close(3)


      CALL SYSTEM('gnuplot -p script.plt')

      END



      SUBROUTINE PROBABILITY (n,d,resultat)
      INTEGER n,d,resultat,i,control,a(d)
      REAL :: r

        DO i=1,d
          a(i)=i
        END DO

        CALL random_seed
        CALL random_number(r)
        control=a(int(r*size(a)) + 1)
        DO i=1,n
          IF (control.EQ.a(i)) THEN
            resultat=1
            go to 42
          END IF
        END DO
      resultat=0
42    continue
      END SUBROUTINE


      SUBROUTINE PERCOLACIO(prob,L,matriu,y)
      IMPLICIT NONE
      INTEGER prob,L,i,j,x,y,contador,resultat,nodes
      INTEGER,DIMENSION (2*L+1,L) :: matriu

      DO i=1,(2*L+1)                                                       !generem la matriu de valors
        DO j=1,L
          matriu(i,j)=0
        END DO
      END DO

      matriu(L+1,L)=1

      DO y=L,1,-1                                                       !bucle que repasa cada altura
        contador=0
        nodes=0
        DO x=1,(2*L+1)
          IF (matriu(x,y).EQ.1) THEN
            nodes=nodes+1                                               !per cada x amb valor 1 sumem un node
            IF (y.ne.1) then
              CALL PROBABILITY (prob,20,resultat)
              IF (resultat.EQ.1) THEN
                matriu(x+1,y-1)=1
              END IF
              CALL PROBABILITY (prob,20,resultat)
              IF (resultat.EQ.1) THEN
                matriu(x-1,y-1)=1
              END IF
            END IF

          ELSE IF ((matriu(x,y).EQ.0)) then
            contador=contador+1
            IF (contador.EQ.(2*L+1)) then                               si el contador arriba a 21 surtim pq no hi ha descendencia i restem un a sobreviu
              GO TO 5555
            END IF
          END IF
        END DO

        write(1,*) L-y,nodes

      END DO
5555  continue
      END SUBROUTINE
