      PROGRAM percolacio1
      IMPLICIT NONE

      INTEGER resultat,prob,x,y,xx,yy,i,j,contador,L,sobreviu,yeah,w
      INTEGER,DIMENSION(21,10) :: matriu
***DADES INICIALS
      L=10
***apartat a*********************************************************
c      OPEN(1,FILE='percolacio1.dat')
c      CALL PERCOLACIO(6,L,matriu,sobreviu)
c      write(*,*) 'ha sobreviscut fins la ',sobreviu
c      write(1,*)
c      write(1,*)
c      CALL PERCOLACIO(12,L,matriu,sobreviu)
c      write(*,*) 'ha sobreviscut fins la ',sobreviu

c      do i=10,1,-1
c      write(1,'(21(2I4))') (matriu(j,i),j=1,21)
c      end do
c      close(1)
***apartat b*********************************************************
c      open(2,FILE='percolacio2.dat')
c      yeah=0 !contador de repeticions que arrivem a la generacio L
c      DO i=1,10000
c      CALL PERCOLACIO(6,L,matriu,sobreviu)
c      IF (sobreviu.EQ.10) then
c        yeah=yeah+1
c      END IF

c      END DO

c      write(2,*) 'prob 1 fraccio',(yeah/10000.d0),'sobre',yeah
c      write(2,*)
c      write(2,*)

c      yeah=0 !contador de repeticions que arrivem a la generacio L
c      DO i=1,10000
c      CALL PERCOLACIO(20,L,matriu,sobreviu)
c      IF (sobreviu.EQ.10) then
c        yeah=yeah+1
c      END IF

c      END DO

c      write(2,*) 'prob 2 fraccio',(yeah/10000.d0),'sobre',yeah

c      close(2)

***apartat c ********************************************************
c      OPEN(3,FILE='percolacio3.dat')

c      DO w=0,20
c        yeah=0
c        DO i=1,10000
c          CALL PERCOLACIO(w,L,matriu,sobreviu)
c          IF (sobreviu.EQ.10) then
c            yeah=yeah+1
c          END IF

c        END DO

c        write(3,*) (w/20.d0),yeah/10000.d0

c      END DO


c      close(3)


      CALL PERCOLACIO (20,L,matriu,sobreviu)

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


      SUBROUTINE PERCOLACIO(prob,L,matriu,sobreviu)
      IMPLICIT NONE
      INTEGER prob,L,i,j,x,y,contador,resultat,nodes,sobreviu
      INTEGER,DIMENSION (2*L+1,L) :: matriu

      DO i=1,2*L+1                                                       !generem la matriu de valors
        DO j=1,L
          matriu(i,j)=0
        END DO
      END DO

      matriu(L+1,L)=1                                                    !node inicial
      sobreviu=0                                                         !contador per veure cuantes generacions sobreviu la repeticio
      DO y=L,1,-1                                                       !bucle que repasa cada altura
        contador=0                                                      !contador per sortir del bucle si no hi ha descendencia
        sobreviu=sobreviu+1                                             !per cada altura sobreviu una generacio
        nodes=0                                                         !contador de nodes totals
        DO x=1,2*L+1
          IF (matriu(x,y).EQ.1) THEN
            nodes=nodes+1                                               !per cada x amb valor 1 sumem un node
            CALL PROBABILITY (prob,20,resultat)
           ! write(*,*) resultat                          !apliquem el metode
            IF (resultat.EQ.1) THEN
              matriu(x+1,y-1)=1
            END IF
            CALL PROBABILITY (prob,20,resultat)
            !write(*,*) resultat
            IF (resultat.EQ.1) THEN
              matriu(x-1,y-1)=1
            END IF

          ELSE
            contador=contador+1                                         !si la x no es 1 sumem un al contador
            IF (contador.EQ.(2*L+1)) then                               si el contador arriba a 21 surtim pq no hi ha descendencia i restem un a sobreviu
              sobreviu=sobreviu-1
              GO TO 5555
            END IF
          END IF

        END DO
        write(1,*) L-y,nodes
      END DO

5555  write(*,*) sobreviu



      END SUBROUTINE
