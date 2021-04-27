      PROGRAM Hola
      IMPLICIT NONE

      DOUBLE PRECISION u,prob1,prob2,prob3,prob4
      DOUBLE PRECISION cont1,cont2,cont3,cont4,passes
      INTEGER i,n,k,j,dist


      OPEN(1,FILE='data.dat')

      DO k=1,1001
        passes=k

!Dades inicials:
        n=0
        cont1=0
        cont2=0
        cont3=0
        cont4=0


        DO i=1,passes

          CALL Markov(n)
!          WRITE(*,*) "li toca anar a la cela =",n

          IF (n==1) THEN
              cont1=cont1+1
          ELSE IF (n==2) THEN
              cont2=cont2+1
          ELSE IF (n==3) THEN
              cont3=cont3+1
          ELSE IF (n==4) THEN
              cont4=cont4+1
          END IF

!         WRITE(*,*) "Contadors 1,2,3,4 =",cont1,cont2,cont3,cont4


        END DO

        prob1=cont1/passes
        prob2=cont2/passes
        prob3=cont3/passes
        prob4=cont4/passes

        WRITE (1,*) passes-1,prob1,prob2,prob3,prob4

        IF (k==1) THEN
            WRITE (1,*) ""
        END IF

      END DO

      CLOSE(1)

      CALL SYSTEM('gnuplot -p script.plt')


      END

      SUBROUTINE Markov(n)
      IMPLICIT NONE
      DOUBLE PRECISION u
      INTEGER dist,j,n

!        WRITE(*,*) "La cela es =",n

        IF ((n==0).OR.(n==1)) THEN
            call random_number(u)
            j = 1+FLOOR((2)*u)

            IF (j==1) THEN
                n=2
            ELSE
                n=3
            END IF

        ELSE IF (n==2) THEN
            call random_number(u)
            j = 1+FLOOR((2)*u)

            IF (j==1) THEN
                n=1
            ELSE
                n=3
            END IF

        ELSE IF (n==3) THEN
            call random_number(u)
            j = 1+FLOOR((3)*u)

            IF (j==1) THEN
                n=1
            ELSE IF (j==2) THEN
                n=2
            ELSE
                n=4
            END IF

        ELSE IF (n==4) THEN
            n=3

        END IF

      END SUBROUTINE

