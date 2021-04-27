      PROGRAM MAIN
      IMPLICIT NONE

      INTEGER,DIMENSION(33) :: seed=166211
      INTEGER n
      DOUBLE PRECISION :: num

      CALL RANDOM_SEED(put=seed)

      DO n=1,5
            CALL RANDOM_NUMBER(num)
            WRITE(*,*) num
      END DO



      END

