      PROGRAM MAIN
      IMPLICIT NONE

      INTEGER i,m
      DOUBLE PRECISION PHI,PI,L,R,x(6),t,TEMPS(10000),POSIS(10000)
      DOUBLE PRECISION x1,x2,x3,x4,x5,x6,t1
800   FORMAT(2X,A,8X,A,6X,A,6X,A,5X,A,6X,A,6X,A)
900   FORMAT(F6.3,2x,F6.3,2x,F6.3,2x,F6.3,2x,F6.3,2x,F6.3,2x,F6.3)

C************** APARTAT 3 ********************************************
      OPEN(1,FILE='P2-1819P-res1.dat')
      !WRITE(1,800)'t','x1','x2','x3','x4','x5','x6'
      !WRITE(1,*)''
      DO i=0,5000
      t=dble(i)/1000.d0
      CALL POSICIO (t,x,L,5.d0)
      WRITE(1,900)t,x(1),x(2),x(3),x(4),x(5),x(6)
      END DO
      CLOSE(1)

C****************** APARTAT 4 ****************************************
C******************* APARTAT 5 ***************************************
C********************APARTAT 6 ***************************************
      OPEN(2,FILE='P2-1819P-res1.dat')
      m=1
      DO WHILE(.TRUE.)
      READ(2,*,END=10) t1,x1,x2,x3,x4,x5,x6
      TEMPS(m)=t1
      POSIS(m)=x3
      m=m+1
      END DO
10    CLOSE(2)
      write(*,*)TEMPS(1),POSIS(5)
C****************** APARTAT 7 ****************************************


      CALL system('gnuplot -p script2.plt')

      END


c**************** APARTAT 1 ********************************************
      SUBROUTINE RADIUS(i,L,R)
      INTEGER i
      DOUBLE PRECISION L,R
      L=22.5d0
      R=(L/dble(i))-0.01d0
      END SUBROUTINE

      DOUBLE PRECISION FUNCTION PHI(i)
      PI=DACOS(-1.D0)
      PHI=dble(i)*PI/6.d0
      END FUNCTION
C******************** APARTAT 2 ****************************************
      SUBROUTINE POSICIO(t,x,L,w)
      INTEGER i
      DOUBLE PRECISION t,x(6),L,w,PHI,R
      DO i=1,6
      CALL RADIUS(i,L,R)
      x(i)=R*dcos(w*t+PHI(i))+dsqrt((L**2)-(R**2)*(dsin(w*t
     + + PHI(i))**2))
      END DO
      END SUBROUTINE
