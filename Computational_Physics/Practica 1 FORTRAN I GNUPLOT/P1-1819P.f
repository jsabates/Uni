c Practica1


      PROGRAM Practica1
      IMPLICIT NONE
      INTEGER k,i,M,N,L
      DOUBLE PRECISION SUMA,P,sasim
!definim variables
c 1)
10    WRITE(*,*) 'Introdueix un enter del 5 al 301'
      READ(*,*) k
      IF(k.gt.301.d0) GO TO 10
      IF(k.lt.5.d0) GO TO 10
      WRITE(*,*) 'el valor de Pk es = ',P(k)
!demanem el valor k i cridem la funcio p(k)
c 2)
      M=14
      N=70
      WRITE(*,*) 'valor de la suma del P14 fins P70 es =',SUMA(M,N)
!cridem la funcio suma
C 3)
      OPEN(1,file='P1-1819P-res1.dat')
      DO N=4,210,2
       WRITE(1,*) N,SUMA(3,N)
      END DO
      WRITE(*,*) 'fitxer 1 generat'
!creem un arxiu on guardarem les dades de la N i la SUMA

      OPEN(3,file='P1-1819P-res3.dat')
C 5)
      OPEN(2,file='P1-1819P-res2.dat')
      DO L=4,210,2
       sasim = (5.d0/9.d0)*L**3
       WRITE(3,*) L,sasim
       WRITE(2,*) L,(SUMA(3,L)/sasim)
      END DO
      CALL system('gnuplot -p gnuplot.plt')
      WRITE(*,*) 'fitxers i grafiques generades'
!creem un altre arxiu on guardarem les noves dades i generem la ultima grafica

      END

      DOUBLE PRECISION FUNCTION P(k) !funcio que retorna el valor Pk
      INTEGER k
      DOUBLE PRECISION PI
      PI=DACOS(-1.D0)
      P=(5.d0/3.d0)*k**2+PI-2*k
      END

      DOUBLE PRECISION FUNCTION SUMA(M,N) !funcio que retorna el valor suma dels PM fins PN
      INTEGER M,N
      DOUBLE PRECISION P
      SUMA=0
      DO i=M,N
       SUMA=SUMA+P(i)
      END DO
      END
