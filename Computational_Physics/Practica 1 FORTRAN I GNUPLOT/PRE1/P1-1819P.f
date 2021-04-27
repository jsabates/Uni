c PREPRACTICA


      PROGRAM PreP1
      IMPLICIT NONE

      INTEGER n,k,i,SUMA,n1,n2,P
      DOUBLE PRECISION sumanum,sumaden,pnum,pden,z
c 1)
10    WRITE(*,*) 'Introdueix un enter del 3 al 35'
      READ(*,*) k
      IF(k.gt.35.d0) GO TO 10
      IF(k.lt.3.d0) GO TO 10
      WRITE(*,*) 'el valor de Pk es = ',P(k)
c 2)
      n1=4
      n2=32
      WRITE(*,*) 'valor de la suma del P4 fins P20 es =',SUMA(n1,n2,P)

c 3)
      OPEN(1,file='P1-1819P-res1.dat')
      DO n2=4,20
       sumanum=SUMA(3,n2,P)
       sumaden=SUMA(4,n2,P)
       WRITE(1,*) n2,sumanum/sumaden
      END DO
      WRITE(*,*) 'fitxer 1 generat'

c 4)
      z=(1+sqrt(5.d0))/2
      OPEN(2,file='P1-1819P-res2.dat')
      DO i=2,12
       pnum=P(i+1)
       pden=P(i)
       WRITE(2,*) i, (pnum/pden)-z
      END DO
      WRITE(*,*) 'fixter 2 generat'
      CALL system('gnuplot -p makedata.plt')
      END

      INTEGER FUNCTION P(n) !funcio que retorna el valor Pk
      INTEGER n,i,P0,P1
      P0=1
      P1=1

      DO i=3,n
       P=P0+P1
       P0=P1
       P1=P
      END DO
      IF((n.eq.0).or.(n.eq.1)) P=1
      END

      INTEGER FUNCTION SUMA(n1,n2,P) !funcio que retorna el valor suma dels Pn1 fins Pn2
      INTEGER n1,n2,P,i
      SUMA=0
      DO i=n1,n2
       SUMA=SUMA+P(i)
      END DO
      END
