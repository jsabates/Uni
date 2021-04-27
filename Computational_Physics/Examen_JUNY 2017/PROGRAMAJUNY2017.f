      PROGRAM MAIN
      IMPLICIT NONE

      DOUBLE PRECISION t,prev
      INTEGER i,ind,poblacio1(10000),poblacio2(100000),infectats

****generem les poblacions infectades a t=0
      t=0.d0
      DO i=1,10000
        poblacio1(i)=1
      END DO

      DO i=1,100000
        poblacio2(i)=1
      END DO



      OPEN(1,FILE='data1.dat')
********************************POBLACIO N1 PROB 1********************
      infectats=10000
      DO WHILE(t.lt.40.d0)
      CALL EPIDEMIA(1,poblacio1,1,infectats)
c      write(*,*) 'infectats totals', infectats
      IF (infectats.EQ.0) THEN
        GO TO 11
      END IF
      t=t+((2.d0/3.d0)/infectats)
      prev=infectats/10000.d0
      IF (mod (int(t*1000.d0),10).eq.0) THEN
        WRITE(1,*) t,prev
      END IF
      END DO
11    WRITE(*,*) 'FINISH POBLACIO 1 PROB 1'
      WRITE(1,*)
      WRITE(1,*)

***************************POBLACIO N1 PROB 2 *************************
      t=0.d0
      DO i=1,10000
        poblacio1(i)=1
      END DO

      DO i=1,100000
        poblacio2(i)=1
      END DO

      infectats=10000
      DO WHILE(t.lt.40.d0)
      CALL EPIDEMIA(1,poblacio1,2,infectats)
c      write(*,*) 'infectats totals', infectats
      IF (infectats.EQ.0) THEN
        GO TO 22
      END IF

      t=t+((1.d0/2.d0)/infectats)
      prev=infectats/10000.d0
      IF (mod (int(t*1000.d0),10).eq.0) THEN
        WRITE(1,*) t,prev
      END IF
      END DO
22    WRITE(*,*) 'FINISH POBLACIO 1 PROB 2'
      WRITE(1,*)
      WRITE(1,*)

***************************POBLACIO N1 PROB 3 *************************
      t=0.d0
      DO i=1,10000
        poblacio1(i)=1
      END DO

      DO i=1,100000
        poblacio2(i)=1
      END DO

      infectats=10000
      DO WHILE(t.lt.40.d0)
      CALL EPIDEMIA(1,poblacio1,3,infectats)
c      write(*,*) 'infectats totals', infectats
      IF (infectats.EQ.0) THEN
        GO TO 33
      END IF

      t=t+((2.d0/5.d0)/infectats)
      prev=infectats/10000.d0
      IF (mod (int(t*1000.d0),10).eq.0) THEN
        WRITE(1,*) t,prev
      END IF
      END DO
33    WRITE(*,*) 'FINISH POBLACIO 1 PROB 3'
      WRITE(1,*)
      WRITE(1,*)


***************************POBLACIO N2 PROB 1 *************************
      t=0.d0
      DO i=1,10000
        poblacio1(i)=1
      END DO

      DO i=1,100000
        poblacio2(i)=1
      END DO

      infectats=100000
      DO WHILE(t.lt.40.d0)
      CALL EPIDEMIA(2,poblacio2,1,infectats)
c      write(*,*) 'infectats totals', infectats
      IF (infectats.EQ.0) THEN
        GO TO 44
      END IF

      t=t+((2.d0/3.d0)/infectats)
      prev=infectats/100000.d0
      IF (mod (int(t*1000.d0),10).eq.0) THEN
        WRITE(1,*) t,prev
      END IF
      END DO
44    WRITE(*,*) 'FINISH POBLACIO 2 PROB 1'
      WRITE(1,*)
      WRITE(1,*)

***************************POBLACIO N2 PROB 2 *************************
      t=0.d0
      DO i=1,10000
        poblacio1(i)=1
      END DO

      DO i=1,100000
        poblacio2(i)=1
      END DO

      infectats=100000
      DO WHILE(t.lt.40.d0)
      CALL EPIDEMIA(2,poblacio2,2,infectats)
c      write(*,*) 'infectats totals', infectats
      IF (infectats.EQ.0) THEN
        GO TO 55
      END IF

      t=t+((1.d0/2.d0)/infectats)
      prev=infectats/100000.d0
      IF (mod (int(t*1000.d0),10).eq.0) THEN
        WRITE(1,*) t,prev
      END IF
      END DO
55    WRITE(*,*) 'FINISH POBLACIO 2 PROB 2'
      WRITE(1,*)
      WRITE(1,*)


***************************POBLACIO N2 PROB 3 *************************
      t=0.d0
      DO i=1,10000
        poblacio1(i)=1
      END DO

      DO i=1,100000
        poblacio2(i)=1
      END DO

      infectats=100000
      DO WHILE(t.lt.40.d0)
      CALL EPIDEMIA(2,poblacio2,3,infectats)
c      write(*,*) 'infectats totals', infectats
      IF (infectats.EQ.0) THEN
        GO TO 66
      END IF

      t=t+((2.d0/5.d0)/infectats)
      prev=infectats/100000.d0
      IF (mod (int(t*1000.d0),10).eq.0) THEN
        WRITE(1,*) t,prev
      END IF
      END DO
66    WRITE(*,*) 'FINISH POBLACIO 2 PROB 3'
      WRITE(1,*)
      WRITE(1,*)

      CLOSE(1)

      CALL SYSTEM('gnuplot -p script.plt')




      END

      SUBROUTINE EPIDEMIA(n,poblacio,probabilitat,infectats)
* n = escollim la poblacio de 10000 o 100000 individus
*n=1  poblacio de 10000
*n=2  poblacio de 100000
*poblacio = llista de individus amb els valors de infectat o no
*ind = triem el individu dins de la llista ( la posicio )
*ind2 = triem un segon individu per infectar
*estat = valor que pren la posicio de la llista , indica si esta infectat o no
*probabilitat =  quina probabilitat escollim 1(2/3),2(1/2),3(2/5)
      IMPLICIT NONE
      INTEGER n,ind,i,j,poblacio(100000),estat,probabilitat
      INTEGER ind2,infectats,resultat
*SELECCIONEM UN INDIVIDU INFECTAT ALEATORI********************
      estat=0
      DO WHILE (estat.ne.1)
c      write(*,*) 'seleccionem un individu infectat aleatori'
      CALL PICKRANDOM(n,ind)
c      write(*,*) 'lindividu seleccionat es ',ind
      estat=poblacio(ind)
c      write(*,*) 'lestat de lindividu es',estat
      END DO
*SELECCIONEM QUINA ACCIO REALITZEM , la p (res=1) o la 1-p (res=0)
      CALL PROBABILITY(probabilitat,resultat)
      IF (resultat.EQ.1) THEN
c        write(*,*) 'com que el resultat es 1 curem'
c        write(*,*) 'estat previ'
c        write(*,*) poblacio(ind)
c        write(*,*) 'estat posterior'
        poblacio(ind)=0
        infectats=infectats-1
c        write(*,*) poblacio(ind)
      ELSE IF (resultat.EQ.0) THEN
c        write(*,*) 'com que el resultat es 0 infectem'
        CALL PICKRANDOM(n,ind2)
c        write(*,*) 'la victima es',ind2
c        write(*,*)'lestat previ de la victima es', poblacio(ind2)
        IF (poblacio(ind2).EQ.0) THEN
        poblacio(ind2)=1
        infectats=infectats+1
c        write(*,*) 'pertant infectem i sumem 1 als infectats'
        END IF
c        write(*,*) 'lestat posterior de la victima es',poblacio(ind2)
      END IF



      END SUBROUTINE

      SUBROUTINE PICKRANDOM(n,ind)  !SUBROUTINA que escull un individu al atzar tenint en compte si es la epidemia de 10^4 o 10^5
      INTEGER i,n,ind
      INTEGER :: a(10000),b(100000)
      REAL :: r

      DO i =1,100000
        a(i)=i
        b(i)=i
      END DO

      IF (n.eq.1) then  !epidemia de 10^4

        CALL random_seed
        CALL random_number(r)
        ind=a(int(r*size(a)) + 1) !GENERADOR DE NOMBRES ALEATORIS


      ELSE IF (n.eq.2) then !epidemia de 10^5
        CALL random_seed
        CALL random_number(r)
        ind=b(int(r*size(b)) + 1)
      END IF
      END SUBROUTINE

      SUBROUTINE PROBABILITY(n,resultat)
      INTEGER n,resultat,i,control
      INTEGER :: a(3) = (/ (i, i = 1, 3) /)
      INTEGER :: b(2) = (/ (i, i = 1, 2) /)
      INTEGER :: c(5) = (/ (i, i = 1, 5) /)
      REAL :: r

      IF (n.eq.1) THEN
        CALL random_seed
        CALL random_number(r)
        control=a(int(r*size(a)) + 1)
        IF ((control.EQ.1).OR.(control.EQ.2)) THEN
          resultat=1
        ELSE IF (control.EQ.3) THEN
          resultat=0
        END IF
c      write(*,*) 'probabilitat 2/3'
c      write(*,*) 'valor de control', control
c      write(*,*) 'pertant el resultat es ',resultat
      ELSE IF (n.eq.2) THEN
        CALL random_seed
        CALL random_number(r)
        control=b(int(r*size(b)) + 1)
        IF (control.EQ.1) THEN
          resultat=1
        ELSE IF (control.EQ.2) THEN
          resultat=0
        END IF
c      write(*,*) 'probabilitat 1/2'
c      write(*,*) 'valor de control', control
c      write(*,*) 'pertant el resultat es ',resultat
      ELSE IF (n.eq.3) THEN
        CALL random_seed
        CALL random_number(r)
        control=c(int(r*size(c)) + 1)
        IF ((control.EQ.1).OR.(control.EQ.2)) THEN
          resultat=1
        ELSE
          resultat=0
        END IF
c      write(*,*) 'probabilitat 2/5'
c      write(*,*) 'valor de control', control
c      write(*,*) 'pertant el resultat es ',resultat
      END IF
      END SUBROUTINE
