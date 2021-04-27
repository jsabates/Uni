c Escrivim un programa que llegeixi numeros enters, k, entre 3 i 35, i fa que el programa
c escrigui en pantalla el valor corresponent Pk


      PROGRAM P1fibo
      IMPLICIT NONE

      INTEGER n,k,i,suma, n2
      DOUBLE PRECISION sumanum,sumaden
      INTEGER,ALLOCATABLE :: lista(:)
c declarem les variables necessaries pel programa i creem una llista dinamica


10    WRITE(*,*) 'introdueix un enter del 3 al 35'
      READ(*,*) k
c demanem per pantalla el valor enter "k" i el llegim

      IF(k.gt.35.d0) GO TO 10
      IF(k.lt.3.d0) GO TO 10
c comprovem que el valor de k esta compres entre 3 i 35

      ALLOCATE(lista(k))
c establim la dimensio de la llista

      lista(1)=0
      lista(2)=1
c implementem els primers valors de la serie de fibonacci 

      DO i=3,k
	
	lista(i)=lista(i-1)+lista(i-2)

      END DO

c amb un DO/ENDDO nem incorporant els valors de la serie de fibonacci dins la llista

      WRITE (*,*) 'el valor de Pk es = ',lista(k)
c mostrem el nombre Pk en pantalla (el valor a la posicio k de la llista dels nombres de fibonacci)

      DEALLOCATE(lista)
c IMPORTANT treiem la dimensio de la llista previament definida pq cuan es torni a executar el programa no ens dongui error de que ja te una dimensio donada XD 

c SEGONA PART DEL PROGRAMA LA SUMA 
      ALLOCATE(lista(32))
c tornem a definir la dimensio de la llista per la suma ( si utilitzem la llista anterior la dimensio dependera de k, i N esta fixat pel problema)

      lista(1)=1
      lista(2)=1
c tornem a definir els primers valors de la serie

      DO i=3,32
	lista(i)=lista(i-1)+lista(i-2)

      END DO
c Generem la llista nova amb els 33 valors (33 ja que el primer es 0)

      suma=0
      DO i=4,32
	suma=suma+lista(i)
      END DO
c Definim la variable suma=0 i amb un DO/ENDDO li nem donant els nous valors fins a 32

      WRITE (*,*) suma
c MOstrem per pantalla el resultat de "suma"


c UTILITZAREM LA LLISTA ANTERIOR PER LA 3A PART
      open(1,file='P1-1819P-res1.dat')

      DO n2=4,20
	sumanum=0
	DO i=3,n2
	 sumanum=sumanum+lista(i)
	END DO
	sumaden=0
	DO i=4,n2
	 sumaden=sumaden+lista(i)
	END DO
	WRITE (1,*) n2,sumanum/sumaden, sumanum, sumaden
      END DO

      DEALLOCATE(lista)
c IMPORTANT treiem la dimensio de la llista previament definida pq cuan es torni a executar el programa no ens dongui error de que ja te una dimensio donada XD 


      END

