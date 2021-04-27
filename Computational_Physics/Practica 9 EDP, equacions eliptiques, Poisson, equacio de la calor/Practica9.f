      PROGRAM MAIN
      IMPLICIT NONE

      EXTERNAL p
      DOUBLE PRECISION Lx,Ly,h,p,error
      DOUBLE PRECISION T1(0:67,0:91),T2(0:67,0:91)
      INTEGER i,j,n
      COMMON/dades/Lx,Ly,h

C ** DADES INICIALS ***************************************************
      !j==index x
      !n==index y
      !T1==matriu de dades
      !T2==matriu de "resultats"
      !icontrol=1 == relaxacio
      !icontrol=2 == jacobi
      !icontrol=3 == sobrerelaxacio

      Lx=33.5d0 !mida caixa x
      Ly=45.5d0 !mida caixa y
      h=0.5d0 !delta
C** APARTAT 3 *********************************************************
      OPEN (11,FILE='conmet.dat')
      WRITE(11,*)'relaxacio 10'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-6,1,10.d0,1.35d0)
      WRITE(11,*)''
      WRITE(11,*)''
      WRITE(11,*)'sobre-relaxacio 10'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-6,3,10.d0,1.35d0)
      WRITE(11,*)''
      WRITE(11,*)''
      WRITE(11,*)'relaxacio 120'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-6,1,120.d0,1.35d0)
      WRITE(11,*)''
      WRITE(11,*)''
      WRITE(11,*)'sobre-relaxacio 120'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-6,3,120.d0,1.35d0)
      WRITE(11,*)''
      WRITE(11,*)''
      WRITE(11,*)'relaxacio 1040'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-6,1,1040.d0,1.35d0)
      WRITE(11,*)''
      WRITE(11,*)''
      WRITE(11,*)'sobre-relaxacio 1040'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-6,3,1040.d0,1.35d0)
      CLOSE(11)
C**********************************************************************
C** apartat 4**********************************************************

      CALL RELAX(T1,T2,Lx,Ly,h,1.d-6,1,10.d0,1.35d0)

      OPEN (1,FILE='prova.dat')
        DO j=0,67
          DO n=0,91
            WRITE(1,*) dble(j)*h,dble(n)*h,T1(j,n)
          END DO
          WRITE(1,*)''
        END DO
      CLOSE(1)
C**********************************************************************
C *****apartat 5 ******************************************************

      CALL RELAXNOFONT(T1,T2,Lx,Ly,h,1.d-6,1,10.d0,1.35d0)

      OPEN (2,FILE='prova2.dat')
        DO j=0,67
          DO n=0,91
            WRITE(2,*) dble(j)*h,dble(n)*h,T1(j,n)
          END DO
          WRITE(2,*)''
        END DO
      CLOSE(2)

      CALL SYSTEM('gnuplot -p scriptP9.plt')




      END


      DOUBLE PRECISION FUNCTION p(x,y) !funcio de les 3 fonts
      DOUBLE PRECISION x,y,r1,r2,p1,p2,p3
      r1=dsqrt(((x-8.d0)**2)+(y-22.5d0)**2)
      p1=10.d0*dexp(-((r1-5.d0)**2)/0.3d0**2)

      IF (x.ge.18.d0 .AND. x.le.22.d0 .AND. y.ge.29.d0 .AND. y.le.35.d0)
     + THEN
        p2=3.d0
      ELSE
        p2=0.d0
      END IF

      r2=dsqrt(((x-22.d0)**2)+(y-10.5d0)**2)
      p3=6.d0*dexp(-((r2-4.d0)**2)/0.8d0**2)


      p=p1+p2+p3

      END FUNCTION



      SUBROUTINE RELAX(T1,T2,Lx,Ly,h,error,icontrol,Tin,w)
      EXTERNAL p
      DOUBLE PRECISION T1(0:67,0:91),T2(0:67,0:91)
      DOUBLE PRECISION Lx,Ly,h,error,p,Tin,w
      INTEGER icontrol,i,n,j,nm,q

      nm=100000 !nombre maxim d'iteracions en cas de no convergir
      q=0 !contador

      IF (icontrol.EQ.1) then
C *********metode relaxacio *******************************
C ** GENEREM LES MATRIUS DE LES TEMPERATURES
        DO n=0,91
          DO j=0,67
          IF (n.EQ.0) then
            T1(j,n)=0.5d0
            T2(j,n)=0.5d0
          ELSE IF (j.EQ.0) then
            T1(j,n)=17.d0
            T2(j,n)=17.d0
          ELSE IF (n.EQ.37) then
            T1(j,n)=11.2d0
            T2(j,n)=11.2d0
          ELSE IF (j.EQ.61) then
            T1(j,n)=25.3d0
            T2(j,n)=25.3d0
          ELSE
            T1(j,n)=Tin
            T2(j,n)=Tin
          END IF
          END DO
        END DO
C **formula
        erro=1.d0 !donem un valor inicial al error per iniciar el bucle
        DO while (erro.ge.error.and.q.le.nm)
          erro=0.d0
          WRITE(11,*) q,T2(15,47) !escrivim en un fitxer la convergencia de la temperatura demanada
          DO n=1,90
            DO j=1,66
            T2(j,n)=0.25d0*(T1(j+1,n)+T1(j-1,n)+T1(j,n+1)+T1(j,n-1)+
     +     ((0.5d0**2)*p(dble(j)*0.5d0,dble(n)*0.5d0)))

	      erro=erro+dabs(T1(j,n)-T2(j,n))!comparem el valor del mitg de les dos matrius
            END DO
          END DO
          !write(*,*) erro
          T1=T2 !cambiem la matriu dels resultats per la de les dades
          q=q+1 !contador
        END DO
C***********************************************************************
      ELSE IF (icontrol.EQ.2) then !NO UTILITZAT
C********************metode jacobi*************************************
        DO n=0,91
          DO j=0,67
          IF (n.EQ.0) then
            T1(j,n)=0.5d0
          ELSE IF (j.EQ.0) then
            T1(j,n)=17.d0
          ELSE IF (n.EQ.37) then
            T1(j,n)=11.2d0
          ELSE IF (j.EQ.61) then
            T1(j,n)=25.3d0
          ELSE
            T1(j,n)=Tin
          END IF
          END DO
        END DO
C **formula
        erro=1.d0
        DO while (erro.ge.error.and.q.le.nm)
          WRITE(22,*) q,T1(15,47) !escrivim en un fitxer la convergencia de la temperatura demanada
          DO n=1,90
            DO j=1,66
            T1(j,n)=0.25d0*(T1(j+1,n)+T1(j-1,n)+T1(j,n+1)+
     +       T1(j,n-1)+((0.5d0**2)*p(dble(j)*0.5d0,dble(n)*0.5d0)))
            !write(*,*) T1(j,n)
            END DO
          END DO
          erro=dabs(T1(33,45)-0.25d0*(T1(34,45)+T1(32,45)+T1(33,46)+
     +     T1(33,44)+
     +     ((0.5d0**2)*p(33.d0*0.5d0,45.d0*0.5d0)))) !comparem el valor del mitg de les dos matrius
          !write(*,*) erro
          q=q+1
        END DO
C***********************************************************************
      ELSE IF (icontrol.EQ.3) then
C********************metode sobrerelaxacio*******************************
        DO n=0,91
          DO j=0,67
          IF (n.EQ.0) then
            T1(j,n)=0.5d0
          ELSE IF (j.EQ.0) then
            T1(j,n)=17.d0
          ELSE IF (n.EQ.37) then
            T1(j,n)=11.2d0
          ELSE IF (j.EQ.61) then
            T1(j,n)=25.3d0
          ELSE
            T1(j,n)=Tin
          END IF
          END DO
        END DO
C **formula
        erro=1.d0
        DO while (erro.ge.error.and.q.le.nm)
          !erro=0.d0
          WRITE(11,*) q,T1(15,47) !escrivim en un fitxer la convergencia de la temperatura demanada
          DO n=1,90
            DO j=1,66
            T1(j,n)=(1.d0-w)*T1(j,n)+(w/4.d0)*(T1(j+1,n)+
     +       T1(j-1,n)+T1(j,n+1)+T1(j,n-1)+
     +     ((0.5d0**2)*p(dble(j)*0.5d0,dble(n)*0.5d0)))

	      !erro=erro+dabs((1.d0-w)*T1(j,n)+(w/4.d0)*(T1(j+1,n)+
     +       !T1(j-1,n)+T1(j,n+1)+T1(j,n-1)+
     +     !((0.5d0**2)*p(dble(j)*0.5d0,dble(n)*0.5d0)))-T1(j,n))!comparem el valor del mitg de les dos matrius
            END DO
          END DO
          erro=dabs((1.d0-w)*T1(33,45)+(w/4.d0)*(T1(34,45)+
     +       T1(32,45)+T1(33,46)+T1(33,44)+
     +     ((0.5d0**2)*p(33.d0*0.5d0,45.d0*0.5d0)))-T1(33,45))!comparem el valor del mitg de les dos matrius
          !write(*,*) erro
          q=q+1
        END DO
C***********************************************************************
      END IF
      END SUBROUTINE

      SUBROUTINE RELAXNOFONT(T1,T2,Lx,Ly,h,error,icontrol,Tin,w)
      DOUBLE PRECISION T1(0:67,0:91),T2(0:67,0:91)
      DOUBLE PRECISION Lx,Ly,h,error,Tin,w
      INTEGER icontrol,i,n,j,nm,q

      nm=100000 !nombre maxim d'iteracions en cas de no convergir
      q=0

      IF (icontrol.EQ.1) then
C *********metode relaxacio *******************************
C ** GENEREM LES MATRIUS DE LES TEMPERATURES
        DO n=0,91
          DO j=0,67
          IF (n.EQ.0) then
            T1(j,n)=0.5d0
            T2(j,n)=0.5d0
          ELSE IF (j.EQ.0) then
            T1(j,n)=17.d0
            T2(j,n)=17.d0
          ELSE IF (n.EQ.37) then
            T1(j,n)=11.2d0
            T2(j,n)=11.2d0
          ELSE IF (j.EQ.61) then
            T1(j,n)=25.3d0
            T2(j,n)=25.3d0
          ELSE
            T1(j,n)=Tin
            T2(j,n)=Tin
          END IF
          END DO
        END DO
C **formula
        erro=1.d0 !donem un valor inicial al error per iniciar el bucle
        DO while (erro.ge.error.and.q.le.nm)
          erro=0.d0
          WRITE(11,*) q,T2(15,47) !escrivim en un fitxer la convergencia de la temperatura demanada
          DO n=1,90
            DO j=1,66
            T2(j,n)=0.25d0*(T1(j+1,n)+T1(j-1,n)+T1(j,n+1)+T1(j,n-1))

	      erro=erro+dabs(T1(j,n)-T2(j,n))!comparem el valor del mitg de les dos matrius
            END DO
          END DO
          !write(*,*) erro
          T1=T2 !cambiem la matriu dels resultats per la de les dades
          q=q+1 !contador
        END DO
      END IF
      END SUBROUTINE
