      PROGRAM MAIN
      IMPLICIT NONE

      EXTERNAL p
      DOUBLE PRECISION Lx,Ly,h,p,error
      DOUBLE PRECISION T1(0:61,0:37),T2(0:61,0:37)
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

      Lx=30.5d0 !mida caixa x
      Ly=18.5d0 !mida caixa y
      h=0.5d0 !delta

C ** GENEREM LES MATRIUS DE LES TEMPERATURES
C      DO n=0,37
C        DO j=0,61
C        IF (n.EQ.0) then
C          T1(j,n)=7.d0
C          T2(j,n)=7.d0
C        ELSE IF (j.EQ.0) then
C          T1(j,n)=10.d0
C          T2(j,n)=10.d0
C        ELSE IF (n.EQ.37) then
C          T1(j,n)=13.6d0
C          T2(j,n)=13.6d0
C        ELSE IF (j.EQ.61) then
C          T1(j,n)=30.d0
C          T2(j,n)=30.d0
C        ELSE
C          T1(j,n)=0.d0
C          T2(j,n)=0.d0
C        END IF
C        END DO
C      END DO



C ***** formula
c      error=1.d0 !parametre de convergencia al resultat
c      DO while (error.ge.1.d-20)
c        DO n=1,36
c          DO j=1,60
c          T2(j,n)=0.25d0*(T1(j+1,n)+T1(j-1,n)+T1(j,n+1)+T1(j,n-1)+
c     +   ((0.5d0**2)*p(dble(j)*0.5d0,dble(n)*0.5d0)))
c          END DO
c        END DO
c        error=T2(30,18)-T1(30,18) !comparem el valor del mitg de les dos matrius
c        !write(*,*) error
c        T1=T2 !cambiem la matriu dels resultats per la de les dades
c      END DO


      OPEN (11,FILE='conmet1.dat')
      WRITE(11,*)'Tin 0'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-5,1,0.d0,1.54d0)
      WRITE(11,*)''
      WRITE(11,*)''
      WRITE(11,*)'Tin 4'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-5,1,4.d0,1.54d0)
      WRITE(11,*)''
      WRITE(11,*)''
      WRITE(11,*)'Tin 700'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-5,1,700.d0,1.54d0)
      WRITE(11,*)''
      WRITE(11,*)''
      CLOSE(11)

      OPEN (22,FILE='conmet2.dat')
      WRITE(22,*)'Tin 0'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-5,2,0.d0,1.54d0)
      WRITE(22,*)''
      WRITE(22,*)''
      WRITE(22,*)'Tin 4'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-5,2,4.d0,1.54d0)
      WRITE(22,*)''
      WRITE(22,*)''
      WRITE(22,*)'Tin 700'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-5,2,700.d0,1.54d0)
      WRITE(22,*)''
      WRITE(22,*)''
      CLOSE(22)

      OPEN (33,FILE='conmet3.dat')
      WRITE(33,*)'Tin 0'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-5,3,0.d0,1.54d0)
      WRITE(33,*)''
      WRITE(33,*)''
      WRITE(33,*)'Tin 4'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-5,3,4.d0,1.54d0)
      WRITE(33,*)''
      WRITE(33,*)''
      WRITE(33,*)'Tin 700'
      CALL RELAX(T1,T2,Lx,Ly,h,1.d-5,3,700.d0,1.54d0)
      WRITE(33,*)''
      WRITE(33,*)''
      CLOSE(33)


      CALL RELAX (T1,T2,Lx,Ly,h,1.d-5,1,0.d0,1.54d0)

      OPEN (1,FILE='prova.dat')
        DO j=0,61
          DO n=0,37
            WRITE(1,*) dble(j)*h,dble(n)*h,T1(j,n)
          END DO
          WRITE(1,*)''
        END DO
      CLOSE(1)



      CALL SYSTEM('gnuplot -p scriptP9.plt')

      END






      DOUBLE PRECISION FUNCTION p(x,y)
      DOUBLE PRECISION x,y,r,p1,p2
      r=dsqrt(((x-22.d0)**2)+(y-10.d0)**2)
      p1=2.17d0*dexp(-((r-7.d0)**2)/0.5d0**2)

      IF (x.ge.7.5d0 .AND. x.le.10.5d0 .AND. y.ge.6.d0 .AND. y.le.10.d0)
     + THEN
        p2=1.83d0
      ELSE
        p2=0.d0
      END IF

      p=p1+p2

      END FUNCTION



      SUBROUTINE RELAX(T1,T2,Lx,Ly,h,error,icontrol,Tin,w)
      EXTERNAL p
      DOUBLE PRECISION T1(0:61,0:37),T2(0:61,0:37)
      DOUBLE PRECISION Lx,Ly,h,error,p,Tin,w
      INTEGER icontrol,i,n,j,nm,q

      nm=10000 !nombre maxim d'iteracions en cas de no convergir
      q=0

      IF (icontrol.EQ.1) then
C *********metode relaxacio *******************************
C ** GENEREM LES MATRIUS DE LES TEMPERATURES
        DO n=0,37
          DO j=0,61
          IF (n.EQ.0) then
            T1(j,n)=7.d0
            T2(j,n)=7.d0
          ELSE IF (j.EQ.0) then
            T1(j,n)=10.d0
            T2(j,n)=10.d0
          ELSE IF (n.EQ.37) then
            T1(j,n)=13.6d0
            T2(j,n)=13.6d0
          ELSE IF (j.EQ.61) then
            T1(j,n)=30.d0
            T2(j,n)=30.d0
          ELSE
            T1(j,n)=Tin
            T2(j,n)=Tin
          END IF
          END DO
        END DO
C **formula
        erro=1.d0 !donem un valor inicial al error per iniciar el bucle
        DO while (erro.ge.error.and.q.le.nm)
          WRITE(11,*) q,T2(32,24) !escrivim en un fitxer la convergencia de la temperatura demanada
          DO n=1,36
            DO j=1,60
            T2(j,n)=0.25d0*(T1(j+1,n)+T1(j-1,n)+T1(j,n+1)+T1(j,n-1)+
     +     ((0.5d0**2)*p(dble(j)*0.5d0,dble(n)*0.5d0)))
            END DO
          END DO
          erro=dabs(T2(30,18)-T1(30,18)) !comparem el valor del mitg de les dos matrius
          !write(*,*) erro
          T1=T2 !cambiem la matriu dels resultats per la de les dades
          q=q+1 !contador
        END DO
C***********************************************************************
      ELSE IF (icontrol.EQ.2) then
C********************metode jacobi*************************************
        DO n=0,37
          DO j=0,61
          IF (n.EQ.0) then
            T1(j,n)=7.d0
          ELSE IF (j.EQ.0) then
            T1(j,n)=10.d0
          ELSE IF (n.EQ.37) then
            T1(j,n)=13.6d0
          ELSE IF (j.EQ.61) then
            T1(j,n)=30.d0
          ELSE
            T1(j,n)=Tin
          END IF
          END DO
        END DO
C **formula
        erro=1.d0
        DO while (erro.ge.error.and.q.le.nm)
          WRITE(22,*) q,T1(32,24) !escrivim en un fitxer la convergencia de la temperatura demanada
          DO n=1,36
            DO j=1,60
            T1(j,n)=0.25d0*(T1(j+1,n)+T1(j-1,n)+T1(j,n+1)+
     +       T1(j,n-1)+((0.5d0**2)*p(dble(j)*0.5d0,dble(n)*0.5d0)))
            !write(*,*) T1(j,n)
            END DO
          END DO
          erro=dabs(T1(30,18)-0.25d0*(T1(31,18)+T1(29,18)+T1(30,19)+
     +     T1(30,17)+
     +     ((0.5d0**2)*p(30.d0*0.5d0,18.d0*0.5d0)))) !comparem el valor del mitg de les dos matrius
          !write(*,*) erro
          q=q+1
        END DO
C***********************************************************************
      ELSE IF (icontrol.EQ.3) then
C********************metode sobrerelaxacio*******************************
        DO n=0,37
            DO j=0,61
            IF (n.EQ.0) then
              T1(j,n)=7.d0
            ELSE IF (j.EQ.0) then
              T1(j,n)=10.d0
            ELSE IF (n.EQ.37) then
              T1(j,n)=13.6d0
            ELSE IF (j.EQ.61) then
              T1(j,n)=30.d0
            ELSE
              T1(j,n)=Tin
            END IF
            END DO
          END DO
C **formula
        erro=1.d0
        DO while (erro.ge.error.and.q.le.nm)
          WRITE(33,*) q,T1(32,24) !escrivim en un fitxer la convergencia de la temperatura demanada
          DO n=1,36
            DO j=1,60
            T1(j,n)=(1.d0-w)*T1(j,n)+(w/4.d0)*(T1(j+1,n)+
     +       T1(j-1,n)+T1(j,n+1)+T1(j,n-1)+
     +     ((0.5d0**2)*p(dble(j)*0.5d0,dble(n)*0.5d0)))
            END DO
          END DO
          erro=dabs((1.d0-w)*T1(30,18)+(w/4.d0)*(T1(31,18)+
     +       T1(29,18)+T1(30,19)+T1(30,17)+
     +     ((0.5d0**2)*p(30.d0*0.5d0,18.d0*0.5d0)))-T1(30,18))!comparem el valor del mitg de les dos matrius
          !write(*,*) erro
          q=q+1
        END DO
C***********************************************************************
      END IF
      END SUBROUTINE
