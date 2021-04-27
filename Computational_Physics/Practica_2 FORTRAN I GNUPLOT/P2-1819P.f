      PROGRAM practica2
      IMPLICIT NONE
      INTEGER i,n,m
      DOUBLE PRECISION x(6),w,L,radi,PI,R,k,TEMPS(400),POSIS(400)
      DOUBLE PRECISION phi,xout,xout0,tin,t,t1,x1,x2,x3,x4,x5,x6

      common/dades/TEMPS,POSIS


      L=22.5d0
      w=5
c43    format (f12,2,2X,6(E20,14,2X))
42    format(F5.2,2X,F10.2,2X,F10.2,2X,F10.2,2x,F10.2,2X,F10.2,2X,F10.2)

      OPEN(1,FILE='P2-1819P-res1.dat')
      DO n=0,510
       k=dble(n)/10
       CALL posicio(k,x,L,w)
       WRITE(1,42) k,x(1),x(2),x(3),x(4),x(5),x(6)
      END DO
      CLOSE(1)

      OPEN(1,FILE='P2-1819P-res1.dat')
      m=1
      DO WHILE(.TRUE.)
      READ(1,42,END=10) t1
      TEMPS(m)=t1
      m=m+1
      END DO
10    CLOSE(1)

      OPEN (2,FILE='P2-1819P-res2.dat')
      DO i=1,1500
       t=dble(i)/500
       CALL interpol(t,xout)
       CALL interpol0(t,xout0)
       WRITE(2,*)t,xout,xout0
      END DO

      CALL system('gnuplot -p gnuplot.plt')

      END

C subroutine radius que calcula el radi de la manovella
      SUBROUTINE radius(i,L,radi)
      INTEGER i
      DOUBLE PRECISION L,radi
      radi=(L/dble(i))-0.01
      END SUBROUTINE

C funcio phi que calcula la seva fase inicial
      DOUBLE PRECISION FUNCTION phi(i)
      PI=DACOS(-1.D0)
      phi=dble(i) * PI/6.d0
      END

      SUBROUTINE posicio(t,x,L,w)
      INTEGER i
      DOUBLE PRECISION t,L,w,radi,R,arrel,phi
      DOUBLE PRECISION,DIMENSION(6) :: x
       DO i=1,6
       CALL radius(i,L,radi)
       R=radi
       arrel=dsqrt((L**2)-(R*dsin(w*t+phi(i)))**2)
       x(i)=R*dcos(w*t + phi(i)) + arrel
       END DO
      END SUBROUTINE POSICIO

      SUBROUTINE interpol(tin,xout)
      common/dades/TEMPS,POSIS
      INTEGER p
      DOUBLE PRECISION tin,xout,pendent,TEMPS(400),POSIS(400)
      p=1
      xout=0
      DO while(xout.lt.POSIS(p))
       IF (tin<TEMPS(p)) then
C        write(*,*) 'temps tin=',tin,'temps TEMPS(p)',TEMPS(p)
        pendent=(POSIS(p)-POSIS(p-1))/(TEMPS(p)-TEMPS(p-1))
C        write(*,*)'pendent=',pendent
        xout=(dble(tin)-TEMPS(p-1))*pendent+POSIS(p)
C        write(*,*) xout,POSIS(p-1),POSIS(p),POSIS(p+1)
       END IF
      p=p+1
      END DO
      END SUBROUTINE

      SUBROUTINE interpol0(tin,xout0)
      common/dades/TEMPS,POSIS
      INTEGER p
      DOUBLE PRECISION tin,pendent,TEMPS(400),POSIS(400),xout0
      p=1
      xout=0
      DO while(xout.lt.POSIS(p))
       IF (tin<TEMPS(p)) then
        IF (p.eq.1) xout0=POSIS(p)
        IF (p.gt.1) xout0=POSIS(p-1)
       END IF
      p=p+1
      END DO
      END SUBROUTINE


