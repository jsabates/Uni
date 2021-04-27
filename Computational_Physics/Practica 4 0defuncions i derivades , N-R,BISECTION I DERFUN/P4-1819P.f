      PROGRAM PROVA
      IMPLICIT NONE

      EXTERNAL fun,funp,funpt,funt
      INTEGER i,niter,ndat
      DOUBLE PRECISION x,fu,dfu,x0,A,B,eps,xarrel,v0(1:5),pl,pg
      DOUBLE PRECISION xd(1000),fud(1000),dfud(1000),dfuu(1000),T
900   FORMAT (10x,A,20x,A,10X,A)

C ndat: numero de subdivisions de linterval
C xd: vector amb la variable v dins linterval
c fud: vector amb el valor de la funcio en v
C dfud: vector amb el valor de la derivada de la funcio calculat amb la formula aproximada
C dfuu: vector amb el valor de la derivada de la funcio EXACTE calculat amb la subrutina fun ( amb la derivada calculada)

      ndat=80
      OPEN(42,FILE='P4-1819P-fig1.dat')
      WRITE(42,900)'V','Derivada numerica','valor de la funcio'
      CALL derfun(ndat,xd,fud,dfud,dfuu)
      DO i=1,ndat
      write(42,*)xd(i),dfud(i),fud(i)
      END DO
      CLOSE(42)

C****************************** APARTAT 2 ****************************

      OPEN(1,FILE='P4-1819P-fig2.dat')
      DO i=4333,20000
       call funp(dble(i)/10000,fu,dfu)
       WRITE(1,*)dble(i)/10000,fu
      END DO
      CLOSE (1)

C*******************************APARTAT 3 *****************************
      OPEN(2,FILE='P4-1819P-res.dat')
      CALL BISECTION (0.6d0,0.9d0,1.d-12,funp,niter,xarrel)
      WRITE(2,*)'Larrel 1 es=', xarrel
      CALL BISECTION (1.4d0,1.6d0,1.d-12,funp,niter,xarrel)
      WRITE(2,*)'Larrel 2 es=', xarrel
      CLOSE(2)


C***************************APARTAT 4********************************
      v0(1)=0.36d0
      v0(2)=0.48d0
      v0(3)=0.99d0
      v0(4)=1.18d0
      v0(5)=1.58d0
      OPEN(3,FILE='P4-1819P-res1.dat')
      DO i=1,5
       CALL NEWTONRAP(v0(i),1.d-12,funpt,niter,xarrel)
       IF (xarrel.gt.(1.d0/3.d0))then
       WRITE(3,*)v0(i),niter,xarrel
       END IF
      END DO
      CLOSE(3)

C*************************** APARTAT 5******************************

      OPEN(4,FILE='P4-1819P-extra.dat')
      DO i=9200,9710
       CALL funt(0.74221286969859646d0,fu,dfu,dble(i)/10000.d0)
        pl=fu
       CALL funt(1.4511411711697293d0,fu,dfu,dble(i)/10000.d0)
        pg=fu

        WRITE(4,*)dble(i)/10000.d0,pl,pg
      END DO
      CLOSE(4)


C*******************************************************************
      CALL SYSTEM('gnuplot -p scriptP4.plt')


      END

C implementem dos arguments que permetran crear un arxiu per guardar les dades de la convergencia
      SUBROUTINE NEWTONRAP(x0,eps,fun,niter,xarrel)
      IMPLICIT NONE
      EXTERNAL fun,funp,funpt
      DOUBLE PRECISION x0,eps,xarrel,fu,dfu
      INTEGER niter
      xarrel=x0
      niter=0
      fu=10000
      DO WHILE(dabs(fu/dfu).GE.eps)
       CALL fun(xarrel,fu,dfu)
       xarrel=xarrel-(fu/dfu)
       niter=niter+1
      END DO
      END SUBROUTINE

      SUBROUTINE BISECTION (A,B,eps,fun,niter,xarrel)
      IMPLICIT NONE
      EXTERNAL fun,funp
      DOUBLE PRECISION A,B,eps,xarrel,fux,fua,fu,dfu,xa,xb
      INTEGER niter

Cper trobar els 0, hem de partir linterval en molts trocets, i comprovant si el producte del punt i el seu proper es negatiu
C si es negatiu hi ha un 0, si es positiu res
      xa=A
      xb=B
      niter=1
      DO WHILE (dabs(xb-xa).gt.eps)
       xarrel=(xa+xb)/2.d0
       CALL fun(xarrel,fu,dfu)
            fux=fu
       CALL fun(xa,fu,dfu)
            fua=fu
       IF ((fux*fua).gt.0) THEN
        xa=xarrel
       ELSE
        xb=xarrel
       END IF
       niter=niter+1
      END DO

      END SUBROUTINE

      SUBROUTINE fun(x,fu,dfu)
      IMPLICIT NONE
      DOUBLE PRECISION x,fu,dfu,T
      T=0.92d0
      fu=((8.d0*T)/(3.d0*x-1))-(3.d0/(x**2))
      dfu=6.d0*((1.d0/x**3)-((4.d0*T)/((3.d0*x-1.d0)**2)))
      END SUBROUTINE

      SUBROUTINE funp(x,fu,dfu)
      IMPLICIT NONE
      DOUBLE PRECISION x,fu,dfu,T
      T=0.92d0
      fu=(4.d0*T*x**3)-(9.d0*x**2)+(6.d0*x)-1.d0
      dfu=(12.d0*T*x**2)-(18.d0*x)+6.d0
      END SUBROUTINE

      SUBROUTINE funpt(x,fu,dfu)
      IMPLICIT NONE
      DOUBLE PRECISION x,fu,dfu,T
      T=0.98d0
      fu=(4.d0*T*x**3)-(9.d0*x**2)+(6.d0*x)-1.d0
      dfu=(12.d0*T*x**2)-(18.d0*x)+6.d0
      END SUBROUTINE

      SUBROUTINE derfun(ndat,xd,fud,dfud,dfuu)
      IMPLICIT NONE
      EXTERNAL fun,funp
      DOUBLE PRECISION xd(1000),fud(1000),dfud(1000),c,d,h,fudh,fudhh
      DOUBLE PRECISION dfu,fu,dfuu(1000)
      INTEGER ndat,i
      c=1.d0/3.d0+0.1d0
      d=4.d0
      h=(d-c)/dble(ndat)
      DO i=0,ndat-1
       xd(i+1)=c+(((d-c)/dble(ndat-1))*i)
       IF (i.eq.0)then
        CALL fun(xd(i+1),fu,dfu)
        dfuu(i+1)=dfu
        fud(i+1)=fu
        CALL fun((xd(i+1)+h),fu,dfu)
        fudh=fu
        dfud(i+1)=(fudh-fud(i+1))/h
       ELSE IF (i.eq.ndat)then
        CALL fun(xd(i+1),fu,dfu)
        dfuu(i+1)=dfu
        fud(i+1)=fu
        CALL fun((xd(i+1)-h),fu,dfu)
        fudh=fu
        dfud(i+1)=(fud(i+1)-fudh)/h
       ELSE
        CALL fun(xd(i+1),fu,dfu)
        dfuu(i+1)=dfu
         fud(i+1)=fu
        CALL fun((xd(i+1)+h),fu,dfu)
         fudh=fu
        CALL fun((xd(i+1)-h),fu,dfu)
         fudhh=fu
        dfud(i+1)=(fudh-fudhh)/(2.d0*h)
       END IF
      END DO
      END SUBROUTINE

      SUBROUTINE funt(x,fu,dfu,T)
      IMPLICIT NONE
      DOUBLE PRECISION x,fu,dfu,T
      fu=((8.d0*T)/(3.d0*x-1))-(3.d0/(x**2))
      dfu=6.d0*((1.d0/x**3)-((4.d0*T)/((3.d0*x-1.d0)**2)))
      END SUBROUTINE

