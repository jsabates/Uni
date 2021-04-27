      PROGRAM PROVA
      IMPLICIT NONE

      EXTERNAL fun
      INTEGER i,niter,ndat
      DOUBLE PRECISION x,fu,dfu,x0,A,B,eps,xarrel,v0(0:10)
      DOUBLE PRECISION xd(1000),fud(1000),dfud(1000),dfuu(1000)
900   FORMAT (E20.12,2x,E20.12)

C ********************* APARTAT A ***********************************

      OPEN(1,FILE='P4-1819P-res.dat')
      DO i=-1000,3000
       call fun(dble(i)/1000,fu,dfu)
       WRITE(1,*)dble(i)/1000,fu,dfu
      END DO
      CLOSE (1)

C********************* APARTAT B **********************************

      CALL BISECTION (-1.d0,-0.5d0,1.d-12,fun,niter,xarrel)
      WRITE(*,*)'Larrel 1 es=', xarrel
      WRITE(*,*)'hem tardat ',niter,'iteracions'
      CALL BISECTION (-0.2d0,0.2d0,1.d-12,fun,niter,xarrel)
      WRITE(*,*)'Larrel 2 es=', xarrel
      WRITE(*,*)'hem tardat ',niter,'iteracions'
      CALL BISECTION (1.d0,1.5d0,1.d-12,fun,niter,xarrel)
      WRITE(*,*)'Larrel 3 es=', xarrel
      WRITE(*,*)'hem tardat ',niter,'iteracions'
      CALL BISECTION (2.d0,3.d0,1.d-12,fun,niter,xarrel)
      WRITE(*,*)'Larrel 4 es=', xarrel
      WRITE(*,*)'hem tardat ',niter,'iteracions'

C********************** APARTAT C *********************************
      v0(1)=-0.5d0
      v0(2)=0.1d0
      v0(3)=0.2d0
      v0(4)=0.67d0
      v0(5)=0.7d0
      v0(6)=1.d0
      v0(7)=1.5d0
      v0(8)=1.6d0
      v0(9)=2.0d0
      v0(10)=2.4d0

      OPEN(2,FILE='P4-1819P-res1.dat')
      DO i=1,10
       CALL NEWTONRAP(v0(i),1.d-12,fun,niter,xarrel,10*i,'inutil')
       WRITE(2,*)v0(i),niter,xarrel
      END DO
      CLOSE(2)

C la subrotina newtonrap genera un document per guardar els valors de la convergencia del valor de larrel
C com que pel primer apartat del c) no ho necesitem , generem un document inutil el qual tindra la convergencia del ultim valor v0
C pero que no sera necesari
      CALL NEWTONRAP(0.2d0,1.d-12,fun,niter,xarrel,3,'conver02')

      CALL NEWTONRAP(0.7d0,1.d-12,fun,niter,xarrel,4,'conver07')

      CALL NEWTONRAP(1.5d0,1.d-12,fun,niter,xarrel,5,'conver15')


C************************** APARTAT 3 *********************************

      ndat=34
      OPEN(42,FILE='P4-1819P-res3-n34.dat')
      CALL derfun(ndat,xd,fud,dfud,dfuu)
      DO i=1,ndat
      write(42,*)xd(i),fud(i),dfud(i),dfuu(i)
      END DO
      CLOSE(42)
      ndat=420
      OPEN(54,FILE='P4-1819P-res3-n420.dat')
      CALL derfun(ndat,xd,fud,dfud,dfuu)
      DO i=1,ndat
      write(54,*)xd(i),fud(i),dfud(i),dfuu(i)
      END DO
      CLOSE(54)

      CALL SYSTEM('gnuplot -p scriptP4.plt')


      END

C implementem dos arguments que permetran crear un arxiu per guardar les dades de la convergencia
      SUBROUTINE NEWTONRAP(x0,eps,fun,niter,xarrel,arx,nom)
      IMPLICIT NONE
      EXTERNAL fun
      DOUBLE PRECISION x0,eps,xarrel,fu,dfu
      INTEGER niter,arx
      CHARACTER (len=8) nom
      OPEN(arx,FILE=nom)
      xarrel=x0
      niter=0
      fu=10000
      DO WHILE(dabs(fu/dfu).GE.eps)
       CALL fun(xarrel,fu,dfu)
       xarrel=xarrel-(fu/dfu)
       niter=niter+1
       write(arx,*)niter,xarrel
      END DO
      CLOSE(arx)
      END SUBROUTINE

      SUBROUTINE BISECTION (A,B,eps,fun,niter,xarrel)
      IMPLICIT NONE
      EXTERNAL fun
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
      DOUBLE PRECISION x,fu,dfu
      !fu=(35/16)+(1/2)*x-(61/20)*x**2+x**3
      !dfu=(1/2)-(61/10)*x+(3*x**2)
      !fu=x**2+2*x-3
      !dfu=2*x+2
      !fu=(x-2)*(x+2)
      !dfu=2*x
      fu=(dsinh(x))*((35.d0/16.d0)+((1.d0/2.d0)*x)-((61.d0/20.d0)*x**2)+
     + (x**3))
      dfu=((1.d0/10.d0)*((30.d0*x**2)-(61.d0*x)+5.d0)*dsinh(x))+((x**3)-
     + ((61.d0/20.d0)*x**2)+(x/2.d0)+(35.d0/16.d0))*dcosh(x)
      END SUBROUTINE


      SUBROUTINE derfun(ndat,xd,fud,dfud,dfuu)
      IMPLICIT NONE
      EXTERNAL fun
      DOUBLE PRECISION xd(1000),fud(1000),dfud(1000),c,d,h,fudh,fudhh
      DOUBLE PRECISION dfu,fu,dfuu(1000)
      INTEGER ndat,i
      c=-1.d0
      d=3.d0
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
C ndat: numero de subdivisions de linterval
C xd: vector amb la variable x dins linterval
c fud: vector amb el valor de la funcio en x
C dfud: vector amb el valor de la derivada de la funcio calculat amb la formula aproximada
C dfuu: vector amb el valor de la derivada de la funcio EXACTE calculat amb la subrutina fun ( amb la derivada calculada)
