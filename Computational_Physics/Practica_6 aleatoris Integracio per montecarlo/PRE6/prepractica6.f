      PROGRAM MAIN
      IMPLICIT NONE

      EXTERNAL fun1,fun2,p
      DOUBLE PRECISION PI,fun1,fun2,a,b,f1,x,f1var1,var1,sigma1,I1
      DOUBLE PRECISION resultat1,resultat2,f2,f2var2,I2,var2,sigma2
      DOUBLE PRECISION p,xnums(1050000),XGAUS(1050000)
      DOUBLE PRECISION fun3,fun4,fun5,I3,I4,sigma3,sigma4,f3,f4
      DOUBLE PRECISION f3var3,f4var4,var3,var4,f5,f5var5,integral,sigma
      DOUBLE PRECISION c,d,I5,sigma5,var5,xg,funmulti,x1,x2,x3,x4,x5
      DOUBLE PRECISION f6,f6var6,I6,var6,sigma6
      INTEGER i,N
      PI=dacos(-1.d0)

      OPEN(1,FILE='apartata.dat')
C MONTECARLO CRU****** APARTAT a *************************************
      a=-PI                                           !extrem inferior
      b=PI                                            !extrem superior
      resultat1=(PI**3)/2.d0                          !resultat I1
      resultat2=(PI/288.d0)*((120.d0)*(PI**2)-677.d0) !resultat I2
      DO N=2500,150000,2500
       f1=0.d0
       f2=0.d0
       f1var1=0.d0
       f2var2=0.d0
C calculem estimadors
       DO i=1,N
        x=(((b-a)*i)/dble(N))+a                 ! xi
        f1=f1+(fun1(x)*(b-a))             !sumatori de f1(xi)
        f2=f2+(fun2(x)*(b-a))             !sumatori de f2(xi)
        f1var1=f1var1+(fun1(x)*(b-a))**2  !sumatori de f1(xi)**2
        f2var2=f2var2+(fun2(x)*(b-a))**2  !sumatori de f2(xi)**2
       END DO

       I1=(1.d0/dble(N))*f1                    ! estimador del valor esperat de la integral 1
       I2=(1.d0/dble(N))*f2                    ! estimador del valor esperat de la integral 2
       var1=(1.d0/dble(N))*f1var1 - I1**2      ! variancia de la I1
       var2=(1.d0/dble(N))*f2var2 - I2**2      ! variancia de la I2
       sigma1=dsqrt(var1/dble(N))              ! desviacio estandar de I1
       sigma2=dsqrt(var2/dble(N))              ! desviacio estandar de I2

       write(1,*)N,I1,sigma1,I2,sigma2,resultat1-I1,resultat2-I2

      END DO

      CLOSE(1)

C************************* APARTAT B ***********************************

      CALL ACCEPTREBUIG(1050000,xnums,-PI,PI,0.4d0,p)

C************************ APARTAT C ************************************

      CALL SUBGAUSS(1050000,0.d0,1.d0,XGAUS)

C************************ APARTAT D ************************************

      OPEN(4,FILE='apartatd.dat')

      DO N=5000,1050000,5000
       f3=0.d0
       f4=0.d0
       f5=0.d0
       f3var3=0.d0
       f4var4=0.d0
       f5var5=0.d0
C calculem estimadors
       DO i=1,N
        x=xnums(i)                        ! xi trets del acceptrebuig
        xg=XGAUS(i)
        f3=f3+(fun3(x)/p(x))             !sumatori de f3(xi)
        f4=f4+(fun4(x)/p(x))             !sumatori de f4(xi)
        f5=f5+(fun5(xg)*dsqrt(2.d0*PI)*dexp(xg**2.d0/2.d0))             !sumatori de f5(xi)
        f3var3=f3var3+(fun3(x)/p(x))**2  !sumatori de f3(xi)**2
        f4var4=f4var4+(fun4(x)/p(x))**2  !sumatori de f4(xi)**2
        f5var5=f5var5+(fun5(xg)*dsqrt(2.d0*PI)*dexp(xg**2 /2.d0))**2  !sumatori de f5(xi)**2
       END DO
C hem de multiplicar i dividir dins de la integral per la pdf que utilitzem i llavors queda la F*pdf*1/pdf, hem de fer el valor esperat de F/pdf
       I3=f3/(dble(N))                    ! estimador del valor esperat de la integral 1
       I4=f4/(dble(N))                       ! estimador del valor esperat de la integral 2
       I5=f5/(dble(N))
       var3=(1.d0/dble(N))*f3var3 - I3**2      ! variancia de la I3
       var4=(1.d0/dble(N))*f4var4 - I4**2      ! variancia de la I4
       var5=(1.d0/dble(N))*f5var5 - I5**2      ! variancia de la I5

       sigma3=dsqrt(var3/dble(N))              ! desviacio estandar de I3
       sigma4=dsqrt(var4/dble(N))              ! desviacio estandar de I4
       sigma5=dsqrt(var5/dble(N))              ! desviacio estandar de I5

       write(4,*)N,I3,sigma3,I4,sigma4,I5,sigma5

      END DO

      CLOSE(4)

C********************** APARTAT 2 *************************************

      OPEN(8,FILE='apartat2.dat')

      DO N=1500,210000,1500
       f6=0.d0
       f6var6=0.d0
C calculem estimadors
       DO i=1,N,5

        x1=XGAUS(i)
        x2=XGAUS(i+1)
        x3=XGAUS(i+2)
        x4=XGAUS(i+3)
        x5=XGAUS(i+4)

        f6=f6+(funmulti(x1,x2,x3,x4,x5)*dsqrt(2.d0*PI)*
     +  dexp((x1**2.d0+x2**2.d0+x3**2.d0+x4**2.d0+x5**2.d0)/2.d0))

        f6var6=f6var6+(funmulti(x1,x2,x3,x4,x5)*dsqrt(2.d0*PI)*
     +  dexp((x1**2.d0+x2**2.d0+x3**2.d0+x4**2.d0+x5**2.d0)/2.d0))**2  !sumatori de f5(xi)**2
       END DO

       I6=f6/(dble(N))

       var6=(f6var6/dble(N))- I6**2      ! variancia de la I5

       sigma6=dsqrt(var6/dble(N))              ! desviacio estandar de I5

       write(8,*)N,I6,sigma6

      END DO

      CLOSE(8)

      CALL SYSTEM('gnuplot -p scriptP6.plt')


      END



C***********************************************************************
C                       FUNCIONS I SUBROUTINES
C***********************************************************************

      DOUBLE PRECISION FUNCTION fun1(x)
      DOUBLE PRECISION x
      PI=dacos(-1.d0)
      fun1=dsqrt((PI**2)-(x**2))
      END FUNCTION

      DOUBLE PRECISION FUNCTION fun2(x)
      DOUBLE PRECISION x
      fun2=(x-((3.d0*x**2)*dsin(x))+x**3)*((dcos(x))**2)*dsin(x)
      END FUNCTION

      DOUBLE PRECISION FUNCTION p(x)
      DOUBLE PRECISION x,PI
      PI=dacos(-1.d0)
      p=(5.d0/4.d0)*dexp(-dabs(x))*(dsin(x)**2)/(1.d0-dexp(-pi))
      END FUNCTION

      SUBROUTINE ACCEPTREBUIG(ndat,xnums,a,b,M,fun)
C ndat: in quantitat de numeros que generarem
C xnums: out llista dels numeros generats
C a,b: in extrems de la x
C M: in funcio que servira de cota superior (normalment una constant, sera mes lenta que una funcio semblant a la pdf)
C fun: in distribucio que seguirem per generar els valors aleatoris
      IMPLICIT NONE
      EXTERNAL fun
      DOUBLE PRECISION xnums(ndat),a,b,var,desv,M,fun,p,x,x1,x2,mitja
      INTEGER ndat,ISEED,i
      ISEED=16650211
      CALL SRAND(ISEED) !generem una llavor pel valor aleatori
      i=1
      DO while(i.le.ndat)
        x1=rand() !generem els nombres aleatoris cada loop un de nou
        x2=rand()
        x=(b-a)*x1 + a !seguim el metode acceptrebuig
        p=M*x2
        IF (fun(x).ge.p) THEN
          xnums(i)=x
          WRITE(1,*)xnums(i)
          i=i+1
        END IF
      END DO
      END SUBROUTINE

      SUBROUTINE SUBGAUSS(NDAT,XMU,XSIGMA,XGAUS)
      IMPLICIT NONE
      DOUBLE PRECISION x1(NDAT),x2(NDAT),XMU,XSIGMA,xgaus(NDAT),PI
     +,theta(NDAT),r(NDAT)
      INTEGER iseed,ndat,i
      PI=dacos(-1.d0)
      iseed=16622152
      CALL srand(iseed)
      DO i=1,NDAT
        theta(i)=2.d0*PI*rand()  !valor aleatori per l'angle
        r(i)=dsqrt(-2.d0*dlog(1.d0-rand()))   ! valor pel radi
      END DO

cºººx1 i x2 son valors aleatoris generats segons una gaussiana amb variança xsigma**2 i valor mitja xmu

      x1=xsigma*r*dcos(theta)+xmu
c     x2=xsigma*r*dsin(theta)+xmu   !nomes necessito ndat termes i amb x1 ja en tinc prou

      xgaus=x1

      END SUBROUTINE


      DOUBLE PRECISION FUNCTION fun3(x)   !funcio INtegral 3
      DOUBLE PRECISION x,PI
      PI=dacos(-1.d0)
      fun3=(1.d0/dsqrt(8.d0*PI))*(dexp(-dabs(x)))*(x**2)*((dsin(x))**2)
      END FUNCTION

      DOUBLE PRECISION FUNCTION fun4(x)    ! funcio integral 4
      DOUBLE PRECISION x
      fun4=dexp((-x**2)/2.d0)*(dcos(x))**2.d0
      END FUNCTION

      DOUBLE PRECISION FUNCTION fun5(x)    !funcio integral 5
      DOUBLE PRECISION x
      fun5=dexp(-(x**2))*((dsin(x))**6)*(x**2)
      END FUNCTION

      DOUBLE PRECISION FUNCTION funmulti(x1,x2,x3,x4,x5)
      DOUBLE PRECISION x1,x2,x3,x4,x5,g
      g=(dexp(x1*dcos(x2+x3)))*((x3**2)*(x4**2)+
     + (dcos(x3+x4)*x5*dsin(x5)))
      funmulti=g*dexp(-(x1**2.d0+x2**2.d0+x3**2.d0+x4**2.d0+x5**2.d0))
      END FUNCTION
