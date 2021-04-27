      PROGRAM MAIN
      IMPLICIT NONE

      EXTERNAL fun,p
      DOUBLE PRECISION fun1,PI,x,fun,a,b,f1,f1var1,I1,var1,sigma1,res1
      DOUBLE PRECISION L,p,xnums(1000000),f2,f2var2,fun2,var2,sigma2,I2
      DOUBLE PRECISION g,f3,f3var3,I3,var3,sigma3
      DOUBLE PRECISION I4,var4,f4,f4var4,fun4,sigma4
      DOUBLE PRECISION x1,x2,x3
      INTEGER i,N
      PI=dacos(-1.d0)
      L=50.d0

c FUN1 up
c fun2 down

      OPEN(1,FILE='P6-1819P-res.dat')
C MONTECARLO CRU****** APARTAT a *************************************
      a=0.d0                                          !extrem inferior
      b=1.d0                                          !extrem superior

      DO N=100,10000,100
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


       write(1,*)N,I1,sigma1,I2,sigma2

      END DO

      CLOSE(1)


C***************************** APARTAT b ******************************

      CALL ACCEPTREBUIG (1000000,xnums,0.d0,2.d0*L,1.d0,p)

C***************************** APARTAT C ******************************

       OPEN(2,FILE='P6-1819P-res2.dat')

      DO N=10000,1000000,10000

       f3=0.d0
       f3var3=0.d0

C calculem estimadors
       DO i=1,N
        x=xnums(i)                        ! xi trets del acceptrebuig
        f3=f3+(g(x))             !sumatori de f3(xi)
        f3var3=f3var3+(g(x))**2  !sumatori de f3(xi)**2
       END DO
C hem de multiplicar i dividir dins de la integral per la pdf que utilitzem i llavors queda la F*pdf*1/pdf, hem de fer el valor esperat de F/pdf
       I3=f3/(dble(N))                    ! estimador del valor esperat de la integral 1
       var3=(1.d0/dble(N))*f3var3 - I3**2      ! variancia de la I3

       sigma3=dsqrt(var3/dble(N))              ! desviacio estandar de I3

       write(2,*)N,I3,sigma3

      END DO

      CLOSE(2)

C**************************** APARTAT 2 *******************************

       OPEN(3,FILE='P6-1819P-res3.dat')

      DO N=10000,330000,10000
        f4=0.d0
        f4var4=0.d0

        DO i=1,N
          x1=xnums(i)
          x2=xnums(i+330000)
          x3=xnums(i+660000)
          f4=f4+((fun4(x1,x2,x3)**2.d0)/(p(x1)*p(x2)*p(x3)))
          f4var4=f4var4+((fun4(x1,x2,x3)**2.d0)/(p(x1)*p(x2)*p(x3)))**2


        END DO

        I4=f4/(dble(N))
        var4=(1.d0/dble(N))*f4var4 - I4**2
        sigma4=dsqrt(var4/dble(N))

        WRITE(3,*)N,I4,sigma4

      END DO


      CLOSE(3)



      CALL SYSTEM('gnuplot -p scriptP6.plt')



      END


      DOUBLE PRECISION FUNCTION fun1(x)
      DOUBLE PRECISION x
      fun1=(3.06d0 *(x**0.8d0)*(1.d0-x)**4)/x
      END FUNCTION

      DOUBLE PRECISION FUNCTION fun2(x)
      DOUBLE PRECISION x
      fun2=(5.11d0 *(x**0.8d0)*(1.d0-x)**3)/x
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

      DOUBLE PRECISION FUNCTION p(x)
      DOUBLE PRECISION x,L,PI
      PI=dacos(-1.d0)
      L=50.d0
      p=(1.d0/L)*(dsin((PI*(x-2.d0*L))/2.d0*L))**2
      END FUNCTION

      DOUBLE PRECISION FUNCTION g(x)
      DOUBLE PRECISION x,PI,L
      PI=dacos(-1.d0)
      L=50.d0
      g=(dsin(8.d0*((PI*(x-2.d0*L))/L)))**2
      END FUNCTION

      DOUBLE PRECISION FUNCTION fun4(x1,x2,x3)
      DOUBLE PRECISION FUNCTION x1,x2,x3,x(3),pi,L
      INTEGER i,j,k,m
      pi=dacos(-1.d0)

      L=50.d0

      x(1)=x1
      x(2)=x2
      x(3)=x3

      fun4=0.d0

      DO m=1,3
        fun4=fun4+dcos((pi*x(m))/(2.d0*L))
      END DO

      DO i=1,3
        fun4=fun4*dsin((pi*x(i))/(2.d0*L))
      END DO

      DO k=1,3
        DO j=1,3
          IF(j.lt.k) then
            fun4=fun4*
     +       ((dcos((pi*x(j))/(2.d0*L)))-(dcos((pi*x(k))/(2.d0*L))))
          END IF
        END DO
      END DO
      END FUNCTION
