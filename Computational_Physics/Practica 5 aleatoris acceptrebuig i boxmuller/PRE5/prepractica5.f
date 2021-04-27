      PROGRAM MAIN
      IMPLICIT NONE

      EXTERNAL fun,funn
      DOUBLE PRECISION fun,xnums(20000),XHIS(150),VHIS(150),ERRHIS(150)
      DOUBLE PRECISION BOXSIZE,t,XGAUS(30000),funn,nu,sigma,pi
      INTEGER b,IERR,i

      pi=dacos(-1.d0)
C cridem subroutines per generar els valors
      CALL ACCEPTREBUIG(20000,xnums,0.d0,1.d0,2.d0,fun)
      CALL HISTOGRAMA(20000,xnums,0.d0,1.d0,30,XHIS,VHIS,ERRHIS,BOXSIZE,
     + IERR)
C generem l'histograma
      OPEN(2,FILE='histograma.dat')
      DO i=1,30
        WRITE(2,*) XHIS(i),VHIS(i),(VHIS(i)-ERRHIS(i)),(VHIS(i)+
     + ERRHIS(i))
      END DO
      CLOSE(2)
C generem la funcio
      OPEN(3,FILE='funcio.dat')
      DO i=0,10000
       t=dble(i)/10000
       WRITE(3,*)t,fun(t)
      END DO
      CLOSE(3)


C *************** gausiana ***********************
      CALL SUBGAUSS(30000,1.d0,0.45d0,XGAUS)
      CALL HISTOGRAMA(30000,XGAUS,-3.d0,5.d0,150,XHIS,VHIS,ERRHIS,
     + BOXSIZE,IERR)
      OPEN(4,FILE='gauss.dat')
      DO i=1,150
        WRITE(4,*) XHIS(i),VHIS(i),(VHIS(i)-ERRHIS(i)),(VHIS(i)+
     + ERRHIS(i))
      END DO
      CLOSE(4)


      OPEN(5,FILE='normal.dat')
      DO i=-10000,30000
        t=dble(i)/10000
        WRITE(5,*)t,funn(t)/10.6d0**20
      END DO
      CLOSE(5)

      CALL SYSTEM('gnuplot -p scriptP5.plt')




      END

      SUBROUTINE HISTOGRAMA(NDAT,XDATA,XA,XB,NBOX,XHIS,VHIS,ERRHIS,
     1 BOXSIZE,IERR)
       IMPLICIT NONE

C INPUT/OUTPUT VARIABLES

       INTEGER NDAT,NBOX
       DOUBLE PRECISION XDATA(NDAT),XA,XB
       DOUBLE PRECISION XHIS(NBOX),VHIS(NBOX),ERRHIS(NBOX)
       INTEGER IERR
C
       INTEGER I,IBOX,ICOUNT
       DOUBLE PRECISION BOXSIZE

       IF (XA.GE.XB) THEN
          IERR=1
          RETURN
       ENDIF
C BOX SIZE
         BOXSIZE=(XB-XA)/NBOX

C COUNTS NUMBER OF POINTS WITHIN THE INTERVAL XA,XB
       ICOUNT=0

C SETS ALL TO ZERO
       DO I=1,NBOX
          VHIS(I)=0
          ERRHIS(I)=0
       ENDDO

C WE RUN THROUGH THE DATASET
       DO I=1,NDAT
C CHECKS IF DATA LIES WITHIN XA,XB
         IF (XDATA(I).GE.XA.AND.XDATA(I).LE.XB) THEN
            IBOX=INT((XDATA(I)-XA)/BOXSIZE)+1
C PUTS XB INTO THE LAST BOX, IF NEEDED
            IF (IBOX.EQ.NBOX+1) IBOX=NBOX

            VHIS(IBOX)=VHIS(IBOX)+1
            ICOUNT=ICOUNT+1
         ENDIF
        ENDDO

        IF (ICOUNT.EQ.0) THEN
           IERR=2
           RETURN
        ENDIF

        IERR=0
        PRINT*,"ACCEPTED:",ICOUNT,
     1  " OUT OF:",NDAT

        DO I=1,NBOX
c CENTRAL VALUE OF THE BAR
           XHIS(I)=XA+BOXSIZE/2.D0+(I-1)*BOXSIZE
C  ERROBAR, STANDARD DEVIATION OF CORRESPONDING BINOMIAL
           ERRHIS(I)=SQRT(VHIS(I)/ICOUNT*(1.D0-VHIS(I)/ICOUNT))
     1      /BOXSIZE / SQRT(DBLE(ICOUNT))
C NORMALIZED VALUE OF THE BAR
           VHIS(I)=VHIS(I)/ICOUNT/BOXSIZE
        ENDDO
        END

      SUBROUTINE ACCEPTREBUIG(ndat,xnums,a,b,M,fun)
      IMPLICIT NONE
      EXTERNAL fun
      DOUBLE PRECISION xnums(ndat),a,b,var,desv,M,fun,p,x,x1,x2,mitja
      INTEGER ndat,ISEED,i
      OPEN (1,FILE='data.dat')
      ISEED=16650211
      CALL SRAND(ISEED)
      i=1
      DO while(i.le.ndat)
        x1=rand()
        x2=rand()
        x=(b-a)*x1 + a
        p=M*x2
        IF (fun(x).ge.p) THEN
          xnums(i)=x
          WRITE(1,*)xnums(i)
          i=i+1
        END IF
      END DO
      CLOSE(1)
C **************** VALOR MITJA ********************************
      mitja=0.d0
      DO i=1,ndat
        mitja=mitja+xnums(i)
      END DO
      mitja=mitja/dble(ndat)
C ***************** VARIANCIA************************************
      var=0.d0
      DO i=1,ndat
        var=var+(xnums(i)-mitja)**2
      END DO
      var=var/dble(ndat)
C ******************* DESVIACIO *******************************
      desv=dsqrt(var)
C *************************************************************

      WRITE(*,*) 'MITJA=',mitja,'VARIANCIA=',var,'DESVIACIO=',desv
      END SUBROUTINE

      DOUBLE PRECISION FUNCTION fun(x)
      DOUBLE PRECISION x
      fun=(6.d0/5.d0)*(1.d0-(1.d0/2.d0)*x**2)
      END FUNCTION

      SUBROUTINE SUBGAUSS(NDAT,XMU,XSIGMA,XGAUS)
      implicit none
      double precision x1(ndat),x2(ndat),XMU,XSIGMA,xgaus(ndat),pi
     +,theta(ndat),r(ndat)
      integer iseed,ndat,i
      pi=dacos(-1.d0)

      iseed=16650211
      call srand(iseed)

      do i=1,ndat
        theta(i)=2.d0*pi*rand()  !valor aleatori per l'angle
        r(i)=dsqrt(-2.d0*dlog(1.d0-rand()))   ! valor pel radi
      enddo

cºººx1 i x2 son valors aleatoris generats segons una gaussiana amb variança xsigma**2 i valor mitja xmu

      x1=xsigma*r*dcos(theta)+xmu
c     x2=xsigma*r*dsin(theta)+xmu   !nomes necessito ndat termes i amb x1 ja en tinc prou

      xgaus=x1
      END SUBROUTINE

      DOUBLE PRECISION FUNCTION funn(x)
      DOUBLE PRECISION x,nu,sigma,t
      nu=1.d0
      sigma=0.45d0
      funn=(nu/(sigma*dsqrt(2.d0*pi)))*dexp(-((x-nu)**2)/
     + (2.d0*(sigma**2)))
      END FUNCTION

