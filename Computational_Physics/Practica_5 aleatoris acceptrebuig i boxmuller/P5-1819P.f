      PROGRAM MAIN
      IMPLICIT NONE

      EXTERNAL fun,funn
      DOUBLE PRECISION fun,XHIS(100),VHIS(100),ERRHIS(100)
      DOUBLE PRECISION BOXSIZE,t,XGAUS(110000),funn,pi,nu,sigma
      DOUBLE PRECISION ymitja,ymitja2,x(220),y(220),var,dist(220)
      INTEGER b,IERR,i,m,n
C ****************** APARTAT 1 ************************************

      CALL SUBGAUSS(110000,0.d0,1.d0,XGAUS)
      CALL HISTOGRAMA(110000,XGAUS,-4.5d0,4.5d0,100,XHIS,VHIS,ERRHIS,
     + BOXSIZE,IERR)
C escrivim l'histograma
      OPEN(1,FILE='P5-1819P-res.dat')
      DO i=1,100
        WRITE(1,*) XHIS(i),VHIS(i),ERRHIS(i)
      END DO
      CLOSE(1)
C escrivim la distribucio normal exacte
      OPEN(2,FILE='normal.dat')
      DO i=-40000,40000
        t=dble(i)/10000
        WRITE(2,*)t,funn(t)
      END DO
      CLOSE(2)

C ***************** APARTAT 2***************************************

      XGAUS=XGAUS*dsqrt(0.02d0*2.62d-5) ! modifiquem la variancia dels nnombres gausians abans definits

      t=0.d0
      x=0.d0
      y=0.d0
C definim el punt inicial del vector posicio
      m=1 !definim un contador que usarem despres
      var=0.d0

      OPEN(3,FILE='P5-1819P-res2.dat')

      OPEN(4,FILE='P5-1819P-res3.dat')

      DO n=1,250  ! 250 pasos de temps 0.02segons
        ymitja=0.d0
        ymitja2=0.d0
        DO i=1,220 ! repetim per cada molecula
          x(i)=x(i)+xgaus(m)
          m=m+1
          y(i)=y(i)+xgaus(m)
          m=m+1
C sumem cada terme de cada molecula a la mitja
          ymitja=ymitja+y(i)
          ymitja2=ymitja2+(y(i)**2.d0) !ymitja2 es la mitja de y**2
        END DO

        ymitja=ymitja/220.d0
        ymitja2=ymitja2/220.d0

        var=ymitja2-ymitja**2.d0 !calculem la variancia mitjant. la formula

        t=n*0.02d0
        WRITE(3,*) t,x(1),y(1),x(2),y(2),x(3),y(3),x(4),y(4),x(5),y(5)

        WRITE(4,*) t,var
      END DO
      CLOSE (3)
      CLOSE (4)

C ******************** extra ****************************

      DO i=1,220
      dist(i)=dsqrt((x(i))**2 + (y(i))**2) !cada x(i) es la distancia final de la particula i
      END DO

      CALL HISTOGRAMA(220,dist,-0.5d0,0.5d0,12,XHIS,VHIS,ERRHIS,
     + BOXSIZE,IERR)
      OPEN(5,FILE='P5-1819P-extra.dat')
      DO i=1,12
        WRITE(5,*) XHIS(i),VHIS(i),ERRHIS(i)
      END DO
      CLOSE(5)

      CALL SYSTEM('gnuplot -p scriptP5.plt')


      END

C **********************************************************************
C                           SUBROUTINES
C***********************************************************************

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

      DOUBLE PRECISION FUNCTION funn(x)
      DOUBLE PRECISION x,nu,sigma,t
      pi=dacos(-1.d0)
      nu=0.d0
      sigma=1.d0
      funn=(1.d0/(sigma*dsqrt(2.d0*pi)))*dexp(-((x-nu)**2)/
     + (2.d0*(sigma**2)))
      END FUNCTION
