      PROGRAM MAIN
      IMPLICIT NONE

      DOUBLE PRECISION m,l,g,wn,PI,tn,t,h
      DOUBLE PRECISION phi(200000),dphi(200000),ddphi(200000)  !en ordre : angle, derivada 1a, derivada 2a, per euler sencill
      DOUBLE PRECISION phiEM(200000),dphiEM(200000),ddphiEM(200000)  !en ordre : angle, derivada 1a, derivada 2a, per euler millorat
      DOUBLE PRECISION phiEMaprox(200000),dphiEMaprox(200000),
     + ddphiEMaprox(200000) !valors aproximats per la implementacio del metode
      DOUBLE PRECISION ECIN,EPOT

      INTEGER n,i


      m=0.95d0
      l=1.05d0
      g=1.66d0
      wn=dsqrt(g/l)
      PI=dacos(-1.d0)
      tn=2.d0*PI/wn

C ** apartat a) *************************************************

      t=0.d0
      n=1400
      h=(6.d0*tn-t)/dble(n)

      phi(1)=0.075d0  !valors inicials per euler sencill
      dphi(1)=0.d0
      ddphi(1)=0.d0

      phiEM(1)=0.075d0      !valors inicials per euler millorat
      dphiEM(1)=0.d0
      ddphiEM(1)=0.d0

      CALL EULER (1400,h,phi,dphi,ddphi)
C      CALL EULERMILLORAT (1400,h,phiEM,dphiEM,ddphiEM)
      CALL CORRECTOR(1400,h,phiEM,dphiEM,ddphiEM,phiEMaprox,
     + dphiEMaprox,ddphiEMaprox)
      OPEN(1,FILE='apartata.dat')

      DO i=1,n
        write(1,*) t,phi(i),phiEM(i)
        t=t+h
      END DO

      CLOSE(1)
C ** apartat b) ***************igual que abans pero canviant CI*********

      t=0.d0
      n=1400
      h=(6.d0*tn-t)/dble(n)

      phi(1)=PI-0.12d0  !valors inicials per euler sencill
      dphi(1)=0.d0
      ddphi(1)=0.d0

      phiEM(1)=PI-0.12d0      !valors inicials per euler millorat
      dphiEM(1)=0.d0
      ddphiEM(1)=0.d0

      CALL EULER (1400,h,phi,dphi,ddphi)
      CALL CORRECTOR(1400,h,phiEM,dphiEM,ddphiEM,phiEMaprox,
     + dphiEMaprox,ddphiEMaprox)
      OPEN(2,FILE='apartatb.dat')

      DO i=1,n
        write(2,*) t,phi(i),dphi(i),phiEM(i),dphiEM(i)
        t=t+h
      END DO

      CLOSE(2)


C ** apartat c) *****************************************************
C ** cas 1
      t=0.d0
      n=1400
      h=(6.d0*tn-t)/dble(n)
      phi(1)=PI-0.012d0  !valors inicials per euler sencill
      dphi(1)=0.1d0
      ddphi(1)=0.d0
      phiEM(1)=PI-0.012d0 !valors inicials per euler millorat
      dphiEM(1)=0.1d0
      ddphiEM(1)=0.d0

      CALL EULER (1400,h,phi,dphi,ddphi)
      CALL CORRECTOR(1400,h,phiEM,dphiEM,ddphiEM,phiEMaprox,
     + dphiEMaprox,ddphiEMaprox)
      OPEN(3,FILE='apartatc.dat')

      DO i=1,n
        write(3,*) t,ECIN(dphi(i)),EPOT(phi(i)),
     +      ECIN(dphi(i))+EPOT(phi(i)),
     +   ECIN(dphiEM(i)),EPOT(phiEM(i)),
     +   ECIN(dphiEM(i))+EPOT(phiEM(i))
        t=t+h
      END DO

      CLOSE(3)

C ** apartat d) *****************************************************
C ** cas +0.05
      t=0.d0
      n=5000
      h=(12.d0*tn-t)/dble(n)

      phiEM(1)=0.0d0   !valors inicials per euler millorat
      dphiEM(1)=2.d0*dsqrt(g/l) + 0.08d0
      ddphiEM(1)=0.d0

      CALL CORRECTOR(5000,h,phiEM,dphiEM,ddphiEM,phiEMaprox,
     + dphiEMaprox,ddphiEMaprox)
      OPEN(5,FILE='apartatd1.dat')

      DO i=1,n
        write(5,*) t,phiEM(i),dphiEM(i)
        t=t+h
      END DO

      CLOSE(5)
C  ** cas -0.05

      t=0.d0
      n=5000
      h=(12.d0*tn-t)/dble(n)

      phiEM(1)=0.0d0   !valors inicials per euler millorat
      dphiEM(1)=2.d0*dsqrt(g/l) - 0.08d0
      ddphiEM(1)=0.d0

      CALL CORRECTOR(5000,h,phiEM,dphiEM,ddphiEM,phiEMaprox,
     + dphiEMaprox,ddphiEMaprox)
      OPEN(6,FILE='apartatd2.dat')

      DO i=1,n
        write(6,*) t,phiEM(i),dphiEM(i)
        t=t+h
      END DO

      CLOSE(6)

C ** apartat e)
C ** 250
      t=0.d0
      n=250
      h=(10.d0*tn-t)/dble(n)

      phiEM(1)=3.05d0   !valors inicials per euler millorat
      dphiEM(1)=0.0d0
      ddphiEM(1)=0.d0

      CALL CORRECTOR(250,h,phiEM,dphiEM,ddphiEM,phiEMaprox,
     + dphiEMaprox,ddphiEMaprox)
      OPEN(7,FILE='apartate250.dat')

      DO i=1,n
        write(7,*) t,ECIN(dphiEM(i))+EPOT(phiEM(i))
        t=t+h
      END DO

      CLOSE(7)
C ** 500
      t=0.d0
      n=500
      h=(10.d0*tn-t)/dble(n)

      phiEM(1)=3.05d0   !valors inicials per euler millorat
      dphiEM(1)=0.0d0
      ddphiEM(1)=0.d0

      CALL CORRECTOR(500,h,phiEM,dphiEM,ddphiEM,phiEMaprox,
     + dphiEMaprox,ddphiEMaprox)
      OPEN(8,FILE='apartate500.dat')

      DO i=1,n
        write(8,*) t,ECIN(dphiEM(i))+EPOT(phiEM(i))
        t=t+h
      END DO

      CLOSE(8)
C ** 3300
      t=0.d0
      n=3300
      h=(10.d0*tn-t)/dble(n)

      phiEM(1)=3.05d0   !valors inicials per euler millorat
      dphiEM(1)=0.0d0
      ddphiEM(1)=0.d0

      CALL CORRECTOR(3300,h,phiEM,dphiEM,ddphiEM,phiEMaprox,
     + dphiEMaprox,ddphiEMaprox)
      OPEN(9,FILE='apartate3300.dat')

      DO i=1,n
        write(9,*) t,ECIN(dphiEM(i))+EPOT(phiEM(i))
        t=t+h
      END DO

      CLOSE(9)

C ** 45000
      t=0.d0
      n=45000
      h=(10.d0*tn-t)/dble(n)

      phiEM(1)=3.05d0   !valors inicials per euler millorat
      dphiEM(1)=0.0d0
      ddphiEM(1)=0.d0

      CALL CORRECTOR(45000,h,phiEM,dphiEM,ddphiEM,phiEMaprox,
     + dphiEMaprox,ddphiEMaprox)
      OPEN(10,FILE='apartate45000.dat')

      DO i=1,n
        write(10,*) t,ECIN(dphiEM(i))+EPOT(phiEM(i))
        t=t+h
      END DO

      CLOSE(10)

C ** final

C ** gif
      t=0.d0
      n=1400
      h=(6.d0*tn-t)/dble(n)

      phi(1)=PI-0.12d0  !valors inicials per euler sencill
      dphi(1)=0.d0
      ddphi(1)=0.d0

      phiEM(1)=PI-0.12d0      !valors inicials per euler millorat
      dphiEM(1)=0.d0
      ddphiEM(1)=0.d0

      CALL EULER (1400,h,phi,dphi,ddphi)
      CALL CORRECTOR(1400,h,phiEM,dphiEM,ddphiEM,phiEMaprox,
     + dphiEMaprox,ddphiEMaprox)
      OPEN(99,FILE='apartatbgif.txt')

      DO i=1,n
        write(99,*) i,t,phi(i),dphi(i),phiEM(i),dphiEM(i)
        write(99,*)''
        write(99,*)''
        t=t+h
      END DO

      CLOSE(99)

C ** final


      CALL SYSTEM('gnuplot -p scriptP7.plt')



      END


      SUBROUTINE EULER (n,h,phi,dphi,ddphi)
      IMPLICIT NONE
      DOUBLE PRECISION phi(n),dphi(n),ddphi(n)
      DOUBLE PRECISION h,g,l,PI
      INTEGER n,i
      l=1.05d0
      g=1.66d0
      PI=dacos(-1.d0)
      DO i=1,n
        ddphi(i)=-g*dsin(phi(i))/l
        dphi(i+1)=dphi(i)+h*ddphi(i)
        phi(i+1)=phi(i)+h*dphi(i)
      END DO
      END SUBROUTINE

      SUBROUTINE EULERMILLORAT (n,h,phiEM,dphiEM,ddphiEM)
      IMPLICIT NONE
      DOUBLE PRECISION phiEM(n),dphiEM(n),ddphiEM(n)
      DOUBLE PRECISION phiEMaprox(n),dphiEMaprox(n),ddphiEMaprox(n) !valors aproximats per la implementacio del metode
      DOUBLE PRECISION h,g,l,PI
      INTEGER n,i

      l=0.45d0
      g=3.711d0
      PI=dacos(-1.d0)

      DO i=1,n
        ddphiEM(i)=-g*dsin(phiEM(i))/l
        dphiEMaprox(i+1)=dphiEM(i)+h*ddphiEM(i)
        phiEMaprox(i+1)=phiEM(i)+h*dphiEMaprox(i)
        ddphiEMaprox(i+1)=-g*dsin(phiEMaprox(i+1))/l
        dphiEM(i+1)=dphiEM(i)+h/2.d0*(ddphiEM(i)+ddphiEMaprox(i+1))
        phiEM(i+1)=phiEM(i)+h/2.d0*(dphiEM(i)+dphiEMaprox(i+1))
      END DO
      END SUBROUTINE

      SUBROUTINE CORRECTOR(n,h,phiEM,dphiEM,ddphiEM,phiEMaprox,
     + dphiEMaprox,ddphiEMaprox)
      IMPLICIT NONE
      DOUBLE PRECISION phiEM(n),dphiEM(n),ddphiEM(n)
      DOUBLE PRECISION phiEMaprox(n),dphiEMaprox(n),ddphiEMaprox(n) !valors aproximats per la implementacio del metode
      DOUBLE PRECISION g,l,m,wn,h,PI
      INTEGER n,i

      PI=dacos(-1.d0)
      m=0.95d0
      l=1.05d0
      g=1.66d0
      wn=dsqrt(g/l)

      DO i=1,n
        ddphiEM(i)=-g*dsin(phiEM(i))/l

        dphiEMaprox(i+1)=dphiEM(i)+h*ddphiEM(i)

        phiEMaprox(i+1)=phiEM(i)+h*dphiEM(i)
        ddphiEMaprox(i+1)=-g*dsin(phiEMaprox(i+1))/l
        dphiEM(i+1)=dphiEM(i)+h/2.d0*(ddphiEM(i)+ddphiEMaprox(i+1))
        phiEM(i+1)=phiEM(i)+h/2.d0*(dphiEM(i)+dphiEMaprox(i+1))

      END DO

      END SUBROUTINE

      DOUBLE PRECISION FUNCTION ECIN(dphiecin)
      DOUBLE PRECISION dphiecin,l,m !definim la variable per utilitzar aqui
      m=0.95d0
      l=1.05d0
      ECIN=(1.d0/2.d0)*m*(dphiecin**2)*l**2
      END FUNCTION

      DOUBLE PRECISION FUNCTION EPOT(phipot)
      DOUBLE PRECISION phipot,l,m,g !definim la variable per utilitzar aqui
      m=0.95d0
      l=1.05d0
      g=1.66d0
      EPOT=-m*g*l*dcos(phipot)
      END FUNCTION

