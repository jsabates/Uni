

      PROGRAM Hola
      IMPLICIT NONE
      INTEGER*4 SEED, i, u,v,NRAND, j, L,IMC,IPAS
      INTEGER*4 MCTOT,MCD,MCINI,CONTADOR
      PARAMETER (L=128)
      INTEGER*2 S(1:L,1:L)
      INTEGER*4 N,DE
      INTEGER*4 PBC(0:L+1)
      REAL*8 W(-8:8)
      REAL*8 ENEBIS,DELTA,sumav,m,e,SUM,SUME,SUME2,SUMM,SUMM2,SUMAM
      REAL*8 x, suma, suma2, sigma, genrand_real2, MAG, MAGNE,ENE,ENERG
      REAL*8 VARE,VARM,NSEED,SEED0,SUS,CAP,IMANPP,DERIV,errore,errorm
      REAL*8 SUMEeTant,errorcap,errorsus
      REAL*4 TIME1,TIME2
      DOUBLE PRECISION TEMP, T,T1,T2
      CHARACTER*20 NOM
      CHARACTER*30 DATE
      NAMELIST /DADES/ NOM, TEMP, NSEED, SEED0, MCTOT, MCINI, MCD

      NOM="MC-L-1282"
!      SEED=234567
      T1=240
      T2=340
      NSEED=200
      SEED0=117654
      MCTOT=40000
      MCINI=2000
      MCD=20
      N=L*L
      m=MAG/N
      e=ENE/N
      CONTADOR=0

!      OPEN(UNIT=10,FILE="MC2.dat")
!      READ(10,DADES)
!      CLOSE(10)



! DEFINIM ELS PBC
      PBC(0)=L
      DO i=1,L
        PBC(i)=i
      ENDDO
      PBC(L+1)=1

      OPEN(UNIT=13,FILE=NOM//".out")  !GENEREM L'ARXIU DE RESULTATS

!CONTROL DE TEMPS DE CPU INICIAL
        CALL CPU_TIME(TIME1)

!BUCLE per totes les temperatures desde t1 fins a t2
      DO T=T1,T2
        CONTADOR=CONTADOR+1
        SUMEeTant=SUME
        TEMP=T/100
        WRITE(*,*) "TEMP= ",TEMP
!millora rapidesa
        DO i=-8,8
          W(i)=exp(-i/TEMP)
        ENDDO

!***VARIABLES INICALS DELS CONTADORS************************************


        SUM=0.0D0
        SUME=0.0D0
        SUME2=0.0D0
        SUMM=0.0D0
        SUMM2=0.0D0
        SUMAM=0.0D0

!bucle per probar diferents llavors per cada temp
        DO SEED=SEED0,SEED0+NSEED-1,1
          WRITE(*,*) "LLAVOR= ",SEED

          CALL init_genrand(SEED)

!generem la matriu de spins aleatoris
          DO i=1,L
            DO j=1,L
              IF (genrand_real2().lt.0.5D0) THEN
                S(i,j)=1
                CALL WRITECONFIG(S,L,i,j)
              ELSE
                S(i,j)=-1
              END IF
            END DO
          END DO

! Calculem les variables amb la matriu inicial, fem un reset
          IMC=0
          MAG=MAGNE(S,L)
          ENE=ENERG(S,L,PBC)
          ENEBIS=ENERG(S,L,PBC)

!Tota la S=+1 per comprovar l'energia *******************
!      DO i=1,L
!        DO j=1,L
!            S(i,j)=1
!        END DO
!      END DO
!********************************************************


!      WRITE(*,*) "PAS=",IMC,"SUM=",SUM,"SUME=",SUME,"SUME2=",SUME2,
!     &   "SUMM=",SUMM,"SUMM2=",SUMM2,"SUMAM=",SUMAM,"MAG=",MAG,
!     &   "ENE=",ENE
!      WRITE(*,*) "La magnetitzacio val = ",MAG
!      WRITE(*,*) "L'energia val = ", ENE
!      WRITE(12,*) IMC, ENE/N, ENEBIS, MAG/N

          DO IMC=1,MCTOT
            DO IPAS=1,N

            u=int(genrand_real2()*L)+1 !prenem 2 nombres aleatoris entre 1 i L per prendre i,j
            v=int(genrand_real2()*L)+1

            sumav=S(u,PBC(v+1)) + S(u,PBC(v-1)) + S(PBC(u+1),v) +
     &S(PBC(u-1),v)

!fem la suma dels valors dels veins
            DE=2*S(u,v)*sumav !trobem el increment d'energia a l'hora de fer el canvi

            IF (DE.LE.0) THEN
              S(u,v)= -S(u,v)
              ENE=ENE+DE
            ELSE IF (DE.GT.0) THEN
              DELTA=genrand_real2()
              IF (DELTA.LT.W(DE)) THEN
                  S(u,v)= -S(u,v)
                  ENE=ENE+DE
              END IF
            END IF
            ENDDO
!            ENEBIS = ENERG(S,L,PBC)
            IF ((IMC.GT.MCINI).AND.(MCD*(IMC/MCD).EQ.IMC)) THEN

                MAG=MAGNE(S,L)

                SUM=SUM+1.0D0
                SUME=SUME+ENE
                SUME2=SUME2+ENE*ENE

                SUMM=SUMM+MAG
                SUMM2=SUMM2+MAG*MAG
                SUMAM=SUMAM+ABS(MAG)

            ENDIF
!        WRITE(*,*) "PAS=",IMC,"SUM=",SUM,"SUME=",SUME,"SUME2=",SUME2,
!     &   "SUMM=",SUMM,"SUMM2=",SUMM2,"SUMAM=",SUMAM,"MAG=",MAG,
!     &   "ENE=",ENE
!        WRITE(*,*) "MC= ",IMC,"ENERGIA = ", ENE, "ENERGIABIS = ",
!     &ENEBIS,"MAGNE = ", MAG
!        WRITE(12,*) IMC, ENE/N, ENEBIS, MAG/N

          ENDDO
        ENDDO
        SUME=SUME/SUM
        SUME2=SUME2/SUM
        SUMM=SUMM/SUM
        SUMM2=SUMM2/SUM
        SUMAM=SUMAM/SUM
        VARE = SUME2-SUME*SUME
        VARM = SUMM2-SUMM*SUMM
        SUS = ((SUMM2-(SUMAM**2))/(N*TEMP))
        CAP = VARE/(N*(TEMP**2))
        IMANPP=sqrt(SUMM2/(N**2))
        DERIV= ((SUME/N)-(SUMEeTant/N))/(0.01)
        errore=(sqrt(VARE))/(N*sqrt(SUM))
        errorm=(sqrt(VARM))/(N*sqrt(SUM))
        errorcap=(2*errore/(abs(SUME)*N*TEMP**2))*(abs(SUME2)-
     & abs(SUME**2))
        errorsus=(2*errorm/(abs(SUMAM)*N*TEMP))*(abs(SUMM2)-
     & abs(SUMAM**2))

        IF (CONTADOR==1) THEN

        WRITE(13,*) L,TEMP,SUM,SUME/N,SUME2/(N*N),VARE/(N*N),SUMM/N,
     & SUMAM/N,SUMM2/(N*N),VARM/(N*N),SUS,CAP,errore,errorm,errorcap,
     & errorsus,IMANPP

        ELSE

        WRITE(13,*) L,TEMP,SUM,SUME/N,SUME2/(N*N),VARE/(N*N),SUMM/N,
     & SUMAM/N,SUMM2/(N*N),VARM/(N*N),SUS,CAP,errore,errorm,errorcap,
     & errorsus,IMANPP,DERIV

        ENDIF

      ENDDO
      CLOSE(13)

! Control de temps de CPU final
      CALL CPU_TIME(TIME2)

! Data i hora final
      CALL FDATE(DATE)

      WRITE(*,*) DATE
      WRITE(*,*) "CPUTIME = ",TIME2-TIME1


!      WRITE(*,*) "PAS=",IMC,"SUM=",SUM,"SUME=",SUME,"SUME2=",SUME2,
!     &   "SUMM=",SUMM,"SUMM2=",SUMM2,"SUMAM=",SUMAM,"MAG=",MAG,
!     &   "ENE=",ENE,"VARE=",VARE,"VARM=",VARM
!      CALL SYSTEM ('gnuplot -p P4-plotconfig.gnu')

!      CLOSE(UNIT=12)
      STOP
      END





      REAL*8 FUNCTION ENERG(S,L,PBC)
      INTEGER*2 S(1:L,1:L)
      INTEGER*4 I,J,L
      INTEGER*4 PBC(0:L+1)
      REAL*8 ENE
      ENE=0.0D0
      DO I=1,L
        DO J=1,L
            ENE=ENE-S(i,j)*S(PBC(I+1),J)-S(i,j)*S(I,PBC(J+1))
        ENDDO
      ENDDO
      ENERG=ENE
      RETURN
      END

      REAL*8 FUNCTION MAGNE(S,L)
      INTEGER*2 S(1:L,1:L)
      INTEGER*4 I,J,L
      REAL*8 MAG
      MAG=0.0D0
      DO I=1,L
        DO J=1,L
            MAG=MAG+S(i,j)
        ENDDO
      ENDDO
      MAGNE=MAG
      RETURN
      END

      SUBROUTINE WRITECONFIG(S,L,i,j)
      INTEGER i,j,L
      INTEGER*2 S(1:L,1:L)
      WRITE(1,*) i,j
      RETURN
      END

      subroutine init_genrand(s)
      integer s
      integer N
      integer DONE
      integer ALLBIT_MASK
      parameter (N=624)
      parameter (DONE=123456789)
      integer mti,initialized
      integer mt(0:N-1)
      common /mt_state1/ mti,initialized
      common /mt_state2/ mt
      common /mt_mask1/ ALLBIT_MASK
c
      call mt_initln
      mt(0)=iand(s,ALLBIT_MASK)
      do 100 mti=1,N-1
        mt(mti)=1812433253*
     &          ieor(mt(mti-1),ishft(mt(mti-1),-30))+mti
        mt(mti)=iand(mt(mti),ALLBIT_MASK)
  100 continue
      initialized=DONE
c
      return
      end
c-----------------------------------------------------------------------
c     initialize by an array with array-length
c     init_key is the array for initializing keys
c     key_length is its length
c-----------------------------------------------------------------------
      subroutine init_by_array(init_key,key_length)
      integer init_key(0:*)
      integer key_length
      integer N
      integer ALLBIT_MASK
      integer TOPBIT_MASK
      parameter (N=624)
      integer i,j,k
      integer mt(0:N-1)
      common /mt_state2/ mt
      common /mt_mask1/ ALLBIT_MASK
      common /mt_mask2/ TOPBIT_MASK
c
      call init_genrand(19650218)
      i=1
      j=0
      do 100 k=max(N,key_length),1,-1
        mt(i)=ieor(mt(i),ieor(mt(i-1),ishft(mt(i-1),-30))*1664525)
     &           +init_key(j)+j
        mt(i)=iand(mt(i),ALLBIT_MASK)
        i=i+1
        j=j+1
        if(i.ge.N)then
          mt(0)=mt(N-1)
          i=1
        endif
        if(j.ge.key_length)then
          j=0
        endif
  100 continue
      do 200 k=N-1,1,-1
        mt(i)=ieor(mt(i),ieor(mt(i-1),ishft(mt(i-1),-30))*1566083941)-i
        mt(i)=iand(mt(i),ALLBIT_MASK)
        i=i+1
        if(i.ge.N)then
          mt(0)=mt(N-1)
          i=1
        endif
  200 continue
      mt(0)=TOPBIT_MASK
c
      return
      end
c-----------------------------------------------------------------------
c     generates a random number on [0,0xffffffff]-interval
c-----------------------------------------------------------------------
      function genrand_int32()
      integer genrand_int32
      integer N,M
      integer DONE
      integer UPPER_MASK,LOWER_MASK,MATRIX_A
      integer T1_MASK,T2_MASK
      parameter (N=624)
      parameter (M=397)
      parameter (DONE=123456789)
      integer mti,initialized
      integer mt(0:N-1)
      integer y,kk
      integer mag01(0:1)
      common /mt_state1/ mti,initialized
      common /mt_state2/ mt
      common /mt_mask3/ UPPER_MASK,LOWER_MASK,MATRIX_A,T1_MASK,T2_MASK
      common /mt_mag01/ mag01
c
      if(initialized.ne.DONE)then
        call init_genrand(21641)
      endif
c
      if(mti.ge.N)then
        do 100 kk=0,N-M-1
          y=ior(iand(mt(kk),UPPER_MASK),iand(mt(kk+1),LOWER_MASK))
          mt(kk)=ieor(ieor(mt(kk+M),ishft(y,-1)),mag01(iand(y,1)))
  100   continue
        do 200 kk=N-M,N-1-1
          y=ior(iand(mt(kk),UPPER_MASK),iand(mt(kk+1),LOWER_MASK))
          mt(kk)=ieor(ieor(mt(kk+(M-N)),ishft(y,-1)),mag01(iand(y,1)))
  200   continue
        y=ior(iand(mt(N-1),UPPER_MASK),iand(mt(0),LOWER_MASK))
        mt(kk)=ieor(ieor(mt(M-1),ishft(y,-1)),mag01(iand(y,1)))
        mti=0
      endif
c
      y=mt(mti)
      mti=mti+1
c
      y=ieor(y,ishft(y,-11))
      y=ieor(y,iand(ishft(y,7),T1_MASK))
      y=ieor(y,iand(ishft(y,15),T2_MASK))
      y=ieor(y,ishft(y,-18))
c
      genrand_int32=y
      return
      end
c-----------------------------------------------------------------------
c     generates a random number on [0,0x7fffffff]-interval
c-----------------------------------------------------------------------
      function genrand_int31()
      integer genrand_int31
      integer genrand_int32
      genrand_int31=int(ishft(genrand_int32(),-1))
      return
      end
c-----------------------------------------------------------------------
c     generates a random number on [0,1]-real-interval
c-----------------------------------------------------------------------
      function genrand_real1()
      double precision genrand_real1,r
      integer genrand_int32
      r=dble(genrand_int32())
      if(r.lt.0.d0)r=r+2.d0**32
      genrand_real1=r/4294967295.d0
      return
      end
c-----------------------------------------------------------------------
c     generates a random number on [0,1)-real-interval
c-----------------------------------------------------------------------
      function genrand_real2()
      double precision genrand_real2,r
      integer genrand_int32
      r=dble(genrand_int32())
      if(r.lt.0.d0)r=r+2.d0**32
      genrand_real2=r/4294967296.d0
      return
      end
c-----------------------------------------------------------------------
c     generates a random number on (0,1)-real-interval
c-----------------------------------------------------------------------
      function genrand_real3()
      double precision genrand_real3,r
      integer genrand_int32
      r=dble(genrand_int32())
      if(r.lt.0.d0)r=r+2.d0**32
      genrand_real3=(r+0.5d0)/4294967296.d0
      return
      end
c-----------------------------------------------------------------------
c     generates a random number on [0,1) with 53-bit resolution
c-----------------------------------------------------------------------
      function genrand_res53()
      double precision genrand_res53
      integer genrand_int32
      double precision a,b
      a=dble(ishft(genrand_int32(),-5))
      b=dble(ishft(genrand_int32(),-6))
      if(a.lt.0.d0)a=a+2.d0**32
      if(b.lt.0.d0)b=b+2.d0**32
      genrand_res53=(a*67108864.d0+b)/9007199254740992.d0
      return
      end
c-----------------------------------------------------------------------
c     initialize large number (over 32-bit constant number)
c-----------------------------------------------------------------------
      subroutine mt_initln
      integer ALLBIT_MASK
      integer TOPBIT_MASK
      integer UPPER_MASK,LOWER_MASK,MATRIX_A,T1_MASK,T2_MASK
      integer mag01(0:1)
      common /mt_mask1/ ALLBIT_MASK
      common /mt_mask2/ TOPBIT_MASK
      common /mt_mask3/ UPPER_MASK,LOWER_MASK,MATRIX_A,T1_MASK,T2_MASK
      common /mt_mag01/ mag01
CC    TOPBIT_MASK = Z'80000000'
CC    ALLBIT_MASK = Z'ffffffff'
CC    UPPER_MASK  = Z'80000000'
CC    LOWER_MASK  = Z'7fffffff'
CC    MATRIX_A    = Z'9908b0df'
CC    T1_MASK     = Z'9d2c5680'
CC    T2_MASK     = Z'efc60000'
      TOPBIT_MASK=1073741824
      TOPBIT_MASK=ishft(TOPBIT_MASK,1)
      ALLBIT_MASK=2147483647
      ALLBIT_MASK=ior(ALLBIT_MASK,TOPBIT_MASK)
      UPPER_MASK=TOPBIT_MASK
      LOWER_MASK=2147483647
      MATRIX_A=419999967
      MATRIX_A=ior(MATRIX_A,TOPBIT_MASK)
      T1_MASK=489444992
      T1_MASK=ior(T1_MASK,TOPBIT_MASK)
      T2_MASK=1875247104
      T2_MASK=ior(T2_MASK,TOPBIT_MASK)
      mag01(0)=0
      mag01(1)=MATRIX_A
      return
      end
