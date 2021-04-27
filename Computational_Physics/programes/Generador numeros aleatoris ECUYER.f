      PROGRAM MAIN
      IMPLICIT NONE

      INTEGER idum
      DOUBLE PRECISION ran2
      idum=-166211
      write(*,*)ran2(idum)
      END

      DOUBLE PRECISION FUNCTION ran2(idum)
      INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
      DOUBLE PRECISION AM,EPS,RNMX
      PARAMETER (IM1=2147483563,IM2=2147483399,AM=1./IM1,IMM1=IM1-1,
     +   IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,
     +   IR2=3791,NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
        !Long period (> 2 x 10 18 ) random number generator of L'Ecuyer with Bays-Durham shuffle
        !and added safeguards. Returns a uniform random deviate between 0.0 and 1.0 (exclusive
        !of the endpoint values). Call with idum a negative integer to initialize; thereafter, do not
        !alter idum between successive deviates in a sequence. RNMX should approximate the largest
        !floating value that is less than 1.
      INTEGER idum2,j,k,iv(NTAB),iy
      SAVE iv,iy,idum2
      DATA idum2/123456789/, iv/NTAB*0/, iy/0/
      if (idum.le.0) then               !Initialize.
          idum=max(-idum,1)             !Be sure to prevent idum = 0.
          idum2=idum
          do j=NTAB+8,1,-1           !Load the shuffle table (after 8 warm-ups).
               k=idum/IQ1
               idum=IA1*(idum-k*IQ1)-k*IR1
               if (idum.lt.0) idum=idum+IM1
               if (j.le.NTAB) iv(j)=idum
          enddo
          iy=iv(1)
      endif
      k=idum/IQ1                        !Start here when not initializing.
      idum=IA1*(idum-k*IQ1)-k*IR1       !Compute idum=mod(IA1*idum,IM1) without over-
      if (idum.lt.0) idum=idum+IM1      !flows by Schrage's method.
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2     !Compute idum2=mod(IA2*idum2,IM2) likewise.
      if (idum2.lt.0) idum2=idum2+IM2
      j=1+iy/NDIV                       !Will be in the range 1:NTAB.
      iy=iv(j)-idum2                    !Here idum is shuffled, idum and idum2 are com-
      iv(j)=idum                        !bined to generate output.
      if(iy.lt.1)iy=iy+IMM1
      ran2=dmin1(AM*dble(iy),RNMX)      !Because users don't expect endpoint values.

      END FUNCTION
