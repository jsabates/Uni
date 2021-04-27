      PROGRAM MAIN
      IMPLICIT NONE

      DOUBLE PRECISION ct,e1,e2,e3,e4,e5,e6,l,x,dx,v,e
      DOUBLE PRECISION e11,e12,e13,norm,prob0,prob1,prob5,integral
      DOUBLE PRECISION yyin(2),yyout(2)
      DOUBLE PRECISION phi1(400),phi2(400),phi3(400),phi4(400)
      DOUBLE PRECISION phi11(400),phi12(400),phi13(400)
      DOUBLE PRECISION p1(400),p2(400),p3(400)
      EXTERNAL v
      INTEGER nequs,npas,i,beta
      common/dades/e,ct,l


c**** DADES
      ct=3.80995d0
!      v=30.4796d0*((dtanh(x))**2 -1.d0)
      e1=-24d0
      e2=-24.5d0
      e3=-19.d0
      e4=-19.5d0
      e5=-3.d0
      e6=-3.5d0
      l=14.d0
      nequs=2
      beta=0

C*** apartat a)

      npas=400
      dx=0.035d0

C ** energia 1
      e=e1
      x=-l/2.d0
      yyin(1)=0.d0
      yyin(2)=2.d-4
      DO i=1,npas
        CALL RK4(x,dx,nequs,yyin,yyout)
        yyin(1)=yyout(1)
        yyin(2)=yyout(2)
        phi1(i)=yyout(1)
        x=x+dx
      END DO

C ** energia 2
      e=e2
      x=-l/2.d0
      yyin(1)=0.d0
      yyin(2)=2.d-4
      DO i=1,npas
        CALL RK4(x,dx,nequs,yyin,yyout)
        yyin(1)=yyout(1)
        yyin(2)=yyout(2)
        phi2(i)=yyout(1)
        x=x+dx
      END DO

C ** energia 3
      e=e3
      x=-l/2.d0
      yyin(1)=0.d0
      yyin(2)=2.d-4
      DO i=1,npas
        CALL RK4(x,dx,nequs,yyin,yyout)
        yyin(1)=yyout(1)
        yyin(2)=yyout(2)
        phi3(i)=yyout(1)
        x=x+dx
      END DO

C ** energia 4

      e=e4
      x=-l/2.d0
      yyin(1)=0.d0
      yyin(2)=2.d-4
      DO i=1,npas
        CALL RK4(x,dx,nequs,yyin,yyout)
        yyin(1)=yyout(1)
        yyin(2)=yyout(2)
        phi4(i)=yyout(1)
        x=x+dx
      END DO

C ** escribim els resultats en el fitxer
C** no utilitzem lanterior DO perque els valors dels phi no concordarien amb les seves corresponents x
      OPEN (1,FILE='P8_1819_res1.dat')
      x=-l/2.d0
      DO i=1,npas
      write(1,*) x,phi1(i),phi2(i),phi3(i),phi4(i)
      x=x+dx

      END DO
      CLOSE(1)

C **** apartat 2)
C**   autovalor 1
      OPEN(4,FILE='convergencia.dat')
      e11=e1
      e12=e2
      CALL energia(npas,e11,e12,e13,phi13,beta)
      write(4,*)
      write(4,*)
      !CALL normalitza (npas,phi13,norm)
      !p1=norm*phi13
      p1=phi13

C**   autovalor 2
      e11=e3
      e12=e4
      CALL energia(npas,e11,e12,e13,phi13,beta)
      write(4,*)
      write(4,*)
      !CALL normalitza (npas,phi13,norm)
      !p2=norm*phi13
      p2=phi13

C**   autovalor 3
      e11=e5
      e12=e6
      CALL energia(npas,e11,e12,e13,phi13,beta)
      !CALL normalitza (npas,phi13,norm)
      !p3=norm*phi13
      p3=phi13

      CLOSE(4)

      x=-l/2.d0
      OPEN (2,FILE='P8-1819-res2.dat')
      DO i=1,npas
        WRITE(2,*) x,p1(i),p2(i),p3(i) !print dels autovectors sense normalitzar perque la subrutina que normalitza no em funciona
        x=x+dx
      END DO
      CLOSE(2)

C NO trobo manera de fer la subrutina per normalitzar
C si lexecuto no em surt del bucle del while






      CALL SYSTEM('gnuplot -p scriptP8.plt')

      END


      SUBROUTINE RK4 (x,dx,nequs,yyin,yyout)
      INTEGER nequs,i
      EXTERNAL v
      DOUBLE PRECISION yyin(nequs),yyout(nequs)
      DOUBLE PRECISION k0(nequs),k1(nequs),k2(nequs),k3(nequs)
      double precision d0(nequs),d1(nequs),d2(nequs),d3(nequs)
      DOUBLE PRECISION x,dx,l,ct,v
      common/dades/e,ct,l


      DO i=1,nequs
        d0(i)=yyin(i)
      END DO
      call derivades (nequs,x,yyin,k0)

      DO i=1,nequs
        d1(i)=yyin(i)+0.5d0*dx*k0(i)
      END DO
      call derivades (nequs,x+0.5d0*dx,d1,k1)

      DO i=1,nequs
        d2(i)=yyin(i)+0.5d0*dx*k1(i)
      END DO
      call derivades (nequs,x+0.5d0*dx,d2,k2)

      DO i=1,nequs
        d3(i)=yyin(i)+dx*k2(i)
      END DO
      call derivades (nequs,x+dx,d3,k3)

      DO i=1,nequs
        yyout(i)=yyin(i)+(dx/6.d0)*(k0(i)+2.d0*k1(i)+2.d0*k2(i)+k3(i))
      END DO


      END SUBROUTINE

      SUBROUTINE derivades(nequ,x,yin,dyout)
      INTEGER nequ
      EXTERNAL v
      DOUBLE PRECISION x,yin(nequ),dyout(nequ),v,e,ct
      COMMON/dades/e,ct,l

      dyout(1)=yin(2) !per definicio
      dyout(2)=yin(1)*(v(x)-e)/ct

      END SUBROUTINE


      SUBROUTINE energia (npas,e11,e12,e13,phi13,beta)
      IMPLICIT NONE
      INTEGER npas,nequs,i,n,beta
      EXTERNAL v
      DOUBLE PRECISION phi11(npas),phi12(npas),phi13(npas)
      DOUBLE PRECISION yyin(2),yyout(2)
      DOUBLE PRECISION e11,e12,e13,e,error,x,dx,v,ct,l
      COMMON/dades/e,ct,l

      nequs=2
      phi13(npas)=1.d0
      error=1.d-6
      n=1
      DO WHILE (dabs(phi13(npas)).gt.error)
        e=e11

	  x=-l/2.d0
        dx=0.035d0
        yyin(1)=0.d0
        yyin(2)=2.d-4
        DO i=1,npas
          CALL RK4(x,dx,nequs,yyin,yyout)
          yyin(1)=yyout(1)
          yyin(2)=yyout(2)
          phi11(i)=yyout(1)
          x=x+dx
        END DO

        e=e12
        x=-l/2.d0
        yyin(1)=0.d0
        yyin(2)=2.d-4
        DO i=1,npas
          CALL RK4(x,dx,nequs,yyin,yyout)
          yyin(1)=yyout(1)
          yyin(2)=yyout(2)
          phi12(i)=yyout(1)
          x=x+dx
        END DO

        e13=(e11*phi12(npas)-e12*phi11(npas))/(phi12(npas)-phi11(npas))
        e=e13
        x=-l/2.d0
        yyin(1)=0.d0
        yyin(2)=2.d-4
        DO i=1,npas
          CALL RK4(x,dx,nequs,yyin,yyout)
          yyin(1)=yyout(1)
          yyin(2)=yyout(2)
          phi13(i)=yyout(1)
          x=x+dx
        END DO

        e11=e12
        e12=e13
        write(4,*) n,e13
        n=n+1

      END DO

      END SUBROUTINE

      SUBROUTINE normalitza(nval,fun,norm)
      IMPLICIT NONE
      INTEGER nval,i,n

      DOUBLE PRECISION norm,fun(nval),integral,errorr

      errorr=1.d0
      n=0
      norm=0.1d0   !no se molt be con començar norm

      DO WHILE(errorr.gt.1.d-2)  !la precisio tambe es arbitraria, divergeix
        integral=0.d0
        DO i=1,nval
          integral=integral+fun(i)**2.d0
        END DO

        integral=integral*norm**2.d0
        errorr=dabs(integral-1.d0)
        norm=norm+0.01d0
        n=n+1
      END DO


      END SUBROUTINE

      DOUBLE PRECISION FUNCTION v(x)
      DOUBLE PRECISION x
      v=30.4796d0*((dtanh(x))**2 -1.d0)
      END FUNCTION
