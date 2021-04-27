      PROGRAM MAIN
      IMPLICIT NONE


      DOUBLE PRECISION v,e,e1,e2,e3,phi3(600),norm,x,dx
      DOUBLE PRECISION p1(600),p2(600),p3(600),p4(600)
      INTEGER i, npas
      COMMON/dades/v,e

      v=-1.2d0

      e1=1.d0
      e2=2.d0
      npas=600

      call energia(npas,e1,e2,e3,phi3)
      call normalitza(npas,phi3,norm)
      p1=phi3*norm

      e1=10.d0
      e2=11.d0

      call energia(npas,e1,e2,e3,phi3)
      call normalitza(npas,phi3,norm)
      p2=phi3*norm

      e1=40.d0
      e2=41.d0

      call energia(npas,e1,e2,e3,phi3)
      call normalitza(npas,phi3,norm)
      p3=phi3*norm

      e1=70.d0
      e2=71.d0

      call energia(npas,e1,e2,e3,phi3)
      call normalitza(npas,phi3,norm)
      p4=phi3*norm

      OPEN(1,FILE='datafile.dat')

      x=0.d0
      dx=1.d0/600.d0
      DO i=1,npas
        WRITE(1,*) x,p1(i),p2(i),p3(i),p4(i)
        x=x+dx
      END DO

      CLOSE(1)

      CALL SYSTEM('gnuplot -p scriptP8.plt')


      END


      SUBROUTINE RK4 (x,dx,nequs,yyin,yyout)
      INTEGER nequs,i
      DOUBLE PRECISION yyin(nequs),yyout(nequs)
      DOUBLE PRECISION k0(nequs),k1(nequs),k2(nequs),k3(nequs)
      double precision d0(nequs),d1(nequs),d2(nequs),d3(nequs)
      DOUBLE PRECISION x,dx

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
      DOUBLE PRECISION x,yin(nequ),dyout(nequ),v,e
      COMMON/dades/v,e

      dyout(1)=yin(2) !per definicio
      dyout(2)=2.d0*yin(1)*(v-e)

      END SUBROUTINE


      SUBROUTINE energia (npas,e1,e2,e3,phi3)
      IMPLICIT NONE
      INTEGER npas,nequs,i
      DOUBLE PRECISION phi1(npas),phi2(npas),phi3(npas)
      DOUBLE PRECISION yyin(2),yyout(2)
      DOUBLE PRECISION e1,e2,e3,e,error,x,dx,v
      COMMON/dades/v,e

      nequs=2
      phi3(npas)=1.d0
      error=1.d-6
      DO WHILE (dabs(phi3(npas)).gt.error)
        e=e1

        x=0.d0
        dx=1.d0/600.d0
        yyin(1)=0.d0
        yyin(2)=0.05d0
        DO i=1,npas
          CALL RK4(x,dx,nequs,yyin,yyout)
          x=x+dx
          yyin(1)=yyout(1)
          yyin(2)=yyout(2)
          phi1(i)=yyout(1)
        END DO

        e=e2
        x=0.d0
        yyin(1)=0.d0
        yyin(2)=0.05d0
        DO i=1,npas
          CALL RK4(x,dx,nequs,yyin,yyout)
          x=x+dx
          yyin(1)=yyout(1)
          yyin(2)=yyout(2)
          phi2(i)=yyout(1)
        END DO

        e3=(e1*phi2(npas)-e2*phi1(npas))/(phi2(npas)-phi1(npas))
        e=e3
        x=0.d0
        yyin(1)=0.d0
        yyin(2)=0.05d0
        DO i=1,npas
          CALL RK4 (x,dx,nequs,yyin,yyout)
          x=x+dx
          yyin(1)=yyout(1)
          yyin(2)=yyout(2)
          phi3(i)=yyout(1)
        END DO

        e1=e2
        e2=e3

      END DO

      END SUBROUTINE

      SUBROUTINE normalitza(nval,fun,norm)
      IMPLICIT NONE
      INTEGER nval,i
cºººnormalitza una funcio calculada en forma de vector (sent nor la ctant de normalitzacio)
cºººels limits d'integracio han d'estar implicits al propi calcul de la funcio, que no va aqui
      DOUBLE PRECISION norm,fun(nval),integral,errorr

      errorr=1.d0
      norm=0.1d0   !no se molt be con començar nor ni que sumar-li a cada iteracio pero provo aixo i si funciona doncs estupendu

      DO WHILE(errorr.gt.1.d-4)  !la precisio tambe es arbitraria, he aconseguit nomes fins aqui, si augmento a d-5 l'erorr divergeix
        integral=0.d0
        DO i=1,nval
          integral=integral+fun(i)**2.d0
        END DO

        integral=integral*norm**2.d0
        errorr=dabs(integral-1.d0)
        norm=norm+0.0001d0
      END DO


      END SUBROUTINE
