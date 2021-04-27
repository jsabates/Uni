      PROGRAM RUNGEKUTTA
      IMPLICIT NONE
      DOUBLE PRECISION x,dx,yyin,yyout,phi(1000),p
      INTEGER i,j,k,npas,nequs
      common/dades/p

      p=1.d0/2.d0
      npas=1000
      dx=0.001d0
      nequs=1

C NO CREC QUE ESTIGUI BEN FET

      x=0.d0
      yyin=1.d0

      DO i=1,npas
        CALL RK4(x,dx,nequs,yyin,yyout)
        yyin=yyout
        phi(i)=yyout
        x=x+dx
        write(*,*) phi(i)
      END DO


      END

      SUBROUTINE RK4 (x,dx,nequs,yyin,yyout)
      INTEGER nequs,i
      DOUBLE PRECISION yyin(nequs),yyout(nequs)
      DOUBLE PRECISION k0(nequs),k1(nequs),k2(nequs),k3(nequs)
      double precision d0(nequs),d1(nequs),d2(nequs),d3(nequs)
      DOUBLE PRECISION x,dx,l,ct
      common/dades/p

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
      DOUBLE PRECISION x,yin(nequ),dyout(nequ),p
      common/dades/p

      dyout(1)=-x+((1.d0-p)/p)*x*(1-x)

      END SUBROUTINE

