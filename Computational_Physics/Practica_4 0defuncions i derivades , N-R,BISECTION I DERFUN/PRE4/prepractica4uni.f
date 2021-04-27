      PROGRAM MAIN
      IMPLICIT NONE

      EXTERNAL fun
      DOUBLE PRECISION
      INTEGER


      subroutine newtonrap(x0,error,fun,n,x1)
      implicit none

      double precision
      integer n

      parameter()

      error=2.d0*delta
      n=0
      do while(error.gt.delta)
       n=n+1
       call fun(x0)


      subroutine derfun(ndat,x,fu,dfu)



      do n=1,ndat

