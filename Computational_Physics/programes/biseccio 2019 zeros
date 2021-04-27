c aquest programa implementa el metode de la biseccio per trobar zeros en funcions continues
c quin programa es
c variables que fem servir
c codi del programa a partir de la columna n7
      program biseccio
      implicit none
c (no fem servir res del que te implicit el programa)
      integer n,m,l
      double precision xa,xb,xt,f,x,error,epsilon
      parameter (epsilon=1.d-10)
100   f(x)=5.d0*x*dexp(-x)-1.d0
c podem posar etiquetes per referenciarnos mes endevant
c podem posar un "+" en la col.6 per continuar la fila anterior
      

10    write(*,*) 'introdueix lextrem esquerre'
c mostra per pantalla sense format el text
      read(*,*) xa
c llegeix lo que hem escrit i li adjudiquem el valor a xa
      write(*,*) 'introdueix lextrem dret'
      read(*,*) xb

      if(f(xa)*f(xb).gt.0.d0)then
	write(*,*) 'interval erroni'
	goto 10
      endif
c gt ens compara si es mes gran que 0 grater than , ge mes gran o igual no sutilitza

      error=2.d0*epsilon
      do while(error.gt.epsilon)

      xt=(xa+xb)/2.d0
      if(f(xa)*f(xt).gt.0)then
	xa=xt
      else
	xb=xt
      endif

      error=dabs(xb-xa)
      enddo

      write(*,*) 'el zero esta entre',xa,'i',xb

      end
c dabs=valor absolut de doble precisio
c gfortran -o bisec2019 bisec2019.f

