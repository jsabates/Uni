      program cumulative
      double precision r,rmin,rmax,a,b
      integer N,Ncount,ir,i
      parameter (N=100)          !number of sampled points in the ccdf
      double precision histo(1:N)
      character*80 filenameinput,filenameoutput

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      filenameinput='r_alphas.dat'
      filenameoutput='ccdf.dat'
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      open(1,file=filenameinput)

      rmin=1.d10
      rmax=0.d0
      Ncount=0
      do while(.true.)
       read(1,*,END=1) r
       Ncount=Ncount+1
       if(r.gt.rmax) rmax=r
       if(r.lt.rmin) rmin=r
      enddo
1     close(1)

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      a=dlog10(rmax/rmin)/dble(N-1)
      b=dlog10(rmin)-a

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      open(1,file=filenameinput)
      histo=0.d0
      do while(.true.)

       read(1,*,END=2) r
       ir=INT((dlog10(r)-b)/a)
       do i=1,ir
        histo(i)=histo(i)+1.d0
       enddo

      enddo
2     histo=histo/dble(Ncount)
      close(1)

      open(1,file=filenameoutput)
      do i=1,N
       write(1,*) 10.d0**(a*dble(i)+b),histo(i)
      enddo
      close(1)

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      CALL SYSTEM('gnuplot -p script2.plt')

      stop
      end


