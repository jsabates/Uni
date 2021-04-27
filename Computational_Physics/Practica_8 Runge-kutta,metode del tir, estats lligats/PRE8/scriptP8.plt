
set term png
set output 'P7-1819-fig1.png'


set grid
set key bot left
set title "phi(0)=0,phi'(0)=0.05,phi(1)=0" font "Helvetica,15"


m="./datafile.dat"

set xlabel 'x/L'
set ylabel 'phi(x,E)'



plot m using 1:2 with lines title 'e1', m using 1:3 with lines title 'e2',m using 1:4 with lines title 'e3',m using 1:5 with lines title 'e4'
set output

