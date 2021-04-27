
set term png
set output 'JUNY16-fig1.png'


set grid
set key bot right
set title "g en funcio de p per diferents valors de L" font "Helvetica,15"


m="./percolacio3.dat"

set xlabel 'probabilitat'
set ylabel 'fraccio g'



plot m i 0 u 1:2 with lines  title 'L=8',m i 1 u 1:2 with lines  title 'L=10',m i 2 u 1:2 with lines  title 'L=12',m i 3 u 1:2 with lines  title 'L=14',
set output
reset
