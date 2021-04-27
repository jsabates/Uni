
set term png
set output 'JUNY18-fig1.png'


set grid
set key bot right
set title "Posicio espins +1 pel TEMPS=0" font "Helvetica,15"


m="./data.dat"

set xlabel 'temps'
set ylabel 'prevelancia'



plot m i 0 u 1:2 with lines  title 'prob1', m i 1 u 1:2 with lines  title 'prob2', m i 2 u 1:2 with lines  title 'prob3'
set output
reset
