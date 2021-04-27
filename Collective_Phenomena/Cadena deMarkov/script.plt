
set term png
set output 'EXEMPLE 1 Markov.png'


set grid
set key bot right
set title "Markov" font "sans,12"


m="./data.dat"

set xlabel 'Passes'
set ylabel 'Probabilitat'
set logscale x



plot m i 0 u 1:2 with linespoints linestyle 1 title 'P1', m i 0 u 1:3 with linespoints linestyle 2 title 'P2',m i 0 u 1:4 with linespoints linestyle 3 title 'P3',m i 0 u 1:5 with linespoints linestyle 4 title 'P4'

set output
reset
