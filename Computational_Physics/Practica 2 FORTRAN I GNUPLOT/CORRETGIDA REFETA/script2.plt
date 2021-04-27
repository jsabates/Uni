set term png
set output 'P2-1819P-fig1.png'

set xrange [0:5]

set grid
set key top right


m="./P2-1819P-res1.dat"


set xlabel 'temps'
set ylabel 'Posicio'

plot m using 1:2 with lines title 'Pisto 1', m using 1:3 with lines title 'Pisto 2', m using 1:7 with lines title 'Pisto 6'
set output
reset
set term png
set output 'P2-1819P-fig2.png'

set xrange [0:50]

set grid
set key top right


m="./P2-1819P-res1.dat"


set xlabel 'Posicio pisto 1'
set ylabel 'Posicio'

plot m using 2:3 with lines title 'Pisto 2', m using 2:6 with lines title 'Pisto 5'
set output

