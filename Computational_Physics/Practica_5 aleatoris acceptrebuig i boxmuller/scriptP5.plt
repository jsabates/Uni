
set term png
set output 'P5-1819P-fig1.png'


set grid
set key top right
set title "histograma gaussiana" font "Helvetica,15"


m="./P5-1819P-res.dat"
n="./normal.dat"

set xlabel 'x'
set ylabel 'P(X)'

plot m using 1:2 with boxes fs pattern 1 title 'histograma', n using 1:2 with lines title 'Exacte'
set output

reset


set term png
set output 'P5-1819P-fig2.png'


set grid
set key top right
set title "Trajectoria de 5 molecules en 5 segons" font "Helvetica,15"


m="./P5-1819P-res2.dat"

set xlabel 'x(m)'
set ylabel 'y(m)'

plot m using 2:3 with lines title '1', m using 4:5 with lines title '2', m using 6:7 with lines title '3', m using 8:9 with lines title '4', m using 10:11 with lines title '5',

reset


set term png
set output 'P5-1819P-fig3.png'


set grid
set key top right
set title "Var(t)" font "Helvetica,15"


m="./P5-1819P-res3.dat"

set xlabel 'x(m)'
set ylabel 'y(m)'

plot m using 1:2 with linespoints title 'Variancia'

reset


set term png
set output 'P5-1819P-EXTRA.png'


set grid
set key top right
set title "histograma distancia" font "Helvetica,15"


m="./P5-1819P-extra.dat"


set xlabel 'x'
set ylabel 'P(X)'

plot m using 1:2 with boxes fs pattern 1 title 'histograma'
set output





