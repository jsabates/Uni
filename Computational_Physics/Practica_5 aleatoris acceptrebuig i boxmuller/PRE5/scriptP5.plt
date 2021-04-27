
set term png
set output 'P5-1819P-fig1.png'


set grid
set key top right
set title "histograma REBUIG" font "Helvetica,15"


m="./histograma.dat"
n="./funcio.dat"

set xlabel 'x'
set ylabel 'P(X)'

plot m using 1:2 with boxes fs pattern 1 title 'P(x)',m using 1:3 with boxes fs solid 0.5 title 'P(x) - error',m using 1:4 with boxes fill empty title 'P(x) + error',n using 1:2 with lines title 'funcio'
set output

reset

set term png
set output 'P5-1819P-fig2.png'


set grid
set key top right
set title "histograma GAUSS" font "Helvetica,15"

set xrange [-1:3]
m="./gauss.dat"
n="./normal.dat"


set xlabel 'x'
set ylabel 'P(X)'

plot m using 1:2 with boxes fs pattern 1 title 'P(x)',m using 1:3 with boxes fs solid 0.5 title 'P(x) - error',m using 1:4 with boxes fill empty title 'P(x) + error',n using 1:2 with lines title 'funcio normal'
set output
