
set term png
set output 'P4-1819P-fig1.png'


set grid
set key top right
set title "Derivada de P(v)" font "Helvetica,15"



m="./P4-1819P-fig1.dat"

set xlabel 'v'
set ylabel 'dP(v)'

plot m using 1:($2<0?$3: 1/0) with lines title 'Derivada de P(V)'
set output

reset


set term png
set output 'P4-1819P-fig2.png'


set grid
set key top right
set title "Corba polinomi POL" font "Helvetica,15"



m="./P4-1819P-fig2.dat"

set xlabel 'v'
set ylabel 'Y'

plot m using 1:2 with lines title 'Funcio POL'
set output

reset

set term png
set output 'P4-1819P-fig3.png'


set grid
set key bot right
set title "Corba PT" font "Helvetica,15"



m="./P4-1819P-extra.dat"

set xlabel 'T'
set ylabel 'P'

plot m using 1:2 with lines title 'Pl', m using 1:3 with lines title 'Pg'
set output


