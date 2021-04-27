
set term png
set output 'P4-1819P-fig1.png'


set grid
set key top left
set title "Grafica funcio i derivada a)" font "Helvetica,15"

set yrange[-6:6]

m="./P4-1819P-res.dat"

set xlabel 'v'
set ylabel 'P(v)'

plot m using 1:2 with lines title 'Funcio P(v)',m using 1:3 with lines title 'Derivada P(v)'
set output
reset

set term png
set output 'P4-1819P-fig2.png'


set grid
set key top right
set title "CONVERGENCIA del metode" font "Helvetica,15"


m="./conver02"
n="./conver07"
p="./conver15"

set xlabel 'n iteracions'
set ylabel 'valor arrel'

plot m using 1:2 with linespoints title 'x0 = 0.2',n using 1:2 with linespoints title 'x0 = 0.7',p using 1:2 with linespoints title 'x0 = 1.5'
set output
reset

set term png
set output 'P4-1819P-fig3.png'


set grid
set key top right
set title "CONVERGENCIA del metode" font "Helvetica,15"

set yrange[-6:6]

m="./P4-1819P-res3-n34.dat"
n="./P4-1819P-res3-n420.dat"
p="./P4-1819P-res.dat"

set xlabel 'x'
set ylabel 'funcio'

plot m using 1:3 with linespoints title 'Derivada aprox 34',n using 1:3 with linespoints title 'Derivada aprox 420',p using 1:3 with lines title 'Derivada exacte'
set output

