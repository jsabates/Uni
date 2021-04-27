
set term png
set output 'P6-1819P-fig1.png'


set grid
set key top right
set title "Numero de quarks amb errors" font "Helvetica,15"


m="./P6-1819P-res.dat"

set xlabel 'N'
set ylabel 'nombre de quarks'



plot m using 1:2:3 with yerrorbars title 'nombre de quarks up', m using 1:4:5 with yerrorbars title 'nombre de quarks Down',
set output

reset

set term png
set output 'P6-1819P-fig2.png'


set grid
set key top right
set title "convergencia calcul I2" font "Helvetica,15"


m="./P6-1819P-res2.dat"

set xlabel 'N'
set ylabel 'valor integral'
set format x '10^{%L}



plot m using 1:2:3 with yerrorbars title 'Integral 2'
set output

reset

set term png
set output 'P6-1819P-fig3.png'


set grid
set key top right
set title "convergencia calcul I3" font "Helvetica,15"


m="./P6-1819P-res3.dat"

set xlabel 'N'
set ylabel 'valor integral'
set format x '10^{%L}



plot m using 1:2:3 with yerrorbars title 'Integral 3'
set output
