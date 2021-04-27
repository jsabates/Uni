
set term png
set output 'P9-1819-fig1.png'


set grid
set key bot left
set title "Convergencia Temperatura punt (16,12) Metode relaxacio" font "Helvetica,15"


m="./conmet1.dat"

set xlabel 'N iteracions'
set ylabel 'Temperatura *C'



plot m i 0 u 1:2 with lines title 'Temperatura inicial 0*C', m i 1 u 1:2 with lines title 'Temperatura inicial 4*C',m i 2 u 1:2 with lines title 'Temperatura inicial 700*C'
set output
reset

set term png
set output 'P9-1819-fig2.png'


set grid
set key bot left
set title "Convergencia Temperatura punt (16,12) Metode Jacobi" font "Helvetica,15"


m="./conmet2.dat"

set xlabel 'N iteracions'
set ylabel 'Temperatura *C'



plot m i 0 u 1:2 with lines title 'Temperatura inicial 0*C', m i 1 u 1:2 with lines title 'Temperatura inicial 4*C',m i 2 u 1:2 with lines title 'Temperatura inicial 700*C'
set output
reset

set term png
set output 'P9-1819-fig3.png'


set grid
set key bot left
set title "Convergencia Temperatura punt (16,12) Metode sobre-relaxacio" font "Helvetica,15"


m="./conmet3.dat"

set xlabel 'N iteracions'
set ylabel 'Temperatura *C'



plot m i 0 u 1:2 with lines title 'Temperatura inicial 0*C', m i 1 u 1:2 with lines title 'Temperatura inicial 4*C',m i 2 u 1:2 with lines title 'Temperatura inicial 700*C'

set output
reset

set term png

set output 'P9-1819-fig4.png'

set title 'Distribució de temperatura  (amb fonts)'

set xrange [0:30.5]
set yrange [0:18.5]

set xlabel 'x (cm)'
set ylabel 'y (cm)'
set cblabel 'T (ºC)'

set view equal xy

set view map

splot 'prova.dat' i 0 u 1:2:3 with pm3d notitle


reset

set term png

set output 'P9-1819-fig5.png'

set title 'Distribució de temperatura  (Sense fonts)'

set xrange [0:30.5]
set yrange [0:18.5]

set xlabel 'x (cm)'
set ylabel 'y (cm)'
set cblabel 'T (ºC)'

set view equal xy

set view map

splot 'prova2.dat' i 0 u 1:2:3 with pm3d notitle


reset
