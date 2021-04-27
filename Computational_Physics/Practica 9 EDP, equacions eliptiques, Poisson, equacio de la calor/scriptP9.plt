
set term png
set output 'P9-1819-fig1.png'


set grid
set key bot left
set title "Convergencia Temperatura punt (7.5,23.5) Tint 10" font "Helvetica,15"


m="./conmet.dat"

set xlabel 'N iteracions'
set ylabel 'Temperatura *C'


plot m i 0 u 1:2 with lines title 'Metode Gauss-Seidel', m i 1 u 1:2 with lines title 'Metode Sobre-relaxacio'
set output
reset

set term png
set output 'P9-1819-fig2.png'


set grid
set key top right
set title "Convergencia Temperatura punt (7.5,23.5) Tint 120" font "Helvetica,15"


m="./conmet.dat"

set xlabel 'N iteracions'
set ylabel 'Temperatura *C'


plot m i 2 u 1:2 with lines title 'Metode Gauss-Seidel', m i 3 u 1:2 with lines title 'Metode Sobre-relaxacio'
set output
reset

set term png
set output 'P9-1819-fig3.png'


set grid
set key top right
set title "Convergencia Temperatura punt (7.5,23.5) Tint 1040" font "Helvetica,15"


m="./conmet.dat"

set xlabel 'N iteracions'
set ylabel 'Temperatura *C'


plot m i 4 u 1:2 with lines title 'Metode Gauss-Seidel', m i 5 u 1:2 with lines title 'Metode Sobre-relaxacio'
set output

reset

set term png

set output 'P9-1819-fig4.png'

set title 'Distribució de temperatura  (amb fonts)'

set xrange [0:33.5]
set yrange [0:45.5]

set xlabel 'x (cm)'
set ylabel 'y (cm)'
set cblabel 'T (ºC)'

set view equal xy

set view map

splot 'prova.dat' i 0 u 1:2:3 with pm3d notitle
reset

set term png

set output 'P9-1819-fig5.png'

set title 'Distribució de temperatura  (sense fonts)'

set xrange [0:33.5]
set yrange [0:45.5]

set xlabel 'x (cm)'
set ylabel 'y (cm)'
set cblabel 'T (ºC)'

set view equal xy

set view map

splot 'prova2.dat' i 0 u 1:2:3 with pm3d notitle



