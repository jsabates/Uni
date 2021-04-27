set term png
set output 'P3-1819-fig1.png'


set grid
set key top left
set title "Variacio de l'error" font "Helvetica,15"

set logscale xy

set format xy '10^{%L}

m="./P3-1819-error1.dat"

set xlabel 'longitud dels subintervals (h)'
set ylabel 'error dels calculs'

plot m using 1:2 with linespoints title 'Trapezis (phi0 = 0)',m using 1:3 with linespoints title 'Simpson (phi0 = 0)',m using 1:4 with linespoints title 'Trapezis (phi0 = PI-0,1)',m using 1:5 with linespoints title 'Simpson (phi0 = PI-0,1)',m using 1:6 with linespoints title 'Trapezis (phi0 = PI-0,001)',m using 1:7 with linespoints title 'Simpson (phi0 = PI-0,001)'
set output

reset

set term png
set output 'P3-1819-fig2.png'


set grid
set key top left
set title "Comparacio Integral vs Aproximat" font "Helvetica,15"

set xrange[3:5]
set format xy '10^{%L}

m="./P3-1819-T.dat"

set logscale y

set xlabel 'longitud dels subintervals (h)'
set ylabel 'integral'

plot m using 3:2 with linespoints title 'Integral'
set output

