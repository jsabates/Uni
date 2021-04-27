set term png
set output 'P3-1819P-fig1.png'


set grid
set key top left
set title "Convergencia Resultats Longituds" font "Helvetica,15"

set logscale xy
set format xy '10^{%L}

m="./P3-1819-res2.dat"

set xlabel 'longitud dels subintervals (h)'
set ylabel 'error dels calculs'

plot m using 1:2 with linespoints title 'Trapezis', m using 1:3 with linespoints title 'Simpson'
set output
reset
set term png
set output 'P3-1819P-fig2.png'

set title "Convergencia Resultats Masa" font "Helvetica,15"


set logscale xy
set format xy '10^{%L}


set grid
set key top left

n="./P3-1819-res3.dat"

set xlabel 'longitud dels subintervals (h)'
set ylabel 'error dels calculs'

plot m using 1:2 with linespoints title 'Trapezis', m using 1:3 with linespoints title 'Simpson'
set output

reset

set term png
set output 'P3-1819P-fig3.png'


set grid
set key top left
set title "Convergencia Resultats Masa (t)" font "Helvetica,15"


set logscale xy
set format xy '10^{%L}

q="./P3-1819-res4.dat"

set xlabel 'longitud dels subintervals (h)'
set ylabel 'error dels calculs'

plot q using 1:2 with linespoints title 'Trapezis', q using 1:3 with linespoints title 'Simpson'
set output




