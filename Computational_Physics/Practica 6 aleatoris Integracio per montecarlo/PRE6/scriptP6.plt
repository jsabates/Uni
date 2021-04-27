
set term png
set output 'P6-1819P-fig1.png'


set grid
set key top right
set title "convergencia calculs" font "Helvetica,15"


m="./apartata.dat"

set xlabel 'N'
set ylabel 'valor error'

set yrange [0:0.21]

plot m using 1:3 with linespoints title 'Error estimat I1', m using 1:5 with linespoints title 'Error estimat I2',m using 1:6 with linespoints title 'Error real I1',m using 1:7 with linespoints title 'Error real I2'
set output


