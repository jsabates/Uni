
set term pdf
set output 'realitzacions.pdf'


set grid
set key bot right
set title "Vector posicio per les diferents alphes" font "Helvetica,15"


m="./levy.dat"

set xlabel 'X'
set ylabel 'Y'



plot m i 0 u 1:2 with lines  title 'alpha=4,5', m i 1 u 1:2 with lines  title 'alpha=3,5',m i 2 u 1:2 with lines  title 'alpha=2,5',
set output
reset
