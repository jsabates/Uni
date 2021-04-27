
set term pdf
set output 'ccdfs.pdf'


set grid
set key bot right
set title "Distribucio acumulada en escala log-log" font "Helvetica,15"


m="./ccdf.dat"

set logscale xy

set xlabel 'LOG(r)'
set ylabel 'LOG(pc)'



plot m i 0 u 1:2 with lines  title 'distribucio acumulada'
set output
reset
