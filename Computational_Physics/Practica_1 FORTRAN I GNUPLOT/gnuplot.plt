set term png
set output 'P1-1819P-fig1.png'

set xrange[11:311]

set grid
set key top left
set logscale y

m="./P1-1819P-res1.dat"
n="./P1-1819P-res3.dat"

set xlabel 'N'
set ylabel 'S_N^8'

plot m using 1:2 with linespoints, n using 1:2 with linespoints
set output

reset
set term png
set output 'P1-1819P-fig2.png'

set xrange[11:311]

set grid
set key top left

m="./P1-1819P-res2.dat"

set xlabel 'N'
set ylabel 'S_N^8'

plot m using 1:2 with linespoints
set output

