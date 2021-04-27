set xlabel "x"
set ylabel "y"
m="./P1-1819P-res2.dat"
set terminal x11 0
set nokey
set grid
set title 'P1-1819P-fig2'
set term png
set output 'P1-1819P-fig2.png'
plot m using 1:2 with linespoints
reset
set xlabel "x"
set ylabel "y"
m="./P1-1819P-res1.dat"
set terminal x11 0
set nokey
set grid
set title 'P1-1819P-fig1'
set term png
set output 'P1-1819P-fig1.png'
plot m using 1:2 with linespoints
