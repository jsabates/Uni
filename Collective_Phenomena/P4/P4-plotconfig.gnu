file1="MC-L-032-TEMP-20TO30.out"

set xrange [0:4]
set yrange [-2:0]
set xlabel "Reduced Temperature T"
set ylabel "Energy per particle e=E/N"
set title "Monte Carlo Ising 2D L=32" 
set terminal jpeg
set output "Energia mitjana per particula.jpeg"
plot file1 using 2:4:13 with yerrorbars t "<e>"

set xrange [0:4]
set yrange [0:1.2]
set xlabel "Reduced Temperature T"
set ylabel "Magnetization per particle m=M/N"
set title "Monte Carlo Ising 2D L=32" 
set terminal jpeg
set output "Imantacio per particula.jpeg"
plot file1 using 2:8:14 with yerrorbars t "<|m|>",file1 using 2:15 with lines t "sqrt(<m²>)"

set xrange [0:4]
set yrange [0:2]
set xlabel "Reduced Temperature T"
set ylabel "Heat Capacity Cv"
set title "Monte Carlo Ising 2D L=32" 
set terminal jpeg
set output "Capacitat Calorifica.jpeg"
plot file1 using 2:12 with lines t "Cᵥ",file1 using 2:16 with lines t "d<e>/dT"

set xrange [0:4]
set yrange [0:25]
set xlabel "Reduced Temperature T"
set ylabel "Susceptibility"
set title "Monte Carlo Ising 2D L=32" 
set terminal jpeg
set output "Susceptibilitat.jpeg"
plot file1 using 2:11 with lines t "X"
