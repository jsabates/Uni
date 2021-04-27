file1="SIM-L-032-TEMP-1800-MCTOT-10K-SEED-234567.out"
file2="SIM-L-032-TEMP-1800-MCTOT-10K-SEED-345672.out"
file3="SIM-L-032-TEMP-1800-MCTOT-10K-SEED-456723.out"
file4="SIM-L-032-TEMP-1800-MCTOT-10K-SEED-567234.out"
file5="SIM-L-032-TEMP-1800-MCTOT-10K-SEED-672345.out"



set xrange [0:10000]
set yrange [-1:1]
set xlabel "MCS"
set ylabel "Energy/N"
plot file1 using 1:4 with lines t "SEED 234567",file2 using 1:4 with lines t "SEED 345672",file3 using 1:4 with lines t "SEED 456723",file4 using 1:4 with lines t "SEED 567234",file5 using 1:4 with lines t "SEED 672345"

pause -1
set terminal jpeg
set output "P3-Magnetitzacio VARIALLAVOR L32 T=1_8.jpeg"
plot file1 using 1:4 with lines t "SEED 234567",file2 using 1:4 with lines t "SEED 345672",file3 using 1:4 with lines t "SEED 456723",file4 using 1:4 with lines t "SEED 567234",file5 using 1:4 with lines t "SEED 672345"
