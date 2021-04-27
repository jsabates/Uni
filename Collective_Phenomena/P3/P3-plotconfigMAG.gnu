file1="SIM-L-032-TEMP-1500-MCTOT-10K.out"
file2="SIM-L-032-TEMP-1800-MCTOT-10K.out"
file3="SIM-L-032-TEMP-2500-MCTOT-10K.out"
file4="SIM-L-032-TEMP-3500-MCTOT-10K.out"
file5="SIM-L-032-TEMP-4500-MCTOT-10K.out"



set xrange [0:10000]
set yrange [-1:1]
set xlabel "MCS"
set ylabel "Magnetization/N"
plot file1 using 1:4 with lines t "TEMP=1.5",file2 using 1:4 with lines t "TEMP=1.8",file3 using 1:4 with lines t "TEMP=2.5",file4 using 1:4 with lines t "TEMP=3.5",file5 using 1:4 with lines t "TEMP=4.5"

pause -1
set terminal jpeg
set output "P3-Magnetitzacio L32.jpeg"
plot file1 using 1:4 with lines t "TEMP=1.5",file2 using 1:4 with lines t "TEMP=1.8",file3 using 1:4 with lines t "TEMP=2.5",file4 using 1:4 with lines t "TEMP=3.5",file5 using 1:4 with lines t "TEMP=4.5"
