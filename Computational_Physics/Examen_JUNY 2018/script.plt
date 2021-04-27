
set term png
set output 'JUNY18-fig1.png'


set grid
set key bot right
set title "Posicio espins +1 pel TEMPS=0" font "Helvetica,15"


m="./time0.dat"

set xlabel 'X'
set ylabel 'Y'



plot m i 0 u 1:2 with points pointtype 6 title ''
set output
reset

set term png
set output 'JUNY18-fig2.png'


set grid
set key bot right
set title "Posicio espins +1 pel TEMPS=1" font "Helvetica,15"


m="./time1.dat"

set xlabel 'X'
set ylabel 'Y'



plot m i 0 u 1:2 with points pointtype 6 title ''
set output
reset

set term png
set output 'JUNY18-fig3.png'


set grid
set key bot right
set title "Posicio espins +1 pel TEMPS=10" font "Helvetica,15"


m="./time10.dat"

set xlabel 'X'
set ylabel 'Y'



plot m i 0 u 1:2 with points pointtype 6 title ''
set output
reset

set term png
set output 'JUNY18-fig4.png'


set grid
set key bot right
set title "Posicio espins +1 pel TEMPS=100" font "Helvetica,15"


m="./time100.dat"

set xlabel 'X'
set ylabel 'Y'



plot m i 0 u 1:2 with points pointtype 6 title ''
set output
reset

set term png
set output 'JUNY18-fig5.png'


set grid
set key bot right
set title "magnetitzacio en funcio del temps" font "Helvetica,15"


m="./magnetitzacio.dat"

set xlabel 'TIME'
set ylabel 'Magnetitzacio'



plot m i 0 u 1:2 with lines title ''
set output
reset

set term png
set output 'JUNY18-fig1BBB.png'


set grid
set key bot right
set title "Posicio espins +1 pel TEMPS=0" font "Helvetica,15"


m="./time0B.dat"

set xlabel 'X'
set ylabel 'Y'



plot m i 0 u 1:2 with points pointtype 6 title ''
set output
reset

set term png
set output 'JUNY18-fig2BBB.png'


set grid
set key bot right
set title "Posicio espins +1 pel TEMPS=1" font "Helvetica,15"


m="./time1B.dat"

set xlabel 'X'
set ylabel 'Y'



plot m i 0 u 1:2 with points pointtype 6 title ''
set output
reset

set term png
set output 'JUNY18-fig3BBBB.png'


set grid
set key bot right
set title "Posicio espins +1 pel TEMPS=10" font "Helvetica,15"


m="./time10B.dat"

set xlabel 'X'
set ylabel 'Y'



plot m i 0 u 1:2 with points pointtype 6 title ''
set output
reset

set term png
set output 'JUNY18-fig4BBB.png'


set grid
set key bot right
set title "Posicio espins +1 pel TEMPS=100" font "Helvetica,15"


m="./time100B.dat"

set xlabel 'X'
set ylabel 'Y'



plot m i 0 u 1:2 with points pointtype 6 title ''
set output
reset

set term png
set output 'JUNY18-fig5BBB.png'


set grid
set key bot right
set title "magnetitzacio en funcio del temps" font "Helvetica,15"


m="./magnetitzacioB.dat"

set xlabel 'TIME'
set ylabel 'Magnetitzacio'



plot m i 0 u 1:2 with lines title ''
set output
reset
