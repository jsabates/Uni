
set term png
set output 'P7-1819P-fig1.png'


set grid
set key top right
set title "Velocitat Angular vs temps" font "Helvetica,15"


m="./apartata.dat"

set xlabel 'temps (s)'
set ylabel 'Velocitat Angular (rad/s)'



plot m using 1:2 with lines title 'euler sencill', m using 1:5 with lines title 'euler millorat'
set output

reset

set term png
set output 'P7-1819P-fig3.png'


set grid
set key top right
set title "Comparacio trajectories" font "Helvetica,15"


m="./apartata.dat"

set xlabel 'angle'
set ylabel 'derivada angle'



plot m using 2:3 with lines title 'euler sencill trajectoria', m using 5:6 with lines title 'euler millorat trajectoria'
set output

reset

set term png
set output 'P7-1819P-fig2.png'


set grid
set key top right
set title "Apartat b petites oscilacions derivada vs temps" font "Helvetica,15"


m="./apartatb.dat"

set xlabel 'temps (s)'
set ylabel 'derivada angle (rad/s)'



plot m using 1:3 with lines title 'euler sencill ', m using 1:5 with lines title 'euler millorat '
set output

reset

set term png
set output 'P7-1819P-fig4.png'


set grid
set key top right
set title "Apartat c cas 1" font "Helvetica,15"


m="./apartatc1.dat"

set xlabel 'temps (s)'
set ylabel 'ENERGIA'



plot m using 1:3 with lines title 'Energia potencial euler sencill cas 1', m using 1:4 with lines title 'Energia TOTAL euler sencill cas 1', m using 1:6 with lines title 'Energia potencial euler millorat cas 1', m using 1:7 with lines title 'Energia TOTAL euler millorat cas 1'
set output

reset

set term png
set output 'P7-1819P-fig5.png'


set grid
set key top right
set title "Apartat c cas 2" font "Helvetica,15"


m="./apartatc2.dat"

set xlabel 'temps (s)'
set ylabel 'ENERGIA'



plot m using 1:3 with lines title 'Energia potencial euler sencill cas 2', m using 1:4 with lines title 'Energia TOTAL euler sencill cas 2', m using 1:6 with lines title 'Energia potencial euler millorat cas 2', m using 1:7 with lines title 'Energia TOTAL euler millorat cas 2'
set output

reset

set term png
set output 'P7-1819P-fig6.png'


set grid
set key top right
set title "front fasic apartat d" font "Helvetica,15"


m="./apartatd1.dat"
n="./apartatd2.dat"

set xlabel 'angle'
set ylabel 'derivada angle (rad/s)'




plot m using 2:3 with lines title '+0.05 ', n using 2:3 with lines title '-0.05'
set output

reset

set term png
set output 'P7-1819P-fig10.png'


set grid
set key top right
set title "energies" font "Helvetica,15"


m="./apartate600.dat"
n="./apartate1300.dat"
o="./apartate2600.dat"
p="./apartate15000.dat"

set xlabel 'angle'
set ylabel 'derivada angle (rad/s)'




plot m using 1:2 with lines title '600 ', n using 1:2 with lines title '1300', o using 1:2 with lines title '2600', p using 1:2 with lines title '15000',
set output

