file="P2-configuration.conf"
L=32
symbsize=1.2
set size square
set xrange [0.5:L+0.5]
set yrange [0.5:L+0.5]
plot file using 1:2 with points pt 10 ps symbsize t ""
pause -1
set terminal jpeg
set output "P2-configuration.jpeg"
plot file using 1:2 with points pt 5 ps symbsize t ""
