set term png

set output 'P8-1819-fig1.png'

set key top left

set title "phi(xo)=0 Å**-(1/2) ; phi'(xo)=2x10^-4 Å**-(3/2)"

set xrange [-7:1]

m='P8_1819_res1.dat'

set xlabel 'x(Å)'
set ylabel 'phi(x) (Å**-(1/2))'

plot m u 1:2 w l t 'E1=-24 eV' , m u 1:3 w l t 'E2=-24.5 eV' , m u 1:4 w l t 'E3=-19 eV' , m u 1:5 w l t 'E4=-19.5 eV'

reset

set term png

set output 'P8-1819-fig2.png'

set title 'Convergència del mètode'
m='convergencia.dat'

set xlabel 'Nombre de passos'
set ylabel 'Energia (eV)'

plot m i 0 u 1:2 w l t 'E1 i E2' , m i 1 u 1:2 w l t 'E3 i E4' , m i 2 u 1:2 w l t 'E5 i E6'


reset

set term png

set output 'P8-1819-fig3.png'

set key top left

set title "autovectors"


m='P8-1819-res2.dat'

set xlabel 'x(Å)'
set ylabel 'phi(x) (Å**-(1/2))'

plot m u 1:2 w l t 'phi1' , m u 1:3 w l t 'phi2' , m u 1:4 w l t 'phi3' ,

