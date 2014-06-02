set term postsc
set output "calibv.ps"
f(x) = a*x+b
g(x) = a2*x+b2
fit f(x) "calibav.dat" u 1:2 via a,b
fit g(x) "calibbv.dat" u 1:2 via a2,b2
set xlabel "DAC setting [ADU]"
set ylabel "Voltage after differential amplifier [V]"
plot "calibav.dat" u 1:2, "calibbv.dat" u 1:2, f(x), g(x)
h(x) = s1*x+o1
i(x) = s2*x+o2
fit h(x) "caliba.dat" u 1:2 via s1,o1
fit i(x) "calibb.dat" u 1:2 via s2,o2

set xlabel "DAC setting [ADU]"
set ylabel "ADC value [ADU]"
plot "caliba.dat" u 1:2, "calibb.dat" u 1:2, h(x), i(x)