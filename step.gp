set term posts
set outp "step.ps"
set xlabel "time [us]"
set ylabel "adc"
plot "step.dat" u 1:2 w l

