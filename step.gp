set term posts
set outp "step.ps"
set xlabel "time [us]"
set ylabel "adc"
set grid
set title "horizontal scanning (parallel to table)"
plot "step.dat" u 1:2 w l

