This is code I use to control my holographic setup to image through
multimode fibers.

![Photography of the holographic multimode fiber imaging system.](/docs/multi-mode-imaging.jpg?raw=true "Multimode imaging system")

A significant part is a wrapper for the Aravis library to read out
multiple GigE Vision cameras.  The file dependencies are described in
arv.asd. I chose to use Clozure Common Lisp for this library because
it comes with a foreign function generator that can parse C header
files. To simplify development I added syntax completion for foreign
functions to slime.

Realtime control is done with an Arduino Due. To ease development (in
particular I don't like the long waiting time when uploading new code
to the Arduino) I ported femtolisp to this Arduino. This is sufficient
to generate the two DAC channels for my XY-scanning mirror and a bunch
of digital trigger signals for the cameras and a shutter.  See
arduino-femtolisp/. From Clozure Common Lisp I talk to the Arduino
using the code in arduino-serial-ccl/.

My main experiment is controlled from test.lisp. I also use the
binding for FFTW https://github.com/plops/cl-cffi-fftw3 to do Fourier
transforms.


Copyright (c) 2014 Martin Kielhorn kielhorn.martin@gmail.com
Licensing: GPL v2
