PYLON_ROOT	?= /home/martin/pylon-4.0.0.62-x86_64/pylon4
GENICAM_ROOT	?= $(PYLON_ROOT)/genicam


CFLAGS=-ggdb -Wall -Wextra -O0 -march=native \
-pedantic  -fPIC -D_BSD_SOURCE -std=c99
CXXFLAGS= -ggdb -fuse-cxa-atexit -O2 -march=native -fPIC \
-I$(GENICAM_ROOT)/library/CPP/include \
-I$(PYLON_ROOT)/include -DUSE_GIGE  -Dcimg_use_fftw3 

LDLIBS_MAIN=-ldl
LDFLAGS=-L$(PYLON_ROOT)/lib64 \
-L$(GENICAM_ROOT)/bin/Linux64_x64 \
-L$(GENICAM_ROOT)/bin/Linux64_x64/GenApi/Generic \
-Wl,-E
LDLIBS=-lvncserver \
-lpylonbase -lGenApi_gcc40_v2_3 -lGCBase_gcc40_v2_3 \
-lLog_gcc40_v2_3 -lMathParser_gcc40_v2_3 \
-lXerces-C_gcc40_v2_7_1 -llog4cpp_gcc40_v2_3

all: main librun-help.so

main: main.c api.h
	$(CC) $(CFLAGS) -o $@ $< $(LDLIBS_MAIN)

%.h.gch: %.h
	$(CXX) $(CXXFLAGS) -shared  $(LDFLAGS)  $< $(LDLIBS) -lX11 -lfftw3_threads -lfftw3 -lfftw3f -lfftw3f_threads

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<


librun-help.so: api.h myinc.h.gch run.o run_step.o
	$(CXX) $(CXXFLAGS) -shared  $(LDFLAGS) -o o.so run.o run_step.o $(LDLIBS) -lX11 -lfftw3_threads -lfftw3 -lfftw3f -lfftw3f_threads
	if [ -a librun2.so ]; then \
	rm librun2.so; \
	mv o.so librun1.so; \
	killall -s SIGUSR1 main; \
	else \
	touch librun1.so; \
	rm librun1.so; \
	mv o.so librun2.so; \
	killall -s SIGUSR2 main; \
	fi;
	touch librun-help.so



#	cp o.so librun.so
#	cp o.so librun2.so
#	killall -s SIGUSR1 main
#	killall -s SIGUSR2 main


clean:
	rm main run.o run_step.o librun1.so myinc.h.gch librun2.so librun-help.so

test : main librun.so
	./$<
