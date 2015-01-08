PYLON_ROOT	?= /home/martin/pylon-4.0.0.62-x86_64/pylon4
GENICAM_ROOT	?= $(PYLON_ROOT)/genicam


CFLAGS=-ggdb -Wall -Wextra -O2 -march=native \
-pedantic  -fPIC -D_BSD_SOURCE -std=c99
CXXFLAGS=-ggdb -Wall -Wextra -O2 -march=native \
-pedantic  -fPIC \
-I$(GENICAM_ROOT)/library/CPP/include \
-I$(PYLON_ROOT)/include -DUSE_GIGE

LDLIBS_MAIN=-ldl
LDFLAGS=-L$(PYLON_ROOT)/lib64 \
-L$(GENICAM_ROOT)/bin/Linux64_x64 \
-L$(GENICAM_ROOT)/bin/Linux64_x64/GenApi/Generic \
-Wl,-E
LDLIBS=-lvncserver \
-lpylonbase -lGenApi_gcc40_v2_3 -lGCBase_gcc40_v2_3 \
-lLog_gcc40_v2_3 -lMathParser_gcc40_v2_3 \
-lXerces-C_gcc40_v2_7_1 -llog4cpp_gcc40_v2_3

all: main librun.so

main: main.c api.h
	$(CC) $(CFLAGS) -o $@ $< $(LDLIBS_MAIN)

librun.so: run.cpp api.h
	$(CXX) $(CXXFLAGS) -shared  $(LDFLAGS) -o $@  $< $(LDLIBS)

clean:
	rm main librun.so

test : main libgame.so
	./$<
