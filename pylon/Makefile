# Makefile for Basler Pylon sample program
.PHONY			: all clean

# The program to build
NAME			:= Grab_MultipleCameras

# Installation directories for GenICam and Pylon
PYLON_ROOT		?= /home/martin/pylon-3.2.1-x86_64/pylon3
GENICAM_ROOT	?= $(PYLON_ROOT)/genicam

# Build tools and flags
CXX				?= g++
LD				:= $(CXX)
CPPFLAGS		:= -I$(GENICAM_ROOT)/library/CPP/include \
				   -I$(PYLON_ROOT)/include -DUSE_GIGE
CXXFLAGS		:= #e.g., CXXFLAGS=-g -O0 for debugging
LDFLAGS			:= -L$(PYLON_ROOT)/lib64 \
				   -L$(GENICAM_ROOT)/bin/Linux64_x64 \
				   -L$(GENICAM_ROOT)/bin/Linux64_x64/GenApi/Generic \
				   -Wl,-E
LIBS			:= -lpylonbase -lGenApi_gcc40_v2_3 -lGCBase_gcc40_v2_3 -lLog_gcc40_v2_3 -lMathParser_gcc40_v2_3 -lXerces-C_gcc40_v2_7 -llog4cpp_gcc40_v2_3

# Rules for building
all				: $(NAME) libpylon_wrapper.so test_pylon_wrapper

$(NAME)			: $(NAME).o
	$(LD) $(LDFLAGS) -o $@ $^ $(LIBS)

$(NAME).o: $(NAME).cpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<

libpylon_wrapper.so: pylon_wrapper.cpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(LDFLAGS) $(LIBS) -fPIC -o libpylon_wrapper.so -shared pylon_wrapper.cpp 

test_pylon_wrapper: test_pylon_wrapper.o libpylon_wrapper.so
	$(LD) -L. -lpylon_wrapper -o $@ $^ 

clean			:
	$(RM) $(NAME).o $(NAME)
