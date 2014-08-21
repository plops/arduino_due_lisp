ARDUINO_ROOT=/home/martin/arduino-1.5.7
C_TOOLCHAIN=$(ARDUINO_ROOT)/hardware/tools/gcc-arm-none-eabi-4.8.3-2014q1/bin
CXX=$(C_TOOLCHAIN)/arm-none-eabi-g++
CC=$(C_TOOLCHAIN)/arm-none-eabi-gcc
AR=$(C_TOOLCHAIN)/arm-none-eabi-ar
OBJCOPY=$(C_TOOLCHAIN)/arm-none-eabi-objcopy

# OPTFLAGS=-g -Os
OPTFLAGS=-O3 -Wall -Wextra
INCLUDES=-I. -I$(ARDUINO_ROOT)/hardware/arduino/sam/system/libsam -I$(ARDUINO_ROOT)/hardware/arduino/sam/system/CMSIS/CMSIS/Include/ -I$(ARDUINO_ROOT)/hardware/arduino/sam/system/CMSIS/Device/ATMEL/ -I$(ARDUINO_ROOT)/hardware/arduino/sam/cores/arduino -I$(ARDUINO_ROOT)/hardware/arduino/sam/variants/arduino_due_x -I$(ARDUINO_ROOT)/hardware/arduino/sam/libraries 

CXXFLAGS=-w -fno-rtti -fno-exceptions -ffunction-sections -fdata-sections -nostdlib --param max-inline-insns-single=500  -Dprintf=iprintf -mcpu=cortex-m3 -DF_CPU=84000000L -DARDUINO=155 -DARDUINO_SAM_DUE -DARDUINO_ARCH_SAM -D__SAM3X8E__ -mthumb -DUSB_VID=0x2341 -DUSB_PID=0x003e -DUSBCON -DUSB_MANUFACTURER="Unknown" -DUSB_PRODUCT=\""Arduino Due\"" 
CFLAGS=-w -ffunction-sections -fdata-sections -nostdlib --param max-inline-insns-single=500  -Dprintf=iprintf -mcpu=cortex-m3 -DF_CPU=84000000L -DARDUINO=155 -DARDUINO_SAM_DUE -DARDUINO_ARCH_SAM -D__SAM3X8E__ -mthumb -DUSB_VID=0x2341 -DUSB_PID=0x003e -DUSBCON -DUSB_MANUFACTURER="Unknown" -DUSB_PRODUCT="\"Arduino Due\"" 

DEPS = cores/arduino/syscalls_sam3.c \
cores/arduino/wiring_shift.c \
cores/arduino/wiring.c \
cores/arduino/itoa.c \
cores/arduino/WInterrupts.c \
cores/arduino/wiring_digital.c \
cores/arduino/cortex_handlers.c \
cores/arduino/iar_calls_sam3.c \
cores/arduino/hooks.c \
cores/arduino/wiring_analog.c \
cores/arduino/avr/dtostrf.c \
cores/arduino/Reset.cpp \
cores/arduino/Print.cpp \
cores/arduino/USB/USBCore.cpp \
cores/arduino/USB/CDC.cpp \
cores/arduino/USB/HID.cpp \
cores/arduino/UARTClass.cpp \
cores/arduino/WMath.cpp \
cores/arduino/main.cpp \
cores/arduino/Stream.cpp \
cores/arduino/wiring_pulse.cpp \
cores/arduino/WString.cpp \
cores/arduino/IPAddress.cpp \
cores/arduino/USARTClass.cpp \
cores/arduino/cxxabi-compat.cpp \
cores/arduino/RingBuffer.cpp \
variants/arduino_due_x/variant.cpp \
libraries/SPI/SPI.cpp \
libraries/Wire/Wire.cpp 
#libraries/ArduCAM/ArduCAM.cpp 


OBJS = $(patsubst %,build/%.o,$(DEPS))

all: build/arduino_due_lisp.cpp.bin #arv-example inotify_show vnc_stream

.PHONY: clean

clean:
	rm -rf build/*

build/arduino_due_lisp.cpp: arduino_due_lisp.ino
	cp arduino_due_lisp.ino build/arduino_due_lisp.cpp

build/arduino_due_lisp.cpp.o: build/arduino_due_lisp.cpp
	$(CXX) $(OPTFLAGS) $(INCLUDES) $(CXXFLAGS) -c -o $@ $<

# make sure all libraries are in this folder:
# mv ~/arduino-nightly/libraries/ArduCAM/ ~/arduino-nightly/hardware/arduino/sam/libraries/
build/%.cpp.o: $(ARDUINO_ROOT)/hardware/arduino/sam/%.cpp
	mkdir -p `dirname $@`
	$(CXX) $(OPTFLAGS) $(INCLUDES) $(CXXFLAGS) -c -o $@ $<

build/%.c.o: $(ARDUINO_ROOT)/hardware/arduino/sam/%.c
	mkdir -p `dirname $@`
	$(CC) $(OPTFLAGS) $(INCLUDES) $(CFLAGS) -c -o $@ $<

build/core.a: $(OBJS)
	$(AR) rcs build/core.a $^

build/arduino_due_lisp.cpp.elf: build/core.a  build/arduino_due_lisp.cpp.o
	$(CXX) $(OPTFLAGS) -Wl,--gc-sections -mcpu=cortex-m3 -T$(ARDUINO_ROOT)/hardware/arduino/sam/variants/arduino_due_x/linker_scripts/gcc/flash.ld -Wl,-Map,build/arduino_due_lisp.cpp.map -o build/arduino_due_lisp.cpp.elf -Lbuild -lm -lgcc -mthumb -Wl,--cref -Wl,--check-sections -Wl,--gc-sections -Wl,--entry=Reset_Handler -Wl,--unresolved-symbols=report-all -Wl,--warn-common -Wl,--warn-section-align -Wl,--warn-unresolved-symbols -Wl,--start-group build/cores/arduino/syscalls_sam3.c.o build/arduino_due_lisp.cpp.o $(ARDUINO_ROOT)/hardware/arduino/sam/variants/arduino_due_x/libsam_sam3x8e_gcc_rel.a build/core.a -Wl,--end-group 

build/arduino_due_lisp.cpp.bin: build/arduino_due_lisp.cpp.elf
	$(OBJCOPY) -O binary build/arduino_due_lisp.cpp.elf build/arduino_due_lisp.cpp.bin 

upload: build/arduino_due_lisp.cpp.bin
	stty -F /dev/ttyACM? speed 1200
	$(ARDUINO_ROOT)/hardware/tools/bossac --port=`ls /dev/ttyACM?|xargs basename` -U false -e -w -b build/arduino_due_lisp.cpp.bin -R

verify: build/arduino_due_lisp.cpp.bin
	stty -F /dev/ttyACM? speed 1200
	$(ARDUINO_ROOT)/hardware/tools/bossac --port=`ls /dev/ttyACM?|xargs basename` -U false -i -d -v -e -w -b build/arduino_due_lisp.cpp.bin -R

# -i -d -v

arv-example: arvexample.c
	gcc -g -O2 -o arv-example arvexample.c -MD -MP -MF -pthread -I/usr/include/aravis-0.4 -I/usr/lib/glib-2.0/include  -I/usr/include/glib-2.0  -lm -L/usr/lib -lgio-2.0 -lgobject-2.0 -lxml2 -lgthread-2.0 -pthread -lrt -lglib-2.0 -lz  -laravis-0.4 -lglfw -lGL -lfftw3 -lfftw3_threads

inotify_show: inotify_show.c
	gcc -ggdb -O0 -o inotify_show inotify_show.c -lglfw -lGL -lfftw3 -lfftw3_threads -lm

vnc_stream: vnc_stream.c
	gcc -ggdb -O0 -o vnc_stream vnc_stream.c -lvncserver -lfftw3 -lm -lfftw3_threads


ft_extract: ft_extract.c
	gcc -ggdb -Wextra -Wall -O3 -o ft_extract ft_extract.c -I/usr/local/include -L/usr/local/lib -lfftw3 -lm -lfftw3_threads -lics -lpthread
