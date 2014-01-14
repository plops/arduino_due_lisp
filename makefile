CXX=/home/martin/arduino-nightly/hardware/tools/g++_arm_none_eabi/bin/arm-none-eabi-g++
CC=/home/martin/arduino-nightly/hardware/tools/g++_arm_none_eabi/bin/arm-none-eabi-gcc
# OPTFLAGS=-g -Os
OPTFLAGS=-Os -Wall -Wextra
CXXFLAGS=-w -fno-rtti -fno-exceptions -ffunction-sections -fdata-sections -nostdlib --param max-inline-insns-single=500  -Dprintf=iprintf -mcpu=cortex-m3 -DF_CPU=84000000L -DARDUINO=155 -DARDUINO_SAM_DUE -DARDUINO_ARCH_SAM -D__SAM3X8E__ -mthumb -DUSB_VID=0x2341 -DUSB_PID=0x003e -DUSBCON -DUSB_MANUFACTURER="Unknown" -DUSB_PRODUCT=\""Arduino Due\"" -I/home/martin/arduino-nightly/hardware/arduino/sam/system/libsam -I/home/martin/arduino-nightly/hardware/arduino/sam/system/CMSIS/CMSIS/Include/ -I/home/martin/arduino-nightly/hardware/arduino/sam/system/CMSIS/Device/ATMEL/ -I/home/martin/arduino-nightly/hardware/arduino/sam/cores/arduino -I/home/martin/arduino-nightly/hardware/arduino/sam/variants/arduino_due_x
CFLAGS=-w -ffunction-sections -fdata-sections -nostdlib --param max-inline-insns-single=500  -Dprintf=iprintf -mcpu=cortex-m3 -DF_CPU=84000000L -DARDUINO=155 -DARDUINO_SAM_DUE -DARDUINO_ARCH_SAM -D__SAM3X8E__ -mthumb -DUSB_VID=0x2341 -DUSB_PID=0x003e -DUSBCON -DUSB_MANUFACTURER="Unknown" -DUSB_PRODUCT="\"Arduino Due\"" -I/home/martin/arduino-nightly/hardware/arduino/sam/system/libsam -I/home/martin/arduino-nightly/hardware/arduino/sam/system/CMSIS/CMSIS/Include/ -I/home/martin/arduino-nightly/hardware/arduino/sam/system/CMSIS/Device/ATMEL/ -I/home/martin/arduino-nightly/hardware/arduino/sam/cores/arduino -I/home/martin/arduino-nightly/hardware/arduino/sam/variants/arduino_due_x

AR=/home/martin/arduino-nightly/hardware/tools/g++_arm_none_eabi/bin/arm-none-eabi-ar

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
variants/arduino_due_x/variant.cpp 



OBJS = $(patsubst %,build/%.o,$(DEPS))

all: build/arduino_due_lisp.cpp.bin

.PHONY: clean

clean:
	rm -rf build/*

build/arduino_due_lisp.cpp: arduino_due_lisp.ino
	cp arduino_due_lisp.ino build/arduino_due_lisp.cpp

build/arduino_due_lisp.cpp.o: build/arduino_due_lisp.cpp
	$(CXX) $(OPTFLAGS) $(CXXFLAGS) -c -o $@ $<

build/%.cpp.o: /home/martin/arduino-nightly/hardware/arduino/sam/%.cpp
	mkdir -p `dirname $@`
	$(CXX) $(OPTFLAGS) $(CXXFLAGS) -c -o $@ $<

build/%.c.o: /home/martin/arduino-nightly/hardware/arduino/sam/%.c
	mkdir -p `dirname $@`
	$(CC) $(OPTFLAGS) $(CFLAGS) -c -o $@ $<

build/core.a: $(OBJS)
	$(AR) rcs build/core.a $^

build/arduino_due_lisp.cpp.elf: build/core.a  build/arduino_due_lisp.cpp.o
	/home/martin/arduino-nightly/hardware/tools/g++_arm_none_eabi/bin/arm-none-eabi-g++ $(OPTFLAGS) -Wl,--gc-sections -mcpu=cortex-m3 -T/home/martin/arduino-nightly/hardware/arduino/sam/variants/arduino_due_x/linker_scripts/gcc/flash.ld -Wl,-Map,build/arduino_due_lisp.cpp.map -o build/arduino_due_lisp.cpp.elf -Lbuild -lm -lgcc -mthumb -Wl,--cref -Wl,--check-sections -Wl,--gc-sections -Wl,--entry=Reset_Handler -Wl,--unresolved-symbols=report-all -Wl,--warn-common -Wl,--warn-section-align -Wl,--warn-unresolved-symbols -Wl,--start-group build/cores/arduino/syscalls_sam3.c.o build/arduino_due_lisp.cpp.o /home/martin/arduino-nightly/hardware/arduino/sam/variants/arduino_due_x/libsam_sam3x8e_gcc_rel.a build/core.a -Wl,--end-group 

build/arduino_due_lisp.cpp.bin: build/arduino_due_lisp.cpp.elf
	/home/martin/arduino-nightly/hardware/tools/g++_arm_none_eabi/bin/arm-none-eabi-objcopy -O binary build/arduino_due_lisp.cpp.elf build/arduino_due_lisp.cpp.bin 

upload: build/arduino_due_lisp.cpp.bin
	stty -F /dev/ttyACM0 speed 1200
	/home/martin/arduino-nightly/hardware/tools/bossac --port=ttyACM0 -U false -e -w -b build/arduino_due_lisp.cpp.bin -R

verify: build/arduino_due_lisp.cpp.bin
	stty -F /dev/ttyACM0 speed 1200
	/home/martin/arduino-nightly/hardware/tools/bossac --port=ttyACM0 -U false -i -d -v -e -w -b build/arduino_due_lisp.cpp.bin -R

# -i -d -v
