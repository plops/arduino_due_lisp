CFLAGS=-ggdb -Wall -Wextra -O2 -march=native -pedantic  -fPIC -D_BSD_SOURCE -std=c99
LDLIBS_MAIN=-ldl
LDLIBS=-lvncserver

all: main librun.so

main: main.c api.h
	$(CC) $(CFLAGS) -o $@ $< $(LDLIBS_MAIN)

librun.so: run.c api.h
	$(CC) $(CFLAGS) -shared  $(LDFLAGS) -o $@  $< $(LDLIBS)

clean:
	rm main librun.so

test : main libgame.so
	./$<
