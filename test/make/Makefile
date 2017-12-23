# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2017 Nandor Licker. All rights reserved.

CC = gcc

.PHONY: all clean

all : main

a.o : a.c a.h
	$(CC) -c -o a.o a.c

b.o : b.c b.h
	$(CC) -c -o b.o b.c

c.o : c.c c.h
	$(CC) -c -o c.o c.c

main.o : main.c
	$(CC) -c -o main.o main.c

main : main.o a.o b.o c.o
	$(CC) -o main main.o a.o b.o c.o

clean:
	rm -f a.o b.o c.o main.o main
