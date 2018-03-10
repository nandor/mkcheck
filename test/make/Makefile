# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2017 Nandor Licker. All rights reserved.

CC = gcc
OUT = out

.PHONY: dirs clean

all : dirs $(OUT)/main

dirs : $(OUT)

$(OUT) :
	mkdir -p $(OUT)

$(OUT)/lib_a.o : lib_a/lib_a.c lib_a/lib_a.h
	$(CC) -c -o $(OUT)/lib_a.o lib_a/lib_a.c

$(OUT)/lib_b.o : lib_b/lib_b.c lib_b/lib_b.h
	$(CC) -c -o $(OUT)/lib_b.o lib_b/lib_b.c

$(OUT)/a.o : a.c a.h
	$(CC) -c -o $(OUT)/a.o a.c

$(OUT)/b.o : b.c b.h
	$(CC) -c -o $(OUT)/b.o b.c

$(OUT)/c.o : c.c c.h
	$(CC) -c -o $(OUT)/c.o c.c

$(OUT)/main.o : main.c
	$(CC) -c -o $(OUT)/main.o main.c

$(OUT)/main : $(OUT)/main.o $(OUT)/a.o $(OUT)/b.o $(OUT)/c.o $(OUT)/lib_a.o $(OUT)/lib_b.o
	$(CC) -o $(OUT)/main $(OUT)/main.o $(OUT)/a.o $(OUT)/b.o $(OUT)/c.o $(OUT)/lib_a.o $(OUT)/lib_b.o

clean:
	rm -rf $(OUT)
