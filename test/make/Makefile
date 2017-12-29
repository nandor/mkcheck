# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2017 Nandor Licker. All rights reserved.

CC = clang
OUT = out

.PHONY: all clean

all : main

$(OUT) :
	mkdir -p $(OUT)

$(OUT)/lib_a.o : $(OUT) lib_a/lib_a.c lib_a/lib_a.h
	$(CC) -c -o $(OUT)/lib_a.o lib_a/lib_a.c

$(OUT)/lib_b.o : $(OUT) lib_b/lib_b.c lib_b/lib_b.h
	$(CC) -c -o $(OUT)/lib_b.o lib_b/lib_b.c

$(OUT)/a.o : $(OUT) a.c a.h $(OUT)/lib_a.o
	$(CC) -c -o $(OUT)/a.o a.c

$(OUT)/b.o : $(OUT) b.c b.h $(OUT)/lib_b.o
	$(CC) -c -o $(OUT)/b.o b.c

$(OUT)/c.o : $(OUT) c.c c.h
	$(CC) -c -o $(OUT)/c.o c.c

$(OUT)/main.o : $(OUT) main.c
	$(CC) -c -o $(OUT)/main.o main.c

main : $(OUT) $(OUT)/main.o $(OUT)/a.o $(OUT)/b.o $(OUT)/c.o $(OUT)/lib_a.o $(OUT)/lib_b.o
	$(CC) -o $(OUT)/main $(OUT)/main.o $(OUT)/a.o $(OUT)/b.o $(OUT)/c.o $(OUT)/lib_a.o $(OUT)/lib_b.o

clean:
	rm -rf $(OUT)
