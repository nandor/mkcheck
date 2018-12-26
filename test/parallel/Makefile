# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2018 Nandor Licker. All rights reserved.

CC = gcc
OUT = out


.PHONY: dirs clean


all : dirs $(OUT)/main

dirs : $(OUT)

$(OUT) :
	mkdir -p $(OUT)


$(OUT)/a.h : a.in
	head -n 4 a.in > $(OUT)/a.h
	tail -n 7 a.in > $(OUT)/a.cpp

$(OUT)/a.o : $(OUT)/a.h $(OUT)/a.cpp
	$(CC) -c -o $(OUT)/a.o $(OUT)/a.cpp

$(OUT)/b.h : b.in
	head -n 4 b.in > $(OUT)/b.h
	tail -n 7 b.in > $(OUT)/b.cpp

$(OUT)/b.o : $(OUT)/b.h $(OUT)/b.cpp
	$(CC) -c -o $(OUT)/b.o $(OUT)/b.cpp

$(OUT)/c.h : c.in
	head -n 4 c.in > $(OUT)/c.h
	tail -n 11 c.in > $(OUT)/c.cpp

$(OUT)/c.o : $(OUT)/c.h $(OUT)/c.cpp
	$(CC) -c -o $(OUT)/c.o $(OUT)/c.cpp


$(OUT)/main.o : main.cpp $(OUT)/a.h $(OUT)/b.h $(OUT)/c.h
	$(CC) -c -o $(OUT)/main.o main.cpp

$(OUT)/main : $(OUT)/a.o $(OUT)/b.o $(OUT)/c.o $(OUT)/main.o
	$(CC) -o $(OUT)/main $^

clean :
	rm -rf $(OUT)

test :
	make clean
	../../build/mkcheck -o /tmp/graph -- make -j1
	@echo "Fuzzing parallel"
	python ../../tools/fuzz_test --tmp-path=/tmp/graph fuzz	
	@echo "Race testing parallel"
	python ../../tools/fuzz_test --tmp-path=/tmp/graph race
    
