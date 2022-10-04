all:
	python3 tiger.py > test.s
	nasm -f elf64 test.s
	gcc -c runtime.c
	gcc -o test test.o runtime.o
	./test