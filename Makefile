all:
	python3 tiger.py > test.s
	nasm -f elf64 test.s
	gcc -c runtime.c
	ld -dynamic-linker /usr/lib64/ld-linux-x86-64.so.2 -o test /usr/lib/x86_64-linux-gnu/Scrt1.o /usr/lib/x86_64-linux-gnu/crti.o -L/usr/bin/../lib64/gcc/x86_64-linux-gnu/9 -L/usr/bin/../lib64/gcc/x86_64-linux-gnu/9/../../.. ./test.o ./runtime.o -lpthread -ldl --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/lib/x86_64-linux-gnu/crtn.o
	./test