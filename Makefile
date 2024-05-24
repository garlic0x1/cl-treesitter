CC = gcc

build:
	$(CC) -ltree-sitter -shared -o shim.so shim.c
