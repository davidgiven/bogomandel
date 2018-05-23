all: mandel.ssd

mandel.ssd: mandel table mkdfs Makefile
	./mkdfs -O mandel.ssd -S 400 -B 3 \
		-f !boot \
		-f table \
		-f mandel

mandel: mandel.asm Makefile
	xa mandel.asm -o mandel -bt '3584' -bz '112'

mkdfs: mkdfs.c
	cc -g -o mkdfs mkdfs.c

mktable: mktable.c
	cc -g -o mktable mktable.c

table: mktable
	./mktable > table
