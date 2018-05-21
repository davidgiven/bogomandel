all: mandel.ssd

mandel.ssd: mandel mkdfs Makefile
	./mkdfs -O mandel.ssd -S 400 -B 3 \
		-f !boot \
		-f mandel

mandel: mandel.asm Makefile
	xa mandel.asm -o mandel -bt '3584' -bz '112'

mkdfs: mkdfs.c
	cc -o mkdfs mkdfs.c


