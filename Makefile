all: mandel.ssd

mandel.ssd: mandel table mkdfs !boot !run Makefile
	./mkdfs -O mandel.ssd -S 400 -B 3 \
		-f !boot \
		-f !run \
		-f table \
		-f mandel \
		-f ThinSet

mandel: mandel.bas Makefile
	((cat mandel.bas | sed -e 's/^ *//' -e 's/\\.*//' | nl -b a -n rn -w 4 -s "" | tr '\n' '\r') && \
		dd if=/dev/zero bs=1 count=1) > mandel

mkdfs: mkdfs.c
	cc -g -o mkdfs mkdfs.c

mktable: mktable.c
	cc -g -o mktable mktable.c

table: mktable
	./mktable > table

