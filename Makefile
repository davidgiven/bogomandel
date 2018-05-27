all: mandel.ssd

mandel.ssd: src/mandel.asm src/vdu.inc src/!boot src/loader.bas src/shell.bas data/ThinSet Makefile
	beebasm -i src/mandel.asm -do mandel.ssd -opt 3 -w
