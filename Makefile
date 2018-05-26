all: mandel.ssd

mandel.ssd: src/mandel.asm src/!boot table Makefile
	beebasm -i src/mandel.asm -do mandel.ssd -opt 3
