MODE 135
HIMEM=&2000
PRINT "Loading..."
*exec
*fx 21,0
*fx 4,2
*key 12 |!|A
*key 13 |!|B
*key 14 |!|C
*key 15 |!|D
*load squares 3000
*srwrite 3000+4000 8000 4
*setscrn
*load mandel 2000
CHAIN "shell"
