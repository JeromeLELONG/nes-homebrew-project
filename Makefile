



build:
	ca65 src/main.asm -I ../ -g -o gamename.o
	ld65 -o gamename.nes -C src/system/nes.cfg gamename.o -m src/gamename.map.txt --dbgfile ../gamename.dbg