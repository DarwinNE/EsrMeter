all: compile program

compile:
	gpasm -p16F883 esr.asm

program:
	cd ../MacOSX/PK2CMDv1-20MacOSX;./pk2cmd -PPIC16f883 -F../../EsrMeter/esr.hex  -M -R
