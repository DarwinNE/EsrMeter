all: compile program

compile:
	gpasm -p16F690 esr.asm

program:
	cd ../MacOSX/PK2CMDv1-20MacOSX;./pk2cmd -PPIC16f690 -F../../EsrMeter/esr.hex -M
