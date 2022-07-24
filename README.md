# EsrMeter
An open hardware in-circuit ESR meter for electrolytic capacitors.

This circuit is based on a PIC16F883 that controls a simple lock-in circuit to measure in circuit the ESR and in some cases the capacitance of electrolytic capacitors.

The circuit has been designed by Davide Bucci, the PCB by Etenmenanki using Eagle. The size of the PCBs is compatible with the free version of the program.

A description (in italian language) of the circuit can be found here: https://www.electroyou.it/darwinne/wiki/misurare-esr-di-condensatori-con-circuito-lock-in

Two versions of the PCB exist. The first one has the digital and analog parts split in two. This approach has the advantage that one can test the analog board with something different from the PIC16F883 (i.e. a STM32 or an Arduino board, for instance). The version with the two boards separated requires some corrections, look at the associated files if you try to build them.
The second version has all digital and analog circuits on the same board and it is much more compact. I ordered the PCB on July 24, 2022, but I haven't yet put together the circuit to test it.

All components are SMD. In the "all in one" boards, two different versions of the PIC16F883 can be used to cope with the pandemic scarcity of components: SOIC and SSOP. I think the PIC16F886 can be used at the place of the PIC16F883 with minimal changes.