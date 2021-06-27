; *****************************************************************************
;               esr.asm
;       A simple ESR measuring system exploiting lock-in techniques
;       Davide Bucci, 2021
;       Version 1.1
; *****************************************************************************
;
; License:
; --------
;
;    Copyright (C) 2021  Davide Bucci  davbucciPleaseNoSpamHerE@tiscali.it
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program; if not, write to the Free Software
;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

        ERRORLEVEL -302         ; disable the annoying warning
                                ; "Register in operand not in bank 0.
                                ; Ensure bank bits are correct."

#include <p16F883.inc>
   __config (_INTRC_OSC_NOCLKOUT & _WDT_OFF & _PWRTE_ON & _BOREN_ON & _MCLRE_ON & _CP_OFF & _IESO_OFF & _FCMEN_OFF & _DEBUG_OFF)

; Constant
OSC_LOTHRESHOLD equ     .35     ; Error if the ampl. A_VH is lower than this.
OSC_HITHRESHOLD equ     .230    ; Error if the ampl. A_VH is higher than this.

; Control lines of the display device
; Control port used for LCD display

DATALCD         equ     PORTB
TRISLCD         equ     TRISB

E               equ     2
RS              equ     0
RW              equ     1

DATA4           equ     7
DATA5           equ     6
DATA6           equ     5
DATA7           equ     4

PORTFSYNC       equ     PORTC
TRISFSYNC       equ     TRISC
FSYNC           equ     RC4


; These registers must be in the same page of the DATALCD register
TMP             equ     0x20      ; Dummy for LCD nibble mode
CNT             equ     0x21      ; Counter
SDR             equ     0x22      ; Short Delay Register
LDR             equ     0x23      ; Long Delay Register
TMP1            equ     0x24      ; Dummy for reversing bit order

TMP_1           equ     0x2A
HND             equ     0x2B
DEC             equ     0x2C
UNT             equ     0x2D


; Used to send 16 bit data via SPI, combined with the w register.
SENDL           equ     0x2E

; Used for the divide operator: the two operands, the result and the remainder
divid0          equ     0x30      ; Most significant byte
divid1          equ     0x31
divid2          equ     0x32
divid3          equ     0x33      ; Least significant byte

divisH          equ     0x34
divisL          equ     0x35

remdrH          equ     0x36
remdrL          equ     0x37

LOOPCOUNT       equ     0x3C
TESTMODE        equ     0x3D

; aHH:aHL:aLH:aLL*bH:bL -> a6:a5:aHH:aHL:aLH:aLL

aLL             equ     0x40
aLH             equ     0x41
aHL             equ     0x42
aHH             equ     0x43
a5              equ     0x44
a6              equ     0x45

bL              equ     0x46
bH              equ     0x47

c1              equ     0x48
c2              equ     0x49
c3              equ     0x4A
c4              equ     0x4B

bitcnt          equ     0x4C

bcd             equ     0x50
; memory used up to bcd+4 included
cnt             equ     0x55
ii              equ     0x56
bin             equ     0x59
; memory used up to store+3
store           equ     0x61

indf            equ     0
fsr             equ     4

NOZ             equ     0x60

; Configuration for the measurement control
CTRLP           equ     PORTC
TRISCTRL        equ     TRISC
PWMPIN          equ     RC2

; The value of the ESR is calculated as
; 10*(V_B-V_C)/(V_A-V_B)
CTRLA           equ     RC6
CTRLB           equ     RC0
CTRLC           equ     RC1

A_VH            equ     0x70
A_VL            equ     0x71

B_VH            equ     0x72
B_VL            equ     0x73

C_VH            equ     0x74
C_VL            equ     0x75

STOREH          equ     0x76
STOREL          equ     0x77

MSBH            equ     0x78
MSBL            equ     0x79
LSBH            equ     0x7A
LSBL            equ     0x7B

FREQ            equ     0x7C
DCVAL           equ     0x7D

BSENSE          equ     0x7E
CHVAL           equ     0x7F

; *****************************************************************************
;               MACROS
; *****************************************************************************

READV           MACRO       CTRL, STH,STL
                BANKSEL     CTRLP
                clrf        CTRLP
                bsf         CTRLP,CTRL
                bsf         ADCON0,ADON ; Activate the ADC.
                call        readadc
                movfw       STOREH
                movwf       STH
                movfw       STOREL
                movwf       STL
                ENDM

                ; Write a message on the LCD display
                ; This version is known not to work properly if the message
                ; crosses a page.
WRITELN         MACRO       msg
                local       loop_ch
                local       end_mes
                BANKSEL     CNT
                clrf        CNT
loop_ch

                movlw       HIGH msg
                movwf       PCLATH
                movlw       LOW msg
                addwf       CNT,w
                movlw       HIGH msg
                btfsc       STATUS,C
                addlw       1
                movwf       PCLATH
                movf        CNT,w
                call        msg
                xorlw       0x00
                btfsc       STATUS,Z        ; Test if the character is a zero
                goto        end_mes
                bsf         DATALCD,RS      ; RS to 1: we are sending a char
                bcf         DATALCD,RW      ; RW to 0: we are writing to display
                call        sendbyte
                incf        CNT, f
                goto        loop_ch
end_mes
                ENDM

                ; DEST = DEST + SOURCE;
                ; Apply the bank select macro to SOURCEL and SOURCEH
ADD16BIT        MACRO       DESTH, DESTL, SOURCEH, SOURCEL
                BANKSEL     SOURCEL
                movfw       SOURCEL
                addwf       DESTL,f
                BANKSEL     SOURCEH
                movfw       SOURCEH
                btfsc       STATUS,C
                incfsz      SOURCEH,w
                addwf       DESTH,f
                ENDM

                ; DEST = DEST - SOURCE;
SUB16BIT        MACRO       DESTH, DESTL, SOURCEH, SOURCEL
                movfw       SOURCEL
                subwf       DESTL,f
                movfw       SOURCEH
                btfss       STATUS,C
                incfsz      SOURCEH,w      ; Adjust borrow if C=1
                subwf       DESTH,f
                ENDM

                ; DEST = DEST + SOURCE;
                ; DEST3 is the LSB
ADD32BIT        MACRO       DEST0, DEST1, DEST2, DEST3, SOURCE0, SOURCE1, SOURCE2, SOURCE3
                movfw       SOURCE3
                addwf       DEST3,f
                movfw       SOURCE2
                btfsc       STATUS,C
                incfsz      SOURCE2,w
                addwf       DEST2,f
                movfw       SOURCE1
                btfsc       STATUS,C
                incfsz      SOURCE1,w
                addwf       DEST1,f
                movfw       SOURCE0
                btfsc       STATUS,C
                incfsz      SOURCE0,w
                addwf       DEST0,f
                ENDM

                ; Divide by two
                ; DEST /=2;
                ; DEST3 is the MSB
DIV2O32BIT      MACRO       DESTHH, DESTHL, DESTLH, DESTLL
                bcf         STATUS,C
                rrf         DESTHH,f
                rrf         DESTHL,f
                rrf         DESTLH,f
                rrf         DESTLL,f
                ENDM

                ; DEST = SOURCE
MOV16FF         MACRO       DESTH, DESTL, SOURCEH, SOURCEL
                movfw       SOURCEH
                movwf       DESTH
                movfw       SOURCEL
                movwf       DESTL
                ENDM

PROGFREQ        MACRO       LSB, MSB, message
                movlw       (LSB & 0xFF00)>>8
                movwf       LSBH
                movlw       (LSB & 0x00FF)
                movwf       LSBL
                movlw       (MSB & 0xFF00)>>8
                movwf       MSBH
                movlw       (MSB & 0x00FF)
                movwf       MSBL
                call        ConfigureAD9833
                WRITELN     message
                ENDM

; *****************************************************************************
;               Interrupt vectors
; *****************************************************************************
                org         0000
                goto        prg
                nop
                nop
                nop
                org         0004
                goto        prg

; *****************************************************************************
;               Text tables (page 0)
; *****************************************************************************
text_welcome    addwf   PCL,f
                DT      "Welcome ESR  1.0",0
text_davide     addwf   PCL,f
                DT      "D. Bucci 2021",0
text_lowosc     addwf   PCL,f
                DT      "Overload...",0
text_hiosc      addwf   PCL,f
                DT      "Oscillator high.",0
text_reshi      addwf   PCL,f
                DT      "Resistance high.",0
text_esr        addwf   PCL,f
                DT      "ESR = ",0
freq1           addwf   PCL,f
                DT      "Freq = 100 Hz",0
freq2           addwf   PCL,f
                DT      "Freq = 200 Hz",0
freq3           addwf   PCL,f
                DT      "Freq = 500 Hz",0
freq4           addwf   PCL,f
                DT      "Freq = 1 kHz",0
freq5           addwf   PCL,f
                DT      "Freq = 2 kHz",0
freq6           addwf   PCL,f
                DT      "Freq = 5 kHz",0
freq7           addwf   PCL,f
                DT      "Freq = 10 kHz",0
freq8           addwf   PCL,f
                DT      "Freq = 20 kHz",0
freq9           addwf   PCL,f
                DT      "Freq = 50 kHz",0
freq10          addwf   PCL,f
                DT      "Freq = 100 kHz",0  ; This fills more or less one page


                org     0x100
freq11          addwf   PCL,f
                DT      "Freq = 200 kHz",0


                ; Values of the PWM for a 5V power supply
dclist          addwf   PCL,f
                retlw   .0      ; not used
                retlw   .8     ; -2V
                retlw   .18     ; -1.5V
                retlw   .29     ; -1V
                retlw   .39     ; -0.5V
                retlw   .50     ; 0V
                retlw   .60     ; 0.5V
                retlw   .71     ; 1V
                retlw   .82     ; 1.5V
                retlw   .93     ; 2V

dcmenu          addwf   PCL,f
                DT      "Choose DC bias",0

dcval1          addwf   PCL,f
                DT      "DC = -2 V",0

dcval2          addwf   PCL,f
                DT      "DC = -1.5 V",0

dcval3          addwf   PCL,f
                DT      "DC = -1 V",0

dcval4          addwf   PCL,f
                DT      "DC = -0.5 V",0

dcval5          addwf   PCL,f
                DT      "DC = 0 V (off)",0

dcval6          addwf   PCL,f
                DT      "DC = 0.5 V",0

dcval7          addwf   PCL,f
                DT      "DC = 1 V",0

dcval8          addwf   PCL,f
                DT      "DC = 1.5 V",0

dcval9          addwf   PCL,f
                DT      "DC = 2 V",0

FREQUENCY = .100
VALUE = .10736*FREQUENCY/.1000
;VALUE = (2<<28)*FREQUENCY/25000000

MSB1 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB1 = (VALUE & 0x3FFF) | 0x4000

FREQUENCY = .200
VALUE = .10736*FREQUENCY/.1000
;VALUE = (2<<28)*FREQUENCY/25000000

MSB2 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB2 = (VALUE & 0x3FFF) | 0x4000

FREQUENCY = .500
VALUE = .10736*FREQUENCY/.1000
;VALUE = (2<<28)*FREQUENCY/25000000

MSB3 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB3 = (VALUE & 0x3FFF) | 0x4000

FREQUENCY = .1000
VALUE = .10736*FREQUENCY/.1000
;VALUE = (2<<28)*FREQUENCY/25000000

MSB4 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB4 = (VALUE & 0x3FFF) | 0x4000

FREQUENCY = .2000
VALUE = .10736*FREQUENCY/.1000
;VALUE = (2<<28)*FREQUENCY/25000000

MSB5 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB5 = (VALUE & 0x3FFF) | 0x4000

FREQUENCY = .5000
VALUE = .10736*FREQUENCY/.1000
;VALUE = (2<<28)*FREQUENCY/25000000

MSB6 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB6 = (VALUE & 0x3FFF) | 0x4000

FREQUENCY = .10000
VALUE = .10736*FREQUENCY/.1000
;VALUE = (2<<28)*FREQUENCY/25000000

MSB7 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB7 = (VALUE & 0x3FFF) | 0x4000

FREQUENCY = .20000
VALUE = .10736*FREQUENCY/.1000
;VALUE = (2<<28)*FREQUENCY/25000000

MSB8 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB8 = (VALUE & 0x3FFF) | 0x4000

FREQUENCY = .50000
VALUE = .10736*FREQUENCY/.1000
;VALUE = (2<<28)*FREQUENCY/25000000

MSB9 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB9 = (VALUE & 0x3FFF) | 0x4000

FREQUENCY = .100000
VALUE = .10736*FREQUENCY/.1000
;VALUE = (2<<28)*FREQUENCY/25000000

MSB10 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB10 = (VALUE & 0x3FFF) | 0x4000

FREQUENCY = .200000
VALUE = .10736*FREQUENCY/.1000
;VALUE = (2<<28)*FREQUENCY/25000000

MSB11 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB11 = (VALUE & 0x3FFF) | 0x4000
; *****************************************************************************
;               Main Program
; *****************************************************************************

prg
                BANKSEL     ANSEL
                clrf        ANSEL
                BANKSEL     ANSELH
                clrf        ANSELH
                BANKSEL     PORTB
                clrf        PORTB
                BANKSEL     TRISB
                clrf        TRISB
                BANKSEL     WDTCON
                bcf         WDTCON,SWDTEN

                BANKSEL     TRISCTRL
                clrf        TRISLCD         ; Display ports as outputs
                bcf         TRISCTRL,CTRLA
                bcf         TRISCTRL,CTRLB
                bcf         TRISCTRL,CTRLC
                bsf         TRISA,RA7       ; Button as input
                BANKSEL     PORTA
                clrf        TESTMODE
                btfss       PORTA,RA7
                bsf         TESTMODE,7
                

                call        longdelay
                call        longdelay
                call        longdelay


                call        functionset
                call        displayon

                call        longdelay
                call        functionset
                call        displayon

                call        longdelay
                call        functionset
                call        definechars
                call        displayon

                call        displayclear
                call        displaychome
                WRITELN     text_welcome    ; Greetings and program version.
                movlw       0x40
                call        displayaddrset  ; Move to the second line
                WRITELN     text_davide     ; Copyright

                clrf        FREQ            ; Standard values for frequency
                movlw       0x4
                movwf       CHVAL           ; Force setting of AD9833

                movlw       0x5             ; Standard value for DC (no DC)
                movwf       DCVAL

                call        ConfigureADC
                call        ConfigureSPI
                call        ResetAD9833

                call        longdelay
                call        longdelay
                call        longdelay

                clrf        store
                clrf        store+1
                clrf        store+2
                clrf        store+3
                call        SetPWM
                ; Main program loop: read ADC values, calculate ESR, repeat.
loop
                READV       CTRLA, A_VH, A_VL ; read A, B and C
                READV       CTRLB, B_VH, B_VL
                READV       CTRLC, C_VH, C_VL

                movfw       CHVAL       ; Update the frequency value if needed
                addwf       FREQ,f

                call        displayclear
                call        SelectFreq
                movfw       CHVAL
                btfss       STATUS,Z
                goto        buttonzero  ; If the frequency has changed, do not
                                        ; show the incomplete measurement.
                btfss       PORTA,RA7   ; Check if the button is depressed
                goto        ChooseDc


                movfw       TESTMODE
                btfss       STATUS,Z
                goto        testloop



                movfw       DCVAL
                xorlw       0x05        ; Check if a DC is present
                btfsc       STATUS,Z
                goto        @nodc
                movlw       0x0E        ; Put cursor in the top right corner
                call        displayaddrset
                movlw       '+'         ; Write '+DC' if appropriate
                call        senddata
                movlw       0x00        ; Write '+DC' if appropriate
                call        senddata
@nodc

checkamplitudes movfw       A_VH        ; Check if the oscillator amplitude is OK.
                sublw       OSC_LOTHRESHOLD
                btfsc       STATUS,C
                goto        err_lowosc

                movfw       A_VH
                sublw       OSC_HITHRESHOLD
                btfss       STATUS,C
                goto        err_hiosc


                ; ESR=(B-C)/(A-B)*10
                BANKSEL     divid0  ; Transfer B in the high 16 bits of divid
                MOV16FF     divid0, divid1, B_VH, B_VL
                clrf        divid2      ; Put 0 to divid2, divid3
                clrf        divid3
                SUB16BIT    divid0, divid1, C_VH, C_VL  ; divid -= C;

                MOV16FF     divisH, divisL, A_VH, A_VL  ; Transfer A in divis
                SUB16BIT    divisH, divisL, B_VH, B_VL  ; divis -= B
                call        divide      ; Divide! Result in divid0,1,2,3.

                MOV16FF     aHH,aHL,divid0,divid1 ; Multiply times 1525
                MOV16FF     aLH,aLL,divid2,divid3
                movlw       0x5         ; Hi-byte of 1525
                movwf       bH
                movlw       0xF5        ; Lo-byte of 1525
                movwf       bL
                call        mult_32_16

                ; store += divid;
                ;ADD32BIT    store, store+1, store+2, store+3, aHH, aHL, aLH, aLL
                ;DIV2O32BIT  store, store+1, store+2, store+3    ; store /=2;
                ;MOV16FF     bin+0, bin+1, store+0, store+1 ; bin=store (32 bit)
                ;MOV16FF     bin+2, bin+3, store+2, store+3

                MOV16FF     bin+0, bin+1, aHH, aHL
                MOV16FF     bin+2, bin+3, aLH, aLL

                call        output2l    ; Write the second line (ESR result)
                goto        loop

SetPWM          movfw       DCVAL
                xorlw       0x05        ; Value that corresponds to DC
                btfsc       STATUS,Z
                goto        SetNoDC
                BANKSEL     TRISCTRL
                bcf         TRISCTRL, PWMPIN    ; Port PWM as output
                movlw       0x0F
                BANKSEL     CCP1CON
                movwf       CCP1CON
                movlw       .100                ; Values in  %
                BANKSEL     PR2
                movwf       PR2
                movfw       DCVAL
                call        dclist
                BANKSEL     CCPR1L
                movwf       CCPR1L
                BANKSEL     T2CON
                bsf         T2CON, TMR2ON
                movlw       0x40
                goto        displayaddrset      ; Strange: if I remove this,
                ;return                         ; the program does not work!

SetNoDC         BANKSEL     TRISCTRL
                bsf         TRISCTRL, PWMPIN    ; Port PWM as input
                return                          ; That makes sort that DC=0

buttonzero      clrf        CHVAL
                goto        loop


err_lowosc      call        displayclear
                WRITELN     text_lowosc
                call        longdelay
                goto        loop

err_hiosc       call        displayclear
                WRITELN     text_hiosc
                call        longdelay
                goto        loop

err_reshi       call        displayclear
                WRITELN     text_reshi
                call        longdelay
                goto        loop

readadc
                call        longdelay
                clrf        STOREH
                clrf        STOREL
                movlw       0x40
                movwf       CNT
@llo            call        shortdelay
                BANKSEL     ADCON0
                bsf         ADCON0,GO
                btfsc       ADCON0,GO
                goto        $-1         ; Wait until conversion is complete
                ADD16BIT    STOREH, STOREL, ADRESH, ADRESL
                decfsz      CNT,f
                goto @llo
                return
                

; This is a test mode end of loop. Shows on the display the value of A, B and C
; (writing separately the high byte and the low byte for each).

testloop        call        displayclear
                movlw       'A'
                call        senddata
                movfw       A_VH
                call        write_number
                movlw       ','
                call        senddata
                movfw       A_VL
                call        write_number
                call        sendspace
                movlw       'B'
                call        senddata
                movfw       B_VH
                call        write_number
                movlw       ','
                call        senddata
                movfw       B_VL
                call        write_number
                
                movlw       0x40
                call        displayaddrset  ; Move to the second line
                movlw       'C'
                call        senddata
                movfw       C_VH
                call        write_number
                movlw       ','
                call        senddata
                movfw       C_VL
                call        write_number
                call        sendspace
                goto        loop

output2l        movlw       0x40
                call        displayaddrset
                WRITELN     text_esr
                call        write_number24
                movlw       0xF4
                call        sendchar    ; Write the ohm symbol
                return
; *****************************************************************************
;               Ancillary routines
; *****************************************************************************

ChooseDc        call        displayclear
                WRITELN     dcmenu
                movlw       0x40
                call        displayaddrset  ; Move to the second line
                movfw       CHVAL
                addwf       DCVAL,f
                clrf        CHVAL
                ;movfw       DCVAL
                ;call        write_number
                call        SelectDC

                call        longdelay

                btfsc       PORTA,RA7
                goto        loop
                goto        ChooseDc

SelectDC        movlw       .1
                xorwf       DCVAL,w
                btfsc       STATUS,Z
                goto        sdc1
                movlw       .2
                xorwf       DCVAL,w
                btfsc       STATUS,Z
                goto        sdc2
                movlw       .3
                xorwf       DCVAL,w
                btfsc       STATUS,Z
                goto        sdc3
                movlw       .4
                xorwf       DCVAL,w
                btfsc       STATUS,Z
                goto        sdc4
                movlw       .5
                xorwf       DCVAL,w
                btfsc       STATUS,Z
                goto        sdc5
                movlw       .6
                xorwf       DCVAL,w
                btfsc       STATUS,Z
                goto        sdc6
                movlw       .7
                xorwf       DCVAL,w
                btfsc       STATUS,Z
                goto        sdc7
                movlw       .8
                xorwf       DCVAL,w
                btfsc       STATUS,Z
                goto        sdc8
                movlw       .9
                xorwf       DCVAL,w
                btfsc       STATUS,Z
                goto        sdc9

                btfsc       DCVAL,7
                goto        @sdc1
                movlw       DCVAL
                btfsc       STATUS,Z
                goto        @sdc1

                movlw       .8
                subwf       DCVAL,w
                btfss       STATUS,C
                goto        @sdc1
                movlw       .9
                movwf       DCVAL
                goto        SelectDC

@sdc1
                movlw       1               ; If we get here, DCVAL contains an
                movwf       DCVAL           ; invalid data. Put 1.
                goto        SelectDC


sdc1            movlw       0x1
                movwf       DCVAL
                WRITELN dcval1
                call        SetPWM
                return

sdc2            movlw       0x2
                movwf       DCVAL
                WRITELN dcval2
                call        SetPWM
                return

sdc3            movlw       0x3
                movwf       DCVAL
                WRITELN dcval3
                call        SetPWM
                return

sdc4            movlw       0x4
                movwf       DCVAL
                WRITELN dcval4
                call        SetPWM
                return

sdc5            movlw       0x5
                movwf       DCVAL
                WRITELN dcval5
                call        SetPWM
                return

sdc6            movlw       0x6
                movwf       DCVAL
                WRITELN dcval6
                call        SetPWM
                return

sdc7            movlw       0x7
                movwf       DCVAL
                WRITELN dcval7
                call        SetPWM
                return

sdc8            movlw       0x8
                movwf       DCVAL
                WRITELN dcval8
                call        SetPWM
                return

sdc9            movlw       0x9
                movwf       DCVAL
                WRITELN dcval9
                call        SetPWM
                return


SelectFreq      movlw       .1
                xorwf       FREQ,w
                btfsc       STATUS,Z
                goto        sfreq1
                movlw       .2
                xorwf       FREQ,w
                btfsc       STATUS,Z
                goto        sfreq2
                movlw       .3
                xorwf       FREQ,w
                btfsc       STATUS,Z
                goto        sfreq3
                movlw       .4
                xorwf       FREQ,w
                btfsc       STATUS,Z
                goto        sfreq4
                movlw       .5
                xorwf       FREQ,w
                btfsc       STATUS,Z
                goto        sfreq5
                movlw       .6
                xorwf       FREQ,w
                btfsc       STATUS,Z
                goto        sfreq6
                movlw       .7
                xorwf       FREQ,w
                btfsc       STATUS,Z
                goto        sfreq7
                movlw       .8
                xorwf       FREQ,w
                btfsc       STATUS,Z
                goto        sfreq8
                movlw       .9
                xorwf       FREQ,w
                btfsc       STATUS,Z
                goto        sfreq9
                movlw       .10
                xorwf       FREQ,w
                btfsc       STATUS,Z
                goto        sfreq10
                movlw       .11
                xorwf       FREQ,w
                btfsc       STATUS,Z
                goto        sfreq11

                btfsc       FREQ,7
                goto        @set1
                movlw       FREQ
                btfsc       STATUS,Z
                goto        @set1

                movlw       .10
                subwf       FREQ,w
                btfss       STATUS,C
                goto        @set1
                movlw       .11
                movwf       FREQ
                goto        SelectFreq

@set1
                movlw       1               ; If we get here, FREQ contains an
                movwf       FREQ            ; invalid data. Put 1.
                goto        SelectFreq

sfreq1:
                PROGFREQ    LSB1,MSB1,freq1
                return

sfreq2:
                PROGFREQ    LSB2,MSB2,freq2
                return

sfreq3:
                PROGFREQ    LSB3,MSB3,freq3
                return

sfreq4:
                PROGFREQ    LSB4,MSB4,freq4
                return

sfreq5:
                PROGFREQ    LSB5,MSB5,freq5
                return

sfreq6:
                PROGFREQ    LSB6,MSB6,freq6
                return

sfreq7:
                PROGFREQ    LSB7,MSB7,freq7
                return

sfreq8:
                PROGFREQ    LSB8,MSB8,freq8
                return

sfreq9:
                PROGFREQ    LSB9,MSB9,freq9
                return

sfreq10:
                PROGFREQ    LSB10,MSB10,freq10
                return

sfreq11:
                PROGFREQ    LSB11,MSB11,freq11
                return


ConfigureADC   ; Configure the ADC, read on A0.
                BANKSEL     TRISA
                bsf         TRISA,0
                bsf         TRISA,2
                bsf         TRISA,3

                BANKSEL     ANSEL
                bsf         ANSEL,0     ; Use as A0 as an analog input.
                bsf         ANSEL,2
                bsf         ANSEL,3
                BANKSEL     ADCON1
                bcf         ADCON1,VCFG1
                bsf         ADCON1,VCFG0
                bsf         ADCON1,ADFM ; Right justified ADC result.
                BANKSEL     ADCON0
                bcf         ADCON0,7    ; Set Fosc/8 (ok for Fosc=4MHz)
                bsf         ADCON0,6
                bcf         ADCON0,5    ; Set input in channel 0.
                bcf         ADCON0,4
                bcf         ADCON0,3
                bcf         ADCON0,2
                BANKSEL     TRISC
                clrf        TRISC
                return

ConfigureSPI    ; Configure SPI communication with AD9833
                BANKSEL     TRISC
                bcf         TRISC,RC3
                bcf         TRISC,RC5
                bcf         TRISFSYNC,FSYNC
                BANKSEL     SSPSTAT
                clrf        SSPSTAT
                bsf         SSPSTAT, CKE
                BANKSEL     SSPCON
                bcf         SSPCON, SSPM0   ; Master SPI, clock Fosc/4
                bcf         SSPCON, SSPM1
                bcf         SSPCON, SSPM2
                bcf         SSPCON, SSPM3
                bsf         SSPCON, CKP     ; Clock polarity, idle high
                bsf         SSPCON, SSPEN   ; Serial port enable
                bcf         SSPCON, SSPOV
                bcf         SSPCON, WCOL

                return

StartReg        BANKSEL     PORTFSYNC
                bcf         PORTFSYNC,FSYNC
                return

StopReg         BANKSEL     PORTFSYNC
                bsf         PORTFSYNC,FSYNC
                return

WriteSPI        BANKSEL     SSPBUF
                movwf       SSPBUF
                BANKSEL     SSPSTAT
                btfss       SSPSTAT, BF
                goto        $-1
                movfw       SSPBUF
                return

ResetAD9833     call        StartReg
                movlw       0x01            ; Control word write, reset
                call        WriteSPI
                movlw       0x00
                call        WriteSPI        ; FREQ0 register write, 14 LSB
                call        StopReg
                return

ConfigureAD9833
                call        StartReg
                movlw       0x21            ; Control word write, reset
                call        WriteSPI
                movlw       0x00
                call        WriteSPI        ; FREQ0 register write, 14 LSB
                movfw       LSBH
                call        WriteSPI
                movfw       LSBL
                call        WriteSPI        ; FREQ0 register write, 14 MSB
                movfw       MSBH
                call        WriteSPI
                movfw       MSBL
                call        WriteSPI
                movlw       0xC0            ; PHASE0 register write
                call        WriteSPI
                movlw       0x00
                call        WriteSPI
                movlw       0x20            ; 0x2000 sine, 0x2028 square
                call        WriteSPI        ; 0x2002 triangle
                movlw       0x00
                call        WriteSPI
                call        StopReg
                return


; *****************************************************************************
; Write a 24-bit number contained in bin to bin+2 (big endian) on the LCD
; *****************************************************************************

WRITE2DIGITS    MACRO       BB
                swapf       BB,w
                andlw       0x0F
                btfsc       STATUS,Z
                btfsc       NOZ,0
                call        write_number
                movfw       BB
                andlw       0x0F
                btfsc       STATUS,Z
                btfsc       NOZ,0
                call        write_number

                ENDM

write_number24
                clrf        NOZ
                call        b2bcd
                WRITE2DIGITS bcd
                swapf       bcd+1,w
                andlw       0x0F
                call        write_number
                movlw       '.'
                call        sendchar    ; Send the channel number
                movfw       bcd+1
                andlw       0x0F
                call        write_number
                WRITE2DIGITS bcd+2
                ;WRITE2DIGITS bcd+3
                ;WRITE2DIGITS bcd+4     ; Those figures are not significative
                return

; *****************************************************************************
; Convert 32-bit binary number at <bin> into a bcd number
; at <bcd>. Uses Mike Keitz's procedure for handling bcd
; adjust; Modified Microchip AN526 for 32-bits.
; http://www.piclist.com/tecHREF/microchip/math/radix/b2bp-32b10d.htm
; bin is the MSB, bin+3 is the LSB
; *****************************************************************************

b2bcd
                movlw       .32              ; 32-bits
                movwf       ii              ; make cycle counter
                clrf        bcd             ; clear result area
                clrf        bcd+1
                clrf        bcd+2
                clrf        bcd+3
                clrf        bcd+4

b2bcd2          movlw       bcd             ; make pointer
                movwf       fsr
                movlw       5
                movwf       cnt

; Mike's routine:

b2bcd3          movlw       0x33
                addwf       indf,f          ; add to both nybbles
                btfsc       indf,3          ; test if low result > 7
                andlw       0xf0            ; low result >7 so take the 3 out
                btfsc       indf,7          ; test if high result > 7
                andlw       0x0f            ; high result > 7 so ok
                subwf       indf,f          ; any results <= 7, subtract back
                incf        fsr,f           ; point to next
                decfsz      cnt,f
                goto        b2bcd3

                rlf         bin+3,f         ; get another bit
                rlf         bin+2,f
                rlf         bin+1,f
                rlf         bin+0,f
                rlf         bcd+4,f         ; put it into bcd
                rlf         bcd+3,f
                rlf         bcd+2,f
                rlf         bcd+1,f
                rlf         bcd+0,f
                decfsz      ii,f            ; all done?
                goto        b2bcd2          ; no, loop
                return                      ; yes

; *****************************************************************************
;               LCD Subroutines
; *****************************************************************************
sendspace
                movlw       ' '
sendchar
                bsf         DATALCD,RS      ; RS to 1: send a character
                bcf         DATALCD,RW      ; RW to 0: write to display
                call        sendbyte        ; Send the channel number
                return

write_number                            ; Print the number in w
                bsf     NOZ,0
                BANKSEL TMP_1
                movwf   TMP_1           ; Save the w register in memory
                movlw   -1
                movwf   HND
                movfw   TMP_1
sub_hnd         addlw   -0x64           ; Substract 100(10)
                incf    HND, f          ; Hundreds
                btfsc   STATUS, C       ; Verify if the result is negative
                goto    sub_hnd

                addlw   0x64            ; Add 100(base10) (to compensate the
                                        ; last subtraction)
                movwf   TMP_1           ; Save the w register in memory
                movlw   -1
                movwf   DEC
                movfw   TMP_1
sub_dec         addlw   -0xA            ; Substract 10(base10)
                incf    DEC, f          ;
                btfsc   STATUS, C       ; Verify if the result is negative
                goto    sub_dec

                addlw   0x0A            ; Add 10(base10) (to compensate the
                                        ; last subtraction)
                movwf   UNT             ; Save the units


                bcf     TMP_1,0         ; Flag: avoid unnecessary zeros
                movfw   HND             ; Send the hundreds
                addlw   0x00            ; Used only to test if w is zero (does
                                        ; not modify w)
                btfsc   TMP_1, 0        ; Force to write all numbers
                goto    force_HND
                btfsc   STATUS, Z       ; Skip if necessary
                goto    skip_HND
force_HND       addlw   '0'             ; Transform it in ASCII
                bsf     DATALCD,RS      ; RS to 1: we are sending a character
                bcf     DATALCD,RW      ; RW to 0: we are writing to display
                call    sendbyte        ; Send the channel number
                bsf     TMP_1, 0        ; Force to write successive zeros
skip_HND        movfw   DEC             ; Send the tenths
                addlw   0x00            ; Used only to test if w is zero
                                        ; (does not modify w)
                btfsc   TMP_1, 0        ; Force to write all numbers
                goto    force_DEC
                btfsc   STATUS, Z       ; Skip if necessary
                goto    skip_DEC
force_DEC       addlw   '0'             ; Transform it in ASCII
                bsf     DATALCD,RS      ; RS to 1: we are sending a character
                bcf     DATALCD,RW      ; RW to 0: we are writing to display
                call    sendbyte        ; Send the channel number
                bsf     TMP_1, 0
skip_DEC        movfw   UNT             ; Send the units (always to be written)
                addlw   '0'             ; Transform it in ASCII
                bsf     DATALCD,RS      ; RS to 1: we are sending a character
                bcf     DATALCD,RW      ; RW to 0: we are writing to display
                call    sendbyte        ; Send the channel number
                return


functionset
                BANKSEL DATALCD
                bcf     DATALCD,RS
                bcf     DATALCD,RW
                movlw   0x02            ; Will be interpreted as nibble mode
                                        ; the first nibble will be ignored
                                        ; the second one will be interpreted
                                        ; as 0x20 if the LCD has just been
                                        ; switched on.
                call    sendbyte
                call    busywait

functionset2    BANKSEL DATALCD
                bcf     DATALCD,RS
                bcf     DATALCD,RW
                movlw   0x28            ; Two lines display setup
                call    sendbyte
                call    busywait
                retlw   0

                ; Send the command in w to the display LCD configured in the
                ; nibble mode
sendbyte
                BANKSEL DATALCD
                movwf   TMP             ; Store command temporarily in TMP
                call    portnibble
                call    pulse_e         ; Send the first half of the command
                swapf   TMP,w           ; swap nibbles of TMP, result in w
                call    portnibble
                call    pulse_e         ; Send the second half of the command
                call    busywait
                retlw   0

                ; Activate the display, cursor underline blinking
displayon
                BANKSEL DATALCD
                bcf     DATALCD,RS
                bcf     DATALCD,RW
                movlw   0x0C             ; Display on,
                call    sendbyte
                retlw   0

                ; Clear the display
displayclear
                BANKSEL DATALCD
                bcf     DATALCD,RS
                bcf     DATALCD,RW
                movlw   0x01
                call    sendbyte
                retlw   0

                ; Put the cursor at home
displaychome
                BANKSEL DATALCD
                bcf     DATALCD,RS
                bcf     DATALCD,RW
                movlw   0x02
                call    sendbyte
                retlw   0

                ; Set cursor address (passed in w)
displayaddrset
                BANKSEL DATALCD
                bcf     DATALCD,RS
                bcf     DATALCD,RW
                iorlw   0x80
                call    sendbyte
                retlw   0

                ; w choose method: bit 0=0 decrement =1 increment;
                ; bit 1=0 display shift off =1 display shift on
displaycurdir
                BANKSEL DATALCD
                bcf     DATALCD,RS
                bcf     DATALCD,RW
                iorlw   0x04
                call    sendbyte
                retlw   0

; *****************************************************************************
;               Delay routines for LCD
; *****************************************************************************
longdelay       movlw       CHVAL          ; Skip if a button is pressed.
                btfss       STATUS,Z
                return
                BANKSEL     TRISA
                bsf         TRISA,RA4
                bsf         TRISA,RA5
                bsf         TRISA,RA7

                BANKSEL     PORTA
    
                movfw       PORTA
                movwf       BSENSE

                call        shortdelay

                movfw       BSENSE
                BANKSEL     PORTA
                xorwf       PORTA,w
                btfsc       STATUS,Z
                goto        @cont
@change
                btfss       BSENSE,RA4
                goto        @invpol

                call        shortdelay
                btfss       PORTA,RA5
                incf        CHVAL,f
                btfsc       PORTA,RA5
                decf        CHVAL,f
                return                      ; Exit immediately

@invpol         call        shortdelay
                btfss       PORTA,RA5
                decf        CHVAL,f
                btfsc       PORTA,RA5
                incf        CHVAL,f
                return                      ; Exit immediately
@cont
                BANKSEL     LDR
                decfsz      LDR,f
                goto        longdelay
                return

middelay
                BANKSEL     LDR
                movlw       0x40
                movwf       LDR
                call        shortdelay
                decfsz      LDR,f
                goto        $-2
                return

shortdelay
                BANKSEL     SDR
                decfsz      SDR,f
                goto        $-1
                return

busywait
                BANKSEL     TRISLCD
                ;movlw      0xF0        ; Set all the port (4 bits) as inputs
                clrf        TMP
                bsf         TRISLCD, DATA4
                bsf         TRISLCD, DATA5
                bsf         TRISLCD, DATA6
                bsf         TRISLCD, DATA7
                BANKSEL     DATALCD
                bcf         DATALCD, RS     ; RS to 0
                bsf         DATALCD, RW     ; RW to 1: Read
                nop
                bsf         DATALCD, E      ; Raise E line
                bcf         STATUS,C
                btfsc       DATALCD, DATA7
                bsf         STATUS,C
                bcf         DATALCD, E
                nop
                nop
                bsf         DATALCD, E      ; Raise E line
                nop
                nop
                nop
                bcf         DATALCD, E
                btfsc       STATUS, C       ; Test the carry
                goto        busywait        ; If busy, continue waiting
                BANKSEL     TRISLCD
                bcf         TRISLCD, DATA4
                bcf         TRISLCD, DATA5
                bcf         TRISLCD, DATA6
                bcf         TRISLCD, DATA7
                BANKSEL     DATALCD
                bcf         DATALCD, RW     ; RW to 1: Read
                return

pulse_e
                BANKSEL     DATALCD
                bsf         DATALCD,E
                nop
                nop
                nop
                bcf         DATALCD,E
                nop
                nop
                nop
                return

                ; Send the upper nibble in W into the data lines
portnibble
                movwf       TMP1
                BANKSEL     DATALCD
                bcf         DATALCD, DATA4
                bcf         DATALCD, DATA5
                bcf         DATALCD, DATA6
                bcf         DATALCD, DATA7
                btfsc       TMP1, 4
                bsf         DATALCD, DATA4
                btfsc       TMP1, 5
                bsf         DATALCD, DATA5
                btfsc       TMP1, 6
                bsf         DATALCD, DATA6
                btfsc       TMP1, 7
                bsf         DATALCD, DATA7
                nop
                nop
                nop
                nop
                return

; *****************************************************************************
;               Divide 32-bit over 16-bit operands
; http://www.piclist.com/techref/microchip/math/div/24by16.htm?key=
; divid3        is the LSB, divid0 is the MSB
; *****************************************************************************

divide          movlw .32               ; 32-bit divide by 16-bit
                movwf cnt
                clrf remdrH             ; Clear remainder
                clrf remdrL

dvloop          clrc                    ; Set quotient bit to 0
                                        ; Shift left dividend and quotient
                rlf divid3,f            ; lsb
                rlf divid2,f
                rlf divid1,f
                rlf divid0,f            ; lsb into carry
                rlf remdrL,f            ; and then into partial remainder
                rlf remdrH,f

                skpnc                   ; Check for overflow
                goto subd
                movfw divisH            ; Compare partial remainder and divisor
                subwf remdrH,w
                skpz
                goto testgt             ; Not equal: test if remdrH is greater
                movfw divisL            ; High bytes equal: compare low bytes
                subwf remdrL,w
testgt          skpc                    ; Carry set if remdr >= divis
                goto remrlt

subd            movfw divisL            ; Subtract divisor from part. remainder
                subwf remdrL,f
                skpc                    ; Test for borrow

                decf remdrH,f           ; Subtract borrow
                movfw divisH
                subwf remdrH,f
                bsf divid3,0            ; Set quotient bit to 1
                     ; Quotient replaces dividend which is lost
remrlt          decfsz cnt,f
                goto dvloop
                return

; *****************************************************************************
;               Multiply 32-bit x 16-bit
; http://www.piclist.com/techref/microchip/math/mul/32x16-a.htm
; divid3        is the LSB, divid0 is the MSB
; *****************************************************************************

;As a thank you for all the code, here is a 32x16 bit Mult.
;Unsigned 32 bit by 16 bit multiplication
;This routine will take aHH:aHL:aLH:aLL*bH:bL -> a6:a5:aHH:aHL:aLH:aLL

mult_32_16:
; Begin rearrange code
                nop
                movfw   aHH
                movwf   a6
                movfw   aHL
                movwf   a5
                movfw   aLH
                movwf   aHH
                movfw   aLL
                movwf   aHL
; End rearrange code
                clrf    aLH          ; clear partial product
                clrf    aLL
                movfw   a6
                movwf   c4
                movf    a5,W
                movwf   c3
                movf    aHH,W
                movwf   c2
                movf    aHL,W
                movwf   c1

                movlw   0x08
                movwf   bitcnt

LOOPUM3216A:
                rrf     bL, F
                btfsc   STATUS, C
                goto    ALUM3216NAP
                decfsz  bitcnt, F
                goto    LOOPUM3216A

                movwf   bitcnt

LOOPUM3216B:
                rrf     bH, F
                btfsc   STATUS, C
                goto    BLUM3216NAP
                decfsz  bitcnt, F
                goto    LOOPUM3216B

                clrf    a6
                clrf    a5
                clrf    aHH
                clrf    aHL
                retlw   0x00

BLUM3216NAP:
                BCF     STATUS, C
                goto    BLUM3216NA

ALUM3216NAP:
                BCF     STATUS, C
                goto    ALUM3216NA

ALOOPUM3216:
                rrf     bL, F
                btfss   STATUS, C
                goto    ALUM3216NA
                movf   c1,W
                addwf   aHL, F
                movf    c2,W
                btfsc   STATUS, C
                incfsz  c2,W
                addwf   aHH, F
                movf    c3,W
                btfsc   STATUS, C
                incfsz  c3,W
                addwf   a5, F
                movf    c4,W
                btfsc   STATUS, C
                incfsz  c4,W
                addwf   a6, F

ALUM3216NA:
                rrf    a6, F
                rrf    a5, F
                rrf    aHH, F
                rrf    aHL, F
                rrf    aLH, F
                decfsz  bitcnt, f
                goto    ALOOPUM3216

                movlw   0x08
                movwf   bitcnt

BLOOPUM3216:
                rrf    bH, F
                btfss  STATUS, C
                goto   BLUM3216NA
                movf   c1,W
                addwf  aHL, F
                movf   c2,W
                btfsc  STATUS, C
                incfsz c2,W
                addwf  aHH, F
                movf   c3,W
                btfsc  STATUS, C
                incfsz c3,W
                addwf  a5, F
                movf   c4,W
                btfsc  STATUS, C
                incfsz c4,W
                addwf  a6, F

BLUM3216NA
                rrf    a6, F
                rrf    a5, F
                rrf    aHH, F
                rrf    aHL, F
                rrf    aLH, F
                rrf    aLL, F
                decfsz  bitcnt, F
                goto    BLOOPUM3216
        nop
        return

; *****************************************************************************
; User-defined chars for the LCD display
; *****************************************************************************

senddata        bsf         DATALCD,RS
                bcf         DATALCD,RW
                call        sendbyte
                return

definechars     BANKSEL     DATALCD
                bcf         DATALCD,RS
                bcf         DATALCD,RW
                movlw       0x40
                call        sendbyte
                movlw       B'00011000'
                call        senddata
                movlw       B'00010100'
                call        senddata
                movlw       B'00010100'
                call        senddata
                movlw       B'00011011'
                call        senddata
                movlw       B'00000100'
                call        senddata
                movlw       B'00000100'
                call        senddata
                movlw       B'00000011'
                call        senddata
                movlw       B'00000000'
                call        senddata
                return

                end