; *****************************************************************************
;               esr.asm
;       A simple ESR measuring system exploiting lock-in techniques
;       Davide Bucci, 2021
;       Version 1.1
; *****************************************************************************

; License:
; --------

;    Copyright (C) 2021  Davide Bucci  davbucciPleaseNoSpamHerE@tiscali.it

;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 3 of the License, or
;    (at your option) any later version.

;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.

;    You should have received a copy of the GNU General Public License
;    along with this program; if not, write to the Free Software
;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

        ERRORLEVEL -302         ; disable the annoying warning
                                ; "Register in operand not in bank 0.
                                ; Ensure bank bits are correct."

#include <p16F883.inc>
   __config (_INTRC_OSC_NOCLKOUT & _WDT_OFF & _PWRTE_ON & _BOREN_ON & _MCLRE_ON & _CP_OFF & _IESO_OFF & _FCMEN_OFF & _DEBUG_OFF)

; Constants
OSC_LOTHRESHOLD equ     .35     ; Error if the ampl. A_VH is lower than this.
OSC_HITHRESHOLD equ     .230    ; Error if the ampl. A_VH is higher than this.
CURR_THRESHOLD  equ     .2      ; Threshold for high byte of A-B (current)

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

UNIT            equ     0x25      ; Multiplier for measuring unit for caps.

TMP_1           equ     0x2A
HND             equ     0x2B
DEC             equ     0x2C
UNT             equ     0x2D


; Used to send 16 bit data via SPI, combined with the w register.
SENDL           equ     0x2E

; Used for the div_32_16 operator: the two operands, the result and the remainder
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

USR             equ     0x4D


bcd             equ     0x50
; memory used up to bcd+4 included
cnt             equ     0x55
ii              equ     0x56
bin             equ     0x57
; memory used up to bin+3 included

; Error constants for the CalcCapacitance routine
FREQLOW         equ     0x01
FREQHI          equ     0x02

CAPHH           equ     0x5B    ; The current value of the capacity.
CAPHL           equ     0x5C
CAPLH           equ     0x5D
CAPLL           equ     0x5E

indf            equ     0
fsr             equ     4

NOZ             equ     0x60


DIVH            equ     0x65
DIVL            equ     0x6F

REGBLL          equ     0x66
REGBLH          equ     0x67
REGBHL          equ     0x68
REGBHH          equ     0x69
REGCLL          equ     0x6A
REGCLH          equ     0x6B
REGCHL          equ     0x6C
REGCHH          equ     0x6D
MCOUNT          equ     0x6E

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

; The result of each ADC conversion (temporary)
STOREH          equ     0x76
STOREL          equ     0x77

; The two 14-bit words that define the frequency on the AD9833
MSBH            equ     0x78
MSBL            equ     0x79
LSBH            equ     0x7A
LSBL            equ     0x7B

; The index in the frequency table
FREQ            equ     0x7C

; The index in the DC value
DCVAL           equ     0x7D

; User interface (CHVAL!=0 means that the knob has moved)
BSENSE          equ     0x7E
CHVAL           equ     0x7F

; *****************************************************************************
;               MACROS
; *****************************************************************************

READV           MACRO       CTRL, STH,STL
                BANKSEL     CTRLP
                clrf        CTRLP
                bsf         CTRLP,CTRL
                call        readadc
                movfw       STOREH
                movwf       STH
                movfw       STOREL
                movwf       STL
                ENDM

                ; Write a message on the LCD display
                ; This version is known not to work properly if the message
                ; crosses a page.
                ; This macro should not be called from an address >= 0x800
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
                call        senddata
                incf        CNT, f
                goto        loop_ch
end_mes
                ENDM

                ; DEST = DEST + SOURCE
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

                ; DEST = DEST - SOURCE
SUB16BIT        MACRO       DESTH, DESTL, SOURCEH, SOURCEL
                movfw       SOURCEL
                subwf       DESTL,f
                movfw       SOURCEH
                btfss       STATUS,C
                incfsz      SOURCEH,w      ; Adjust borrow if C=1
                subwf       DESTH,f
                ENDM

                ; DEST = DEST + SOURCE
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
                ; DEST /=2
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

PROGFREQ        MACRO       LSB, MSB, message, DIVHA, DIVLA
                movlw       (LSB & 0xFF00)>>8
                movwf       LSBH
                movlw       (LSB & 0x00FF)
                movwf       LSBL
                movlw       (MSB & 0xFF00)>>8
                movwf       MSBH
                movlw       (MSB & 0x00FF)
                movwf       MSBL
                movlw       DIVHA
                movwf       DIVH
                movlw       DIVLA
                movwf       DIVL
                lcall       ConfigureAD9833
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
                DT      "DC short...",0
text_hiosc      addwf   PCL,f
                DT      "Osc. high.",0
text_reshi      addwf   PCL,f
                DT      "Res. high.",0
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

text_cap        addwf   PCL,f
                DT      "C = ",0


DCVALUE_NO_DC   equ     .6

                ; Values of the PWM for a 5V power supply
dclist          addwf   PCL,f
                retlw   .0      ; not used
                retlw   .8     ; -2V
                retlw   .18     ; -1.5V
                retlw   .29     ; -1V
                retlw   .39     ; -0.5V
                retlw   .43     ; -0.3V
                retlw   .50     ; 0V
                retlw   .57     ; 0.3V
                retlw   .60     ; 0.5V
                retlw   .71     ; 1V
                retlw   .82     ; 1.5V
                retlw   .93     ; 2V

dcmenu          addwf   PCL,f
                DT      "Choose DC bias",0

dcval1          addwf   PCL,f
                DT      "DC = -2 V     ",0

dcval2          addwf   PCL,f
                DT      "DC = -1.5 V   ",0

dcval3          addwf   PCL,f
                DT      "DC = -1 V     ",0

dcval4          addwf   PCL,f
                DT      "DC = -0.5 V   ",0

dcval5          addwf   PCL,f
                DT      "DC = -0.3 V   ",0

dcval6          addwf   PCL,f
                DT      "DC = 0 V (off)",0

dcval7          addwf   PCL,f
                DT      "DC = 0.3 V    ",0

dcval8          addwf   PCL,f
                DT      "DC = 0.5 V    ",0

dcval9          addwf   PCL,f
                DT      "DC = 1 V      ",0

dcval10          addwf   PCL,f
                DT      "DC = 1.5 V    ",0

dcval11         addwf   PCL,f
                DT      "DC = 2 V      ",0

nosignal        addwf   PCL,f
                DT      "      ----",0

currlow         addwf   PCL,f
                DT      "Test current low"
DIVC1  = .11
DIVC2  = .21
DIVC3  = .53
DIVC4  = .105
DIVC5  = .211
DIVC6  = .527
DIVC7  = .1054
DIVC8  = .2108
DIVC9  = .5271
DIVC10 = .10541
DIVC11 = .21083

FREQUENCY = .100
VALUE = .10736*FREQUENCY/.1000
MSB1 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB1 = (VALUE & 0x3FFF) | 0x4000
DIVH1 = (DIVC1 & 0xFF00) >> .8
DIVL1 = DIVC1 & 0x00FF

FREQUENCY = .200
VALUE = .10736*FREQUENCY/.1000
MSB2 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB2 = (VALUE & 0x3FFF) | 0x4000
DIVH2 = (DIVC2 & 0xFF00) >> .8
DIVL2 = DIVC2 & 0x00FF


FREQUENCY = .500
VALUE = .10736*FREQUENCY/.1000
MSB3 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB3 = (VALUE & 0x3FFF) | 0x4000
DIVH3 = (DIVC3 & 0xFF00) >> .8
DIVL3 = DIVC3 & 0x00FF

FREQUENCY = .1000
VALUE = .10736*FREQUENCY/.1000
MSB4 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB4 = (VALUE & 0x3FFF) | 0x4000
DIVH4 = (DIVC4 & 0xFF00) >> .8
DIVL4 = DIVC4 & 0x00FF

FREQUENCY = .2000
VALUE = .10736*FREQUENCY/.1000
MSB5 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB5 = (VALUE & 0x3FFF) | 0x4000
DIVH5 = (DIVC5 & 0xFF00) >> .8
DIVL5 = DIVC5 & 0x00FF

FREQUENCY = .5000
VALUE = .10736*FREQUENCY/.1000
MSB6 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB6 = (VALUE & 0x3FFF) | 0x4000
DIVH6 = (DIVC6 & 0xFF00) >> .8
DIVL6 = DIVC6 & 0x00FF


FREQUENCY = .10000
VALUE = .10736*FREQUENCY/.1000
MSB7 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB7 = (VALUE & 0x3FFF) | 0x4000
DIVH7 = (DIVC7 & 0xFF00) >> .8
DIVL7 = DIVC7 & 0x00FF

FREQUENCY = .20000
VALUE = .10736*FREQUENCY/.1000
MSB8 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB8 = (VALUE & 0x3FFF) | 0x4000
DIVH8 = (DIVC8 & 0xFF00) >> .8
DIVL8 = DIVC8 & 0x00FF

FREQUENCY = .50000
VALUE = .10736*FREQUENCY/.1000
MSB9 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB9 = (VALUE & 0x3FFF) | 0x4000
DIVH9 = (DIVC9 & 0xFF00) >> .8
DIVL9 = DIVC9 & 0x00FF


FREQUENCY = .100000
VALUE = .10736*FREQUENCY/.1000
MSB10 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB10 = (VALUE & 0x3FFF) | 0x4000
DIVH10 = (DIVC10 & 0xFF00) >> .8
DIVL10 = DIVC10 & 0x00FF

FREQUENCY = .200000
VALUE = .10736*FREQUENCY/.1000
MSB11 = ((VALUE & 0xFFFC000) >> .14) | 0x4000
LSB11 = (VALUE & 0x3FFF) | 0x4000
DIVH11 = (DIVC11 & 0xFF00) >> .8
DIVL11 = DIVC11 & 0x00FF

; *****************************************************************************
;               Main Program
; *****************************************************************************

prg
                lcall       InitAll
                lcall       SetPWM
                lgoto       AutomaticMeas
                ; Main program loop: read ADC values, calculate ESR, repeat.
loop
                call        ReadAllADC
                movfw       CHVAL       ; Update the frequency value if needed
                addwf       FREQ,f
                call        displayclear
                call        SelectFreq
                movfw       CHVAL
                btfss       STATUS,Z
                goto        buttonzero  ; If the frequency has changed, do not
                                        ; show the incomplete measurement.
                btfss       PORTA,RA7   ; Check if the button is depressed
                goto        ChooseDc    ; If yes, go to the DC choice loop
                movfw       TESTMODE
                btfsc       STATUS,Z
                goto        notest
                lgoto       testloop    ; If we are in test mode, show A B C
notest
                lcall       displayclear
                lcall       CalcESR
                lcall       CalcCapacitance
                lgoto       $+1
                xorlw       0x0
                skpz
                goto        nocap

                lcall       displaychome
                lcall       WriteCap
nocap
                lgoto       loop

CalcESR         movfw       DCVAL
                xorlw       DCVALUE_NO_DC   ; Check if a DC is present
                btfsc       STATUS,Z
                goto        checkamplitudes
                movlw       0x0E        ; Put cursor in the top right corner
                call        displayaddrset
                call        writedc
checkamplitudes movfw       A_VH        ; Check if oscillator amplitude is OK.
                sublw       OSC_LOTHRESHOLD
                btfsc       STATUS,C
                goto        err_lowosc
                movfw       A_VH
                sublw       OSC_HITHRESHOLD
                btfss       STATUS,C
                goto        err_hiosc

                ; ESR=(B-C)/(A-B)*10
                ; Transfer B in the high 16 bits of divid
                MOV16FF     divid0, divid1, B_VH, B_VL
                clrf        divid2      ; Put 0 to divid2, divid3
                clrf        divid3
                SUB16BIT    divid0, divid1, C_VH, C_VL  ; divid -= C
                MOV16FF     divisH, divisL, A_VH, A_VL  ; Transfer A in divis
                SUB16BIT    divisH, divisL, B_VH, B_VL  ; divis -= B
checkcurrent    movfw       divisH          ; Check if test current is OK.
                sublw       CURR_THRESHOLD  ; (A-B) should be greater than a
                btfsc       STATUS,C        ; certain threshold for the ESR
                goto        err_lowcurr     ; to be meaningful
                btfsc       divisH,7        ; In some cases (A-B) is <0!!!
                goto        err_lowcurr
                call        div_32_16       ; Divide! Result in divid0,1,2,3.
                MOV16FF     aHH,aHL,divid0,divid1 ; Multiply times 1525
                MOV16FF     aLH,aLL,divid2,divid3
                movlw       0x5         ; Hi-byte of 1525
                movwf       bH
                movlw       0xF5        ; Lo-byte of 1525
                movwf       bL
                lcall       mult_32_16
                MOV16FF     bin+0, bin+1, aHH, aHL
                MOV16FF     bin+2, bin+3, aLH, aLL
                ; Write the second line (ESR result)
outputESR       lcall        display2line
                WRITELN     text_esr
                call        WriteNumber24
                call        sendspace
                movlw       0xF4
                call        sendchar    ; Write the ohm symbol
                return

; *****************************************************************************

ReadAllADC      READV       CTRLA, A_VH, A_VL ; read A, B and C
                READV       CTRLB, B_VH, B_VL
                READV       CTRLC, C_VH, C_VL
                return

writedc         movfw       DCVAL
                sublw       DCVALUE_NO_DC
                btfsc       STATUS,C
                goto        dcneg
                movlw       '+'         ; Write '+DC' if appropriate
                call        senddata
                movlw       0x00        ; Special character ("DC")
                call        senddata
                return
dcneg           movlw       '-'         ; Write '-DC' if appropriate
                call        senddata
                movlw       0x00        ; Special character ("DC")
                call        senddata
                return

SetPWM          movfw       DCVAL
                xorlw       DCVALUE_NO_DC       ; Value that corresponds to DC
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
                lcall       dclist
                BANKSEL     CCPR1L
                movwf       CCPR1L
                BANKSEL     T2CON
                bsf         T2CON, TMR2ON
                goto        display2line        ; Strange: if I remove this,
                ;return                         ; the program does not work!

SetNoDC         BANKSEL     TRISCTRL
                bsf         TRISCTRL, PWMPIN    ; Port PWM as input
                return                          ; That makes sort that DC=0

buttonzero      clrf        CHVAL
                goto        loop

err_lowosc      call        displayclear
                movfw       DCVAL               ; If a DC bias is present,
                xorlw       DCVALUE_NO_DC       ; then 99% of the times we have
                btfsc       STATUS,Z            ; a short circuit.
                goto        nosig
                WRITELN     text_lowosc
                call        activedelay
                return

err_lowcurr
nosig           WRITELN     nosignal
                call        activedelay
                return

err_hiosc       call        displayclear
                WRITELN     text_hiosc
                call        activedelay
                return

err_reshi       call        displayclear
                WRITELN     text_reshi
                call        activedelay
                return

readadc
                bsf         ADCON0,ADON ; Activate the ADC.
                call        activedelay
                clrf        STOREH
                clrf        STOREL
                movlw       0x40
                movwf       CNT
@llo            bsf         ADCON0,GO
                btfsc       ADCON0,GO
                goto        $-1         ; Wait until conversion is complete
                ADD16BIT    STOREH, STOREL, ADRESH, ADRESL
                decfsz      CNT,f
                goto @llo
                return

; *****************************************************************************
;               Ancillary routines
; *****************************************************************************
WriteCap        WRITELN     text_cap
                MOV16FF     bin+0, bin+1,CAPHH, CAPHL
                MOV16FF     bin+2, bin+3,CAPLH, CAPLL
                call        WriteNumber24
                call        sendspace
                movfw       UNIT
                call        sendchar   ; Write the farad symbol
                movlw       'F'
                call        sendchar   ; Write the farad symbol
                return


; In this loop one can choose the DC value to be set.
ChooseDc        lcall       displayclear
                WRITELN     dcmenu
ChooseDcLoop    lcall       display2line  ; Move to the second line
                movfw       CHVAL           ; Update the DC value if needed
                addwf       DCVAL,f
                clrf        CHVAL
                lcall       SelectDC        ; Show the DC value
                call        activedelay
                btfsc       PORTA,RA7
                goto        loop            ; Exit from this loop
                goto        ChooseDcLoop

SelectDC        btfsc       DCVAL,7         ; Check if DCVAL is negative
                goto        sdc1            ; If yes, put DCVAL=1
                movfw       DCVAL
                btfsc       STATUS,Z        ; Check if DCVAL is zero
                goto        sdc1            ; If yes, put DCVAL=1
                movlw       .11
                subwf       DCVAL,w         ; Check if DCVAL is greater than 9
                btfsc       STATUS,C
                goto        sdc11            ; If yes, put DCVAL=9
@choose         movlw       .1
                xorwf       DCVAL,w         ; Write the correct message
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
                movlw       .10
                xorwf       DCVAL,w
                btfsc       STATUS,Z
                goto        sdc10
                movlw       .11
                xorwf       DCVAL,w
                btfsc       STATUS,Z
                goto        sdc11

sdc1            movlw       0x1             ; We need to be sure that negative
                movwf       DCVAL           ; values are transformed into 1
                WRITELN     dcval1
                goto        SetPWM

sdc2                                        ; No need to set values for DCVAL
                WRITELN     dcval2
                goto        SetPWM

sdc3
                WRITELN     dcval3
                goto        SetPWM

sdc4
                WRITELN     dcval4
                goto        SetPWM

sdc5
                WRITELN     dcval5
                goto        SetPWM

sdc6
                WRITELN     dcval6
                goto        SetPWM

sdc7
                WRITELN     dcval7
                goto        SetPWM

sdc8
                WRITELN     dcval8
                goto        SetPWM

sdc9
                WRITELN     dcval9
                goto        SetPWM

sdc10
                WRITELN     dcval10
                goto        SetPWM

sdc11           movlw       .11             ; We need to be sure that >11 ->11
                movwf       DCVAL
                WRITELN     dcval11
                goto        SetPWM

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
                PROGFREQ    LSB1,MSB1,freq1,DIVH1,DIVL1
                return

sfreq2:
                PROGFREQ    LSB2,MSB2,freq2,DIVH2,DIVL2
                return

sfreq3:
                PROGFREQ    LSB3,MSB3,freq3,DIVH3,DIVL3
                return

sfreq4:
                PROGFREQ    LSB4,MSB4,freq4,DIVH4,DIVL4
                return

sfreq5:
                PROGFREQ    LSB5,MSB5,freq5,DIVH5,DIVL5
                return

sfreq6:
                PROGFREQ    LSB6,MSB6,freq6,DIVH6,DIVL6
                return

sfreq7:
                PROGFREQ    LSB7,MSB7,freq7,DIVH7,DIVL7
                return

sfreq8:
                PROGFREQ    LSB8,MSB8,freq8,DIVH8,DIVL8
                return

sfreq9:
                PROGFREQ    LSB9,MSB9,freq9,DIVH9,DIVL9
                return

sfreq10:
                PROGFREQ    LSB10,MSB10,freq10,DIVH10,DIVL10
                return

sfreq11:
                PROGFREQ    LSB11,MSB11,freq11,DIVH11,DIVL11
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


; *****************************************************************************
; Write a 24-bit number contained in bin to bin+2 (big endian) on the LCD
; *****************************************************************************

WRITE2DIGITS    MACRO       BB
                swapf       BB,w
                andlw       0x0F
                btfsc       STATUS,Z
                btfsc       NOZ,0
                call        WriteNumber8
                movfw       BB
                andlw       0x0F
                btfsc       STATUS,Z
                btfsc       NOZ,0
                call        WriteNumber8
                ENDM

WriteNumber24
                clrf        NOZ
                call        b2bcd
                WRITE2DIGITS bcd
                swapf       bcd+1,w
                andlw       0x0F
                call        WriteNumber8
                movlw       '.'
                call        sendchar    ; Send the channel number
                movfw       bcd+1
                andlw       0x0F
                call        WriteNumber8
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

                ; Print the 8-bit number in w
WriteNumber8    bsf         NOZ,0
                BANKSEL     TMP_1
                movwf       TMP_1           ; Save the w register in memory
                movlw       -1
                movwf       HND
                movfw       TMP_1
sub_hnd         addlw       -0x64           ; Substract 100(10)
                incf        HND, f          ; Hundreds
                btfsc       STATUS, C       ; Verify if the result is negative
                goto        sub_hnd
                addlw       0x64            ; Add 100(b10) (to compensate the
                                            ; last subtraction)
                movwf       TMP_1           ; Save the w register in memory
                movlw       -1
                movwf       DEC
                movfw       TMP_1
sub_dec         addlw       -0xA            ; Substract 10(base10)
                incf        DEC, f
                btfsc       STATUS, C       ; Verify if the result is negative
                goto        sub_dec
                addlw       0x0A            ; Add 10(base10) (to compensate the
                                            ; last subtraction)
                movwf       UNT             ; Save the units
                bcf         TMP_1,0         ; Flag: avoid unnecessary zeros
                movfw       HND             ; Send the hundreds
                btfsc       TMP_1, 0        ; Force to write all numbers
                goto        force_HND
                btfsc       STATUS, Z       ; Skip if necessary
                goto        skip_HND
force_HND       addlw       '0'             ; Transform it in ASCII
                call        senddata
                bsf         TMP_1, 0        ; Force to write successive zeros
skip_HND        movfw       DEC             ; Send the tenths
                btfsc       TMP_1, 0        ; Force to write all numbers
                goto        force_DEC
                btfsc       STATUS, Z       ; Skip if necessary
                goto        skip_DEC
force_DEC       addlw       '0'             ; Transform it in ASCII
                call        senddata
                bsf         TMP_1, 0
skip_DEC        movfw       UNT             ; Send the units (always written)
                addlw       '0'             ; Transform it in ASCII
                goto        senddata

                ; Init the display (8 bit mode)
init
                movlw       0x30
                lgoto       sendcommand

functionset
                movlw       0x02        ; Will be interpreted as nibble mode
                                        ; the first nibble will be ignored
                                        ; the second one will be interpreted
                                        ; as 0x20 if the LCD has just been
                                        ; switched on.
                lcall        sendcommand
functionset2
                movlw       0x28            ; Two lines display setup
                goto        sendcommand

                ; Send the command in w to the display LCD configured in the
                ; nibble mode
sendbyte
                BANKSEL     DATALCD
                movwf       TMP             ; Store command temporarily in TMP
                call        portnibble
                call        pulse_e         ; Send the first half of the command
                swapf       TMP,w           ; swap nibbles of TMP, result in w
                call        portnibble
                call        pulse_e         ; Send the second half of the command
                goto        busywait

                ; Send a space to the display.
sendspace
                movlw       ' '
sendchar        ; Send character/data to the display (RS=1)
senddata        BANKSEL     DATALCD
                bsf         DATALCD,RS
                lgoto       simplesend

                ; Send a generic command to the display (RS=0)
sendcommand
                BANKSEL     DATALCD
                bcf         DATALCD,RS
simplesend      bcf         DATALCD,RW
                lgoto        sendbyte

                ; Activate the display, cursor underline blinking
displayon
                movlw       0x0C             ; Display on,
                lcall        sendcommand
                return

                ; Clear the display
displayclear
                movlw       0x01
                lgoto        sendcommand

                ; Put the cursor at home
displaychome
                movlw       0x02
                lgoto        sendcommand

sendlinespaces  movlw       0x10
                movwf       TMP_1
loopf           movlw       ' '
                call        sendchar
                decfsz      TMP_1,f
                goto        loopf
                return

clear1stline    lcall       displaychome
                call        sendlinespaces
                goto        displaychome
                
clear2ndline    lcall        display2line
                call        sendlinespaces
                goto        display2line

display2line
                movlw       0x40
                ; Set cursor address (passed in w)
displayaddrset
                iorlw       0x80
                lgoto        sendcommand

; *****************************************************************************
;               Delay routines
; *****************************************************************************

; Most of the time, the PIC will be executing this routine. It checks for
; button presses and knob action here.

activedelay
                BANKSEL     TRISA
                bsf         TRISA,RA4
                bsf         TRISA,RA5
                BANKSEL     LDR
                clrf        LDR
@loop           movfw       CHVAL          ; Skip if a button is depressed.
                btfss       STATUS,Z
                return
                ;BANKSEL     PORTA
                movfw       PORTA
                andlw       b'00010000'
                movwf       BSENSE
                call        shortdelay
                ;BANKSEL     PORTA
                movfw       PORTA
                andlw       b'00010000'
                xorwf       BSENSE,w
                btfsc       STATUS,Z
                goto        @cont
@change         btfsc       PORTA,RC4
                goto        @invpol
                btfss       PORTA,RC5
                incf        CHVAL,f
                btfsc       PORTA,RC5
                decf        CHVAL,f
                return                      ; Exit immediately
@invpol
                btfss       PORTA,RC5
                decf        CHVAL,f
                btfsc       PORTA,RC5
                incf        CHVAL,f
                return
@cont
                BANKSEL     LDR
                decfsz      LDR,f
                goto        @loop
                return

middelay
                BANKSEL     LDR
                movlw       0x40
                movwf       LDR
                call        shortdelay
                decfsz      LDR,f
                goto        $-2
                return

longdelay
                BANKSEL     LDR
                clrf        LDR
                call        shortdelay
                call        shortdelay
                decfsz      LDR,f
                goto        $-3
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
                return

; *****************************************************************************
;               Divide 32-bit over 16-bit operands
; http://www.piclist.com/techref/microchip/math/div/24by16.htm?key=
; divid3        is the LSB, divid0 is the MSB
; *****************************************************************************

div_32_16       movlw       .32         ; 32-bit div_32_16 by 16-bit
                movwf       cnt
                clrf        remdrH      ; Clear remainder
                clrf        remdrL

dvloop          clrc                    ; Set quotient bit to 0
                                        ; Shift left div_32_16nd and quotient
                rlf         divid3,f    ; lsb
                rlf         divid2,f
                rlf         divid1,f
                rlf         divid0,f    ; lsb into carry
                rlf         remdrL,f    ; and then into partial remainder
                rlf         remdrH,f

                skpnc               ; Check for overflow
                goto        subd
                movfw       divisH  ; Compare partial remainder and divisor
                subwf       remdrH,w
                skpz
                goto        testgt  ; Not equal: test if remdrH is greater
                movfw       divisL  ; High bytes equal: compare low bytes
                subwf       remdrL,w
testgt          skpc                ; Carry set if remdr >= divis
                goto        remrlt

subd            movfw       divisL  ; Subtract divisor from part. remainder
                subwf       remdrL,f
                skpc                ; Test for borrow

                decf        remdrH,f    ; Subtract borrow
                movfw       divisH
                subwf       remdrH,f
                bsf         divid3,0    ; Set quotient bit to 1
                     ; Quotient replaces div_32_16nd which is lost
remrlt          decfsz      cnt,f
                goto        dvloop
                return

; *****************************************************************************
; User-defined chars for the LCD display
; *****************************************************************************
; -----------------------------------------------------------------------------
; Second 2 k word page
; -----------------------------------------------------------------------------

                org         0x800

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
                movfw       aHH
                movwf       a6
                movfw       aHL
                movwf       a5
                movfw       aLH
                movwf       aHH
                movfw       aLL
                movwf       aHL
; End rearrange code
                clrf        aLH          ; clear partial product
                clrf        aLL
                movfw       a6
                movwf       c4
                movf        a5,W
                movwf       c3
                movf        aHH,W
                movwf       c2
                movf        aHL,W
                movwf       c1

                movlw       0x08
                movwf       bitcnt

LOOPUM3216A:
                rrf         bL, F
                btfsc       STATUS, C
                goto        ALUM3216NAP
                decfsz      bitcnt, F
                goto        LOOPUM3216A

                movwf       bitcnt

LOOPUM3216B:
                rrf         bH, F
                btfsc       STATUS, C
                goto        BLUM3216NAP
                decfsz      bitcnt, F
                goto        LOOPUM3216B

                clrf        a6
                clrf        a5
                clrf        aHH
                clrf        aHL
                retlw       0x00

BLUM3216NAP:
                BCF         STATUS, C
                goto        BLUM3216NA

ALUM3216NAP:
                BCF         STATUS, C
                goto        ALUM3216NA

ALOOPUM3216:
                rrf         bL, F
                btfss       STATUS, C
                goto        ALUM3216NA
                movf        c1,W
                addwf       aHL, F
                movf        c2,W
                btfsc       STATUS, C
                incfsz      c2,W
                addwf       aHH, F
                movf        c3,W
                btfsc       STATUS, C
                incfsz      c3,W
                addwf       a5, F
                movf        c4,W
                btfsc       STATUS, C
                incfsz      c4,W
                addwf       a6, F

ALUM3216NA:
                rrf         a6, F
                rrf         a5, F
                rrf         aHH, F
                rrf         aHL, F
                rrf         aLH, F
                decfsz      bitcnt, f
                goto        ALOOPUM3216

                movlw       0x08
                movwf       bitcnt

BLOOPUM3216:
                rrf         bH, F
                btfss       STATUS, C
                goto        BLUM3216NA
                movf        c1,W
                addwf       aHL, F
                movf        c2,W
                btfsc       STATUS, C
                incfsz      c2,W
                addwf       aHH, F
                movf        c3,W
                btfsc       STATUS, C
                incfsz      c3,W
                addwf       a5, F
                movf        c4,W
                btfsc       STATUS, C
                incfsz      c4,W
                addwf       a6, F

BLUM3216NA
                rrf         a6, F
                rrf         a5, F
                rrf         aHH, F
                rrf         aHL, F
                rrf         aLH, F
                rrf         aLL, F
                decfsz      bitcnt, F
                goto        BLOOPUM3216
                nop
                return

; *****************************************************************************



definechars
                movlw       0x40
                lcall       sendcommand
                movlw       B'00011000'
                lcall       senddata
                movlw       B'00010100'
                lcall       senddata
                movlw       B'00010100'
                lcall       senddata
                movlw       B'00011011'
                lcall       senddata
                movlw       B'00000100'
                lcall       senddata
                movlw       B'00000100'
                lcall       senddata
                movlw       B'00000011'
                lcall       senddata
                movlw       B'00000000'
                lcall       senddata
                return

InitAll         BANKSEL     ANSEL
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

                lcall        InitDisplay

                ;WRITELN     text_welcome    ; Greetings and program version.

                ;lcall        display2line    ; Move to the second line
                ;WRITELN     text_davide     ; Copyright

                clrf        FREQ            ; Standard values for frequency
                movlw       0x4
                movwf       CHVAL           ; Force setting of AD9833

                movlw       DCVALUE_NO_DC   ; Standard value for DC (no DC)
                movwf       DCVAL

                lcall       ConfigureADC
                lcall       ConfigureSPI
                lcall       ResetAD9833

                lcall        longdelay
                lcall        longdelay
                lcall        longdelay
                return

InitDisplay
                lcall       init            ; Call three times the init
                lcall       longdelay
                lcall       init
                lcall       longdelay
                lcall       init
                lcall       longdelay

                lcall       functionset
                lcall       displayon
                lcall       longdelay
                lcall       functionset
                lcall       displayon

                lcall       longdelay
                lcall       functionset
                lcall       displayon
                lcall       displayon
                lcall       displayon

                lcall       definechars

                lcall       displayclear
                lcall       displaychome
                return

ConfigureAD9833
                lcall        StartReg
                movlw       0x21            ; Control word write, reset
                lcall        WriteSPI
                movlw       0x00
                lcall        WriteSPI        ; FREQ0 register write, 14 LSB
                movfw       LSBH
                lcall        WriteSPI
                movfw       LSBL
                lcall        WriteSPI        ; FREQ0 register write, 14 MSB
                movfw       MSBH
                lcall        WriteSPI
                movfw       MSBL
                lcall        WriteSPI
                movlw       0xC0            ; PHASE0 register write
                lcall       WriteSPI
                movlw       0x00
                lcall       WriteSPI
                movlw       0x20            ; 0x2000 sine, 0x2028 square
                lcall       WriteSPI        ; 0x2002 triangle
                movlw       0x00
                lcall       WriteSPI
                lcall       StopReg
                return

; This is a test mode end of loop. Shows on the display the value of A, B and C
; (writing separately the high byte and the low byte for each).

testloop        call        writeABC
                lgoto       loop

writeABC        lcall       displayclear
                movlw       'A'
                lcall       senddata
                movfw       A_VH
                lcall       WriteNumber8
                movlw       ','
                lcall       senddata
                movfw       A_VL
                lcall       WriteNumber8
                lcall       sendspace
                movlw       'B'
                lcall       senddata
                movfw       B_VH
                lcall       WriteNumber8
                movlw       ','
                lcall       senddata
                movfw       B_VL
                lcall       WriteNumber8

                lcall       display2line  ; Move to the second line
                movlw       'C'
                lcall       senddata
                movfw       C_VH
                lcall       WriteNumber8
                movlw       ','
                lcall       senddata
                movfw       C_VL
                lcall       WriteNumber8
                lcall       sendspace
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

; Calculate the capacitance from the data contained in A, B and C and the
; frequency (constants contained in DIVH and DIVL).
; w at return contains an error code:
; 0x0 - all OK, the capacitance value is stored in CAP0, CAP1, CAP2, CAP3
; FREQLOW (0x1) - test current too low (usually frequency too low)
; FREQHI  (0x2) - the ESR affects measurement (usually frequency too high)

CalcCapacitance
                MOV16FF     divisH, divisL, A_VH, A_VL  ; Transfer A in divis
                SUB16BIT    divisH, divisL, B_VH, B_VL  ; divis -= B
                movfw       divisH          ; Check if test current is OK.
                sublw       CURR_THRESHOLD  ; (A-B) should be greater than a
                btfsc       STATUS,C        ; certain threshold for the ESR
                retlw       FREQLOW         ; to be meaningful
                btfsc       divisH,7        ; In some cases (A-B) is <0!!!
                retlw       FREQLOW

                ; C^2 = sqrt((A-C)/(B-C)-1)/(2*pi*f*R)
                MOV16FF     divid0, divid1, A_VH, A_VL  ; A-> 16 MSB divid
                clrf        divid2      ; Put 0 to divid2, divid3
                clrf        divid3
                SUB16BIT    divid0, divid1, C_VH, C_VL  ; divid -= C
                MOV16FF     divisH, divisL, B_VH, B_VL  ; Transfer B in divis
                SUB16BIT    divisH, divisL, C_VH, C_VL  ; divis -= C
                lcall       div_32_16      ; Divide! Result in divid0,1,2,3

                ; Here divid0 and divid1 contain the integer part and
                ; divid2 and divid3 contain the fraction part. In other
                ; words, 65536 represent 1.0, 32768 represent 0.5 and so on.

                ; If the ratio is greater than 2, continue. Otherwise, the ESR
                ; may affect the measurement too much.
                movlw       low  .2
                subwf       divid1,w
                movlw       high .2
                btfss       STATUS,C
                addlw       .1
                subwf       divid0,w

                btfsc       STATUS,C
                retlw       FREQHI      ; If the ratio<2, error code FREQHI

                MOV16FF     aHH,aHL,divid0,divid1
                MOV16FF     aLH,aLL,divid2,divid3
                movlw       0x1         ; Put 1 in b
                movwf       bL
                clrf        bH
                SUB16BIT    aHH, aHL, bH, bL  ; a -= 1

                movlw       high .256        ; Hi-byte of 256
                movwf       bH
                movlw       low .256         ; Lo-byte of 256
                movwf       bL
                lcall       mult_32_16
                lcall       sqrt    ; Calculate the square root
                ; Result is in aHH, aHL, aLH, aLL
                movlw       high 0x1000     ; Hi-byte of 4096
                movwf       bH
                movlw       low  0x1000     ; Lo-byte of 4096
                movwf       bL
                lcall       mult_32_16
                MOV16FF     divid0,divid1,aHH,aHL
                MOV16FF     divid2,divid3,aLH,aLL
                movfw       DIVH
                movwf       divisH
                movfw       DIVL
                movwf       divisL
                lcall       div_32_16

                ; Multiply times 1000 to show the result in uF
                MOV16FF     aHH, aHL, divid0, divid1
                MOV16FF     aLH, aLL, divid2, divid3
                movlw       high .1000  ; 1000 in decimal!
                movwf       bH
                movlw       low  .1000
                movwf       bL
                lcall       mult_32_16
                movlw       'u'
                movwf       UNIT
                MOV16FF     CAPHH, CAPHL, aHH, aHL
                MOV16FF     CAPLH, CAPLL, aLH, aLL
                retlw       0x0         ; Everything was OK.

; Try to measure automatically the capacitance and the ESR. Cycle the frequency
; starting from the lowest value until a capacitance can be read.
AutomaticMeas   movlw       0x01
                movwf       FREQ
AutomaticMeasL  clrf        CHVAL
                lcall       SelectFreq
                lcall       ReadAllADC
                lcall       clear1stline
                lcall       CalcCapacitance
                lgoto       $+1
                xorlw       0x0
                skpz
                goto        nocap1
                lcall       displaychome
                lcall       WriteCap
                lgoto       $+1
                ; Now we measure the ESR, depending on the capacity.
                ; We test CAPHH:CAPHL and the rules are as follows:
                ; if C< 80 nF, ESR is not measured.
                movlw       low  0x000C
                subwf       CAPHL,w
                movlw       high 0x000C
                btfss       STATUS,C
                addlw       .1
                subwf       CAPHH,w
                btfss       STATUS,C
                goto        noESR
                ; if  80 nF < C <= 500 nF, ESR is measured at f=200 kHz
                movlw       low  0x004C
                subwf       CAPHL,w
                movlw       high 0x004C
                btfss       STATUS,C
                addlw       .1
                subwf       CAPHH,w
                btfss       STATUS,C
                goto        meas200
                ; if 500 nF < C <= 2 µF, ESR is measured at f=100 kHz
                movlw       low  0x0131
                subwf       CAPHL,w
                movlw       high 0x0131
                btfss       STATUS,C
                addlw       .1
                subwf       CAPHH,w
                btfss       STATUS,C
                goto        meas100
                ; if 2µF < C <= 80 µF, ESR is measured at f=20 kHz
                movlw       low  0x2FAF
                subwf       CAPHL,w
                movlw       high 0x2FAF
                btfss       STATUS,C
                addlw       .1
                subwf       CAPHH,w
                btfss       STATUS,C
                goto        meas20
                ; if C > 80 µF, ESR is measured at f = 10 kHz
                goto        meas10

meas200         movfw       FREQ
                movwf       USR
                movlw       .11
                movwf       FREQ
                lcall       SelectFreq
                lcall       ReadAllADC
                lcall       CalcESR
                movfw       USR
                movwf       FREQ
                lgoto       AutomaticMeasL

meas100         movfw       FREQ
                movwf       USR
                movlw       .10
                movwf       FREQ
                lcall       SelectFreq
                lcall       ReadAllADC
                lcall       CalcESR
                movfw       USR
                movwf       FREQ
                lgoto       AutomaticMeasL

meas20          movfw       FREQ
                movwf       USR
                movlw       .8
                movwf       FREQ
                lcall       SelectFreq
                lcall       ReadAllADC
                lcall       CalcESR
                movfw       USR
                movwf       FREQ
                lgoto       AutomaticMeasL

meas10          movfw       FREQ
                movwf       USR
                movlw       .7
                movwf       FREQ
                lcall       SelectFreq
                lcall       ReadAllADC
                lcall       CalcESR
                movfw       USR
                movwf       FREQ
                lgoto       AutomaticMeasL

noESR           lcall       clear2ndline
                lgoto       AutomaticMeasL


nocap1          lcall       clear2ndline
                incf        FREQ,f
                movfw       FREQ
                xorlw       .12
                skpnz
                clrf        FREQ
                lgoto       AutomaticMeasL


;*** 32 BIT SQUARE ROOT ***
;sqrt(a) -> a
;Return carry set if negative
; SOURCE: http://www.massmind.org/techref/microchip/math/32bmath-ph.htm

sqrt            rlf             aHH,w        ;Trap negative values
                skpnc
                return
                call            movac           ;Move REGA to REGC
                call            clrba           ;Clear remainder (REGB) and root (REGA)
                movlw           D'16'           ;Loop counter
                movwf           MCOUNT

sqloop          rlf             REGCLL,f        ;Shift two msb's
                rlf             REGCLH,f        ;into remainder
                rlf             REGCHL,f
                rlf             REGCHH,f
                rlf             REGBLL,f
                rlf             REGBLH,f
                rlf             REGBHL,f
                rlf             REGCLL,f
                rlf             REGCLH,f
                rlf             REGCHL,f
                rlf             REGCHH,f
                rlf             REGBLL,f
                rlf             REGBLH,f
                rlf             REGBHL,f

                setc                            ;Add 1 to root
                rlf             aLL,f        ;Align root
                rlf             aLH,f
                rlf             aHL,f

                movf            aHL,w        ;Test if remdr (REGB) >= root (REGA)
                subwf           REGBHL,w
                skpz
                goto            ststgt
                movf            aLH,w
                subwf           REGBLH,w
                skpz
                goto            ststgt
                movf            aLL,w
                subwf           REGBLL,w
ststgt          skpc                            ;Carry set if remdr >= root
                goto            sremlt

                movf            aLL,w        ;Subtract root (REGA) from remdr (REGB)
                subwf           REGBLL,f
                movf            aLH,w
                skpc
                incfsz          aLH,w
                subwf           REGBLH,f
                movf            aHL,w
                skpc
                incfsz          aHL,w
                subwf           REGBHL,f
                bsf             aLL,1        ;Set current root bit

sremlt          bcf             aLL,0        ;Clear test bit
                decfsz          MCOUNT,f        ;Next
                goto            sqloop

                clrc
                rrf             aHL,f        ;Adjust root alignment
                rrf             aLH,f
                rrf             aLL,f
                return

;Move REGA to REGC
;Used by multiply, sqrt

movac           movf            aLL,w
                movwf           REGCLL
                movf            aLH,w
                movwf           REGCLH
                movf            aHL,w
                movwf           REGCHL
                movf            aHH,w
                movwf           REGCHH
                return


;Clear REGB and REGA
;Used by sqrt

clrba           clrf            REGBLL
                clrf            REGBLH
                clrf            REGBHL
                clrf            REGBHH

;Clear REGA
;Used by multiply, sqrt

clra            clrf            aLL
                clrf            aLH
                clrf            aHL
                clrf            aHH
                return
                end