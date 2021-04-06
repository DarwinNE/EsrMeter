; *****************************************************************************
;               esr.asm
;       A simple ESR measuring system exploiting lock-in techniques
;       Davide Bucci, 2021
;       Version 1.0
; *****************************************************************************
;
; License:
; --------
;
;    Copyright (C) 2021  Davide Bucci  davbucciPleaseNoSpamHerE@tiscali.it
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
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

#include <p16F690.inc>
   __config (_INTRC_OSC_NOCLKOUT & _WDT_OFF & _PWRTE_OFF & _MCLRE_OFF & _CP_OFF & _BOD_OFF & _IESO_OFF & _FCMEN_OFF)

; Constant
OSC_LOTHRESHOLD equ     .100    ; Error if the ampl. A_VH is lower than this.
OSC_HITHRESHOLD equ     .230    ; Error if the ampl. A_VH is higher than this.

; Control lines of the display device
E               equ     01
RS              equ     02
RW              equ     03

; Control port used for LCD display
DATALCD         equ     PORTC
TRISLCD         equ     TRISC

; These registers must be in the same page of the DATALCD register
TMP             equ     20      ; Dummy for LCD nibble mode
CNT             equ     21      ; Counter
SDR             equ     22      ; Short Delay Register
LDR             equ     23      ; Long Delay Register

TMP_1           equ     2A
HND             equ     2B
DEC             equ     2C
UNT             equ     2D

; Used for the divide operator: the two operands, the result and the remainder
divid0          equ     30      ; Most significant byte
divid1          equ     31
divid2          equ     32
divid3          equ     33      ; Least significant byte

divisH          equ     34
divisL          equ     35

remdrH          equ     36
remdrL          equ     37

; aHH:aHL:aLH:aLL*bH:bL -> a6:a5:aHH:aHL:aLH:aLL

aLL             equ 40
aLH             equ 41
aHL             equ 42
aHH             equ 43
a5              equ 44
a6              equ 45

bL              equ 46
bH              equ 47

c1              equ 48
c2              equ 49
c3              equ 4A
c4              equ 4B

bitcnt          equ 4C

bcd             equ     50
; memory used up to bcd+4 included
cnt             equ     55
ii              equ     56
bin             equ     59
; memory used up to bin+3
store           equ     61

indf            equ     0
fsr             equ     4

LOOPCOUNT       equ     3C
NOZ             equ     60

; Configuration for the measurement control
CTRLP           equ     PORTB
TRISCTRL        equ     TRISB

; The value of the ESR is calculated as
; 10*(V_B-V_C)/(V_A-V_B)
CTRLA           equ     4
CTRLB           equ     5
CTRLC           equ     6

A_VH            equ     70
A_VL            equ     71

B_VH            equ     72
B_VL            equ     73

C_VH            equ     74
C_VL            equ     75

STOREH          equ     76
STOREL          equ     77

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

; *****************************************************************************
;               Main Program
; *****************************************************************************
                org         0000
                goto        prg
                nop
                nop
                nop
                org         0004
                goto        prg

prg
                BANKSEL     TRISLCD
                clrf        TRISLCD         ; Display ports as outputs
                BANKSEL     ANSEL
                clrf        ANSEL
                clrf        ANSELH
                BANKSEL     DATALCD
                call        longdelay
                call        longdelay
                call        longdelay
                call        longdelay
                call        functionset
                call        displayon
                call        displayclear
                call        displaychome

                WRITELN     text_welcome    ; Greetings and program version.
                movlw       0x40
                call        displayaddrset  ; Move to the second line
                WRITELN     text_davide     ; Copyright
                call        longdelay
                call        longdelay
                call        longdelay
                BANKSEL     TRISCTRL
                clrf        TRISCTRL
                BANKSEL     SSPCON
                clrf        SSPCON
                call        ConfigureAll

                clrf        store
                clrf        store+1
                clrf        store+2
                clrf        store+3

                ; Main program loop: read ADC values, calculate ESR, repeat.
loop
                READV       CTRLA, A_VH, A_VL ; read A, B and C
                READV       CTRLB, B_VH, B_VL
                READV       CTRLC, C_VH, C_VL

                call        displayclear
                call        output1l    ; Write the first line (debug)

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
                ADD32BIT    store, store+1, store+2, store+3, aHH, aHL, aLH, aLL
                DIV2O32BIT  store, store+1, store+2, store+3    ; store /=2;

                MOV16FF     bin+0, bin+1, store+0, store+1 ; bin=store (32 bit)
                MOV16FF     bin+2, bin+3, store+2, store+3

                call        output2l    ; Write the second line (ESR result)
                goto        loop

err_lowosc      WRITELN     text_lowosc
                call        longdelay
                goto        loop

err_hiosc       WRITELN     text_hiosc
                call        longdelay
                goto        loop

err_reshi       WRITELN     text_reshi
                call        longdelay
                goto        loop

readadc
                call        longdelay
                clrf        STOREH
                clrf        STOREL
                movlw       0x10
                movwf       CNT
llo
                BANKSEL     ADCON0
                bsf         ADCON0,GO
                btfsc       ADCON0,GO
                goto        $-1         ; Wait until conversion is complete
                ADD16BIT    STOREH, STOREL, ADRESH, ADRESL
                decfsz      CNT,f
                goto llo

                return

checkamplitudes movfw       A_VH        ; Check if the oscillator amplitude is OK.
                sublw       OSC_LOTHRESHOLD
                ;btfsc      STATUS,C
                ;goto       err_lowosc

                movfw       A_VH
                sublw       OSC_HITHRESHOLD
                ;btfss      STATUS,C
                ;goto       err_hiosc
                return

output1l        movfw       A_VH
                call        write_number
                call        sendspace
                movfw       A_VL
                call        write_number
                call        sendspace

                movfw       B_VH
                call        write_number
                call        sendspace
                movfw       B_VL
                call        write_number
                call        sendspace

                movfw       C_VH
                call        write_number
                call        sendspace
                movfw       C_VL
                call        write_number
                call        sendspace
                return

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

ConfigureAll    ; Configure the ADC, read on A0.
                BANKSEL     TRISA
                bsf         TRISA,0
                BANKSEL     ANSEL
                bsf         ANSEL,0     ; Use as A0 as an analog input.
                BANKSEL     ADCON1
                bcf         ADCON1,4    ; /32 ADC frequency
                bsf         ADCON1,5
                bcf         ADCON1,6
                BANKSEL     ADCON0
                bsf         ADCON0,ADFM ; Left justified ADC result.
                bsf         ADCON0,VCFG ; Vref voltage reference.
                bcf         ADCON0,5    ; Set input in channel 0.
                bcf         ADCON0,4
                bcf         ADCON0,3
                bcf         ADCON0,2
                BANKSEL     TRISC
                clrf        TRISC
                return

; *****************************************************************************
; Write a 24-bit number contained in bin to bin+3 (big endian) on the LCD
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
                movlw   .32              ; 32-bits
                movwf   ii              ; make cycle counter
                clrf    bcd             ; clear result area
                clrf    bcd+1
                clrf    bcd+2
                clrf    bcd+3
                clrf    bcd+4

b2bcd2          movlw   bcd             ; make pointer
                movwf   fsr
                movlw   5
                movwf   cnt

; Mike's routine:

b2bcd3          movlw   0x33
                addwf   indf,f          ; add to both nybbles
                btfsc   indf,3          ; test if low result > 7
                andlw   0xf0            ; low result >7 so take the 3 out
                btfsc   indf,7          ; test if high result > 7
                andlw   0x0f            ; high result > 7 so ok
                subwf   indf,f          ; any results <= 7, subtract back
                incf    fsr,f           ; point to next
                decfsz  cnt,f
                goto    b2bcd3

                rlf     bin+3,f         ; get another bit
                rlf     bin+2,f
                rlf     bin+1,f
                rlf     bin+0,f
                rlf     bcd+4,f         ; put it into bcd
                rlf     bcd+3,f
                rlf     bcd+2,f
                rlf     bcd+1,f
                rlf     bcd+0,f
                decfsz  ii,f            ; all done?
                goto    b2bcd2          ; no, loop
                return                  ; yes



; *****************************************************************************
;               LCD Subroutines
; *****************************************************************************
sendspace
                movlw   ' '
sendchar
                bsf     DATALCD,RS      ; RS to 1: we are sending a character
                bcf     DATALCD,RW      ; RW to 0: we are writing to display
                call    sendbyte        ; Send the channel number
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
                movlw   0x20            ; Nibble mode of the LCD
                movwf   DATALCD
                call    pulse_e
                call    busywait
functionset2    bcf     DATALCD,RS
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
                movlw   0x0D            ; Display on, increment, blink off
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
longdelay
                BANKSEL LDR
                call    shortdelay
                decfsz  LDR,f
                goto    longdelay
                return

middelay
                BANKSEL LDR
                movlw   0x40
                movwf   LDR
                call    shortdelay
                decfsz  LDR,f
                goto    $-2
                return

shortdelay
                BANKSEL SDR
                decfsz  SDR,f
                goto    $-1
                return

busywait
                BANKSEL TRISLCD
                movlw   0xF0            ; Set all the port (4 bits) as inputs
                movwf   TRISLCD
                BANKSEL DATALCD
                bcf     DATALCD, RS     ; RS to 0
                bsf     DATALCD, RW     ; RW to 1: Read
                nop
                bsf     DATALCD, E      ; Raise E line
                nop
                rlf     DATALCD, w      ; Rotate the busy flag in the carry
                bcf     DATALCD, E
                nop
                nop
                bsf     DATALCD, E      ; Raise E line
                nop
                nop
                nop
                bcf     DATALCD, E
                btfsc   STATUS, C       ; Test the carry
                goto    busywait        ; If busy, continue waiting
                BANKSEL TRISLCD
                movlw   0x00            ; Set all the port (4 bits) as outputs
                movwf   TRISLCD
                BANKSEL DATALCD
                bcf     DATALCD, RW     ; RW to 1: Read
                return

pulse_e
                BANKSEL DATALCD
                bsf     DATALCD,E
                nop
                nop
                nop
                bcf     DATALCD,E
                return

portnibble      andlw   0xF0            ; clear the lower 4 bits of w
                BANKSEL DATALCD
                iorwf   DATALCD, f      ; or this with port B
                iorlw   0x0F            ; set lower 4 bits of w
                andwf   DATALCD, f      ; and this with port B
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
;               Text tables
; *****************************************************************************

text_welcome    addwf   PCL,f
                DT      "Welcome ESR  1.0",0
text_davide     addwf   PCL,f
                DT      "D. Bucci 2021   ",0
text_lowosc     addwf   PCL,f
                DT      "ERROR: Low osc. ",0
text_hiosc      addwf   PCL,f
                DT      "ERROR: High osc.",0
text_reshi      addwf   PCL,f
                DT      "Resistance high.",0
text_esr        addwf   PCL,f
                DT      "ESR = ",0
                end