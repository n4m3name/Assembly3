;
; a3part-B.asm
;
; Part B of assignment #3
;
;
; Student name:
; Student ID:
; Date of completed work:
;
; **********************************
; Code provided for Assignment #3
;
; Author: Mike Zastre (2022-Nov-05)
;
; This skeleton of an assembly-language program is provided to help you 
; begin with the programming tasks for A#3. As with A#2 and A#1, there are
; "DO NOT TOUCH" sections. You are *not* to modify the lines within these
; sections. The only exceptions are for specific changes announced on
; Brightspace or in written permission from the course instruction.
; *** Unapproved changes could result in incorrect code execution
; during assignment evaluation, along with an assignment grade of zero. ***
;


; =============================================
; ==== BEGINNING OF "DO NOT TOUCH" SECTION ====
; =============================================
;
; In this "DO NOT TOUCH" section are:
; 
; (1) assembler direction setting up the interrupt-vector table
;
; (2) "includes" for the LCD display
;
; (3) some definitions of constants that may be used later in
;     the program
;
; (4) code for initial setup of the Analog-to-Digital Converter
;     (in the same manner in which it was set up for Lab #4)
;
; (5) Code for setting up three timers (timers 1, 3, and 4).
;
; After all this initial code, your own solutions's code may start
;

.cseg
.org 0
	jmp reset

; Actual .org details for this an other interrupt vectors can be
; obtained from main ATmega2560 data sheet
;
.org 0x22
	jmp timer1

; This included for completeness. Because timer3 is used to
; drive updates of the LCD display, and because LCD routines
; *cannot* be called from within an interrupt handler, we
; will need to use a polling loop for timer3.
;
; .org 0x40
;	jmp timer3

.org 0x54
	jmp timer4

.include "m2560def.inc"
.include "lcd.asm"

.cseg
#define CLOCK 16.0e6
#define DELAY1 0.01
#define DELAY3 0.1
#define DELAY4 0.5

#define BUTTON_RIGHT_MASK 0b00000001	
#define BUTTON_UP_MASK    0b00000010
#define BUTTON_DOWN_MASK  0b00000100
#define BUTTON_LEFT_MASK  0b00001000

#define BUTTON_RIGHT_ADC  0x032
#define BUTTON_UP_ADC     0x0b0   ; was 0x0c3
#define BUTTON_DOWN_ADC   0x160   ; was 0x17c
#define BUTTON_LEFT_ADC   0x22b
#define BUTTON_SELECT_ADC 0x316

.equ PRESCALE_DIV=1024   ; w.r.t. clock, CS[2:0] = 0b101

; TIMER1 is a 16-bit timer. If the Output Compare value is
; larger than what can be stored in 16 bits, then either
; the PRESCALE needs to be larger, or the DELAY has to be
; shorter, or both.
.equ TOP1=int(0.5+(CLOCK/PRESCALE_DIV*DELAY1))
.if TOP1>65535
.error "TOP1 is out of range"
.endif

; TIMER3 is a 16-bit timer. If the Output Compare value is
; larger than what can be stored in 16 bits, then either
; the PRESCALE needs to be larger, or the DELAY has to be
; shorter, or both.
.equ TOP3=int(0.5+(CLOCK/PRESCALE_DIV*DELAY3))
.if TOP3>65535
.error "TOP3 is out of range"
.endif

; TIMER4 is a 16-bit timer. If the Output Compare value is
; larger than what can be stored in 16 bits, then either
; the PRESCALE needs to be larger, or the DELAY has to be
; shorter, or both.
.equ TOP4=int(0.5+(CLOCK/PRESCALE_DIV*DELAY4))
.if TOP4>65535
.error "TOP4 is out of range"
.endif

reset:
; ***************************************************
; **** BEGINNING OF FIRST "STUDENT CODE" SECTION ****
; ***************************************************

; Anything that needs initialization before interrupts
; start must be placed here.

	; Stack init
	ldi r16, low(RAMEND)
	out SPL, r16
	ldi r16, high(RAMEND)
	out SPH, r16

	; Definitions for using the Analog to Digital Conversion
	.equ ADCSRA_BTN=0x7A
	.equ ADCSRB_BTN=0x7B
	.equ ADMUX_BTN=0x7C
	.equ ADCL_BTN=0x78
	.equ ADCH_BTN=0x79

	; Variables/registers
	.def DATAH=r25  ;DATAH:DATAL  store 10 bits data from ADC
	.def DATAL=r24
	.def BOUNDARY_H=r1  ;hold byte value of the threshold for button
	.def BOUNDARY_L=r0

	; LCD init
	rcall lcd_init

; ***************************************************
; ******* END OF FIRST "STUDENT CODE" SECTION *******
; ***************************************************

; =============================================
; ====  START OF "DO NOT TOUCH" SECTION    ====
; =============================================

	; initialize the ADC converter (which is needed
	; to read buttons on shield). Note that we'll
	; use the interrupt handler for timer 1 to
	; read the buttons (i.e., every 10 ms)
	;
	ldi temp, (1 << ADEN) | (1 << ADPS2) | (1 << ADPS1) | (1 << ADPS0)
	sts ADCSRA, temp
	ldi temp, (1 << REFS0)
	sts ADMUX, r16

	; Timer 1 is for sampling the buttons at 10 ms intervals.
	; We will use an interrupt handler for this timer.
	ldi r17, high(TOP1)
	ldi r16, low(TOP1)
	sts OCR1AH, r17
	sts OCR1AL, r16
	clr r16
	sts TCCR1A, r16
	ldi r16, (1 << WGM12) | (1 << CS12) | (1 << CS10)
	sts TCCR1B, r16
	ldi r16, (1 << OCIE1A)
	sts TIMSK1, r16

	; Timer 3 is for updating the LCD display. We are
	; *not* able to call LCD routines from within an 
	; interrupt handler, so this timer must be used
	; in a polling loop.
	ldi r17, high(TOP3)
	ldi r16, low(TOP3)
	sts OCR3AH, r17
	sts OCR3AL, r16
	clr r16
	sts TCCR3A, r16
	ldi r16, (1 << WGM32) | (1 << CS32) | (1 << CS30)
	sts TCCR3B, r16
	; Notice that the code for enabling the Timer 3
	; interrupt is missing at this point.

	; Timer 4 is for updating the contents to be displayed
	; on the top line of the LCD.
	ldi r17, high(TOP4)
	ldi r16, low(TOP4)
	sts OCR4AH, r17
	sts OCR4AL, r16
	clr r16
	sts TCCR4A, r16
	ldi r16, (1 << WGM42) | (1 << CS42) | (1 << CS40)
	sts TCCR4B, r16
	ldi r16, (1 << OCIE4A)
	sts TIMSK4, r16

	sei

; =============================================
; ====    END OF "DO NOT TOUCH" SECTION    ====
; =============================================

; ****************************************************
; **** BEGINNING OF SECOND "STUDENT CODE" SECTION ****
; ****************************************************

; Note: Comments only cover code new to part B

start:
	in temp, TIFR3
	sbrs temp, OCF3A
	rjmp start
	ldi temp, 1<<OCF3A
	out TIFR3, temp

	update_lcd:
		push temp
		lds temp, BUTTON_IS_PRESSED
		cpi temp, 1
		breq test_button

	dash:
		ldi r16, 1
		ldi r17, 15
		rcall set_loc
		ldi r16, '-'
		rcall letter
		pop temp
		rjmp start

	; Added for part B: LAST_BUTTON_PRESSED stores 1=Right, 2=Up, 3=Down, 4=Left, depending on timer 1 cutoffs.
	; Timer 3 now displays corresponding char (R,U,L,D) alomgside the '*' when BUTTON_IS_PRESSED=1.
	test_button:
		; Load val in LAST_BUTTON_PRESSED
		lds r18, LAST_BUTTON_PRESSED
		cpi r18, 1
		breq right ; If 1, set right
		cpi r18, 2
		breq up; If 2, set up
		cpi r18, 3
		breq down; If 3, set down
		cpi r18, 4
		breq left; If 4, set left
		pop r18
		pop temp
		rjmp start; Else loop

	; Displays 'R' in 0th column of 1st row LCD
	; up, down, left contain same code except in 1st, 2nd, 3rd column respectively
	right:
		rcall star; Display star
		ldi r16, 1; Row
		ldi r17, 0; Column
		rcall set_loc; Sets loactioon to (1,0)
		ldi r16, 'R'
		rcall letter; Display 'R'
		rcall space; Display space in following column
		rcall space; Display space in following column
		rcall space; Display space in following column
		pop temp
		rjmp start; Loop

	up:
		rcall star
		ldi r16, 1
		ldi r17, 0
		rcall set_loc
		rcall space; Displays space in (1,0)
		ldi r16, 'U'; U i (1,1)
		rcall letter; ...
		rcall space
		rcall space
		rjmp start

	down:
		rcall star
		ldi r16, 1
		ldi r17, 0
		rcall set_loc
		rcall space
		rcall space
		ldi r16, 'D'
		rcall letter
		rcall space
		rjmp start

	left:
		rcall star
		ldi r16, 1
		ldi r17, 0
		rcall set_loc
		rcall space
		rcall space
		rcall space
		ldi r16, 'L'
		rcall letter
		pop temp
		rjmp start

	; Displays star in (1,15) when called
	star:
		ldi r16, 1
		ldi r17, 15
		rcall set_loc; ...
		ldi r16, '*'
		rcall letter; ...
		ret

	set_loc:
		push r16
		push r17
		rcall lcd_gotoxy
		pop r17
		pop r16 
		ret

	letter:
		push r16
		rcall lcd_putchar
		pop r16
		ret

	space:
		ldi r16, ' '
		push r16
		rcall lcd_putchar
		pop r16
		ret

	; Unnecessary
	stop:
		rjmp stop


; Added functionality: Check cutoffs (low to high) of each individual button
; to determine which button is being pressed
timer1:
	push r16
	lds r16, SREG
	push r16
	rcall check_button
	pop r16
	sts SREG, r16
	pop r16
	reti

	check_button:
		lds	r16, ADCSRA_BTN
		ori r16, 0x40
		sts	ADCSRA_BTN, r16

	wait:
		lds r16, ADCSRA_BTN
		andi r16, 0x40
		brne wait

		; Clear registers used
		clr r23
		clr r24

	; Checks if right button pressed based off of given boundary BUTTON_RIGHT_ADC
	check_right:
		; If DATA < BOUNDARY, set BUTTON_IS_PRESSED
		; Load BUTTON_<LOCN>_ADC int BOUNDARY.
		; Note: check_up, check_down, check_left follow same logic except with BUTTON_<direction>_ADC as boundary.
		; Since each button has a lower value than the next, we check consecutively from lo to hi to rule out buttons one-by-one.
		ldi r16, low(BUTTON_RIGHT_ADC)
		mov BOUNDARY_L, r16
		ldi r16, high(BUTTON_RIGHT_ADC)
		mov BOUNDARY_H, r16
		; Load button val into DATA
		lds DATAL, ADCL_BTN
		lds DATAH, ADCH_BTN
		; Compare DATA, BOUNDARY
		cp DATAL, BOUNDARY_L
		cpc DATAH, BOUNDARY_H
		; if DATA<BOUNDARY, ...
		brlo set_right
		rjmp check_up
	; set BUTTON_IS_PRESSED=1 and LAST_BUTTON_PRESSED with corresponding value (In this case, right = 1)
	set_right:
		ldi r23, 1
		ldi r24, 1
		rjmp skip

	; Repeat same logic as above with new cutoff, new corresponding value
	check_up:
		ldi r16, low(BUTTON_UP_ADC)
		mov BOUNDARY_L, r16
		ldi r16, high(BUTTON_UP_ADC)
		mov BOUNDARY_H, r16
		lds DATAL, ADCL_BTN
		lds DATAH, ADCH_BTN
		cp DATAL, BOUNDARY_L
		cpc DATAH, BOUNDARY_H
		brsh check_down
	set_up:
		ldi r23, 1
		ldi r24, 2
		rjmp skip

	check_down:
		ldi r16, low(BUTTON_DOWN_ADC)
		mov BOUNDARY_L, r16
		ldi r16, high(BUTTON_DOWN_ADC)
		mov BOUNDARY_H, r16
		lds DATAL, ADCL_BTN
		lds DATAH, ADCH_BTN
		cp DATAL, BOUNDARY_L
		cpc DATAH, BOUNDARY_H
		brsh check_left
	set_down:
		ldi r23, 1
		ldi r24, 3
		rjmp skip

	check_left:
		ldi r16, low(BUTTON_LEFT_ADC)
		mov BOUNDARY_L, r16
		ldi r16, high(BUTTON_LEFT_ADC)
		mov BOUNDARY_H, r16
		lds DATAL, ADCL_BTN
		lds DATAH, ADCH_BTN
		cp DATAL, BOUNDARY_L
		cpc DATAH, BOUNDARY_H
		brsh skip; Reached end of cutoff, thus no button pressed
	set_left:
		ldi r23, 1
		ldi r24, 4
		rjmp skip

	; Load values into BUTTON_IS_PRESSED, LAST_BUTTON_PRESSED, end interrupt handler.
	skip:	
		sts BUTTON_IS_PRESSED, r23
		sts LAST_BUTTON_PRESSED, r24
		ret

; Unused
timer4:
	reti


; ****************************************************
; ******* END OF SECOND "STUDENT CODE" SECTION *******
; ****************************************************


; =============================================
; ==== BEGINNING OF "DO NOT TOUCH" SECTION ====
; =============================================

; r17:r16 -- word 1
; r19:r18 -- word 2
; word 1 < word 2? return -1 in r25
; word 1 > word 2? return 1 in r25
; word 1 == word 2? return 0 in r25
;
compare_words:
	; if high bytes are different, look at lower bytes
	cp r17, r19
	breq compare_words_lower_byte

	; since high bytes are different, use these to
	; determine result
	;
	; if C is set from previous cp, it means r17 < r19
	; 
	; preload r25 with 1 with the assume r17 > r19
	ldi r25, 1
	brcs compare_words_is_less_than
	rjmp compare_words_exit

compare_words_is_less_than:
	ldi r25, -1
	rjmp compare_words_exit

compare_words_lower_byte:
	clr r25
	cp r16, r18
	breq compare_words_exit

	ldi r25, 1
	brcs compare_words_is_less_than  ; re-use what we already wrote...

compare_words_exit:
	ret

.cseg
AVAILABLE_CHARSET: .db "0123456789abcdef_", 0


.dseg

BUTTON_IS_PRESSED: .byte 1			; updated by timer1 interrupt, used by LCD update loop
LAST_BUTTON_PRESSED: .byte 1        ; updated by timer1 interrupt, used by LCD update loop

TOP_LINE_CONTENT: .byte 16			; updated by timer4 interrupt, used by LCD update loop
CURRENT_CHARSET_INDEX: .byte 16		; updated by timer4 interrupt, used by LCD update loop
CURRENT_CHAR_INDEX: .byte 1			; ; updated by timer4 interrupt, used by LCD update loop


; =============================================
; ======= END OF "DO NOT TOUCH" SECTION =======
; =============================================


; ***************************************************
; **** BEGINNING OF THIRD "STUDENT CODE" SECTION ****
; ***************************************************

.dseg

; If you should need additional memory for storage of state,
; then place it within the section. However, the items here
; must not be simply a way to replace or ignore the memory
; locations provided up above.


; ***************************************************
; ******* END OF THIRD "STUDENT CODE" SECTION *******
; ***************************************************
