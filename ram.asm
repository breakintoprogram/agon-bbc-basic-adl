;
; Title:	BBC Basic Interpreter - Z80 version
;		RAM Module for BBC Basic Interpreter
;		For use with Version 2.0 of BBC BASIC
;		Standard CP/M Distribution Version
; Author:	(C) Copyright  R.T.Russell 31-12-1983
; Modified By:	Dean Belfield
; Created:	12/05/2023
; Last Updated:	26/06/2023
;
; Modinfo:
; 06/06/2023:	Modified to run in ADL mode
; 26/06/2023:	Added temporary stores R0 and R1

			.ASSUME	ADL = 1

			DEFINE	LORAM, SPACE = ROM
			SEGMENT LORAM

			XDEF	ACCS
			XDEF	BUFFER
			XDEF	STAVAR
			XDEF	DYNVAR
			XDEF	FNPTR
			XDEF	PROPTR
			XDEF	PAGE_
			XDEF	TOP
			XDEF	LOMEM
			XDEF 	FREE
			XDEF	HIMEM
			XDEF	LINENO
			XDEF	TRACEN
			XDEF	AUTONO
			XDEF	ERRTRP
			XDEF	ERRTXT
			XDEF	DATPTR
			XDEF	ERL
			XDEF	ERRLIN
			XDEF	RANDOM
			XDEF	COUNT
			XDEF	WIDTH
			XDEF	ERR
			XDEF	LISTON
			XDEF	INCREM
			
			XDEF	FLAGS
			XDEF	OSWRCHPT
			XDEF	OSWRCHCH
			XDEF	OSWRCHFH
			XDEF	KEYDOWN 
			XDEF	KEYASCII
			XDEF	KEYCOUNT

			XDEF	R0
			XDEF	R1
			
			XDEF	RAM_START
			XDEF	RAM_END
			XDEF	USER

			ALIGN 		256		; ACCS, BUFFER & STAVAR must be on page boundaries			
RAM_START:		
;
ACCS:			DS		256             ; String Accumulator
BUFFER:			DS		256             ; String Input Buffer
STAVAR:			DS	 	27*4            ; Static Variables
DYNVAR: 		DS 		54*3            ; Dynamic Variable Pointers
FNPTR:  		DS    		3               ; Dynamic Function Pointers
PROPTR: 		DS		3               ; Dynamic Procedure Pointers
;
PAGE_:   		DS		3               ; Start of User Program
TOP:    		DS		3               ; First Location after User Program
LOMEM:  		DS		3               ; Start of Dynamic Storage
FREE:   		DS		3               ; First Free Space Byte
HIMEM:  		DS		3               ; First Protected Byte
;
LINENO: 		DS		3               ; Line Number
TRACEN:			DS		3               ; Trace Flag
AUTONO:			DS		3               ; Auto Flag
ERRTRP:			DS		3               ; Error Trap
ERRTXT:			DS		2               ; Error Message Pointer
DATPTR:			DS		2               ; Data Pointer
ERL:			DS		2               ; Error Line
ERRLIN:			DS		3               ; The "ON ERROR" Line
RANDOM:			DS		5               ; Random Number
COUNT:			DS		1               ; Print Position
WIDTH:			DS		1               ; Print Width
ERR:			DS		1               ; Error Number
LISTON:			DS		1               ; LISTO (bottom nibble)
							; - BIT 0: If set, output a space after the line number
							; - BIT 1: If set, then indent FOR/NEXT loops
							; - BIT 2: If set, then indent REPEAT/UNTIL loops
							; - BIT 3: If set, then output to buffer for *EDIT
							; OPT FLAG (top nibble)
							; - BIT 4: If set, then list whilst assembling
							; - BIT 5: If set, then assembler errors are reported
							; - BIT 6: If set, then place the code starting at address pointed to by O%
							; - BIT 7: If set, then assemble in ADL mode, otherwise assemble in Z80 mode
INCREM:			DS		1               ; Auto-Increment Value
;
; Extra Agon-implementation specific system variables
;
FLAGS:			DS		1		; Miscellaneous flags
							; - BIT 7: Set if ESC pressed
							; - BIT 6: Set to disable ESC
OSWRCHPT:		DS		2		; Pointer for *EDIT
OSWRCHCH:		DS		1		; Channel of OSWRCH
							; - 0: Console
							; - 1: File
OSWRCHFH:		DS		1		; File handle for OSWRCHCHN
KEYDOWN:		DS		1		; Keydown flag
KEYASCII:		DS		1		; ASCII code of pressed key
KEYCOUNT:		DS		1		; Counts every time a key is pressed
R0:			DS		3		; General purpose storage for 8/16 to 24 bit operations
R1:			DS		3		; 
;
; This must be at the end
;
RAM_END:		
			ALIGN	256			
USER:							; Must be aligned on a page boundary
	