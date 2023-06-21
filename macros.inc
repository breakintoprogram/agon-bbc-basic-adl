;
; Title:	BBC Basic Interpreter - Z80 version
;		Useful macros
; Author:	Dean Belfield
; Created:	12/05/2023
; Last Updated:	11/06/2023
;
; Modinfo:
; 11/06/2023:	Modified to run in ADL mode

EXREG:			MACRO	rp1, rp2
			PUSH 	rp1 
			POP	rp2
			ENDMACRO 
	
ADD8U_DE:		MACRO 	reg
			ADD	A, E 
			LD	E, A 
			ADC	A, D
			SUB	E
			LD	D, A 
			ENDMACRO 

ADD8U_HL:		MACRO 	reg
			ADD	A, L 
			LD	L, A 
			ADC	A, H
			SUB	L
			LD	H, A 
			ENDMACRO 

VDU:			MACRO	val
			LD	A, val
			CALL	OSWRCH
			ENDMACRO

SET_GPIO:		MACRO	reg, val
			IN0	A,(reg)
			OR	val
			OUT0	(reg),A
			ENDMACRO
				
RES_GPIO:		MACRO	reg, val
			PUSH	BC
			LD	A, val
			CPL
			LD	C, A
			IN0	A,(reg)
			AND	C
			OUT0	(reg),A
			POP	BC
			ENDMACRO			