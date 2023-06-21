;
; Title:	BBC Basic for AGON - Interrupts
; Author:	Dean Belfield
; Created:	12/05/2023
; Last Updated:	07/06/2023
;
; Modinfo:
; 07/06/2023:	Modified to run in ADL mode

			.ASSUME	ADL = 1
				
			INCLUDE	"macros.inc"
			INCLUDE	"equs.inc"
			INCLUDE "mos_api.inc"	; In MOS/src

			SEGMENT CODE
				
			XDEF	VBLANK_INIT
			XDEF	VBLANK_STOP
			XDEF	VBLANK_HANDLER	

			XREF	ESCSET	
			XREF	KEYDOWN		; In ram.asm
			XREF	KEYASCII 	; In ram.asm
			XREF	KEYCOUNT	; In ram.asm

; Hook into the MOS VBLANK interrupt
;
VBLANK_INIT:		DI
			LD		HL, VBLANK_HANDLER		; this interrupt handler routine who's
			LD		E, 32h				; Set up the VBlank Interrupt Vector
			MOSCALL		mos_setintvector
			EX		HL, DE 				; DEU: Pointer to the MOS interrupt vector
			LD		HL, VBLANK_HANDLER_JP + 1	; Pointer to the JP address in this segment
			LD		(HL), DE			; Self-modify the code
			EI	
			RET

; Unhook the custom VBLANK interrupt
;
VBLANK_STOP:		DI
			LD		HL, VBLANK_HANDLER_JP + 1	; Pointer to the JP address in this segment
			LD		DE, (HL)			
			EX		DE, HL 				; HLU: Address of MOS interrupt vector
			LD		E, 32h
			MOSCALL		mos_setintvector		; Restore the MOS interrupt vector
			EI
			RET 

; A safe LIS call to ESCSET
; 
DO_KEYBOARD:		MOSCALL		mos_sysvars			; Get the system variables
			LD		HL, KEYCOUNT 			; Check whether the keycount has changed
			LD		A, (IX + sysvar_vkeycount)	; by comparing the MOS copy
			CP 		(HL)				; with our local copy
			JR		NZ, DO_KEYBOARD_1		; Yes it has, so jump to the next bit
;
DO_KEYBOARD_0:		XOR		A 				; Clear the keyboard values 
			LD		(KEYASCII), A
			LD		(KEYDOWN), A 
			RET	 					; And return
;
DO_KEYBOARD_1:		LD		(HL), A 			; Store the updated local copy of keycount 
			LD		A, (IX + sysvar_vkeydown)	; Fetch key down value (1 = key down, 0 = key up)
			OR		A 
			JR		Z, DO_KEYBOARD_0		; If it is key up, then clear the keyboard values
;			
			LD		(KEYDOWN), A 			; Store the keydown value
			LD		A, (IX + sysvar_keyascii)	; Fetch key ASCII value
			LD		(KEYASCII), A 			; Store locally
			CP		1Bh				; Is it escape?
			CALL		Z, ESCSET			; Yes, so set the escape flags
			RET						; Return to the interrupt handler

VBLANK_HANDLER:		DI 
			PUSH		AF 
			PUSH		HL
			PUSH		IX
			CALL		DO_KEYBOARD
			POP		IX 
			POP		HL
			POP		AF 
;
; Finally jump to the MOS interrupt
;
VBLANK_HANDLER_JP:	JP		0				; This is self-modified by VBLANK_INIT				