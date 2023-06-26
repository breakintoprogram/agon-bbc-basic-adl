;
; Title:	BBC Basic Interpreter - Z80 version
;		Statement Execution & Assembler Module - "EXEC"
; Author:	(C) Copyright  R.T.Russell  1984
; Modified By:	Dean Belfield
; Created:	12/05/2023
; Last Updated:	26/06/2023
;
; Modinfo:
; 27/01/1984:	Version 2.1
; 02/03/1987:	Version 3.0
; 11/06/1987:	Version 3.1
; 12/05/2023:	Modified by Dean Belfield
; 07/06/2023:	Modified to run in ADL mode
; 26/06/2023:	Fixed DIM, USR, and address output of inline assembler

			.ASSUME	ADL = 1

			INCLUDE	"equs.inc"

			SEGMENT CODE
				
			XDEF	XEQ
			XDEF	CHAIN0
			XDEF	RUN
			XDEF	SYNTAX
			XDEF	ESCAPE
			XDEF	FN
			XDEF	USR
			XDEF	STORE5
			XDEF	STORE4
			XDEF	CHECK
			XDEF	TERMQ
			XDEF	FILL
			XDEF	X4OR5
			XDEF	MUL16
			XDEF	CHANEL
				
			XREF	AUTO
			XREF	DELETE
			XREF	LOAD
			XREF	LIST_
			XREF	NEW
			XREF	OLD
			XREF	RENUM
			XREF	SAVE
			XREF	SOUND
			XREF	CLG
			XREF	DRAW
			XREF	ENVEL
			XREF	GCOL
			XREF	MODE
			XREF	MOVE
			XREF	PLOT
			XREF	COLOUR
			XREF	EXPRS
			XREF	HIMEM
			XREF	LOAD0
			XREF	RANDOM
			XREF	CLEAR
			XREF	ERRTRP
			XREF	PAGE_
			XREF	DATAPTR
			XREF	ERRLIN
			XREF	TRAP
			XREF	NXT
			XREF	SETLIN
			XREF	CLOOP
			XREF	OSSHUT
			XREF	WARM
			XREF	TRACEN
			XREF	OUTCHR
			XREF	PBCDL
			XREF	OSCLI
			XREF	LISTON
			XREF	GETVAR
			XREF	PUTVAR
			XREF	DATPTR
			XREF	ERROR_
			XREF	EXPR
			XREF	CREATE
			XREF	EXPRI
			XREF	BRAKET
			XREF	FREE
			XREF	OSBPUT
			XREF	COUNT
			XREF	STR
			XREF	HEXSTR
			XREF	CRLF
			XREF	ITEMI
			XREF	FINDL
			XREF	TEST
			XREF	EXPRN
			XREF	DLOAD5
			XREF	DLOAD5_SPL
			XREF	LOADN
			XREF	FPP
			XREF	SWAP
			XREF	GETDEF
			XREF	ZERO
			XREF	OSBGET
			XREF	BUFFER
			XREF	CONS
			XREF	VAL0
			XREF	OSLINE
			XREF	CLRSCN
			XREF	TELL
			XREF	SAYLN
			XREF	REPORT
			XREF	PUTPTR
			XREF	PUTIME
			XREF	PUTIMS
			XREF	LOMEM
			XREF	WIDTH
			XREF	OSWRCH
			XREF	COMMA
			XREF	OSCALL
			XREF	SFIX
			XREF	LOAD4
			XREF	PUSHS
			XREF	POPS
			XREF	LOADS
			XREF	PUTCSR
			XREF	OUT_
;
; List of token values used in this module
;
TAND:			EQU     80H
TOR:			EQU     84H
TERROR:			EQU     85H
LINE_:			EQU     86H
OFF_:			EQU     87H
STEP:			EQU     88H
SPC:			EQU     89H
TAB:			EQU     8AH
ELSE_:			EQU     8BH
THEN:			EQU     8CH
LINO:			EQU     8DH
TO:			EQU     B8H
TCMD:			EQU     C6H
TCALL:			EQU     D6H
DATA_:			EQU     DCH
DEF_:			EQU     DDH
TGOSUB:			EQU     E4H
TGOTO:			EQU     E5H
TON:			EQU     EEH
TPROC:			EQU     F2H
TSTOP:			EQU     FAH

; The command table
; Commands are tokens from C6H onwards; this lookup table is used to
; run the corresponding function; Note that DATA and DEF both use the same
; code as REM
;
CMDTAB:			DW24  AUTO			; C6H
			DW24  DELETE			; C7H
			DW24  LOAD			; C8H
			DW24  LIST_			; C9H
			DW24  NEW			; CAH
			DW24  OLD			; CBH
			DW24  RENUM			; CCH
			DW24  SAVE			; CDH
			DW24  PUT			; CEH
			DW24  PTR			; CFH
			DW24  PAGEV			; D0H
			DW24  TIMEV			; D1H
			DW24  LOMEMV			; D2H
			DW24  HIMEMV			; D3H
			DW24  SOUND			; D4H
			DW24  BPUT			; D5H
			DW24  CALL_			; D6H
			DW24  CHAIN			; D7H
			DW24  CLR			; D8H
			DW24  CLOSE			; D9H
			DW24  CLG			; DAH
			DW24  CLS			; DBH
			DW24  REM             		; DCH: DATA
			DW24  REM             		; DDH: DEF
			DW24  DIM			; DEH
			DW24  DRAW			; DFH
			DW24  END_			; E0H
			DW24  ENDPRO			; E1H
			DW24  ENVEL			; E2H
			DW24  FOR			; E3H
			DW24  GOSUB			; E4H
			DW24  GOTO			; E5H
			DW24  GCOL			; E6H
			DW24  IF_			; E7H
			DW24  INPUT			; E8H
			DW24  LET			; E9H
			DW24  LOCAL_			; EAH
			DW24  MODE			; EBH
			DW24  MOVE			; ECH
			DW24  NEXT			; EDH
			DW24  ON_			; EEH
			DW24  VDU			; EFH
			DW24  PLOT			; F0H
			DW24  PRINT_			; F1H
			DW24  PROC			; F2H
			DW24  READ			; F3H
			DW24  REM			; F4H
			DW24  REPEAT			; F5H
			DW24  REPOR			; F6H
			DW24  RESTOR			; F7H
			DW24  RETURN			; F8H
			DW24  RUN			; F9H
			DW24  STOP			; FAH
			DW24  COLOUR			; FBH
			DW24  TRACE			; FCH
			DW24  UNTIL			; FDH
			DW24  WIDTHV			; FEH
			DW24  CLI             		; FFH: OSCLI

; RUN 
; RUN "filename"
;
RUN:			CALL    TERMQ			; Standalone RUN command?
			JR      Z,RUN0			; Yes, so just RUN the code

; CHAIN "filename"
;
CHAIN:			CALL    EXPRS			; Get the filename
			LD      A,CR			; Terminate it with a CR
			LD      (DE),A
CHAIN0:			LD      SP,(HIMEM)		; Reset SP
			CALL    LOAD0			; And load the file in
;
RUN0:			LD      SP,(HIMEM)      	; Prepare for RUN
			LD      IX,RANDOM		; Pointer to the RANDOM sysvar
$$:			LD      A, R			; Use the R register to seed the random number generator
			JR      Z, $B			; Loop unti we get a non-zero value in A
			RLCA				; Rotate it
			RLCA
			LD      (IX+3),A		; And store
			SBC     A,A			; Depending upon the C flag, this will either be 00h or FFh
			LD      (IX+4),A		; And store
			CALL    CLEAR
			LD      HL,0			; Clear the error trap sysvar
			LD      (ERRTRP),HL
			LD      HL,(PAGE_)		; Load HL with the start of program memory (PAGE)
			LD      A,DATA_			; The DATA token value
			CALL    SEARCH          	; Search for the first DATA token in the tokenised listing
			LD      (DATPTR),HL     	; Set data pointer
			LD      IY,(PAGE_)		; Load IY with the start of program memory
;			
XEQ0:			CALL    NEWLIN
XEQ:			LD      (ERRLIN),IY     	; Error pointer
			CALL    TRAP           		; Check keyboard
XEQ1:			CALL    NXT
			INC     IY
			CP      ':'             	; Seperator
			JR      Z,XEQ1
			CP      CR
			JR      Z,XEQ0          	; New program line
			SUB     TCMD
			JP      C,LET0          	; Implied "LET"
			
			LD	BC, 3
			LD	B, A 
			MLT	BC 
			LD	HL,CMDTAB
			ADD	HL, BC 
			LD	HL, (HL)		; Table entry

;			ADD     A,A
;			LD      C,A
;			LD      B,0
;			LD      HL,CMDTAB
;			ADD     HL,BC
;			LD      A,(HL)          	; Table entry
;			INC     HL
;			LD      H,(HL)
;			LD      L,A

			CALL    NXT
			JP      (HL)            	; Execute the statement

;END
;
END_:			CALL    SETLIN          ;FIND CURRENT LINE
			LD      A,H
			OR      L               ;DIRECT?
			JP      Z,CLOOP
			LD      E,0
			CALL    OSSHUT          ;CLOSE ALL FILES
			JP      WARM            ;"Ready"
;
NEWLIN:			LD      A,(IY+0)        ;A=LINE LENGTH
			LD      BC,3
			ADD     IY,BC
			OR      A
			JR      Z,END_           ;LENGTH=0, EXIT
			LD      HL,(TRACEN)
			LD      A,H
			OR      L
			RET     Z
			LD	DE, 0		;Clear DE
			LD      D,(IY-1)        ;DE = LINE NUMBER
			LD      E,(IY-2)
			SBC     HL,DE
			RET     C
			EX      DE,HL
			LD      A,'['           ;TRACE
			CALL    OUTCHR
			CALL    PBCDL
			LD      A,']'
			CALL    OUTCHR
			LD      A,' '
			JP      OUTCHR

; Routines for each statement -------------------------------------------------

; OSCLI
;
CLI:			CALL    EXPRS
			LD      A,CR
			LD      (DE),A
			LD      HL,ACCS
			CALL    OSCLI
			JP      XEQ

; REM, *
;
EXT:			PUSH    IY
			POP     HL
			CALL    OSCLI
REM:			PUSH    IY
			POP     HL
			LD      A,CR
			LD      B,A
			CPIR                    ;FIND LINE END
			PUSH    HL
			POP     IY
			JP      XEQ0

; [LET] var = expr
;
LET0:			CP      ELSE_-TCMD
			JR      Z,REM
			CP      ('*'-TCMD) & 0FFH
			JR      Z,EXT
			CP      ('='-TCMD) & 0FFH
			JR      Z,FNEND
			CP      ('['-TCMD) & 0FFH
			JR      Z,ASM
			DEC     IY
LET:			CALL    ASSIGN
			JP      Z,XEQ
			JR      C,SYNTAX        ;"Syntax error"
			PUSH    AF              ;SAVE STRING TYPE
			CALL    EQUALS
			PUSH    HL
			CALL    EXPRS
			POP     IX
			POP     AF
			CALL    STACCS
XEQR:			JP      XEQ
;
ASM0:			CALL    NEWLIN
ASM:			LD      (ERRLIN),IY
			CALL    TRAP
			CALL    ASSEM
			JR      C,SYNTAX
			CP      CR
			JR      Z,ASM0
			LD      HL,LISTON
			LD      A,(HL)
			AND     0FH
			OR      30H
			LD      (HL),A
			JR      XEQR
;
VAR_:			CALL    GETVAR
			RET     Z
			JP      NC,PUTVAR
SYNTAX:			LD      A,16            ;"Syntax error"
			JR	ERROR0
ESCAPE:			LD      A,17            ;"Escape"
ERROR0:			JP      ERROR_

; =
;
FNEND:			CALL    EXPR            ;FUNCTION RESULT
			LD      B,E
			EX      DE,HL
			EXX                     ;SAVE RESULT
			EX      DE,HL           ; IN DEB'C'D'E'
FNEND5:			POP     BC
			LD      HL,LOCCHK
			OR      A
			SBC     HL,BC
			JR      Z,FNEND0        ;LOCAL VARIABLE
			LD      HL,FNCHK
			OR      A
			SBC     HL,BC
			LD      A,7
			JR      NZ,ERROR0       ;"No FN"
			POP     IY
			LD      (ERRLIN),IY     ;IN CASE OF ERROR
			EX      DE,HL
			EXX
			EX      DE,HL
			LD      DE,ACCS
			LD      E,B
			EX      AF,AF'
			RET
;
FNEND0:			POP     IX
			POP     BC
			LD      A,B
			OR      A
			JP      M,FNEND1        ;STRING
			POP     HL
			EXX
			POP     HL
			EXX
			CALL    STORE
			JR      FNEND5
FNEND1:			LD      HL,0
			ADD     HL,SP
			PUSH    DE
			LD      E,C
			CALL    STORES
			POP     DE
			LD      SP,HL
			JR      FNEND5

; DIM var(dim1[,dim2[,...]])[,var(...]
; DIM var expr[,var expr...]
;
DIM:			CALL    GETVAR          	; Get the variable
			JP      C,BADDIM		; Throw a "Bad Dim" error
			JP      Z,DIM4			; If Z then the command is DIM var% expr, so don't need to create an entity
			CALL    CREATE			; Create a new entity
			PUSH    HL			; HL: Address of the entity
			POP     IX			; IX: Address of the entity
			LD      A,(IY)			; Fetch the next character from the tokenised string
			CP      '('			; Check for opening brackets
			LD      A,D			;  A: The dimension variable type (04h = Integer, 05h = Float, 81h = String)
			JR      NZ,DIM4			; It is not a bracket; the command is DIM var expr
;
; At this point we're reserving a variable array
;
			PUSH    HL			; HL: Address of the entity
			PUSH    AF           	   	;  A: Entity type (04h = Integer, 05h = Float, 81h = String)
			LD      DE,1			; DE: Total size of array accumulator (important for multi-dimensioned arrays)
			LD      B,D			;  B: The number of dimensions in the array
;
DIM1:			INC     IY			; Skip to the next token
			PUSH    BC			; Stack the dimension counter
			PUSH    DE			; Stack the total size of array accumulator
			PUSH    IX			; Stack the entity address
			CALL    EXPRI           	; Fetch the size of this dimension
			BIT     7,H			; If it is negative then
			JR      NZ,BADDIM		; Throw a "Bad Dim" error
			EXX
			INC     HL			; HL: Size of this dimension; increment (BBC BASIC DIMs are always one bigger)
			POP     IX			; IX: The entity address
			INC     IX				
			LD      (IX),L          	; Save the size of this dimension in the entity
			INC     IX
			LD      (IX),H
			POP     BC
			CALL    MUL16           	; HL = HL * BC (Number of Dimensions * Total size of array accumulator)
			JR      C,NOROOM        	; Throw a "No Room" error if overflow
			EX      DE,HL           	; DE: The new total size of array accumulator
			POP     BC
			INC     B               	;  B: The dimension counter; increment
			LD      A,(IY)			; Fetch the nex token
			CP      ','             	; Check for another dimension in the array
			JR      Z,DIM1			; And loop
;
			CALL    BRAKET          	; Check for closing bracket
			POP     AF              	; Restore the type
			INC     IX
			EX      (SP),IX
			LD      (IX),B          	; Number of dimensions
			CALL    X4OR5           	; Dimension Accumulator Value * 4 or * 5 depending on type
			POP     HL			; Restore the entity address
			JR      C,NOROOM		; Throw a "No Room" error if there is an overflow
;
; We now allocate the memory for the array
;			
DIM3:			ADD     HL,DE
			JR      C,NOROOM
			PUSH    HL
			INC     H
			JR      Z,NOROOM
			SBC     HL,SP
			JR      NC,NOROOM       	; Throw an "Out of Space" error
			POP     HL
			LD      (FREE),HL
DIM2:			LD      A,D
			OR      E
			JR      Z,DIM5
			DEC     HL
			LD      (HL),0         		; Initialise the array
			DEC     DE
			JR      DIM2
DIM5:			CALL    NXT
			CP      ','            		; Another variable?
			JP      NZ,XEQ
			INC     IY
			CALL    NXT
			JP      DIM
;
; DIM errors
;
BADDIM:			LD      A,10            	; Throw a "Bad DIM" error
			JR	ERROR1			
NOROOM:			LD      A,11            	; Throw a "DIM space" error
ERROR1:			JP      ERROR_
;
; At this point we're reserving a block of memory, i.e.
; DIM var expr[,var expr...]
;
DIM4:			OR      A			;  A: The dimension variable type 
			JR      Z,BADDIM		; Throw "Bad Dim" if variable is an 8-bit indirection
			JP      M,BADDIM        	; or a string
			LD      B,A			; Temporarily store the dimension variable type in B
			LD      A,(IY-1)		; Get the last character but one
			CP      ')'			; Check if it is a trailing bracket
			JR      Z,BADDIM		; And throw a "Bad Dim" error if there is a trailing bracket
;
			LD	HL,0			; Clear HL
			LD	A,(FREE+0)		; HL: Lower 16 bits of FREE
			LD	L,A
			LD	A,(FREE+1)
			LD	H,A
			LD	A,B			; Restore the dimension variable type
			EXX
			LD	HL,0			; Clear HL
			LD	B,A			; Temporarily store the dimension variable type in B
			LD	A,(FREE+2)		; HL: Upper 8 bits of FREE (bits 16-23)
			LD	L,A
			LD	A,B			; Restore the dimension variable type
			LD	C,H
			CALL    STORE           	; Store the address
			CALL    EXPRI			; Get the number of bytes to store
			EXX
			INC     HL			; Add one to it
			EX      DE,HL
			LD      HL,(FREE)
			JR      DIM3			; Continue with the DIM

; PRINT list...
; PRINT #channel,list...
;
PRINT_:			CP      '#'
			JR      NZ,PRINT0
			CALL    CHNL            ;CHANNEL NO. = E
PRNTN1:			CALL    NXT
			CP      ','
			JP      NZ,XEQ
			INC     IY
			PUSH    DE
			CALL    EXPR            ;ITEM TO PRINT
			EX      AF,AF'
			JP      M,PRNTN2        ;STRING
			POP     DE
			PUSH    BC
			EXX
			LD      A,L
			EXX
			CALL    OSBPUT
			EXX
			LD      A,H
			EXX
			CALL    OSBPUT
			LD      A,L
			CALL    OSBPUT
			LD      A,H
			CALL    OSBPUT
			POP     BC
			LD      A,C
			CALL    OSBPUT
			JR      PRNTN1
PRNTN2:			LD      C,E
			POP     DE
			LD      HL,ACCS
			INC     C
PRNTN3:			DEC     C
			JR      Z,PRNTN4
			LD      A,(HL)
			INC     HL
			PUSH    BC
			CALL    OSBPUT
			POP     BC
			JR      PRNTN3
PRNTN4:			LD      A,CR
			CALL    OSBPUT
			JR      PRNTN1
;
PRINT6:			LD      B,2
			JR      PRINTC
PRINT8:			LD      BC,100H
			JR      PRINTC
PRINT9:			LD      HL,STAVAR
			XOR     A
			CP      (HL)
			JR      Z,PRINT0
			LD      A,(COUNT)
			OR      A
			JR      Z,PRINT0
PRINTA:			SUB     (HL)
			JR      Z,PRINT0
			JR      NC,PRINTA
			NEG
			CALL    FILL
PRINT0:			LD      A,(STAVAR)
			LD      C,A             ;PRINTS
			LD      B,0             ;PRINTF
PRINTC:			CALL    TERMQ
			JR      Z,PRINT4
			RES     0,B
			INC     IY
			CP      '~'
			JR      Z,PRINT6
			CP      ';'
			JR      Z,PRINT8
			CP      ','
			JR      Z,PRINT9
			CALL    FORMAT          ;SPC, TAB, '
			JR      Z,PRINTC
			DEC     IY
			PUSH    BC
			CALL    EXPR            ;VARIABLE TYPE
			EX      AF,AF'
			JP      M,PRINT3        ;STRING
			POP     DE
			PUSH    DE
			BIT     1,D
			PUSH    AF
			CALL    Z,STR           ;DECIMAL
			POP     AF
			CALL    NZ,HEXSTR       ;HEX
			POP     BC
			PUSH    BC
			LD      A,C
			SUB     E
			CALL    NC,FILL         ;RIGHT JUSTIFY
PRINT3:			POP     BC
			CALL    PTEXT           ;PRINT
			JR      PRINTC
PRINT4:			BIT     0,B
			CALL    Z,CRLF
			JP      XEQ

; ON ERROR statement [:statement...]
; ON ERROR OFF
;
ONERR:			INC     IY              ;SKIP "ERROR"
			LD      HL,0
			LD      (ERRTRP),HL
			CALL    NXT
			CP      OFF_
			INC     IY
			JP      Z,XEQ
			DEC     IY
			LD      (ERRTRP),IY
			JP      REM

; ON expr GOTO line[,line...] [ELSE statement]
; ON expr GOTO line[,line...] [ELSE line]
; ON expr GOSUB line[,line...] [ELSE statement]
; ON expr GOSUB line[,line...] [ELSE line]
; ON expr PROCone [,PROCtwo..] [ELSE PROCotherwise]
;
ON_:			CP      TERROR
			JR      Z,ONERR         ;"ON ERROR"
			CALL    EXPRI
			LD      A,(IY)
			INC     IY
			LD      E,','           ;SEPARATOR
			CP      TGOTO
			JR      Z,ON1
			CP      TGOSUB
			JR      Z,ON1
			LD      E,TPROC
			CP      E
			LD      A,39
			JR      NZ,ERROR2       ;"ON syntax"
ON1:			LD      D,A
			EXX
			PUSH    HL
			EXX
			POP     BC              ;ON INDEX
			LD      A,B
			OR      H
			OR      L
			JR      NZ,ON4          ;OUT OF RANGE
			OR      C
			JR      Z,ON4
			DEC     C
			JR      Z,ON3           ;INDEX=1
ON2:			CALL    TERMQ
			JR      Z,ON4           ;OUT OF RANGE
			INC     IY              ;SKIP DELIMITER
			CP      E
			JR      NZ,ON2
			DEC     C
			JR      NZ,ON2
ON3:			LD      A,E
			CP      TPROC
			JR      Z,ONPROC
			PUSH    DE
			CALL    ITEMI           ;LINE NUMBER
			POP     DE
			LD      A,D
			CP      TGOTO
			JR      Z,GOTO2
			CALL    SPAN            ;SKIP REST OF LIST
			JR      GOSUB1
;
ON4:			LD      A,(IY)
			INC     IY
			CP      ELSE_
			JP      Z,IF1           ;ELSE CLAUSE
			CP      CR
			JR      NZ,ON4
			LD      A,40
ERROR2:			JP      ERROR_           ;"ON range"
;
ONPROC:			LD      A,TON
			JP      PROC

; GOTO line
;
GOTO:			CALL    ITEMI           	; Fetch the line number
GOTO1:			CALL    TERMQ			; Check for terminator
			JP      NZ,SYNTAX		; Throw a "Syntax Error" if not found
GOTO2:			EXX
			CALL    FINDL			; HL: Line number - Find the line
			PUSH    HL			; HL: Address of the line
			POP     IY			; IY = HL
			JP      Z,XEQ0			; If the line is found, then continue execution at that point
			LD      A,41			; Otherwise throw a "No such line" error
			JR      ERROR2

; GOSUB line
; This pushes the following data onto the execution stack
; - 3 bytes: Current execution address
; - 3 bytes: Marker (the address of label GOSCHK)
;
GOSUB:			CALL    ITEMI			; Fetch the line number
GOSUB1:			PUSH    IY              	; Push the current execution address onto the execution stack
			CALL    CHECK           	; Check there is enough room
			CALL    GOTO1           	; Push the marker (address of GOSCHK) onto the execution stack and GOTO the line number
GOSCHK:			EQU     $

; RETURN
; This pops the following data off the execution stack as pushed by GOSUB
; - 3 bytes: Marker (should be the address of label GOSCHK)
; - 3 bytes: The return execution address
;
RETURN:			POP     DE			; Pop the marker off the execution stack
			LD      HL,GOSCHK		; Compare with GOSCHK
			OR      A
			SBC     HL,DE
			POP     IY			; Pop the return address off the execution stack
			JP      Z,XEQ			; Provided this has been called by a GOSUB then continue execution at the return address
			LD      A,38			; Otherwise throw a "No GOSUB" error
			JR      ERROR2

; REPEAT
; This pushes the following data onto the execution stack
; - 3 bytes: Current execution address
; - 3 bytes: Marker (the address of label REPCHK)
;
REPEAT:			PUSH    IY			; Push the current execution address onto the execution stack
			CALL    CHECK			; Check if there is enough room
			CALL    XEQ			; Push the marker (address of REPCHK) onto the execution stack and continue execution
REPCHK:			EQU     $

; UNTIL expr
; This pops the following data off the execution stack
; - 3 bytes: Marker (should be the address of label REPCHK)
; - 3 bytes: The address of the REPEAT instruction
; It also ensures that the data is pushed back on for subsequent UNTIL instructions
;
UNTIL:			POP     BC			; Fetch the marker
			PUSH    BC			; And push it back onto the execution stack
			LD      HL,REPCHK		; Compare with REPCHK
			OR      A
			SBC     HL,BC
			LD      A,43
			JR      NZ,ERROR2		; Throw a "No REPEAT" if this value does not match
			CALL    EXPRI			; Fetch the expression
			CALL    TEST			; Test if the expression evaluates to zero		
			POP     BC			; Pop the marker
			POP     DE			; Pop the address of the REPEAT instruction
			JR      NZ,XEQ2         	; If it is TRUE, then continue execution after the UNTIL instruction (we're done looping)
			PUSH    DE			; Push the address of the REPEAT instruction back on the stack
			PUSH    BC			; Push the marker back on the stack
			PUSH    DE			; IY = DE
			POP     IY			; This sets the execution address back to the REPEAT instruction
XEQ2:			JP      XEQ			; Continue execution

; FOR var = expr TO expr [STEP expr]
; This pushes the following data onto the execution stack
; - 3 bytes: The limit value
; - 3 bytes: The step value
; - 3 bytes: The current execution address
; - 3 bytes: The address of the loop variable
; - 3 bytes: Marker (the address of FORCHK)
;
FORVAR:			LD      A,34
			JR      ERROR2          	; Throw "FOR variable" error
;
FOR:			CALL    ASSIGN			; Assign the START expression value to a variable
			JR      NZ,FORVAR       	; If the variable is a string, or invalid, then throw a "FOR variable" error
			PUSH    AF              	; Save the variable type
			LD      A,(IY)			; Check the next token
			CP      TO			; Compare with the token value for "TO"
			LD      A,36			; Set the error code to 36 ("No TO")
			JP      NZ,ERROR2       	; And throw the error if that token is missing
			INC     IY			; Skip to the next token
;
			PUSH    IX
			CALL    EXPRN           	; Fetch the LIMIT expression value
			POP     IX
			POP     AF
			LD      B,A             	; B: LIMIT value type (04h = Integer, 05h = Float)
			PUSH    BC              	; Stack the LIMIT value
			PUSH    HL
			LD      HL,0
			LD      C,H
			EXX
			PUSH    HL
;			
			LD      HL,1            	; The preset STEP value is 1
			EXX
			LD      A,(IY)			; Fetch the next token
			CP      STEP			; Compare with the token value for "STEP"
			JR      NZ,FOR1			; If there is no STEP token, then skip the next bit
;
			INC     IY			; Skip past the STEP token
			PUSH    IX
			CALL    EXPRN          		; Fetch the STEP expression value
			POP     IX
;			
FOR1:			PUSH    BC			; Stack the STEP value
			PUSH    HL
			EXX
			PUSH    HL
			EXX
;			
			PUSH    IY              	; Stack the current execution address
			PUSH    IX              	; Stack the loop variable
			CALL    CHECK
			CALL    XEQ
FORCHK:			EQU     $

; NEXT [var[,var...]]
; This pops the following data off the execution stack
; - 3 bytes: Marker (the address of FORCHK)
; - 3 bytes: The address of the loop variable
; - 3 bytes: The current execution address
; - 3 bytes: The step value
; - 3 bytes: The limit value
; It also ensures that the data is pushed back on for subsequent NEXT instructions
;
NEXT:			POP     BC              	; Pop the marker off the execution stack
			LD      HL,FORCHK		; Compare with FORCHK
			OR      A
			SBC     HL,BC
			LD      A,32
			JP      NZ,ERROR3      		; If this does not match, throw a "No FOR" error
			CALL    TERMQ			; Check for terminator (a NEXT without a variable)
			POP     HL			; Pop the address of the loop variable off the execution stack
			PUSH    HL			; Push it back onto the execution stack
			PUSH    BC			; Push the marker back onto the execution stack
			PUSH    HL			; HL: Address of the loop variable off the stack
			CALL    NZ,GETVAR       	; If there is no terminator, get the variable from the args
			POP     DE			; DE: Address of the loop variable off the stack
			EX      DE,HL			; HL: Address of the loop variable off the stack, DE: Address of the variable from args
			OR      A
NEXT0:			SBC     HL,DE			; Compare to make sure that the variables match
			JR      NZ,NEXT1		; They don't, so jump to NEXT1
			PUSH    DE
			LD      IX,9+3			; IX: Address of the STEP value on the execution stack
			ADD     IX,SP
			CALL    DLOAD5_SPL      	; Load the STEP value
			LD      A,(IX+16)       	; Get the STEP type
			POP     IX
			CALL    LOADN           	; Load the LOOP variable
			BIT     7,D             	; Check the sign
			PUSH    AF
			LD      A,'+' & 0FH
			CALL    FPP             	; Add the STEP
			JR      C,ERROR3
			POP     AF              	; Restore TYPE
			PUSH    AF
			CALL    STORE           	; Update the variable
			LD      IX,18+3			; IX: Address of the LIMIT value on the execution stack
			ADD     IX,SP
			CALL    DLOAD5_SPL      	; Load the LIMIT value
			POP     AF
			CALL    Z,SWAP			; Swap the arguments if the sign is ?
			LD      A,0+('<'-4) & 0FH
			CALL    FPP             	; Test against the limit
			JR      C,ERROR3		; Throw an error if FPP returns bad
			INC     H
			JR      NZ,LOOP_        	; Keep looping
			LD      HL,27			; Adjust the stack
			ADD     HL,SP
			LD      SP,HL
			CALL    NXT
			CP      ','			; Check for multiple variables
			JP      NZ,XEQ			; No, so we are done at ths point
			INC     IY			; Increment to the next variable
			JR      NEXT			; And continue
;
LOOP_:			POP     BC
			POP     DE
			POP     IY
			PUSH    IY
			PUSH    DE
			PUSH    BC
			JP      XEQ
;
NEXT1:			LD      HL,27			; TODO: What does this do?	
			ADD     HL,SP
			LD      SP,HL			; Adjust the stack
			POP     BC
			LD      HL,FORCHK
			SBC     HL,BC
			POP     HL              	; Variable pointer
			PUSH    HL
			PUSH    BC
			JR      Z,NEXT0
;			
			LD      A,33
ERROR3:			JP      ERROR_           	; Throw the error "Can't match FOR"

; FNname
; N.B. ENTERED WITH A <> TON
;
FN:			PUSH    AF              	; Push A onto the stack; this'll be checked for the token ON (TON) in PROC5
			CALL    PROC1
FNCHK:			EQU     $			; This will never fall through as PROC1 will do a JP XEQ

; PROCname
; N.B. ENTERED WITH A = ON PROC FLAG (EEh or the first character of the token name)
; This pushes the following data onto the execution stack
; - 3 bytes: Token in A
; - 3 bytes: Marker (the address of PROCHK)
;
PROC:			PUSH    AF			; Push A onto the stack; this'll be checked for the token ON (TON) in PROC5
			CALL    PROC1
PROCHK:			EQU     $			; This will never fall through as PROC1 will do a JP XEQ
;
PROC1:			CALL    CHECK			; Check there is space for this
			DEC     IY			; Decrement IY to the PROC token
			PUSH    IY			; Stack the pointer
			CALL    GETDEF			; Search for this PROC/FN entry in the dynamic area
			POP     BC			; BC = IY
			JR      Z,PROC4			; If found in the dynamic area then skip to PROC4
			LD      A,30
			JR      C,ERROR3        	; Throw error "Bad call" if invalid PROC/FN call
;
; At this point the PROC/FN has not yet been registered in the dynamic area
; So we need to search through the listing and find where the DEFPROC/FN is and save the address
;			
			PUSH    BC			; BC: Still pointing to the PROC token in the tokenised line
			LD      HL,(PAGE_)		; HL: Start of program memory
;
PROC2:			LD      A,DEF_			;  A: The token to search for
			CALL    SEARCH          	; Look for "DEF" as the first token in a program line
			JR      C,PROC3			; Not found, so jump to PROC3
			PUSH    HL			; HL: Points to the DEF token in the DEFPROC
			POP     IY			; IY = HL
			INC     IY              	; Skip the DEF token
			CALL    NXT			; And any whitespace
			CALL    GETDEF			; Search for this PROC/FN entry in the dynamic area
			PUSH    IY
			POP     DE			; DE: Points to the PROC/FN token in tokenised line of the DEFPROC
			JR      C,PROC6			; Skip if there is an error (neither FN or PROC first)
			CALL    NZ,CREATE		; Create an entity in the dynamic area
			PUSH    IY			; IY: Pointer to the DEFPROC/FN arguments
			POP     DE			; DE = IY
			LD	(HL),DE			; Save address
;
PROC6:			EX      DE,HL			; HL: Address of the procedure
			LD      A,CR			; The character to search for
			LD	BC,100h			; Only need to search 256 bytes or so ahead; maximum line length
			CPIR                    	; Skip to next line
			JR      PROC2			; Rinse, lather and repeat
;
; At this point a DEF has not been found for the PROC/FN
;
PROC3:			POP     IY              	; Restore the execution address
			CALL    GETDEF			; Search for this PROC/FN entry in the dynamic area
			LD      A,29
			JR      NZ,ERROR3      		; Throw error "No such FN/PROC" if not found
;
; At this point we have a PROC/FN entry in the dynamic area
; 			
PROC4:			LD	DE,(HL)			; HL: Address of pointer; fetch entity address in DE
			LD	HL,3
			ADD     HL,SP
			CALL    NXT             	; Allow space before "("
			PUSH    DE              	; Exchange DE and IY
			EX      (SP),IY
			POP     DE
			CP      '('             	; Arguments?
			JR      NZ,PROC5
			CALL    NXT             	; Allow space before "("
			CP      '('
			JP      NZ,SYNTAX       	; Throw "Syntax error"
			PUSH    IY
			POP     BC              	; Save IY in BC
			EXX
			CALL    SAVLOC          	; Save dummy variables
			CALL    BRAKET          	; Closing bracket
			EXX
			PUSH    BC
			POP     IY              	; Restore IY
			PUSH    HL
			CALL    ARGUE           	; Transfer arguments
			POP     HL
;
PROC5:			LD	(HL), DE		; Save return address		
			INC	HL
			LD	A, (HL)			; Fetch the flag pushed at start of PROC routine (from AF)
			CP	TON			; Was it "ON PROC"?
			JP	NZ, XEQ			; No
			PUSH    DE
			EX      (SP),IY
			CALL    SPAN            	; Skip rest of ON list
			EX      (SP),IY
			POP     DE
			DEC	HL
			LD	(HL), DE
			JP      XEQ

; LOCAL var[,var...]
;
LOCAL_:			POP     BC
			PUSH    BC
			LD      HL,FNCHK
			OR      A
			SBC     HL,BC
			JR      Z,LOCAL1
			LD      HL,PROCHK
			OR      A
			SBC     HL,BC
			JR      Z,LOCAL1
			LD      HL,LOCCHK
			OR      A
			SBC     HL,BC
			LD      A,12
			JP      NZ,ERROR_        ;"Not LOCAL"
LOCAL1:			PUSH    IY
			POP     BC
			EXX
			DEC     IY
			CALL    SAVLOC
			EXX
			PUSH    BC
			POP     IY
LOCAL2:			CALL    GETVAR
			JP      NZ,SYNTAX
			OR      A               ;TYPE
			EX      AF,AF'
			CALL    ZERO
			EX      AF,AF'
			PUSH    AF
			CALL    P,STORE         ;ZERO
			POP     AF
			LD      E,C
			CALL    M,STORES
			CALL    NXT
			CP      ','
			JP      NZ,XEQ
			INC     IY
			CALL    NXT
			JR      LOCAL2

; ENDPROC
;
ENDPRO:			POP     BC
			LD      HL,LOCCHK
			OR      A
			SBC     HL,BC
			JR      Z,UNSTK         ;LOCAL VARIABLE
			LD      HL,PROCHK       ;PROC MARKER
			OR      A
			SBC     HL,BC
			POP     IY
			JP      Z,XEQ
			LD      A,13
			JP      ERROR_           ;"No PROC"
;
UNSTK:			POP     IX
			POP     BC
			LD      A,B
			OR      A
			JP      M,UNSTK1        ;STRING
			POP     HL
			EXX
			POP     HL
			EXX
			CALL    STORE
			JR      ENDPRO
UNSTK1:			LD      HL,0
			ADD     HL,SP
			LD      E,C
			CALL    STORES
			LD      SP,HL
			JR      ENDPRO

; INPUT #channel,var,var...
;
INPUTN:			CALL    CHNL            ;E = CHANNEL NUMBER
INPN1:			CALL    NXT
			CP      ','
			JP      NZ,XEQ
			INC     IY
			CALL    NXT
			PUSH    DE
			CALL    VAR_
			POP     DE
			PUSH    AF              ;SAVE TYPE
			PUSH    HL              ;VARPTR
			OR      A
			JP      M,INPN2         ;STRING
			CALL    OSBGET
			EXX
			LD      L,A
			EXX
			CALL    OSBGET
			EXX
			LD      H,A
			EXX
			CALL    OSBGET
			LD      L,A
			CALL    OSBGET
			LD      H,A
			CALL    OSBGET
			LD      C,A
			POP     IX
			POP     AF              ;RESTORE TYPE
			PUSH    DE              ;SAVE CHANNEL
			CALL    STORE
			POP     DE
			JR      INPN1
INPN2:			LD      HL,ACCS
INPN3:			CALL    OSBGET
			CP      CR
			JR      Z,INPN4
			LD      (HL),A
			INC     L
			JR      NZ,INPN3
INPN4:			POP     IX
			POP     AF
			PUSH    DE
			EX      DE,HL
			CALL    STACCS
			POP     DE
			JR      INPN1

; INPUT ['][SPC(x)][TAB(x[,y])]["prompt",]var[,var...]
; INPUT LINE [SPC(x)][TAB(x[,y])]["prompt",]var[,var...]
;
INPUT:			CP      '#'
			JR      Z,INPUTN
			LD      C,0             ;FLAG PROMPT
			CP      LINE_
			JR      NZ,INPUT0
			INC     IY              ;SKIP "LINE"
			LD      C,80H
INPUT0:			LD      HL,BUFFER
			LD      (HL),CR         ;INITIALISE EMPTY
INPUT1:			CALL    TERMQ
			JP      Z,XEQ           ;DONE
			INC     IY
			CP      ','
			JR      Z,INPUT3        ;SKIP COMMA
			CP      ';'
			JR      Z,INPUT3
			PUSH    HL              ;SAVE BUFFER POINTER
			CP      34		;ASCII ""
			JR      NZ,INPUT6
			PUSH    BC
			CALL    CONS
			POP     BC
			CALL    PTEXT           ;PRINT PROMPT
			JR      INPUT9
INPUT6:			CALL    FORMAT          ;SPC, TAB, '
			JR      NZ,INPUT2
INPUT9:			POP     HL
			SET     0,C             ;FLAG NO PROMPT
			JR      INPUT0
INPUT2:			DEC     IY
			PUSH    BC
			CALL    VAR_
			POP     BC
			POP     HL
			PUSH    AF              ;SAVE TYPE
			LD      A,(HL)
			INC     HL
			CP      CR              ;BUFFER EMPTY?
			CALL    Z,REFILL
			BIT     7,C
			PUSH    AF
			CALL    NZ,LINES
			POP     AF
			CALL    Z,FETCHS
			POP     AF              ;RESTORE TYPE
			PUSH    BC
			PUSH    HL
			OR      A
			JP      M,INPUT4        ;STRING
			PUSH    AF
			PUSH    IX
			CALL    VAL0
			POP     IX
			POP     AF
			CALL    STORE
			JR      INPUT5
INPUT4:			CALL    STACCS
INPUT5:			POP     HL
			POP     BC
INPUT3:			RES     0,C
			JR      INPUT1
;
REFILL:			BIT     0,C
			JR      NZ,REFIL0       ;NO PROMPT
			LD      A,'?'
			CALL    OUTCHR          ;PROMPT
			LD      A,' '
			CALL    OUTCHR
REFIL0:			LD      HL,BUFFER
			PUSH    BC
			PUSH    HL
			PUSH    IX
			CALL    OSLINE
			POP     IX
			POP     HL
			POP     BC
			LD      B,A             ;POS AT ENTRY
			XOR     A
			LD      (COUNT),A
			CP      B
			RET     Z
REFIL1:			LD      A,(HL)
			CP      CR
			RET     Z
			INC     HL
			DJNZ    REFIL1
			RET

; READ var[,var...]
;
READ:			CP      '#'
			JP      Z,INPUTN
			LD      HL,(DATPTR)
READ0:			LD      A,(HL)
			INC     HL              ;SKIP COMMA OR "DATA"
			CP      CR              ;END OF DATA STMT?
			CALL    Z,GETDAT
			PUSH    HL
			CALL    VAR_
			POP     HL
			OR      A
			JP      M,READ1         ;STRING
			PUSH    HL
			EX      (SP),IY
			PUSH    AF              ;SAVE TYPE
			PUSH    IX
			CALL    EXPRN
			POP     IX
			POP     AF
			CALL    STORE
			EX      (SP),IY
			JR      READ2
READ1:			CALL    FETCHS
			PUSH    HL
			CALL    STACCS
READ2:			POP     HL
			LD      (DATPTR),HL
			CALL    NXT
			CP      ','
			JP      NZ,XEQ
			INC     IY
			CALL    NXT
			JR      READ0
;
GETDAT:			LD      A,DATA_
			CALL    SEARCH
			INC     HL
			RET     NC
			LD      A,42
ERROR4:			JP      ERROR_           ;"Out of DATA"

; IF expr statement
; IF expr THEN statement [ELSE statement]
; IF expr THEN line [ELSE line]
;
IF_:			CALL    EXPRI
			CALL    TEST
			JR      Z,IFNOT         ;FALSE
			LD      A,(IY)
			CP      THEN
			JP      NZ,XEQ
			INC     IY              ;SKIP "THEN"
IF1:			CALL    NXT
			CP      LINO
			JP      NZ,XEQ          ;STATEMENT FOLLOWS
			JP      GOTO            ;LINE NO. FOLLOWS
IFNOT:			LD      A,(IY)
			CP      CR
			INC     IY
			JP      Z,XEQ0          ;END OF LINE
			CP      ELSE_
			JR      NZ,IFNOT
			JR      IF1

; CLS
;
CLS:		CALL    CLRSCN
			XOR     A
			LD      (COUNT),A
			JP      XEQ

; STOP
;
STOP:			CALL    TELL
			DB	CR
			DB	LF
			DB	TSTOP
			DB	0
			CALL    SETLIN          ;FIND CURRENT LINE
			CALL    SAYLN
			CALL    CRLF
			JP      CLOOP

; REPORT
;
REPOR:			CALL    REPORT
			JP      XEQ

; CLEAR
;
CLR:			CALL    CLEAR
			LD      HL,(PAGE_)
			JR      RESTR1

; RESTORE [line]
;
RESTOR:			LD      HL,(PAGE_)
			CALL    TERMQ
			JR      Z,RESTR1
			CALL    ITEMI
			EXX
			CALL    FINDL           ;SEARCH FOR LINE
			LD      A,41
			JP      NZ,ERROR4       ;"No such line"
RESTR1:			LD      A,DATA_
			CALL    SEARCH
			LD      (DATPTR),HL
			JP      XEQ

; PTR#channel=expr
; PAGE=expr
; TIME=expr
; LOMEM=expr
; HIMEM=expr
;
PTR:			CALL    CHANEL
			CALL    EQUALS
			LD      A,E
			PUSH    AF
			CALL    EXPRI
			PUSH    HL
			EXX
			POP     DE
			POP     AF
			CALL    PUTPTR
			JP      XEQ
;
PAGEV:			CALL    EQUALS
			CALL    EXPRI
			EXX
			LD      L,0
			LD      (PAGE_),HL
			JP      XEQ
;
TIMEV:			CP      '$'
			JR      Z,TIMEVS
			CALL    EQUALS
			CALL    EXPRI
			PUSH    HL
			EXX
			POP     DE
			CALL    PUTIME
			JP      XEQ
;
TIMEVS:			INC     IY              ;SKIP '$'
			CALL    EQUALS
			CALL    EXPRS
			CALL    PUTIMS
			JP      XEQ
;
LOMEMV:			CALL    EQUALS
			CALL    EXPRI
			CALL    CLEAR
			EXX
			LD      (LOMEM),HL
			LD      (FREE),HL
			JP      XEQ
;
HIMEMV:			CALL    EQUALS
			CALL    EXPRI
			EXX
			LD      DE,(FREE)
			INC     D
			XOR     A
			SBC     HL,DE
			ADD     HL,DE
			JP      C,ERROR_         ;"No room"
			LD      DE,(HIMEM)
			LD      (HIMEM),HL
			EX      DE,HL
			SBC     HL,SP
			JP      NZ,XEQ
			EX      DE,HL
			LD      SP,HL           ;LOAD STACK POINTER
			JP      XEQ

; WIDTH expr
;
WIDTHV:			CALL    EXPRI
			EXX
			LD      A,L
			LD      (WIDTH),A
			JP      XEQ

; TRACE ON
; TRACE OFF
; TRACE line
;
TRACE:			INC     IY
			LD      HL,0
			CP      TON
			JR      Z,TRACE0
			CP      OFF_
			JR      Z,TRACE1
			DEC     IY
			CALL    EXPRI
			EXX
TRACE0:			DEC     HL
TRACE1:			LD      (TRACEN),HL
			JP      XEQ

; VDU expr,expr;....
;
VDU:			CALL    EXPRI
			EXX
			LD      A,L
			CALL    OSWRCH
			LD      A,(IY)
			CP      ','
			JR      Z,VDU2
			CP      ';'
			JR      NZ,VDU3
			LD      A,H
			CALL    OSWRCH
VDU2:			INC     IY
VDU3:			CALL    TERMQ
			JR      NZ,VDU
			JP      XEQ

; CLOSE channel number
;
CLOSE:			CALL    CHANEL
			CALL    OSSHUT
			JP      XEQ

; BPUT channel,byte
;
BPUT:			CALL    CHANEL          ;CHANNEL NUMBER
			PUSH    DE
			CALL    COMMA
			CALL    EXPRI           ;BYTE
			EXX
			LD      A,L
			POP     DE
			CALL    OSBPUT
			JP      XEQ

; CALL address[,var[,var...]]
;
CALL_:			CALL    EXPRI           ;ADDRESS
			EXX
			PUSH    HL              ;SAVE IT
			LD      B,0             ;PARAMETER COUNTER
			LD      DE,BUFFER       ;VECTOR
CALL1:			CALL    NXT
			CP      ','
			JR      NZ,CALL2
			INC     IY
			INC     B
			CALL    NXT
			PUSH    BC
			PUSH    DE
			CALL    VAR_
			POP     DE
			POP     BC
			INC     DE
			LD      (DE),A          ;PARAMETER TYPE
			INC     DE
			EX      DE,HL
			LD      (HL),E          ;PARAMETER ADDRESS
			INC     HL
			LD      (HL),D
			EX      DE,HL
			JR      CALL1
CALL2:			LD      A,B
			LD      (BUFFER),A      ;PARAMETER COUNT
			POP     HL              ;RESTORE ADDRESS
			CALL    USR1
			JP      XEQ

; USR(address)
;
USR:			CALL    ITEMI			; Evaluate the expression
			LD	A,L			;  A: MSB of address
			EXX
			LD	(R0+0),HL		; HL: LSW of address
			LD	(R0+2),A		
			LD	HL,(R0)			; Get the 24-bit address in HL
;
USR1:			PUSH    HL              	; Address on stack
			EX      (SP),IY
			INC     H               	; PAGE &FF? TODO: This needs fixing as &FF00 does not make much sense in 24-bit land
			LD      HL,USR2         	; Return address
			PUSH    HL
			LD      IX,STAVAR
			CALL    Z,OSCALL        	; Intercept &FF (see TODO)
			LD      C,(IX+24)
			PUSH    BC
			POP     AF              	; Load flags
			LD      A,(IX+4)        	; Load Z80 registers
			LD      B,(IX+8)		; TODO: Need to load the MSW into BC
			LD      C,(IX+12)		; TODO: Then LSB into C
			LD      D,(IX+16)		; TODO: Same for DE
			LD      E,(IX+20)
			LD      H,(IX+32)		; TODO: And HL
			LD      L,(IX+48)
			LD      IX,BUFFER
			JP      (IY)            	; Off to user routine
;
USR2:			POP     IY	
			XOR     A
			LD      C,A
			RET

; PUT port,data
;
PUT:			CALL    EXPRI           ;PORT ADDRESS
			EXX
			PUSH    HL
			CALL    COMMA
			CALL    EXPRI           ;DATA
			EXX
			POP     BC
			OUT     (C),L           ;OUTPUT TO PORT BC
			JP      XEQ

; SUBROUTINES -----------------------------------------------------------------

; ASSIGN - Assign a numeric value to a variable.
; Outputs: NC,  Z - OK, numeric.
;          NC, NZ - OK, string.
;           C, NZ - illegal
;
ASSIGN:			CALL    GETVAR          ;VARIABLE
			RET     C               ;ILLEGAL VARIABLE
			CALL    NZ,PUTVAR
			OR      A
			RET     M               ;STRING VARIABLE
			PUSH    AF              ;NUMERIC TYPE
			CALL    EQUALS
			PUSH    HL
			CALL    EXPRN
			POP     IX
			POP     AF
STORE:			BIT     0,A
			JR      Z,STOREI
			CP      A               ;SET ZERO
STORE5:			LD      (IX+4),C
STORE4:			EXX
			LD      (IX+0),L
			LD      (IX+1),H
			EXX
			LD      (IX+2),L
			LD      (IX+3),H
			RET
STOREI:			PUSH    AF
			INC     C               ;SPEED - & PRESERVE F'
			DEC     C               ; WHEN CALLED BY FNEND0
			CALL    NZ,SFIX         ;CONVERT TO INTEGER
			POP     AF
			CP      4
			JR      Z,STORE4
			CP      A               ;SET ZERO
STORE1:			EXX
			LD      (IX+0),L
			EXX
			RET
;
STACCS:			LD      HL,ACCS
STORES:			RRA
			JR      NC,STORS3       ;FIXED STRING
			PUSH    HL
			CALL    LOAD4
			LD      A,E             ;LENGTH OF STRING
			EXX
			LD      L,A
			LD      A,H             ;LENGTH ALLOCATED
			EXX
			CP      E
			JR      NC,STORS1       ;ENOUGH ROOM
			EXX
			LD      H,L
			EXX
			PUSH    HL
;			LD      B,0
			LD	BC, 0
			LD      C,A
			ADD     HL,BC
			LD      BC,(FREE)
			SBC     HL,BC           ;IS STRING LAST?
			POP     HL
			SCF
			JR      Z,STORS1
;			LD      H,B
;			LD      L,C
			LD	HL, BC
STORS1:			CALL    STORE4          ;PRESERVES CARRY!
;			LD      B,0
			LD	BC, 0
			LD      C,E
			EX      DE,HL
			POP     HL
			DEC     C
			INC     C
			RET     Z               ;NULL STRING
			LDIR
			RET     NC              ;STRING REPLACED
			LD      (FREE),DE
CHECK:			PUSH    HL
			LD      HL,(FREE)
			INC     H
			SBC     HL,SP
			POP     HL
			RET     C
			XOR     A
			JP      ERROR_		; Throw error "No room"
;
STORS3:			LD      C,E
			PUSH    IX
			POP     DE
			XOR     A
			LD      B,A
			CP      C
			JR      Z,STORS5
			LDIR
STORS5:			LD      A,CR
			LD      (DE),A
			RET

; ARGUE: TRANSFER FN OR PROC ARGUMENTS FROM THE
;  CALLING STATEMENT TO THE DUMMY VARIABLES VIA
;  THE STACK.  IT MUST BE DONE THIS WAY TO MAKE
;  PROCFRED(A,B)    DEF PROCFRED(B,A)     WORK.
;    Inputs: DE addresses parameter list 
;            IY addresses dummy variable list
;   Outputs: DE,IY updated
;  Destroys: Everything
;
ARGUE:			LD      A,-1
			PUSH    AF              ;PUT MARKER ON STACK
ARGUE1:			INC     IY              ;BUMP PAST ( OR ,
			INC     DE
			PUSH    DE
			CALL    NXT
			CALL    GETVAR
			JR      C,ARGERR
			CALL    NZ,PUTVAR
			POP     DE
			PUSH    HL              ;VARPTR
			OR      A               ;TYPE
			PUSH    AF
			PUSH    DE
			EX      (SP),IY
			JP      M,ARGUE2        ;STRING
			CALL    EXPRN           ;PARAMETER VALUE
			EX      (SP),IY
			POP     DE
			POP     AF
			EXX
			PUSH    HL
			EXX
			PUSH    HL
			LD      B,A
			PUSH    BC
			CALL    CHECK           ;CHECK ROOM
			JR      ARGUE4
ARGUE2:			CALL    EXPRS
			EX      (SP),IY
			EXX
			POP     DE
			EXX
			POP     AF
			CALL    PUSHS
			EXX
ARGUE4:			CALL    NXT
			CP      ','
			JR      NZ,ARGUE5
			LD      A,(DE)
			CP      ','
			JR      Z,ARGUE1        ;ANOTHER
ARGERR:			LD      A,31
			JP      ERROR_           ;"Arguments"
ARGUE5:			CALL    BRAKET
			LD      A,(DE)
			CP      ')'
			JR      NZ,ARGERR
			INC     DE
			EXX
ARGUE6:			POP     BC
			LD      A,B
			INC     A
			EXX
			RET     Z               ;MARKER POPPED
			EXX
			DEC     A
			JP      M,ARGUE7        ;STRING
			POP     HL
			EXX
			POP     HL
			EXX
			POP     IX
			CALL    STORE           ;WRITE TO DUMMY
			JR      ARGUE6
ARGUE7:			CALL    POPS
			POP     IX
			CALL    STACCS
			JR      ARGUE6

; SAVLOC: SUBROUTINE TO STACK LOCAL PARAMETERS
;   OF A FUNCTION OR PROCEDURE.
; THERE IS A LOT OF STACK MANIPULATION - CARE!!
;    Inputs: IY is parameters pointer
;   Outputs: IY updated
;  Destroys: A,B,C,D,E,H,L,IX,IY,F,SP
;
SAVLOC:			POP     DE              ;RETURN ADDRESS
SAVLO1:			INC     IY              ;BUMP PAST ( OR ,
			CALL    NXT
			PUSH    DE
			EXX
			PUSH    BC
			PUSH    DE
			PUSH    HL
			EXX
			CALL    VAR_             ;DUMMY VARIABLE
			EXX
			POP     HL
			POP     DE
			POP     BC
			EXX
			POP     DE
			OR      A               ;TYPE
			JP      M,SAVLO2        ;STRING
			EXX
			PUSH    HL              ;SAVE H'L'
			EXX
			LD      B,A             ;TYPE
			CALL    LOADN
			EXX
			EX      (SP),HL
			EXX
			PUSH    HL
			PUSH    BC
			JR      SAVLO4
SAVLO2:			PUSH    AF              ;STRING TYPE
			PUSH    DE
			EXX
			PUSH    HL
			EXX
			CALL    LOADS
			EXX
			POP     HL
			EXX
			LD      C,E
			POP     DE
			CALL    CHECK
			POP     AF              ;LEVEL STACK
			LD      HL,0
			LD      B,L
			SBC     HL,BC
			ADD     HL,SP
			LD      SP,HL
			LD      B,A             ;TYPE
			PUSH    BC
			JR      Z,SAVLO4
			PUSH    DE
			LD      DE,ACCS
			EX      DE,HL
			LD      B,L
			LDIR                    ;SAVE STRING ON STACK
			POP     DE
SAVLO4:			PUSH    IX              ;VARPTR
			CALL    SAVLO5
LOCCHK:			EQU     $
SAVLO5:			CALL    CHECK
			CALL    NXT
			CP      ','             ;MORE?
			JR      Z,SAVLO1
			EX      DE,HL
			JP      (HL)            ;"RETURN"
;
DELIM:			LD      A,(IY)          ;ASSEMBLER DELIMITER
			CP      ' '
			RET     Z
			CP      ','
			RET     Z
			CP      ')'
			RET     Z
TERM:			CP      ';'             ;ASSEMBLER TERMINATOR
			RET     Z
			CP      '\'
			RET     Z
			JR      TERM0
;
TERMQ:			CALL    NXT
			CP      ELSE_
			RET     NC
TERM0:			CP      ':'             ;ASSEMBLER SEPARATOR
			RET     NC
			CP      CR
			RET
;
SPAN:			CALL    TERMQ
			RET     Z
			INC     IY
			JR      SPAN
;
EQUALS:			CALL    NXT
			INC     IY
			CP      '='
			RET     Z
			LD      A,4
			JP      ERROR_           ;"Mistake"
;
FORMAT:			CP      TAB
			JR      Z,DOTAB
			CP      SPC
			JR      Z,DOSPC
			CP      '''
			RET     NZ
			CALL    CRLF
			XOR     A
			RET
;
DOTAB:			PUSH    BC
			CALL    EXPRI
			EXX
			POP     BC
			LD      A,(IY)
			CP      ','
			JR      Z,DOTAB1
			CALL    BRAKET
			LD      A,L
TABIT:			LD      HL,COUNT
			CP      (HL)
			RET     Z
			PUSH    AF
			CALL    C,CRLF
			POP     AF
			SUB     (HL)
			JR      FILL
DOTAB1:			INC     IY
			PUSH    BC
			PUSH    HL
			CALL    EXPRI
			EXX
			POP     DE
			POP     BC
			CALL    BRAKET
			CALL    PUTCSR
			XOR     A
			RET
;
DOSPC:			PUSH    BC
			CALL    ITEMI
			EXX
			LD      A,L
			POP     BC
FILL:			OR      A
			RET     Z
			PUSH    BC
			LD      B,A
FILL1:			LD      A,' '
			CALL    OUTCHR
			DJNZ    FILL1
			POP     BC
			XOR     A
			RET
;
PTEXT:			LD      HL,ACCS
			INC     E
PTEXT1:			DEC     E
			RET     Z
			LD      A,(HL)
			INC     HL
			CALL    OUTCHR
			JR      PTEXT1
;
FETCHS:			PUSH    AF
			PUSH    BC
			PUSH    HL
			EX      (SP),IY
			CALL    XTRACT
			CALL    NXT
			EX      (SP),IY
			POP     HL
			POP     BC
			POP     AF
			RET
;
LINES:			LD      DE,ACCS
LINE1S:			LD      A,(HL)
			LD      (DE),A
			CP      CR
			RET     Z
			INC     HL
			INC     E
			JR      LINE1S
;
XTRACT:			CALL    NXT
			CP      34		;ASCII ""
			INC     IY
			JP      Z,CONS
			DEC     IY
			LD      DE,ACCS
XTRAC1:			LD      A,(IY)
			LD      (DE),A
			CP      ','
			RET     Z
			CP      CR
			RET     Z
			INC     IY
			INC     E
			JR      XTRAC1

; Search for a token at the start of a program line
; - HL: Pointer to the start of a tokenised line in the program area
; Returns:
; - HL: Pointer to the 
; -  F: Carry set if not found
; Corrupts:
; - BC
;
SEARCH:			LD      BC,0			; Clear BC
;
SRCH1:			LD      C,(HL)			;  C: Fetch the line length
			INC     C			; Check for 0, i.e. end of program marker
			DEC     C
			JR      Z,SRCH2         	; Not found the token, so end
			INC     HL			; Skip the line length and line number
			INC     HL
			INC     HL
			CP      (HL)			; Compare with the token
			RET     Z			; Found it, so return with carry not set
			DEC     C			; Skip to the next line
			DEC     C
			DEC     C
			ADD     HL,BC
			JR      SRCH1			; Rinse, lather and repeat
; 			
SRCH2:			DEC     HL              	; Token not found, so back up to the CR at the end of the last line
			SCF				; And set the carry flag
			RET

; Multiply by 4 or 5
; This is a 24-bit operation
; - DE: Number to multiple
; -  A: 4 or 5
; Returns:
; - DE: Multiplied by 5 if A = 5, otherwise multiplies by 4
; -  F: Carry if overflow
; Corrupts:
; - HL
X4OR5:			CP      5			; Check A = 5 (Z flag is used later)
;			LD      H,D			; HL = DE
;			LD      L,E
			LD	HL, DE
			ADD     HL,HL			; Multiply by 2 (note this operation preserves the zero flag)
			RET     C			; Exit if overflow
			ADD     HL,HL			; Multiply by 2 again
			RET     C			; Exit if overflow
			EX      DE,HL			; DE: Product
			RET     NZ			; Exit if A <> 5 
			ADD     HL,DE			; Add original value to HL (effectively multiplying by 5)
			EX      DE,HL			; DE: Product
			RET

; 16-bit unsigned multiply
; - HL: Operand 1
; - BC: Operand 2
; Returns:
; - HL: Result
; -  F: C if overflow
;
MUL16:			PUSH	BC
			LD	D, C			; Set up the registers for the multiplies
			LD	E, L		
			LD	L, C
			LD	C, E
			MLT	HL			; HL = H * C (*256)
			MLT	DE			; DE = L * C
			MLT	BC			; BC = B * L (*256)
			ADD	HL, BC			; HL = The sum of the two most significant multiplications
			POP	BC
			XOR	A
			SBC	H			; If H is not zero then it's an overflow
			RET	C
			LD	H, L			; HL = ((H * C) + (B * L) * 256) + (L * C)
			LD	L, A
			ADD	HL, DE
			RET

;MUL16:			EX      DE,HL
;			LD      HL,0
;			LD      A,16
;MUL16_1:		ADD     HL,HL
;			RET     C               ;OVERFLOW
;			SLA     E
;			RL      D
;			JR      NC,MUL16_2
;			ADD     HL,BC
;			RET     C
;MUL16_2:		DEC     A
;			JR      NZ,MUL16_1
;			RET

;
CHANEL:			CALL    NXT
			CP      '#'
			LD      A,45
			JP      NZ,ERROR_        ;"Missing #"
CHNL:			INC     IY              ;SKIP '#'
			CALL    ITEMI
			EXX
			EX      DE,HL
			RET

; ASSEMBLER -------------------------------------------------------------------

; LANGUAGE-INDEPENDENT CONTROL SECTION:
;  Outputs: A=delimiter, carry set if syntax error.
;
ASSEM:			CALL    SKIP
			INC     IY
			CP      ':'
			JR      Z,ASSEM
			CP      ']'
			RET     Z
			CP      CR
			RET     Z
			DEC     IY
			LD      IX,(PC)         	; Program counter (P% - defined in equs.inc)
			LD      HL,LISTON
			BIT     6,(HL)
			JR      Z,ASSEM0
			LD      IX,(OC)         	; Code origin (O% - defined in equs.inc)
ASSEM0:			PUSH    IX
			PUSH    IY
			CALL    ASMB
			POP     BC
			POP     DE
			RET     C
			CALL    SKIP
			SCF
			RET     NZ
			DEC     IY
ASSEM3:			INC     IY
			LD      A,(IY)
			CALL    TERM0
			JR      NZ,ASSEM3
			LD      A,(LISTON)
			PUSH    IX
			POP     HL
			OR      A
			SBC     HL,DE
			EX      DE,HL           	; DE: Number of bytes
			PUSH    HL
			LD      HL,(PC)
			PUSH    HL
			ADD     HL,DE
			LD      (PC),HL         	; Update PC
			BIT     6,A
			JR      Z,ASSEM5
			LD      HL,(OC)
			ADD     HL,DE
			LD      (OC),HL         	; Update OC
ASSEM5:			POP     HL              	; Old PC
			POP     IX              	; Code here
			BIT     4,A
			JR      Z,ASSEM
			LD	(R0),HL			; Store HL in R0 so we can access the MSB
			LD	A,(R0+2)		; Print out the address
			CALL	HEX
			LD      A,H			
			CALL    HEX
			LD      A,L
			CALL    HEXSP
			XOR     A
			CP      E
			JR      Z,ASSEM2
ASSEM1:			LD      A,(COUNT)
			CP      17
			LD      A,5
			CALL    NC,TABIT        ;NEXT LINE
			LD      A,(IX)
			CALL    HEXSP
			INC     IX
			DEC     E
			JR      NZ,ASSEM1
ASSEM2:			LD      A,18
			CALL    TABIT
			PUSH    IY
			POP     HL
			SBC     HL,BC
ASSEM4:			LD      A,(BC)
			CALL    OUT_
			INC     BC
			DEC     L
			JR      NZ,ASSEM4
			CALL    CRLF
			JP      ASSEM
;
HEXSP:			CALL    HEX
			LD      A,' '
			JR      OUTCH1
HEX:			PUSH    AF
			RRCA
			RRCA
			RRCA
			RRCA
			CALL    HEXOUT
			POP     AF
HEXOUT:			AND     0FH
			ADD     A,90H
			DAA
			ADC     A,40H
			DAA
OUTCH1:			JP      OUT_
;
;PROCESSOR-SPECIFIC TRANSLATION SECTION:
;
;REGISTER USAGE: B - TYPE OF MOST RECENT OPERAND
;                C - OPCODE BEING BUILT
;                D - (IX) OR (IY) FLAG
;                E - OFFSET FROM IX OR IY
;               HL - NUMERIC OPERAND VALUE
;               IX - CODE DESTINATION
;               IY - SOURCE TEXT POINTER
;   Inputs: A = initial character
;  Outputs: Carry set if syntax error.
;
ASMB:			CP      '.'
			JR      NZ,ASMB1
			INC     IY
			PUSH    IX
			CALL    VAR_
			PUSH    AF
			CALL    ZERO
			EXX
			LD      HL,(PC)
			EXX
			POP     AF
			CALL    STORE
			POP     IX
ASMB1:			CALL    SKIP
			RET     Z
			CP      TCALL
			LD      C,0C4H
			INC     IY
			JP      Z,GRPC
			DEC     IY
			LD      HL,OPCODS
			CALL    FIND
			RET     C
			LD      C,B     ;ROOT OPCODE
			LD      D,0     ;CLEAR IX/IY FLAG
;
;GROUP 0: Trivial cases requiring no computation
;GROUP 1: As Group 0, but with "ED" prefix
;
			SUB     39			; The number of opcodes in GROUP0 and GROUP1
			JR      NC,GROUP2		; If not in that range, then check GROUP2
			CP      15-39			; Anything between 15 and 39 (neat compare trick here)
			CALL    NC,ED			; Needs to be prefixed with ED
			JR      BYTE0			; Then write the opcode byte
;
;GROUP 2: BIT, RES, SET
;GROUP 3: RLC, RRC, RL, RR, SLA, SRA, SRL
;
GROUP2:			SUB     10			; The number of opcodes in GROUP2 and GROUP3
			JR      NC,GROUP4		; If not in that range, then check GROUP4
			CP      3-10			; 
			CALL    C,BIT_
			RET     C
			CALL    REGLO
			RET     C
			CALL    CB
			JR      BYTE0
;
;GROUP 4 - PUSH, POP, EX (SP)
;
GROUP4:			SUB     3			; The number of opcodes in GROUP4
			JR      NC,GROUP5		; If not in that range, then check GROUP5
G4:			CALL    PAIR				
			RET     C
			JR      BYTE0				
;
;GROUP 5 - SUB, AND, XOR, OR, CP
;GROUP 6 - ADD, ADC, SBC
;
GROUP5:			SUB     8+2
			JR      NC,GROUP7
			CP      5-8
			LD      B,7
			CALL    NC,OPND
			LD      A,B
			CP      7
			JR      NZ,G6HL
G6:			CALL    REGLO
			LD      A,C
			JR      NC,BIND1
			XOR     46H
			CALL    BIND
DB_:			CALL    NUMBER
			JP      VAL8
;
G6HL:			AND     3FH
			CP      12
			SCF
			RET     NZ
			LD      A,C
			CP      80H
			LD      C,9
			JR      Z,G4
			XOR     1CH
			RRCA
			LD      C,A
			CALL    ED
			JR      G4
;
;GROUP 7 - INC, DEC
;
GROUP7:			SUB     2
			JR      NC,GROUP8
			CALL    REGHI
			LD      A,C
BIND1:			JP      NC,BIND
			XOR     64H
			RLCA
			RLCA
			RLCA
			LD      C,A
			CALL    PAIR1
			RET     C
BYTE0:			LD      A,C
			JP      BYTE2
;
;GROUP 8 - IN
;GROUP 9 - OUT
;
GROUP8:			SUB     2
			JR      NC,GROUPA
			CP      1-2
			CALL    Z,CORN
			EX      AF,AF'
			CALL    REGHI
			RET     C
			EX      AF,AF'
			CALL    C,CORN
			INC     H
			JR      Z,BYTE0
			LD      A,B
			CP      7
			SCF
			RET     NZ
			LD      A,C
			XOR     3
			RLCA
			RLCA
			RLCA
			CALL    BYTE_
			JR      VAL8
;
;GROUP 10 - JR, DJNZ
;
GROUPA:			SUB     2
			JR      NC,GROUPB
			CP      1-2
			CALL    NZ,COND_
			LD      A,C
			JR      NC,GRPA
			LD      A,18H
GRPA:			CALL    BYTE_
			CALL    NUMBER
			LD      DE,(PC)
			INC     DE
			SCF
			SBC     HL,DE
			LD      A,L
			RLA
			SBC     A,A
			CP      H
TOOFAR:			LD      A,1
			JP      NZ,ERROR_        ;"Out of range"
VAL8:			LD      A,L
			JR      BYTE2
;
;GROUP 11 - JP
;
GROUPB:			LD      B,A		
			JR      NZ,GROUPC
			CALL    COND_
			LD      A,C
			JR      NC,GRPB
			LD      A,B
			AND     3FH
			CP      6
			LD      A,0E9H
			JR      Z,BYTE2
			LD      A,0C3H
GRPB:			CALL    BYTE_
			JR	ADDR_
;
;GROUP 12 - CALL
;
GROUPC:			DJNZ    GROUPD
GRPC:			CALL    GRPE
ADDR_:			CALL    NUMBER
VAL16:			CALL    VAL8
			LD      A,H
			JR      BYTE2
;
;GROUP 13 - RST
;
GROUPD:			DJNZ    GROUPE
			CALL    NUMBER
			AND     C
			OR      H
			JR      NZ,TOOFAR
			LD      A,L
			OR      C
BYTE2:  		JP      BYTE1
;
;GROUP 14 - RET
;
GROUPE:			DJNZ    GROUPF
GRPE:			CALL    COND_
			LD      A,C
			JP      NC,BYTE1
			OR      9
			JP      BYTE1
;
;GROUP 15 - LD
;
GROUPF:			DEC	B
			JP	NZ, GROUPZ
;			DJNZ	GROUPZ
			CALL    LDOP
			JR      NC,LDA
			CALL    REGHI
			EX      AF,AF'
			CALL    SKIP
			CP      '('
			JR      Z,LDIN
			EX      AF,AF'
			JP      NC,G6
			LD      C,1
			CALL    PAIR1
			RET     C
			LD      A,14
			CP      B
			LD      B,A
			CALL    Z,PAIR
			LD      A,B
			AND     3FH
			CP      12
			LD      A,C
			JR      NZ,GRPB
			LD      A,0F9H
			JR      BYTE1
;
LDIN:			EX      AF,AF'
			PUSH    BC
			CALL    NC,REGLO
			LD      A,C
			POP     BC
			JP      NC,BIND
			LD      C,0AH
			CALL    PAIR1
			CALL    LD16
			JP      NC,GRPB
			CALL    NUMBER
			LD      C,2
			CALL    PAIR
			CALL    LD16
			RET     C
			CALL    BYTE_
			JP      VAL16	
;
;OPT - SET OPTION
;
OPT:			DEC     B
			JP      Z,DB_
			DEC	B
			JP	NZ, ADDR_
;			DJNZ    ADDR_
			CALL    NUMBER
			LD      HL,LISTON
			LD      C,A
			RLD
			LD      A,C
			RRD
			RET
;
LDA:			CP      4
			CALL    C,ED
			LD      A,B
BYTE1:			JP      BYTE_
;
;GROUP 16 - eZ80 specific stuff
;
GROUPZ:			LD	A, B
			SUB	4			; Number of instructions in this group
			LD	B, A
			JR	Z, GROUPZ1		; TODO: Need to refactor assembler after this point
			JR	NC, MISC		; TODO: as this double JR is a bit ugly
;
GROUPZ1:
;
EDBYTE:			CALL	ED			; All eZ80 instructions with no operands			
			JP	BYTE0			
;
;MISC - DEFB, DEFW, DEFM
;
MISC:			DJNZ    OPT
			PUSH    IX
			CALL    EXPRS
			POP     IX
			LD      HL,ACCS
DB1:			XOR     A
			CP      E
			RET     Z
			LD      A,(HL)
			INC     HL
			CALL    BYTE_
			DEC     E
			JR      DB1
;
;SUBROUTINES:
;
LD16:			LD      A,B
			JR      C,LD8
			LD      A,B
			AND     3FH
			CP      12
			LD      A,C
			RET     Z
			CALL    ED
			LD      A,C
			OR      43H
			RET
;
LD8:			CP      7
			SCF
			RET     NZ
			LD      A,C
			OR      30H
			RET
;
CORN:			PUSH    BC
			CALL    OPND
			BIT     5,B
			POP     BC
			JR      Z,NUMBER
			LD      H,-1
ED:			LD      A,0EDH
			JR      BYTE_
;
CB:			LD      A,0CBH
BIND:			CP      76H
			SCF
			RET     Z               ;REJECT LD (HL),(HL)
			CALL    BYTE_
			INC     D
			RET     P
			LD      A,E
			JR      BYTE_
;
; Search through the operand table
;
OPND:			PUSH    HL			; Preserve HL
			LD      HL,OPRNDS		; The operands table
			CALL    FIND			; Find the operand
			POP     HL
			RET     C			; Ret
			BIT     7,B
			RET     Z
			BIT     3,B
			PUSH    HL
			CALL    Z,OFFSET
			LD      E,L
			POP     HL
			LD      A,0DDH
			BIT     6,B
			JR      Z,OP1
			LD      A,0FDH
OP1:			OR      A
			INC     D
			LD      D,A
			RET     M
BYTE_:			LD      (IX),A
			INC     IX
			OR      A
			RET
;
OFFSET:			LD      A,(IY)
			CP      ')'
			LD      HL,0
			RET     Z
NUMBER:			CALL    SKIP
			PUSH    BC
			PUSH    DE
			PUSH    IX
			CALL    EXPRI
			POP     IX
			EXX
			POP     DE
			POP     BC
			LD      A,L
			OR      A
			RET
;
REG:			CALL    OPND
			RET     C
			LD      A,B
			AND     3FH
			CP      8
			CCF
			RET
;
REGLO:			CALL    REG
			RET     C
			JR      ORC
;
REGHI:			CALL    REG
			RET     C
			JR      SHL3
;
COND_:			CALL    OPND
			RET     C
			LD      A,B
			AND     1FH
			SUB     16
			JR      NC,SHL3
			CP      -15
			SCF
			RET     NZ
			LD      A,3
			JR      SHL3
;
PAIR:			CALL    OPND
			RET     C
PAIR1:			LD      A,B
			AND     0FH
			SUB     8
			RET     C
			JR      SHL3
;
BIT_:			CALL    NUMBER
			CP      8
			CCF
			RET     C
SHL3:			RLCA
			RLCA
			RLCA
ORC:			OR      C
			LD      C,A
			RET
;
LDOP:			LD      HL,LDOPS
;
FIND:			CALL    SKIP				; sKIP
EXIT_:			LD      B,0
			SCF
			RET     Z
			CP      DEF_
			JR      Z,FIND0
			CP      TOR+1
			CCF
			RET     C
FIND0:			LD      A,(HL)
			OR      A
			JR      Z,EXIT_
			XOR     (IY)
			AND     01011111B
			JR      Z,FIND2
FIND1:			BIT     7,(HL)
			INC     HL
			JR      Z,FIND1
			INC     HL
			INC     B
			JR      FIND0
;
FIND2:			PUSH    IY
FIND3:			BIT     7,(HL)
			INC     IY
			INC     HL
			JR      NZ,FIND5
			CP      (HL)
			CALL    Z,SKIP0
			LD      A,(HL)
			XOR     (IY)
			AND     01011111B
			JR      Z,FIND3
FIND4:			POP     IY
			JR      FIND1
;
FIND5:			CALL    DELIM
			CALL    NZ,SIGN
			JR      NZ,FIND4
FIND6:			LD      A,B
			LD      B,(HL)
			POP     HL
			RET
;
SKIP0:			INC     HL
SKIP:			CALL    DELIM
			RET     NZ
			CALL    TERM
			RET     Z
			INC     IY
			JR      SKIP
;
SIGN:			CP      '+'
			RET     Z
			CP      '-'
			RET

; Z80 opcode list
;
; Group 0:
;
OPCODS:			DB    'NO','P'+80H,00h
			DB    'RLC','A'+80H,07h
			DB    'EX',0,'AF',0,'AF','''+80H,08h
			DB    'RRC','A'+80H,0FH
			DB    'RL','A'+80H,17H
			DB    'RR','A'+80H,1FH
			DB    'DA','A'+80H,27H
			DB    'CP','L'+80H,2FH
			DB    'SC','F'+80H,37H
			DB    'CC','F'+80H,3FH
			DB    'HAL','T'+80H,76H
			DB    'EX','X'+80H,D9H
			DB    'EX',0,'DE',0,'H','L'+80H,EBH
			DB    'D','I'+80H,F3H
			DB    'E','I'+80H,FBH
;
; Group 1:
;
			DB    'NE','G'+80H,44H
			DB    'IM',0,'0'+80H,46H
			DB    'RET','N'+80H,45H
			DB    'RET','I'+80H,4DH
			DB    'IM',0,'1'+80H,56H
			DB    'IM',0,'2'+80H,5EH
			DB    'RR','D'+80H,67H
			DB    'RL','D'+80H,6FH
			DB    'LD','I'+80H,A0H
			DB    'CP','I'+80H,A1H
			DB    'IN','I'+80H,A2H
			DB    'OUT','I'+80H,A3H
			DB    'LD','D'+80H,A8H
			DB    'CP','D'+80H,A9H
			DB    'IN','D'+80H,AAH
			DB    'OUT','D'+80H,ABH
			DB    'LDI','R'+80H,B0H
			DB    'CPI','R'+80H,B1H
			DB    'INI','R'+80H,B2H
			DB    'OTI','R'+80H,B3H
			DB    'LDD','R'+80H,B8H
			DB    'CPD','R'+80H,B9H
			DB    'IND','R'+80H,BAH
			DB    'OTD','R'+80H,BBH
;
; Group 2:
;
			DB    'BI','T'+80H,40H
			DB    'RE','S'+80H,80H
			DB    'SE','T'+80H,C0H
;
; Group 3:
;
			DB    'RL','C'+80H,00H
			DB    'RR','C'+80H,08H
			DB    'R','L'+80H,10H
			DB    'R','R'+80H,18H
			DB    'SL','A'+80H,20H
			DB    'SR','A'+80H,28H
			DB    'SR','L'+80H,38H
;
; Group 4:
;
			DB    'PO','P'+80H,C1H
			DB    'PUS','H'+80H,C5H
			DB    'EX',0,'(S','P'+80H,E3H
;
; Group 5:
;
			DB    'SU','B'+80H,90H
			DB    'AN','D'+80H,A0H
			DB    'XO','R'+80H,A8H
			DB    'O','R'+80H,B0H
			DB    'C','P'+80H,B8H
			DB    TAND,A0H				; TAND: Tokenised AND
			DB    TOR,B0H				; TOR: Tokenised OR
;
; Group 6
;
			DB    'AD','D'+80H,80H
			DB    'AD','C'+80H,88H
			DB    'SB','C'+80H,98H
;
; Group 7:
;
			DB    'IN','C'+80H,04H
			DB    'DE','C'+80H,05H
;
; Group 8:
;
			DB    'I','N'+80H,40H
;
; Group 9:
;
			DB    'OU','T'+80H,41H
;
; Group 10:
;
			DB    'J','R'+80H,20H
			DB    'DJN','Z'+80H,10H
;
; Group 11:
;
			DB    'J','P'+80H,C2H
;
; Group 12:
;
			DB    'CAL','L'+80H,C4H
;
; Group 13:
;
			DB    'RS','T'+80H,C7H
;
; Group 14:
;
			DB    'RE','T'+80H,C0H
;
; Group 15:
;
			DB    'L','D'+80H,40H
;
; Group 16: eZ80 specific (4)
;
			DB    'MLT',0,'B','C'+80H,4CH
			DB    'MLT',0,'D','E'+80H,5CH						
			DB    'MLT',0,'H','L'+80H,6CH
			DB    'MLT',0,'S','P'+80H,7CH
;
; Assembler Directives
;
			DB    DEF_ & 7FH,'M'+80H,00H
			DB    DEF_ & 7FH,'B'+80H,00H
			DB    'OP','T'+80H,00H
			DB    DEF_ & 7FH,'W'+80H,00H
;
			DB    0
			
; Operands
;
OPRNDS:			DB    'B'+80H, 00H
			DB    'C'+80H, 01H
			DB    'D'+80H, 02H
			DB    'E'+80H, 03H
			DB    'H'+80H, 04H
			DB    'L'+80H, 05H
			DB    '(H','L'+80H,06H
			DB    'A'+80H, 07H
			DB    '(I','X'+80H,86H
			DB    '(I','Y'+80H,C6H
;
			DB    'B','C'+80H,08H
			DB    'D','E'+80H,0AH
			DB    'H','L'+80H,0CH
			DB    'I','X'+80H,8CH
			DB    'I','Y'+80H,CCH
			DB    'A','F'+80H,0EH
			DB    'S','P'+80H,0EH
;
			DB    'N','Z'+80H,10H
			DB    'Z'+80H,11H
			DB    'N','C'+80H,12H
			DB    'P','O'+80H,14H
			DB    'P','E'+80H,15H
			DB    'P'+80H,16H
			DB    'M'+80H,17H
;
			DB    '(','C'+80H,20H
;
			DB    0
; Load operations
;
LDOPS:			DB    'I',0,'A'+80H,47H
			DB    'R',0,'A'+80H,4FH
			DB    'A',0,'I'+80H,57H
			DB    'A',0,'R'+80H,5FH
			DB    '(BC',0,'A'+80H,02h
			DB    '(DE',0,'A'+80H,12H
			DB    'A',0,'(B','C'+80H,0AH
			DB    'A',0,'(D','E'+80H,1AH
;
			DB    0
;
; .LIST
;
LF:			EQU     0AH
CR:			EQU     0DH