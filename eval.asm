;
; Title:	BBC Basic Interpreter - Z80 version
;		Expression Evaluation & Arithmetic Module - "EVAL"
; Author:	(C) Copyright  R.T.Russell  1984
; Modified By:	Dean Belfield
; Created:	12/05/2023
; Last Updated:	17/08/2023
;
; Modinfo:
; 07/06/2023:	Modified to run in ADL mode
; 26/06/2023:	Fixed HEX and HEXSTR
; 13/08/2023:	Added INKEY(-n) support (requires MOS 1.04)
; 17/08/2023:	Added binary constants

			.ASSUME	ADL = 1

			INCLUDE	"equs.inc"
			INCLUDE "macros.inc"
			INCLUDE "mos_api.inc"	; In MOS/src

			SEGMENT CODE
				
			XDEF	EXPR
			XDEF	EXPRN
			XDEF	EXPRI
			XDEF	EXPRS
			XDEF	ITEMI
			XDEF	LOADN
			XDEF	LOAD4
			XDEF	CONS
			XDEF	LOADS
			XDEF	SFIX
			XDEF	VAL0
			XDEF	SEARCH
			XDEF	SWAP
			XDEF	TEST
			XDEF	DECODE
			XDEF	HEXSTR
			XDEF	STR
			XDEF	ZERO
			XDEF	PUSHS
			XDEF	POPS
			XDEF	COMMA
			XDEF	BRAKET
			XDEF	NXT
			XDEF	COUNT0
				
			XREF	ADVAL
			XREF	FN
			XREF	POINT
			XREF	USR
			XREF	SYNTAX
			XREF	ERROR_
			XREF	CHECK
			XREF	GETVAR
			XREF	LISTON
			XREF	RANGE
			XREF	FPP
			XREF	GETCSR
			XREF	CHANEL
			XREF	OSSTAT
			XREF	OSBGET
			XREF	LOMEM
			XREF	HIMEM
			XREF	PAGE_
			XREF	TOP
			XREF	ERL
			XREF	ERR
			XREF	COUNT
			XREF	OSOPEN
			XREF	GETEXT
			XREF	GETPTR
			XREF	GETIME
			XREF	GETIMS
			XREF	LEXAN2
			XREF	RANDOM
			XREF	STORE5
			XREF	GETSCHR
			XREF	OSRDCH
			XREF	OSKEY
			XREF	INKEY1
			XREF	EXTERR
;
; BINARY FLOATING POINT REPRESENTATION:
;    32 BIT SIGN-MAGNITUDE NORMALIZED MANTISSA
;     8 BIT EXCESS-128 SIGNED EXPONENT
;    SIGN BIT REPLACES MANTISSA MSB (IMPLIED "1")
;    MANTISSA=0 & EXPONENT=0 IMPLIES VALUE IS ZERO.
;
; BINARY INTEGER REPRESENTATION:
;    32 BIT 2'S-COMPLEMENT SIGNED INTEGER
;     "EXPONENT" BYTE = 0 (WHEN PRESENT)
;
; NORMAL REGISTER ALLOCATION: MANTISSA - HLH'L'
;                             EXPONENT - C
;

;
; Table of addresses for functions
;
FUNTOK:			EQU	8DH			; First token number
;
FUNTBL:			DW24	DECODE			; Line number
			DW24	OPENIN			; OPENIN
			DW24	PTR			; PTR
			DW24	PAGEV			; PAGE
			DW24	TIMEV			; TIME
			DW24	LOMEMV			; LOMEM
			DW24	HIMEMV			; HIMEM
			DW24	ABSV			; ABS
			DW24	ACS			; ACS
			DW24	ADVAL			; ADVAL
			DW24	ASC			; ASC
			DW24	ASN			; ASN
			DW24	ATN			; ATN
			DW24	BGET			; BGET
			DW24	COS			; COS
			DW24	COUNTV			; COUNT
			DW24	DEG			; DEG
			DW24	ERLV			; ERL
			DW24	ERRV			; ERR
			DW24	EVAL_			; EVAL
			DW24	EXP			; EXP
			DW24	EXT			; EXT
			DW24	ZERO			; FALSE
			DW24	FN			; FN
			DW24	GET			; GET
			DW24	INKEY			; INKEY
			DW24	INSTR			; INSTR(
			DW24	INT_			; INT
			DW24	LEN			; LEN
			DW24	LN			; LN
			DW24	LOG			; LOG
			DW24	NOTK			; NOT
			DW24	OPENUP			; OPENUP
			DW24	OPENOT			; OPENOUT
			DW24	PI			; PI
			DW24	POINT			; POINT(
			DW24	POS			; POS
			DW24	RAD			; RAD
			DW24	RND			; RND
			DW24	SGN			; SGN
			DW24	SIN			; SIN
			DW24	SQR			; SQR
			DW24	TAN			; TAN
			DW24	TOPV			; TO(P)
			DW24	TRUE			; TRUE
			DW24	USR			; USR
			DW24	VAL			; VAL
			DW24	VPOS			; VPOS
			DW24	CHRS			; CHRS
			DW24	GETS			; GETS
			DW24	INKEYS			; INKEYS
			DW24	LEFTS			; LEFTS(
			DW24	MIDS			; MIDS(
			DW24	RIGHTS			; RIGHTS(
			DW24	STRS			; STR$
			DW24	STRING_			; STRINGS(
			DW24	EOF			; EOF
;
FUNTBL_END:		EQU	$
TCMD:			EQU     FUNTOK+(FUNTBL_END-FUNTBL)/3
;
ANDK:			EQU     80H
DIVK:			EQU     81H
EORK:			EQU     82H
MODK:			EQU     83H
ORK:			EQU     84H
;
SOPTBL:			DW24	SLE			; <= (STRING)
			DW24	SNE			; <>
			DW24	SGE			; >=
			DW24	SLT			; <
			DW24	SEQ			; =
			DW24	SGT			; >
;
; EXPR - VARIABLE-TYPE EXPRESSION EVALUATION
;     Expression type is returned in A'F':
;        Numeric - A' bit 7=0, F' sign bit cleared.
;         String - A' bit 7=1, F' sign bit set.
; Floating-point or integer result returned in HLH'L'C
; Integer result denoted by C=0 and HLH'L' non-zero.
; String result returned in string accumulator, DE set.
;
; Hierarchy is: (1) Variables, functions, constants, bracketed expressions.
;               (2) ^
;               (3) * / MOD DIV
;               (4) + -
;               (5) = <> <= >= > <
;               (6) AND
;               (7) EOR OR

;
; Level 7: EOR and OR
;
EXPR:			CALL    EXPR1			; Get first operator by calling Level 6
EXPR0A:			CP      EORK            	; Is operator EOR?
			JR      Z,EXPR0B		; Yes, so skip to next bit
			CP      ORK			; Is operator OR
			RET     NZ			; No, so return
;
EXPR0B:			CALL    SAVE            	; Save first operand
			CALL    EXPR1           	; Get second operand
			CALL    DOIT            	; Do the operation
			JR      EXPR0A          	; And continue
;
; Level 6: AND
;
EXPR1:			CALL    EXPR2			; Get first operator by calling Level 5
EXPR1A:			CP      ANDK			; Is operator AND?
			RET     NZ			; No, so return
			CALL    SAVE			; Save first operand
			CALL    EXPR2			; Get second operand
			CALL    DOIT			; Do the operation
			JR      EXPR1A			; And continue
;
; Level 5: Comparisons
;
EXPR2:			CALL    EXPR3			; Get first operator by calling Level 4
			CALL    RELOP?			; Is it ">", "=" or "<"?
			RET     NZ			; No, so return
			LD      B,A			; Store the first operator in B
			INC     IY              	; Bump over operator
			CALL    NXT			; 
			CALL    RELOP?          	; Is it a compound operator?
			JR      NZ,EXPR2B		; No, so skip next bit
			INC     IY			; Bump over operator
			CP      B			; Compare with first
			JP      Z,SYNTAX        	; Trap illegal combinations ">>", "==", "<<" (but not "><", "=>", "=<")
			ADD     A,B			
			LD      B,A			; B: Unique code for the compound operator
EXPR2B:			LD      A,B			; A: Code for the operator/compound operator
			EX      AF,AF'
			JP      M,EXPR2S		; If it is a string, then branch here to handle it
			EX      AF,AF'
			SUB     4
			CP      '>'-4
			JR      NZ,EXPR2C
			ADD     A,2
EXPR2C:			CALL    SAVE1
			CALL    EXPR3
			CALL    DOIT            	; NB: Must NOT be "JP DOIT"
			RET
;
EXPR2S:			EX      AF,AF'			; Handle string comparisons
			DEC     A
			AND     7
			CALL    PUSHS           	; Save string on the stack
			PUSH    AF              	; Save the operator
			CALL    EXPR3           	; Get the second string
			EX      AF,AF'
			JP      P,TYPE_
			POP     AF
			LD      C,E             	; Length of string #2
			POP     DE
			LD      HL,0
			ADD     HL,SP
			LD      B,E             	; Length of string #1
			PUSH    DE
			LD      DE,ACCS
			EX      DE,HL
			CALL    DISPT2
			POP     DE
			EX      DE,HL
			LD	A,L
			LD	HL,0
			LD	L,A
			ADD     HL,SP
			LD      SP,HL
			EX      DE,HL
			XOR     A               	; Numeric marker
			LD      C,A             	; Integer marker
			EX      AF,AF'
			LD      A,(IY)
			RET
;
; Level 4: + and -
;
EXPR3:			CALL    EXPR4			; Get first operator by calling Level 3
EXPR3A:			CP      '-'			; Is it "-"?
			JR      Z,EXPR3B		; Yes, so skip the next bit
			CP      '+'			; Is it "+"?
			RET     NZ			; No, so return
			EX      AF,AF'			; Get the type
			JP      M,EXPR3S		; Branch here if string
			EX      AF,AF'
EXPR3B:			CALL    SAVE			; Save the first operator
			CALL    EXPR4			; Fetch the second operator
			CALL    DOIT			; Do the operation
			JR      EXPR3A			; And continue
;
EXPR3S:			EX      AF,AF'			; Handle string concatenation
			INC     IY              	; Bump past the "+"
			CALL    PUSHS           	; Save the string on the stack
			CALL    EXPR4           	; Fetch the second operator
			EX      AF,AF'
			JP      P,TYPE_			; If it is not a string, then Error: "Type mismatch"
			LD	BC, 0			; Clear BC
			LD      C,E             	; C: Length of the second string
			POP     DE
			PUSH    DE
			LD      HL,ACCS
			LD	A,E			;  E: Length of the first string
			LD      DE,ACCS
			LD	E,A 			; DE: Pointer to the end of the first string
			LD      A,C			
			OR      A
			JR      Z,EXP3S3
			LD      L,A             	; Source
			ADD     A,E
			LD      E,A             	; Destination
			LD      A,19
			JP      C,ERROR_         	; A carry indicates string > 255 bytes, so Error: "String too long"
			PUSH    DE
			DEC     E
			DEC     L
			LDDR                    	; Copy
			POP     DE
EXP3S3:			EXX
			POP     BC
			CALL    POPS            	; Restore from stack
			EXX
			OR      80H             	; Flag as a string
			EX      AF,AF'
			LD      A,(IY)			; Fetch the next character
			JR      EXPR3A			; And continue
;
; Level 3: * / MOD DIV
;
EXPR4:			CALL    EXPR5			; Get first operator by calling Level 2
EXPR4A:			CP      '*'			; "*" is valid
			JR      Z,EXPR4B
			CP      '/'			; "/" is valid
			JR      Z,EXPR4B
			CP      MODK			; MOD token is valid
			JR      Z,EXPR4B
			CP      DIVK			; DIV token is valid
			RET     NZ			; And return if it is anything else
EXPR4B:			CALL    SAVE
			CALL    EXPR5
			CALL    DOIT
			JR      EXPR4A
;
; Level 2: ^
;
EXPR5:			CALL    ITEM			; Get variable
			OR      A               	; Test type
			EX      AF,AF'          	; Save type 
EXPR5A:			CALL    NXT			; Skip spaces
			CP      '^'			; Is the operator "^"?
			RET     NZ			; No, so return
			CALL    SAVE			; Save first operand
			CALL    ITEM			; Get second operand
			OR      A			; Test type
			EX      AF,AF'			; Save type
			CALL    DOIT			; Do the operation
			JR      EXPR5A			; And continue
;
; Evaluate a numeric expression
;
EXPRN:			CALL    EXPR			; Evaluate expression
			EX      AF,AF'			; Get the type
			RET     P			; And return if it is a number
			JR      TYPE_			; Otherwise Error: "Type mismatch"
;
; Evaluate a fixed-point expression 
;
EXPRI:			CALL    EXPR			; Evaluate the expression
			EX      AF,AF'			; Get the type
			JP      P,SFIX			; If it is numeric, then convert to fixed-point notation
			JR      TYPE_			; Otherwise Error: "Type mismatch"
;	
; Evaluate a string expression
;	
EXPRS:			CALL    EXPR			; Evaluate the expression
			EX      AF,AF'			; Get the type
			RET     M			; And return if it is a string
			JR      TYPE_			; Otherwise Error: "Type mismatch"
;
; Get a numeric variable
;
ITEMN:			CALL    ITEM			; Get the variable
			OR      A			; Test the type
			RET     P			; And return if it is a number
			JR      TYPE_			; Otherwise Error: "Type mismatch"
;
; Get a fixed-point variable 
;
ITEMI:			CALL    ITEM			; Get the variable
			OR      A			; Test the type
			JP      P,SFIX			; If it is numeric, then convert to fixed-point notation
			JR      TYPE_			; Otherwise Error: "Type mismatch"
;
; Get a string variable 
;
ITEMS:			CALL    ITEM			; Get the variable
			OR      A			; Test the type
			RET     M			; If it is a string, then return
;							; Otherwise
TYPE_:			LD      A,6			; Error: "Type mismatch"
			JP      ERROR_           	
;
; Evaluate a bracketed expression
;
ITEM1:			CALL    EXPR            	; Evaluate the expression
			CALL    BRAKET			; Check for closing bracket
			EX      AF,AF'
			RET
;
; HEX - Get hexadecimal constant.
;   Inputs: ASCII string at (IY)
;  Outputs: Integer result in H'L'HL, C=0, A7=0.
;           IY updated (points to delimiter)
;
HEX:			CALL    ZERO			; Set result to 0
			CALL    HEXDIG			; Fetch the character from IY
			JR      C,BADHEX		; If invalid HEX character, then Error: "Bad HEX"
HEX1:			INC     IY			; Move pointer to next character
			AND     0FH			; Clear the top nibble
			LD      B,4			; Loop counter
;
HEX2:			EXX				; Shift the result left B (4) times. This makes
			ADD.S   HL,HL			; space for the incoming nibble in the least significant 4 bits
			EXX				; .
			ADC.S   HL,HL			; .
			DJNZ    HEX2			; And loop
			EXX
			OR      L			; OR in the digit
			LD      L,A
			EXX
;
			CALL    HEXDIG			; Fetch the next character
			JR      NC,HEX1			; If it is a HEX digit then loop
			XOR     A			; Clear A
			RET
;
BADHEX:			LD      A,28
			JP      ERROR_          	; Error: "Bad HEX"
;
; BIN - Get binary constant.
;   Inputs: ASCII string at (IY)
;  Outputs: Integer result in H'L'HL, C=0, A7=0.
;           IY updated (points to delimiter)
;
BIN:			CALL    ZERO			; Set result to 0
			CALL	BINDIG			; Fetch the character from IY
			JR	C,BADBIN		; If invalid BIN character then Error: "Bad Binary"
BIN1:			INC	IY			; Move pointer to next character
			RRCA				; Bit 0 of ASCII '0' is 0, and ASCII '1' is 1, so shift that bit into carry
			EXX				; 
			ADC.S	HL,HL			; And shift back into into H'L'HL (note the ADC)
			EXX
			ADC.S	HL,HL
			CALL	BINDIG			; Fetch the next character
			JR	NC,BIN1
			XOR	A			; Clear A
			RET
;
BADBIN:			LD	A, 28			; Error: "Bad Binary" - reuses same error code as Bad HEX
			CALL	EXTERR
			DB	"Bad Binary", 0
;
; MINUS - Unary minus.
;   Inputs: IY = text pointer
;  Outputs: Numeric result, same type as argument.
;           Result in H'L'HLC
;
MINUS:			CALL    ITEMN			; Get the numeric argument
MINUS0:			DEC     C			; Check exponent (C)
			INC     C			; If it is zero, then it's either a FP zero or an integer
			JR      Z,NEGATE        	; So do an integer negation
;
			LD      A,H			; Do a FP negation by 
			XOR     80H             	; Toggling the sign bit (H)
			LD      H,A
			XOR     A               	; Numeric marker
			RET
;
NEGATE:			EXX				; This section does a two's complement negation on H'L'HLC
			LD      A,H			; First do a one's complement by negating all the bytes
			CPL
			LD      H,A
			LD      A,L
			CPL
			LD      L,A
			EXX
			LD      A,H
			CPL
			LD      H,A
			LD      A,L
			CPL
			LD      L,A
ADD1:			EXX				; Then add 1
			INC     HL			
			LD      A,H
			OR      L
			EXX
			LD      A,0             	; Numeric marker
			RET     NZ
			INC     HL
			RET
;
; ITEM - VARIABLE TYPE NUMERIC OR STRING ITEM.
; Item type is returned in A:  Bit 7=0 numeric.
;                              Bit 7=1 string.
; Numeric item returned in HLH'L'C.
; String item returned in string accumulator,
;   DE addresses byte after last (E=length).
;
ITEM:			CALL    CHECK			; Check there's at least a page of free memory left and Error: "No room" if not
			CALL    NXT			; Skip spaces
			INC     IY			; Move to the prefix character
			CP      '&'			; If `&`
			JP      Z,HEX           	; Then get a HEX constant
			CP	'%'			; If '%'
			JR	Z,BIN			; Then get a BINARY constant
			CP      '-'			; If `-`
			JR      Z,MINUS         	; Then get a negative number
			CP      '+'			; If `+`
			JP      Z,ITEMN         	; Then just fetch the number (unary plus)
			CP      '('			; If `(`
			JP      Z,ITEM1         	; Start of a bracketed expression
			CP      34			; If `"`
			JR      Z,CONS          	; Start of a string constant
			CP      TCMD			; Is it out of range of the function table?
			JP      NC,SYNTAX       	; Error: "Syntax Error"
			CP      FUNTOK			; If it is in range, then 
			JP      NC,DISPAT       	; It's a function
			DEC     IY			
			CP      ':'
			JR      NC,ITEM2		; VARIABLE?
			CP      '0'
			JP      NC,CON			; NUMERIC CONSTANT
			CP      '.'
			JP      Z,CON			; NUMERIC CONSTANT
ITEM2:			CALL    GETVAR			; VARIABLE
			JR      NZ,NOSUCH
			OR      A
			JP      M,LOADS			; STRING VARIABLE
LOADN:			OR      A
			JR      Z,LOAD1			; BYTE VARIABLE
			LD      C,0
			BIT     0,A
			JR      Z,LOAD4			; INTEGER VARIABLE
LOAD5:			LD      C,(IX+4)
LOAD4:			EXX
			LD	HL, 0			; TODO: Optimise
			LD      L,(IX+0)
			LD      H,(IX+1)
			EXX
			LD	HL, 0			; TODO: Optimise
			LD      L,(IX+2)
			LD      H,(IX+3)
			RET
;
LOAD1:			LD      HL,0
			EXX
			LD      HL,0			; TODO: Optimise
			LD      L,(IX+0)
			EXX
			LD      C,H
			RET
;
NOSUCH:			JP      C,SYNTAX
			LD      A,(LISTON)
			BIT     5,A
			LD      A,26
			JR      NZ,ERROR0		; Throw "No such variable"
NOS1:			INC     IY
			CALL    RANGE
			JR      NC,NOS1
			LD      IX,PC
			XOR     A
			LD      C,A
			JR      LOAD4
;
;CONS - Get string constant from ASCII string.
;   Inputs: ASCII string at (IY)
;  Outputs: Result in string accumulator.
;           D = MS byte of ACCS, E = string length
;           A7 = 1 (string marker)
;           IY updated
;
CONS:			LD      DE,ACCS			; DE: Pointer to the string accumulator
CONS3:			LD      A,(IY)			; Fetch the first character and
			INC     IY			; Increment the pointer
			CP      '"'			; Check for start quote
			JR      Z,CONS2			; Yes, so jump to the bit that parses the string
;
CONS1:			LD      (DE),A			; Store the character in the string accumulator
			INC     E			; Increment the string accumulator pointer
			CP      CR			; Is it CR
			JR      NZ,CONS3		; No, so keep looping
;
			LD      A,9
ERROR0:			JP      ERROR_           	; Throw error "Missing '"'
;
CONS2:			LD      A,(IY)			; Fetch the next character
			CP      '"'			; Check for end quote?
			INC     IY			; Increment the pointer
			JR      Z,CONS1			; It is the end of string marker so jump to the end routine
			DEC     IY			; 
			LD      A,80H           	; String marker
			RET
;
;CON - Get unsigned numeric constant from ASCII string.
;   Inputs: ASCII string at (IY).
;  Outputs: Variable-type result in HLH'L'C
;           IY updated (points to delimiter)
;           A7 = 0 (numeric marker)
;
CON:			PUSH    IY
			POP     IX
			LD      A,36
			CALL    FPP
			JR      C,ERROR0
			PUSH    IX
			POP     IY
			XOR     A
			RET
;
LOADS:			LD      DE,ACCS			; Where to store the string
			RRA
			JR      NC,LOADS2       	; Skip if it is a fixed string
;
			EXX				; This block was a call to LOAD4
			LD      L,(IX+0)		; The length of the string currently stored in the allocated space
			LD      H,(IX+1)		; The maximum original string length
			EXX
			LD	HL,(IX+2)		; Address of the string (24-bit)
;
			EXX
			LD      A,L
			EXX
			OR      A
			LD	BC,0			; BC: Number of bytes to copy
			LD      C,A
			LD      A,80H           	; String marker
			RET     Z
			LDIR
			RET
LOADS2:			LD      A,(HL)
			LD      (DE),A
			INC     HL
			CP      CR
			LD      A,80H           	; String marker
			RET     Z
			INC     E
			JR      NZ,LOADS2
			RET                     	; Return null string
;
;VARIABLE-TYPE FUNCTIONS:
;
;Result returned in HLH'L'C (floating point)
;Result returned in HLH'L' (C=0) (integer)
;Result returned in string accumulator & DE (string)
;All registers destroyed.
;IY (text pointer) updated.
;Bit 7 of A indicates type: 0 = numeric, 1 = string.
;
;POS - horizontal cursor position.
;VPOS - vertical cursor position.
;EOF - return status of file.
;BGET - read byte from file.
;INKEY - as GET but wait only n centiseconds.
;GET - wait for keypress and return ASCII value.
;GET(n) - input from Z80 port n.
;ASC - ASCII value of string.
;LEN - length of string.
;LOMEM - location of dynamic variables.
;HIMEM - top of available RAM.
;PAGE - start of current text page.
;TOP - address of first free byte after program.
;ERL - line number where last error occurred.
;ERR - number of last error.
;COUNT - number of printing characters since CR.
;Results are integer numeric.
;
POS:			CALL    GETCSR			; Return the horizontal cursor position
			EX      DE,HL			;  L: The X cursor position
			JP      COUNT1			; Return an 8-bit value
;			
VPOS:			CALL    GETCSR			; Return the vertical cursor position
			JP      COUNT1			; Return an 8-bit value
;			
EOF:			CALL    CHANEL			; Check for EOF
			CALL    OSSTAT
			JP      Z,TRUE			; Yes, so return true
			JP      ZERO			; Otherwise return false (zero)
;			
BGET:			CALL    CHANEL          	; Channel number
			CALL    OSBGET
			LD      L,A
			JP      COUNT0			; Return an 8-bit value
;			
INKEY:			CALL    ITEMI			; Get the argument
			BIT	7, H			; Check the sign
			EXX				; HL: The argument
			JP	NZ, INKEYM		; It's negative, so do INKEY(-n)
			CALL	INKEY0 			; Do INKEY(n)
			JR      ASC0			; Return a numeric value
;			
GET:			CALL    NXT			; Skip whitespace
			CP      '('			; Is it GET(
			JR      NZ,GET0			; No, so get a keyboard character
			CALL    ITEMI           	; Yes, so fetch the port address
			EXX
			LD      B,H			; BC: The port address
			LD      C,L
			IN      L,(C)           	;  L: Input from port BC
			JR      COUNT0			; Return an 8-bit value
;
GET0:			CALL    GETS			; Read the keyboard character			
			JR      ASC1			; And return the value
;			
ASC:			CALL    ITEMS			; Get the string argument argument
ASC0:			XOR     A			; Quickly check the length of the string in ACCS
			CP      E			; Is the pointer 0
			JP      Z,TRUE          	; Yes, so return -1 as it is a null string
ASC1:			LD      HL,(ACCS)		;  L: The first character (H will be discarded in COUNT0
			JR      COUNT0			; An 8-bit value
;
LEN:			CALL    ITEMS			; Get the string argument
			EX      DE,HL			; HL: Pointer into ACCS
			JR      COUNT0			; Return L
;			
LOMEMV:			LD      HL,(LOMEM)		; Return the LOMEM system variable
			LD	A, (LOMEM+2)
			JR      COUNT2			; A 24-bit value
;			
HIMEMV:			LD      HL,(HIMEM)		; Return the HIMEM system variable
			LD	A, (HIMEM+2)
			JR      COUNT2			; A 24-bit value
;			
PAGEV:			LD    	HL,(PAGE_)		; Return the PAGE system variable
			LD	A, (PAGE_+2)		; A 24-bit value
			JR      COUNT2
;			
TOPV:			LD      A,(IY)			; Return the TOP system variable
			INC     IY              	; Skip "P"
			CP      'P'
			JP      NZ,SYNTAX       	; Throw "Syntax Error"
			LD      HL,(TOP)
			LD	A, (TOP+2)
			JR      COUNT2
;			
ERLV:			LD      HL,(ERL)		; Return the error line
			JR      COUNT1			; A 16-bit value
;			
ERRV:			LD      HL,(ERR)		; Return the error value
			JR      COUNT0			; An 8-bit value
;			
COUNTV:			LD      HL,(COUNT)		; Return the print position sysvar

COUNT0:			LD      H,0			; Return L
COUNT1:			EXX				; Return HL
			XOR     A
			LD      C,A             	; Integer marker
			LD      H,A
			LD      L,A
			RET
COUNT2:			EXX
			LD	L,A 
			XOR	A 
			LD	C,A			; Integer marker
			LD	H,A 
			RET
;
;OPENIN - Open a file for reading.
;OPENOT - Open a file for writing.
;OPENUP - Open a file for reading or writing.
;Result is integer channel number (0 if error)
;
OPENOT:			XOR     A			; Open for writing
			JR	OPENIN_1
;			
OPENUP:			LD      A,2			; Open for reading / writing
			JR	OPENIN_1
;
OPENIN:			LD      A,1			; Open for reading
;
OPENIN_1:		PUSH    AF              	; Save OPEN type
			CALL    ITEMS           	; Fetch the filename
			LD      A,CR
			LD      (DE),A
			POP     AF              	; Restore the OPEN type
			ADD     A,-1            	; Affect the flags
			LD      HL,ACCS
			CALL    OSOPEN			; Call the OS specific OPEN routine in patch.asm
			LD      L,A			; L: Channel number
			JR      COUNT0			; Return channel number to BASIC
;
;EXT - Return length of file.
;PTR - Return current file pointer.
;Results are integer numeric.
;
EXT:			CALL    CHANEL
			CALL    GETEXT
			JR      TIME0
;
PTR:			CALL    CHANEL
			CALL    GETPTR
			JR      TIME0
;
;TIME - Return current value of elapsed time.
;Result is integer numeric.
;
TIMEV:			LD      A,(IY)
			CP      '$'
			JR      Z,TIMEVS
			CALL    GETIME
TIME0:			PUSH    DE
			EXX
			POP     HL
			XOR     A
			LD      C,A
			RET
;
;TIME$ - Return date/time string.
;Result is string
;
TIMEVS:			INC     IY              ;SKIP $
			CALL    GETIMS
			LD      A,80H           ;MARK STRING
			RET
;
;String comparison:
;
SLT:			CALL    SCP
			RET     NC
			JR      TRUE
;
SGT:			CALL    SCP
			RET     Z
			RET     C
			JR      TRUE
;
SGE:			CALL    SCP
			RET     C
			JR      TRUE
;
SLE:			CALL    SCP
			JR      Z,TRUE
			RET     NC
			JR      TRUE
;
SNE:			CALL    SCP
			RET     Z
			JR      TRUE
;
SEQ:			CALL    SCP
			RET     NZ
TRUE:			LD      A,-1
			EXX
			LD      H,A
			LD      L,A
			EXX
			LD      H,A
			LD      L,A
			INC     A
			LD      C,A
			RET
;
;PI - Return PI (3.141592654)
;Result is floating-point numeric.
;
PI:			LD      A,35
			JR      FPP1
;
;ABS - Absolute value
;Result is numeric, variable type.
;
ABSV:			LD      A,16
			JR      FPPN
;
;NOT - Complement integer.
;Result is integer numeric.
;
NOTK:			LD      A,26
			JR      FPPN
;
;DEG - Convert radians to degrees
;Result is floating-point numeric.
;
DEG:			LD      A,21
			JR      FPPN
;
;RAD - Convert degrees to radians
;Result is floating-point numeric.
;
RAD:			LD      A,27
			JR      FPPN
;
;SGN - Return -1, 0 or +1
;Result is integer numeric.
;
SGN:			LD      A,28
			JR      FPPN
;
;INT - Floor function
;Result is integer numeric.
;
INT_:			LD      A,23
			JR      FPPN
;
;SQR - square root
;Result is floating-point numeric.
;
SQR:			LD      A,30
			JR      FPPN
;
;TAN - Tangent function
;Result is floating-point numeric.
;
TAN:			LD      A,31
			JR      FPPN
;
;COS - Cosine function
;Result is floating-point numeric.
;
COS:			LD      A,20
			JR      FPPN
;
;SIN - Sine function
;Result is floating-point numeric.
;
SIN:			LD      A,29
			JR      FPPN
;
;EXP - Exponential function
;Result is floating-point numeric.
;
EXP:			LD      A,22
			JR      FPPN
;
;LN - Natural log.
;Result is floating-point numeric.
;
LN:			LD      A,24
			JR      FPPN
;
;LOG - base-10 logarithm.
;Result is floating-point numeric.
;
LOG:			LD      A,25
			JR      FPPN
;
;ASN - Arc-sine
;Result is floating-point numeric.
;
ASN:			LD      A,18
			JR      FPPN
;
;ATN - arc-tangent
;Result is floating-point numeric.
;
ATN:			LD      A,19
			JR      FPPN
;
;ACS - arc-cosine
;Result is floating point numeric.
;
ACS:			LD      A,17
FPPN:			PUSH    AF
			CALL    ITEMN
			POP     AF
FPP1:			CALL    FPP
			JP      C,ERROR_
			XOR     A
			RET
;
;SFIX - Convert to fixed-point notation
;
SFIX:			LD      A,38
			JR      FPP1
;
;SFLOAT - Convert to floating-point notation
;
SFLOAT:			LD      A,39
			JR      FPP1
;
;VAL - Return numeric value of string.
;Result is variable type numeric.
;
VAL:			CALL    ITEMS
VAL0:			XOR     A
			LD      (DE),A
			LD      IX,ACCS
			LD      A,36
			JR      FPP1
;
;EVAL - Pass string to expression evaluator.
;Result is variable type (numeric or string).
;
EVAL_:			CALL    ITEMS
			LD      A,CR
			LD      (DE),A
			PUSH    IY
			LD      DE,ACCS
			LD      IY,ACCS
			LD      C,0
			CALL    LEXAN2          ;TOKENISE
			LD      (DE),A
			INC     DE
			XOR     A
			CALL    PUSHS           ;PUT ON STACK
			LD      IY,SIZEW	;WAS 2
			ADD     IY,SP
			CALL    EXPR
			POP     IY
			ADD     IY,SP
			LD      SP,IY           ;ADJUST STACK POINTER
			POP     IY
			EX      AF,AF'
			RET
;
;RND - Random number function.
; RND gives random integer 0-&FFFFFFFF
; RND(-n) seeds random number & returns -n.
; RND(0) returns last value in RND(1) form.
; RND(1) returns floating-point 0-0.99999999.
; RND(n) returns random integer 1-n.
;
RND:			LD      IX,RANDOM
			CALL    NXT
			CP      '('
			JR      Z,RND5          ;ARGUMENT FOLLOWS
			CALL    LOAD5
RND1:			RR      C
			LD      B,32
RND2:			EXX                     ;CALCULATE NEXT
			ADC.S   HL,HL
			EXX
			ADC.S   HL,HL
			BIT     3,L
			JR      Z,RND3
			CCF
RND3:			DJNZ    RND2
RND4:			RL      C               ;SAVE CARRY
			CALL    STORE5          ;STORE NEW NUMBER
			XOR     A
			LD      C,A
			RET
RND5:			CALL    ITEMI
			LD      IX,RANDOM
			BIT     7,H             ;NEGATIVE?
			SCF
			JR      NZ,RND4         ;SEED
			CALL    TEST
			PUSH    AF
			CALL    SWAP
			EXX
			CALL    LOAD5
			CALL    NZ,RND1         ;NEXT IF NON-ZERO
			EXX                     ;SCRAMBLE (CARE!)
			LD      C,7FH
RND6:			BIT     7,H             ;FLOAT
			JR      NZ,RND7
			EXX
			ADD.S   HL,HL
			EXX
			ADC.S   HL,HL
			DEC     C
			JR      NZ,RND6
RND7:			RES     7,H             ;POSITIVE 0-0.999999
			POP     AF
			RET     Z               ;ZERO ARGUMENT
			EXX
			LD      A,E
			DEC     A
			OR      D
			EXX
			OR      E
			OR      D
			RET     Z               ;ARGUMENT=1
			LD      B,0             ;INTEGER MARKER
			LD      A,10
			CALL    FPP             ;MULTIPLY
			JP      C,ERROR_
			CALL    SFIX
			JP      ADD1
;
; INSTR - String search.
; Result is integer numeric.
;
INSTR:			CALL    EXPRSC			; Get the first string expression
			CALL    PUSHS           	; Push the string onto the stack
			CALL    EXPRS           	; Get the second string expression
			POP     BC			;  C: String length, B: Value of A before PUSHS was called
			LD      HL,0
			ADD     HL,SP           	; HL: Pointer to main string
			PUSH    BC              	;  C: Main string length
			LD      B,E             	;  B: Sub-string length
			CALL    NXT			; Skip whitespace
			CP      ','			; Check if there is a comma for the third parameter
			LD      A,0			;  A: Default start position in string
			JR      NZ,INSTR1		; No, so skip the next bit
			INC     IY              	; Skip the comma
			PUSH    BC              	; Save the lengths
			PUSH    HL              	; Save the pointer to the main string
			CALL    PUSHS			; Push the string onto the stack
			CALL    EXPRI			; Get the third (numeric) parameter - the starting position
			POP     BC			;  C: String length, B: Value of A before PUSHS was called (discarded)
			CALL    POPS			; Pop the string off the stack
			POP     HL              	; Restore the pointer to the main string
			POP     BC              	; Restore the lengths
			EXX
			LD      A,L			; A: The start position in the  string
			EXX
			OR      A			; Set the flags
			JR      Z,INSTR1		; If it is zero, then skip
			DEC     A
INSTR1:			LD      DE,ACCS         	; DE: Pointer to the sub string
			CALL    SEARCH			; Do the search
			POP     DE
			JR      Z,INSTR2        	; NB: Carry cleared
			SBC     HL,HL
			ADD     HL,SP
INSTR2:			SBC     HL,SP
			EX      DE,HL
			LD	A,L
			LD      HL,0
			LD	L,A
			ADD     HL,SP
			LD      SP,HL
			EX      DE,HL
			CALL    BRAKET			; Check for closing bracket
			JP      COUNT1			; Return a numeric integer
;
; SEARCH - Search string for sub-string
;    Inputs: Main string at HL length C
;            Sub-string  at DE length B
;            Starting offset A
;   Outputs: NZ - not found
;            Z - found at location HL-1
;            Carry always cleared
;
SEARCH:			PUSH    BC			; Add the starting offset to HL
			LD      BC,0
			LD      C,A
			ADD     HL,BC           	; New start address
			POP     BC
			SUB     C			; If the starting offset > main string length, then do nothing
			JR      NC,SRCH4
			NEG
			LD      C,A             	; Remaining length
;
SRCH1:			PUSH    BC
			LD	A,C
			LD	BC,0
			LD	C,A
			LD      A,(DE)
			CPIR                    	; Find the first character
			LD      A,C
			POP     BC
			JR      NZ,SRCH4
			LD      C,A
;
; This block of four instructions was commented as a bug fix by R.T.Russell
;
			DEC     B			; Bug fix
			CP      B			; Bug fix
			INC     B			; Bug fix
			JR      C,SRCH4			; Bug fix
;			
			PUSH    BC
			PUSH    DE
			PUSH    HL
			DEC     B
			JR      Z,SRCH3         	; Found!
SRCH2:			INC     DE
			LD      A,(DE)
			CP      (HL)
			JR      NZ,SRCH3
			INC     HL
			DJNZ    SRCH2
SRCH3:			POP     HL
			POP     DE
			POP     BC
			JR      NZ,SRCH1
			XOR     A               	; Flags: Z, NC
			RET                     	; Found
;
SRCH4:			OR      0FFH            	; Flags: NZ, NC
			RET                     	; Not found
;
;CHRS - Return character with given ASCII value.
;Result is string.
;
CHRS:			CALL    ITEMI
			EXX
			LD      A,L
			JR      GET1
;
;GETS - Return key pressed as stringor character at position (X,Y).
;Result is string.
;
GETS:			CALL	NXT		;NEW CODE FOR GET$(X,Y)
			CP	'('
			JP	Z, GETSCHR	;CALL FUNCTION IN PATCH.Z80
			CALL    OSRDCH
GET1:			SCF
			JR      INKEY1
;
; INKEYS - Wait up to n centiseconds for keypress.
;          Return key pressed as string or null
;          string if time elapsed.
; Result is string.
;
INKEYS:			CALL    ITEMI			; Fetch the argument
			EXX
INKEY0:			CALL    OSKEY			; This is the entry point for INKEY(n)
INKEY1:			LD      DE,ACCS			; Store the result in the string accumulator
			LD      (DE),A
			LD      A,80H
			RET     NC
			INC     E
			RET
;
; INKEYM - Check immediately whether a given key is being pressed
; Result is integer numeric
;
INKEYM:			MOSCALL	mos_getkbmap		; Get the base address of the keyboard
			INC	HL			; Index from 0
			LD	A, L			; Negate the LSB of the answer
			NEG
			LD	C, A			;  E: The positive keycode value
			LD	A, 1			; Throw an "Out of range" error
			JP	M, ERROR_		; if the argument < - 128
;
			LD	HL, BITLOOKUP		; HL: The bit lookup table
			LD	DE, 0
			LD	A, C
			AND	00000111b		; Just need the first three bits
			LD	E, A			; DE: The bit number
			ADD	HL, DE
			LD	B, (HL)			;  B: The mask
;
			LD	A, C			; Fetch the keycode again
			AND	01111000b		; And divide by 8
			RRCA
			RRCA
			RRCA
			LD	E, A			; DE: The offset (the MSW has already been cleared previously)
			ADD	IX, DE			; IX: The address
			LD	A, B			;  B: The mask
			AND	(IX+0)			; Check whether the bit is set
			JP	Z, ZERO			; No, so return 0
			JP	TRUE			; Otherwise return -1
;
; A bit lookup table
;
BITLOOKUP:		DB	01h, 02h, 04h, 08h
			DB	10h, 20h, 40h, 80h
;
; MID$ - Return sub-string.
; Result is string.
;
MIDS:			CALL    EXPRSC			; Get the first string expression
			CALL    PUSHS           	; Push the string onto the stack from the string accumulator (ACCS)
			CALL    EXPRI			; Get the second expression
			POP     BC			; C: String length, B: Value of A before PUSHS was called
			CALL    POPS			; Pop the string back off the stack to the string accumulator
			EXX
			LD      A,L			; A: The start index
			EXX
			OR      A			; If the start index is 0, then we don't need to do the next bit
			JR      Z,MIDS1
			DEC     A			
			LD      L,A			; L: The start index - 1
			SUB     E			; Subtract from the string length
			LD      E,0			; Preemptively set the string length to 0
			JR      NC,MIDS1		; If the first parameter is greater than the string length, then do nothing
			NEG				; Negate the answer and
			LD      C,A			; C: Number of bytes to copy
			CALL    RIGHT1			; We can do a RIGHT$ at this point with the result
MIDS1:			CALL    NXT			; Skip whitespace
			CP      ','			; Check for a comma
			INC     IY			; Advance to the next character in the BASIC line
			JR      Z,LEFT1			; If there is a comma then we do a LEFT$ on the remainder
			DEC     IY			; Restore the BASIC program pointer
			CALL    BRAKET			; Check for a bracket
			LD      A,80H			; String marker
			RET
;
; LEFT$ - Return left part of string.
; Carry cleared if entire string returned.
; Result is string.
;
LEFTS:			CALL    EXPRSC			; Get the first string expression
LEFT1:			CALL    PUSHS           	; Push the string onto the stack from the string accumulator (ACCS)
			CALL    EXPRI			; Get the second expression
			POP     BC			; C: String length, B: Value of A before PUSHS was called
			CALL    POPS			; Pop the string back off the stack to the string accumulator (ACCS)
			CALL    BRAKET			; Check for closing bracket
			EXX
			LD      A,L			; L: The second parameter
			EXX
			CP      E			; Compare with the string length
			JR      NC,LEFT3		; If it is greater than or equal then do nothing
			LD      L,E             	; For RIGHTS, no effect in LEFTS
LEFT2:			LD      E,A			; E: The new length of string
LEFT3:			LD      A,80H           	; String marker
			RET
;
; RIGHT$ - Return right part of string.
; Result is string.
;
RIGHTS:			CALL    LEFTS			; Call LEFTS to get the string
			RET     NC			; Do nothing if the second parameter is >= string length
			INC     E			; Check for a zero length string
			DEC     E
			RET     Z			; Yes, so do nothing
			LD      C,E			;  C: Number of bytes to copy
			LD      A,L
			SUB     E
			LD      L,A			;  L: Index into the string
RIGHT1:			LD	A,C
			LD	BC,0
			LD	C,A			; BC: Number of bytes to copy (with top word cleared)
			LD	A,L
			LD	HL,ACCS
			LD	L,A			; HL: Source (in ACCS)
			LD      DE,ACCS			; DE: Destination (start of ACCS)
			LDIR                    	; Copy
			LD      A,80H			; String marker
			RET
;
; STRINGS - Return n concatenations of a string.
; Result is string.
;
STRING_:		CALL    EXPRI			; Get number of times to replicate
			CALL    COMMA			; Check for comma
			EXX
			LD      A,L			; L: Number of iterations of string
			EXX
			PUSH    AF
			CALL    EXPRS			; Get the string
			CALL    BRAKET			; Check for closing bracket
			POP     AF			; A: Number of iterations of string
			OR      A			; Set flags
			JR      Z,LEFT2         	; If iterations is 0, then this will return an empty string
			DEC     A
			LD      C,A			; C: Loop counter
			LD      A,80H			; String marker
			RET     Z
			INC     E			; Check for empty string
			DEC     E
			RET     Z              		; And return
			LD      B,E			; B: String length tally
			LD	HL,ACCS
STRIN1:			PUSH    BC
STRIN2:			LD      A,(HL)
			INC     HL
			LD      (DE),A
			INC     E
			LD      A,19
			JP      Z,ERROR_         	; Throw a "String too long" error
			DJNZ    STRIN2
			POP     BC
			DEC     C
			JR      NZ,STRIN1
			LD      A,80H
			RET
;
;SUBROUTINES
;
;SWAP - Swap arguments
;Exchanges DE,HL D'E',H'L' and B,C
;Destroys: A,B,C,D,E,H,L,D',E',H',L'
;
SWAP:			LD      A,C
			LD      C,B
			LD      B,A
			EX      DE,HL
			EXX
			EX      DE,HL
			EXX
			RET
;
;TEST - Test HLH'L' for zero
;Outputs: Z-flag set & A=0 if zero
;Destroys: A,F
;
TEST:			LD      A,H
			OR      L
			EXX
			OR      H
			OR      L
			EXX
			RET
;
;DECODE - Decode line number in pseudo-binary.
;   Inputs: IY = Text pointer.
;   Outputs: HL=0, H'L'=line number, C=0.
;   Destroys: A,C,H,L,H',L',IY,F
;
DECODE:			EXX
			LD	HL, 0
			LD      A,(IY)
			INC     IY
			RLA
			RLA
			LD      H,A
			AND     0C0H
			XOR     (IY)
			INC     IY
			LD      L,A
			LD      A,H
			RLA
			RLA
			AND     0C0H
			XOR     (IY)
			INC     IY
			LD      H,A
			EXX
;			XOR     A
;			LD      C,A
;			LD      H,A
;			LD      L,A
			LD	HL, 0
			LD	C, L
			RET
;
;HEXSTR - convert numeric value to HEX string.
;   Inputs: HLH'L'C = integer or floating-point number
;  Outputs: String in string accumulator.
;           E = string length.  D = ACCS/256
;
HEXSTS:			INC     IY              ;SKIP TILDE
			CALL    ITEMN
			CALL    HEXSTR
			LD      A,80H
			RET
;
HEXSTR:			CALL    SFIX
			LD      BC,8
			LD      DE,ACCS
HEXST1:			PUSH    BC
			LD      B,4
			XOR     A
HEXST2:			EXX
			ADD.S	HL,HL
			EXX
			ADC.S	HL,HL
			RLA
			DJNZ    HEXST2
			POP     BC
			DEC     C
			RET     M
			JR      Z,HEXST3
			OR      A
			JR      NZ,HEXST3
			CP      B
			JR      Z,HEXST1
HEXST3:			ADD     A,90H
			DAA
			ADC     A,40H
			DAA
			LD      (DE),A
			INC     DE
			LD      B,A
			JR      HEXST1
;
;Function STR - convert numeric value to ASCII string.
;   Inputs: HLH'L'C = integer or floating-point number.
;  Outputs: String in string accumulator.
;           E = length, D = ACCS/256
;           A = 80H (type=string)
;
;First normalise for decimal output:
;
STRS:			CALL    NXT
			CP      '~'
			JR      Z,HEXSTS
			CALL    ITEMN
			LD      IX,STAVAR
			LD      A,(IX+3)
			OR      A
			LD      IX,G9-1         ;G9 FORMAT
			JR      Z,STR0
STR:			LD      IX,STAVAR
STR0:			LD      DE,ACCS
			LD      A,37
			CALL    FPP
			JP      C,ERROR_
			BIT     0,(IX+2)
STR1:			LD      A,80H           ;STRING MARKER
			RET     Z
			LD      A,C
			ADD     A,4
STR2:			CP      E
			JR      Z,STR1
			EX      DE,HL	
			LD      (HL),' '        ;TRAILING SPACE
			INC     HL
			EX      DE,HL
			JR      STR2
;
G9:			DW    9
;
;STRING COMPARE
;Compare string (DE) length B with string (HL) length C.
;Result preset to false.
;
SCP:			CALL	SCP0
;
ZERO:			LD      A,0
			EXX
			LD      H,A
			LD      L,A
			EXX
			LD      H,A
			LD      L,A
			LD      C,A
			RET
;
SCP0:			INC     B
			INC     C
SCP1:			DEC     B
			JR      Z,SCP2
			DEC     C
			JR      Z,SCP3
			LD      A,(DE)
			CP      (HL)
			RET     NZ
			INC     DE
			INC     HL
			JR      SCP1
SCP2:			OR      A
			DEC     C
			RET     Z
			SCF
			RET
SCP3:			OR      A
			INC     C
			RET
;
; PUSHS - SAVE STRING ON STACK.
;     Inputs: String in string accumulator.
;             E = string length.
;             A - saved on stack.
;   Destroys: B,C,D,E,H,L,IX,SP,F
;
PUSHS:			CALL    CHECK			; Check if there is sufficient space on the stack
			POP     IX              	; IX: Return address
			OR      A               	; Clear the carry flag
			LD	BC,0			; BC: Length of the string
			LD	C,E
			LD      HL,ACCS			; HL: Pointer to the string accumulator
			LD	DE,ACCS
			LD	E,C 			; DE: Pointer to the end of the string in the accumulator
			SBC     HL,DE			; HL: Number of bytes to reserve on the stack (a negative number)
			ADD     HL,SP			; Grow the stack
			LD      SP,HL
			LD      D,A			;  D: This needs to be set to A for some functions
			LD	B,A			; Stack A and C (the string length)
			PUSH    BC			; Note that this stacks 3 bytes, not 2; the MSB is irrelevant
			LD	B,0			; Reset B to 0 for the LDIR in this function
			JR      Z,PUSHS1        	; Is it zero length?
			LD      DE,ACCS			; DE: Destination
			EX      DE,HL			; HL: Destination, DE: Address on stack
			LDIR	                    	; Copy to stack
			CALL    CHECK			; Final check to see if there is sufficient space on the stack
PUSHS1:			JP      (IX)            	; Effectively "RET" (IX contains the return address)
;
; POPS - RESTORE STRING FROM STACK.
;     Inputs: C = string length.
;    Outputs: String in string accumulator.
;             E = string length.
;   Destroys: B,C,D,E,H,L,IX,SP,F
;
POPS:			POP     IX              	; IX: Return address
			LD	L,C			; Temporarily store string length in L
			LD	BC,0
			LD	C,L			; BC: Number of bytes to copy
			LD      HL,0			; HL: 0
			ADD     HL,SP			; HL: Stack address
			LD      DE,ACCS			; DE: Destination
			INC     C			; Quick check to see if this is a zero length string
			DEC     C
			JR      Z,POPS1         	; Yes it is, so skip
			LDIR                    	; No, so copy from the stack
POPS1:			LD      SP,HL			; Shrink the stack
			JP      (IX)            	; Effectively "RET" (IX contains the return address)
;
HEXDIG:			LD      A,(IY)
			CP      '0'
			RET     C
			CP      '9'+1
			CCF
			RET     NC
			CP      'A'
			RET     C
			SUB     'A'-10
			CP      16
			CCF
			RET
;
BINDIG:			LD	A,(IY)
			CP	'0'
			RET	C
			CP	'1'+1
			CCF
			RET
;
RELOP?:			CP      '>'
			RET     NC
			CP      '='
			RET     NC
			CP      '<'
			RET
;
EXPRSC:			CALL    EXPRS
COMMA:			CALL    NXT
			INC     IY
			CP      ','
			RET     Z
			LD      A,5
			JR      ERROR1          ;"Missing ,"
;
BRAKET:			CALL    NXT
			INC     IY
			CP      ')'
			RET     Z
			LD      A,27
ERROR1:			JP      ERROR_           ;"Missing )"
;
SAVE:			INC     IY
SAVE1:			EX      AF,AF'
			JP      M,TYPE_
			EX      AF,AF'
			EX      (SP),HL
			EXX
			PUSH    HL
			EXX
			PUSH    AF
			PUSH    BC
			JP      (HL)
;
DOIT:			EX      AF,AF'
			JP      M,TYPE_
			EXX
			POP     BC              ;RETURN ADDRESS
			EXX
			LD      A,C
			POP     BC
			LD      B,A
			POP     AF              ;OPERATOR
			EXX
			EX      DE,HL
			POP     HL
			EXX
			EX      DE,HL
			POP     HL
			EXX
			PUSH    BC
			EXX
			AND     0FH
			CALL    FPP
			JR      C,ERROR1
			XOR     A
			EX      AF,AF'          ;TYPE
			LD      A,(IY)
			RET
;
; Skip spaces
; - IY: String pointer
; Returns:
;  - A: The non-space character found
; - IY: Points to the character before that
; 
NXT:			LD      A,(IY)			; Fetch the character	
			CP      ' '			; If it is space, then return
			RET     NZ
			INC     IY			; Increment the pointer and
			JP      NXT			; Loop
;
DISPT2:			PUSH    HL
			LD      HL,SOPTBL
			JR      DISPT0
;
DISPAT:			PUSH    HL
			SUB     FUNTOK
			LD      HL,FUNTBL
DISPT0:			PUSH    BC
			
			LD	BC, 3
			LD	B, A
			MLT	BC
			ADD	HL, BC
			LD	HL, (HL)

;			ADD     A,A
;			LD      C,A
;			LD      B,0
;			ADD     HL,BC
;			LD      A,(HL)
;			INC     HL
;			LD      H,(HL)
;			LD      L,A

			POP     BC
			EX      (SP),HL
			RET                     ;OFF TO ROUTINE

