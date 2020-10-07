;* Yggdrasil (TM) Core Operating System (MCS-51): Data Pointer (DPTR) Library
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

$INCLUDE (System.inc)

#if LIB_DPTR == 1
	#ifndef DPTR_INCLUDED
		#define DPTR_INCLUDED 1
		;********BYTE FUNCTIONS********
		;IF(DPTR_BYTE_ARITH = 1)
				;****ARITHMETIC OPERATIONS****
					;**RAM <- FLASH, NO OFFSET
						PUBLIC	DPTRBAMFDNNA, DPTRBAMFDNND, DPTRBAMFDNNM, DPTRBAMFDNNS
					;**RAM <- FLASH, WITH OFFSET
						PUBLIC	DPTRBAMFDONA, DPTRBAMFDOND, DPTRBAMFDONM, DPTRBAMFDONS
					;**RAM <- REGISTER, NO OFFSET
						PUBLIC	DPTRBAMRDNNA, DPTRBAMRDNND, DPTRBAMRDNNM, DPTRBAMRDNNS
					;**RAM <- REGISTER, WITH OFFSET
						PUBLIC	DPTRBAMRDONA, DPTRBAMRDOND, DPTRBAMRDONM, DPTRBAMRDONS
					;**REGISTER <- RAM, WITH OFFSET
						PUBLIC	DPTRBARMDONA, DPTRBARMDOND, DPTRBARMDONM, DPTRBARMDONS
		;ENDIF
		;IF(DPTR_BYTE_BIT = 1)
				;****BIT OPERATIONS****
					;**RAM <- FLASH, NO OFFSET

					;**RAM <- FLASH, WITH OFFSET

					;**RAM <- REGISTER, NO OFFSET

					;**RAM <- REGISTER, WITH OFFSET

					;**REGISTER <- RAM, WITH OFFSET
		;ENDIF
		;IF(DPTR_BYTE_LOGIC = 1)
				;****LOGIC OPERATIONS****
					;**RAM <- FLASH, NO OFFSET
						PUBLIC	DPTRBGMFDNNA, DPTRBGMFDNNC, DPTRBGMFDNNO, DPTRBGMFDNNX
					;**RAM <- FLASH, WITH OFFSET
						PUBLIC	DPTRBGMFDONA, DPTRBGMFDONC, DPTRBGMFDONO, DPTRBGMFDONX
					;**RAM <- REGISTER, NO OFFSET
						PUBLIC	DPTRBGMRDNNA, DPTRBGMRDNNC, DPTRBGMRDNNO, DPTRBGMRDNNX
					;**RAM <- REGISTER, WITH OFFSET
						PUBLIC	DPTRBGMRDONA, DPTRBGMRDONC, DPTRBGMRDONO, DPTRBGMRDONX
					;**REGISTER <- RAM, WITH OFFSET
						PUBLIC	DPTRBGRMDONA, DPTRBGRMDONC, DPTRBGRMDONO, DPTRBGRMDONX
		;ENDIF
		;IF(DPTR_BYTE_MOVE = 1)
				;****MOVEMENT OPERATIONS****
						;BYTE LOADING FUNCTIONS
						EXTRN	CODE	(FLASHWRITE)
						PUBLIC	DPTRBLRFDON, DPTRBLRFINN, DPTRBLRFION, DPTRBLRXDON, DPTRBLRXION

						PUBLIC	DPTRBCFFDON, DPTRBCFMDON,	DPTRBCXFDON, DPTRBMMMDNN

						;BYTE STORING FUNCTIONS
						PUBLIC	DPTRBSRXDON, DPTRBSXRING, DPTRBSXRINN, DPTRBSXRION

		;DPTR LOADING FUNCTIONS
		PUBLIC DPTRWLDXDNN, DPTRWLDXDON, DPTRWLRXDNN, DPTRWLRXDON

		PUBLIC	DPTRWAXIDNN

		;********WORD FUNCTIONS********
			;****ARITHMETIC OPERATIONS****
				;**RAM <- FLASH, NO OFFSET

				;**RAM <- FLASH, WITH OFFSET

				;**RAM <- REGISTER, NO OFFSET

				;**RAM <- REGISTER, WITH OFFSET

				;**REGISTER <- RAM, WITH OFFSET

			;****BIT OPERATIONS****
				;**RAM <- FLASH, NO OFFSET

				;**RAM <- FLASH, WITH OFFSET

				;**RAM <- REGISTER, NO OFFSET

				;**RAM <- REGISTER, WITH OFFSET

				;**REGISTER <- RAM, WITH OFFSET

			;****LOGIC OPERATIONS****
				;**RAM <- FLASH, NO OFFSET

				;**RAM <- FLASH, WITH OFFSET

				;**RAM <- REGISTER, NO OFFSET

				;**RAM <- REGISTER, WITH OFFSET

				;**REGISTER <- RAM, WITH OFFSET


			;********STRING FUNCTIONS********
			;****ARITHMETIC OPERATIONS****
				;**RAM <- FLASH, NO OFFSET

				;**RAM <- FLASH, WITH OFFSET

				;**RAM <- REGISTER, NO OFFSET

				;**RAM <- REGISTER, WITH OFFSET

				;**REGISTER <- RAM, WITH OFFSET

			;****BIT OPERATIONS****
				;**RAM <- FLASH, NO OFFSET

				;**RAM <- FLASH, WITH OFFSET

				;**RAM <- REGISTER, NO OFFSET

				;**RAM <- REGISTER, WITH OFFSET

				;**REGISTER <- RAM, WITH OFFSET

			;****LOGIC OPERATIONS****
				;**RAM <- FLASH, NO OFFSET

				;**RAM <- FLASH, WITH OFFSET

				;**RAM <- REGISTER, NO OFFSET

				;**RAM <- REGISTER, WITH OFFSET

				;**REGISTER <- RAM, WITH OFFSET

			;****MOVEMENT OPERATIONS****
				;**FLASH <- FLASH, NO OFFSET

				;**FLASH <- FLASH, WITH OFFSET

				;**RAM <- FLASH, NO OFFSET

				;**RAM <- FLASH, WITH OFFSET

				;**RAM <- REGISTER, NO OFFSET

				;**RAM <- REGISTER, WITH OFFSET

				;**REGISTER <- RAM, WITH OFFSET


		;WORD COPYING FUNCTIONS
		PUBLIC	DPTRWCFFDON, DPTRWCFMDON, DPTRWCXFDNN, DPTRWCXFDON

		;WORD LOADING FUNCTIONS
		PUBLIC	DPTRWLDFDNN

		;WORD STORING FUNCTIONS
		PUBLIC	DPTRWSXRDNN, DPTRWSXRINN, DPTRWSXRDON, DPTRWSXRION

		;DWORD ARITHMETIC
		PUBLIC	DPTRDAMRDONA

		;DWORD COPYING FUNCTIONS
		PUBLIC	DPTRDCFFDON, DPTRDCFMDON, DPTRDCXFDON

		;DWORD LOADING FUNCTIONS
		PUBLIC	DPTRDLRXDON

		;DWORD STORING FUNCTIONS
		PUBLIC	DPTRDSXRDON

		;QWORD COPYING FUNCTIONS
		PUBLIC	DPTRQCFFDON, DPTRQCFMDON, DPTRQCMFDON

		;QWORD LOADING FUNCTIONS
		PUBLIC	DPTRQLXRDON

		;QWORD STORING FUNCTIONS
		PUBLIC	DPTRQSXRDON

		DPTR_ROUTINES	SEGMENT		CODE

		RSEG	DPTR_ROUTINES

			;WORD SIZED DATA LAYOUT IN XRAM
			;DPTR:
			;+-----+
			;| LSB |
			;+-----+
			;DPTR + 1:
			;+-----+
			;| MSB |
			;+-----+

			;********************************************************************
			;FUNCTION NAMING
			;********************************************************************
			;DPTRBLXRIOG
			;|__||||||||
			; |  |||||||
			; 1  |||||||
			;    2||||||
			;     3|||||
			;			 4||||
			;       5|||
			;				 6||
			;         7|
			;          8
			;1 = DPTR - DATA POINTER LIBRARY
			;2 - DATA SIZE
			;2 = B - BYTE
			;2 = W - WORD
			;2 = D - DOUBLEWORD
			;2 = Q - QUADWORD
			;3 - OPERATION TYPE
			;3 = A - ARITHMETIC
			;3 = B - BIT
			;3 = G - LOGIC
			;3 = M - MOVE
			;3 = R - SHIFT/ROTATE
			;3 = C - COPY
			;3 = X - EXCHANGE
			;4 - DESTINATION SELECT
			;4 = D - DPTR
			;4 = R - REGISTER(S)
			;4 = M - CURRENT RAM SPACE (IRAM, XRAM, EXTERNAL RAM)
			;4 = F - FLASH
			;5 - SOURCE SELECT
			;5 = D - DPTR
			;5 = R - REGISTER(S)
			;5 = M - CURRENT RAM SPACE (IRAM, XRAM, EXTERNAL RAM)
			;5 = F - FLASH
			;6 - ADDRESSING TYPE
			;6 = D - DIRECT ADDRESSING USING DPTR
			;6 = I - INDIRECT ADDRESSING USING [DPTR]
			;7 - ADDRESS OFFSET
			;7 = N - NO OFFSET
			;7 = O - USE OFFSET IN B IN INITIAL ADDRESS
			;8 - INCREMENT \ DECREMENT FINAL ADDRESS
			;8 = N - NO INCREMENT \ DECREMENT
			;8 = A - PRE-DECREMENT
			;8 = B - PRE-INCREMENT
			;8 = F - POST-DECREMENT
			;8 = G - POST-INCREMENT
			;9 - ARITHMETIC OPERATION
			;9 = A - ADDITION
			;9 = D - DIVISION
			;9 = M - MULTIPLICATION
			;9 = S - SUBTRACTION
			;9 - BIT OPERATION
			;9 = C - COPY
			;9 = N - NEGATE
			;9 = R - RESET
			;9 = S - SET
			;9 = T - TEST
			;9 = X - SWAP
			;9 - SHIFT/ROTATE OPERATION
			;9 = RL  - ROTATE LEFT
			;9 = RLC - ROTATE RIGHT THROUGH CARRY
			;9 = RR  - ROTATE LEFT
			;9 = RRC - ROTATE RIGHT THROUGH CARRY
			;9 = SL  - SHIFT LEFT
			;9 = SLC - SHIFT RIGHT THROUGH CARRY
			;9 = SR  - SHIFT LEFT
			;9 = SRC - SHIFT RIGHT THROUGH CARRY
			;9 - LOGIC OPERATION
			;9 = A - AND
			;9 = C - COMPLIMENT
			;9 = O - OR
			;9 = X - XOR

			;********************************************************************
			;FUNCTION INDEX
			;********************************************************************

			;********************************************************************
			;INTERNAL FUNCTIONS
			;********************************************************************
				;ADDS THE VALUE IN B TO THE DPTR
				DPTRADDB:
					PUSH	PSW
					PUSH	ACC
					MOV		A, B
					ADD		A, DPL
					MOV		DPL, A
					MOV		A, DPH
					ADDC	A, #0x00
					MOV		DPH, A
					POP		ACC
					POP		PSW
					RET

				;COPIES THE BIT IN R2 INDEXED BY A
				;INTO THE BIT IN R3 INDEXED BY B
				DPTRBITCOPY:
					;DETERMINE SHIFT DIRECTION

				;COPIES THE BYTE LOCATED AT FLASH:DPTR TO FLASH:R1:R0
				DPTRCBFF:
					CLR		A
					MOVC	A, @A+DPTR
					INC		DPTR
					CALL	LDPTRXCHG01
					CALL	FLASHWRITE
					INC		DPTR
					CALL	LDPTRXCHG01
					RET

				;COPIES THE BYTE LOCATED AT XRAM:DPTR TO FLASH:R1:R0
				DPTRCBFM:
					MOVX	A, @DPTR
					INC		DPTR
					CALL	LDPTRXCHG01
					CALL	FLASHWRITE
					INC		DPTR
					CALL	LDPTRXCHG01
					RET
				
				;COPIES THE BYTE LOCATED AT FLASH:DPTR TO XRAM:R1:R0
				DPTRCBMF:
					CLR		A
					MOVC	A, @A+DPTR
					INC		DPTR
					CALL	LDPTRXCHG01
					MOVX	@DPTR, A
					INC		DPTR
					CALL	LDPTRXCHG01
					RET
	
				;LOADS THE WORD FROM XRAM LOCATED AT DPTR INTO R1:R0
				DPTRLWX:
					PUSH	ACC
					MOVX	A, @DPTR
					MOV		R0, A
					INC		DPTR
					MOVX	A, @DPTR
					MOV		R1, A
					POP		ACC
					RET

				;STORES THE WORD IN R1:R0 AT THE XRAM LOCATION SPECIFIED BY DPTR
				DPTRSWX:
					PUSH ACC
					MOV A, R0
					MOVX @DPTR, A
					INC DPTR
					MOV A, R1
					MOVX @DPTR, A
					POP ACC
					RET

			;********************************************************************
			;FUNCTIONS
			;********************************************************************
			#if DPTR_BADD == 1
				PUBLIC	LDPTRBADD
				;ADDS THE VALUE IN A TO THE DPTR
				LDPTRBADD	PROC
					ADD		A, DPL
					MOV		DPL, A
					MOV		A, DPH
					ADDC	A, #0x00
					MOV		DPH, A
					RET
				ENDP
			#endif

			#if DPTR_BSUB == 1
				PUBLIC	LDPTRBSUB
				;SUBTRACTS THE VALUE IN A FROM THE DPTR
				LDPTRBSUB	PROC
					XCH		A, DPL
					CLR		C
					SUBB	A, DPL
					MOV		DPL, A
					MOV		A, DPH
					SUBB	A, #0x00
					MOV		DPH, A
					RET
				ENDP
			#endif

			#if DPTR_WADDAB == 1
				PUBLIC	LDPTRWADDAB
				;ADDS THE VALUE IN B:A TO THE DPTR
				;INTENDED FOR USE AFTER "MUL AB" INSTRUCTION
				LDPTRWADDAB	PROC
					ADD		A, DPL
					MOV		DPL, A
					MOV		A, DPH
					ADDC	A, B
					MOV		DPH, A
					RET
				ENDP
			#endif

			#if DPTR_WADD == 1
				PUBLIC	LDPTRWADD
				;ADDS THE VALUE IN R1:R0 TO THE DPTR
				LDPTRWADD	PROC
					PUSH	ACC
					MOV		A, DPL
					ADD		A, R0
					MOV		DPL, A
					MOV		A, DPH
					ADDC	A, R1
					MOV		DPH, A
					POP		ACC
					RET
				ENDP
			#endif

			#if	DPTR_WSUB == 1
				PUBLIC	LDPTRWSUB
				;SUBTRACTS THE VALUE IN R1:R0 FROM THE DPTR
				LDPTRWSUB	PROC
					CLR		C
					MOV		A, DPL
					SUBB	A, R0
					MOV		DPL, A
					MOV		A, DPH
					SUBB	A, R1
					MOV		DPH, A
					RET
				ENDP
			#endif

			#if DPTR_DEC == 1
				PUBLIC	LDPTRDEC
				;DECREMENTS THE DPTR
				LDPTRDEC	PROC
					CLR		C
					XCH		A, DPL
					SUBB	A, #0x01
					XCH		A, DPL
					XCH		A, DPH
					SUBB	A, #0x00
					XCH		A, DPH
					RET
				ENDP
			#endif

			#if DPTR_XCHG01 == 1
				PUBLIC	LDPTRXCHG01
				;EXCHANGES THE VALUE IN THE DPTR WITH THE VALUE IN R1:R0
				LDPTRXCHG01	PROC
					XCH		A, R0
					XCH		A, DPL
					XCH		A, R0
					XCH		A, R1
					XCH		A, DPH
					XCH		A, R1
					RET
				ENDP
			#endif

			#if DPTR_XCHG23 == 1
				PUBLIC	LDPTRXCHG23
				;EXCHANGES THE VALUE IN THE DPTR WITH THE VALUE IN R3:R2
				LDPTRXCHG23	PROC
					XCH		A, R2
					XCH		A, DPL
					XCH		A, R2
					XCH		A, R3
					XCH		A, DPH
					XCH		A, R3
					RET
				ENDP
			#endif

			;#if DPTR_XCHG45 == 1
				PUBLIC	LDPTRXCHG45
				;EXCHANGES THE VALUE IN THE DPTR WITH THE VALUE IN R3:R2
				LDPTRXCHG45	PROC
					XCH		A, R4
					XCH		A, DPL
					XCH		A, R4
					XCH		A, R5
					XCH		A, DPH
					XCH		A, R5
					RET
				ENDP
			;#endif

			#if DPTR_XCHG67 == 1
				PUBLIC	LDPTRXCHG67
				;EXCHANGES THE VALUE IN THE DPTR WITH THE VALUE IN R7:R6
				LDPTRXCHG67	PROC
					XCH		A, R6
					XCH		A, DPL
					XCH		A, R6
					XCH		A, R7
					XCH		A, DPH
					XCH		A, R7
					RET
				ENDP
			#endif

		;********************************************************************
		;BYTE OPERATIONS
		;********************************************************************
			;****ARITHMETIC****
				;**DESTINATION = RAM, SOURCE = FLASH**
					;*NO OFFSET*
						;ADDS THE BYTE IN FLASH POINTED TO BY DPTR
						;TO THE BYTE IN RAM POINTED TO BY R1:R0
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = SUM
						DPTRBAMFDNNA:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							CLR		A
							MOVC	A, @A+DPTR
							MOV		B, A
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE ADDITION & STORE IN RAM
							ADD		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;DIVIDES THE BYTE IN RAM POINTED TO BY R1:R0
						;BY THE BYTE IN FLASH POINTED TO BY DPTR
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = QUOTIENT
						;		[RAM:R1:R0+1] = REMAINDER
						DPTRBAMFDNND:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							CLR		A
							MOVC	A, @A+DPTR
							MOV		B, A
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE ADDITION & STORE IN RAM
							DIV		AB
							MOVX	@DPTR, A
							INC		DPTR
							MOV		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;MULTIPLIES THE BYTE IN RAM POINTED TO BY R1:R0
						;BY THE BYTE IN FLASH POINTED TO BY DPTR
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = PRODUCT LSB
						;		[RAM:R1:R0+1] = PRODUCT MSB
						DPTRBAMFDNNM:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							CLR		A
							MOVC	A, @A+DPTR
							MOV		B, A
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE MULTIPLY & STORE IN RAM
							MUL		AB
							MOVX	@DPTR, A
							INC		DPTR
							MOV		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;SUBTRACTS THE BYTE IN FLASH POINTED TO BY DPTR
						;FROM THE BYTE IN RAM POINTED TO BY R1:R0
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = DIFFERENCE
						DPTRBAMFDNNS:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							CLR		A
							MOVC	A, @A+DPTR
							MOV		B, A
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE SUBTRACTION & STORE IN RAM
							CLR		C
							SUBB	A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

					;*WITH OFFSET*
						;ADDS THE BYTE IN FLASH POINTED TO BY DPTR + B
						;TO THE BYTE IN RAM POINTED TO BY R1:R0 + A
						;ON ENTRY:
						;		A = RAM OFFSET
						;		B = FLASH OFFSET
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		B = VALUE ON ENTRY
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = SUM
						DPTRBAMFDONA:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							XCH		A, B
							MOVC	A, @A+DPTR
							XCH		A, B
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							CALL	DPTRADDB
							MOVX	A, @DPTR
							;DO THE ADDITION & STORE IN RAM
							ADD		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;DIVIDES THE BYTE IN RAM POINTED TO BY R1:R0
						;BY THE BYTE IN FLASH POINTED TO BY DPTR
						;ON ENTRY:
						;		A = RAM OFFSET
						;		B = FLASH OFFSET
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		B = VALUE ON ENTRY
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = QUOTIENT
						;		[RAM:R1:R0+1] = REMAINDER
						DPTRBAMFDOND:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							XCH		A, B
							MOVC	A, @A+DPTR
							XCH		A, B
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							CALL	DPTRADDB
							MOVX	A, @DPTR
							;DO THE ADDITION & STORE IN RAM
							DIV		AB
							MOVX	@DPTR, A
							INC		DPTR
							MOV		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;MULTIPLIES THE BYTE IN RAM POINTED TO BY R1:R0
						;BY THE BYTE IN FLASH POINTED TO BY DPTR
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR] = PRODUCT LSB
						;		[RAM:DPTR+1] = PRODUCT MSB
						DPTRBAMFDONM:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							XCH		A, B
							MOVC	A, @A+DPTR
							XCH		A, B
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							CALL	DPTRADDB
							MOVX	A, @DPTR
							;DO THE MULTIPLY & STORE IN RAM
							MUL		AB
							MOVX	@DPTR, A
							INC		DPTR
							MOV		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;SUBTRACTS THE BYTE IN FLASH POINTED TO BY DPTR
						;FROM THE BYTE IN RAM POINTED TO BY R1:R0
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR] = DIFFERENCE
						DPTRBAMFDONS:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							XCH		A, B
							MOVC	A, @A+DPTR
							XCH		A, B
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							CALL	DPTRADDB
							MOVX	A, @DPTR
							;DO THE SUBTRACTION & STORE IN RAM
							CLR		C
							SUBB	A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET


				;**DESTINATION = RAM, SOURCE = RAM**
					;*NO OFFSET*
						;ADDS THE BYTE IN RAM POINTED TO BY DPTR
						;TO THE BYTE IN RAM POINTED TO BY R1:R0
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = SUM
						DPTRBAMMDNNA:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM RAM
							CLR		A
							MOVX	A, @DPTR
							MOV		B, A
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE ADDITION & STORE IN RAM
							ADD		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;DIVIDES THE BYTE IN RAM POINTED TO BY R1:R0
						;BY THE BYTE IN RAM POINTED TO BY DPTR
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = QUOTIENT
						;		[RAM:R1:R0+1] = REMAINDER
						DPTRBAMMDNND:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM RAM
							CLR		A
							MOVX	A, @DPTR
							MOV		B, A
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE ADDITION & STORE IN RAM
							DIV		AB
							MOVX	@DPTR, A
							INC		DPTR
							MOV		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;MULTIPLIES THE BYTE IN RAM POINTED TO BY R1:R0
						;BY THE BYTE IN RAM POINTED TO BY DPTR
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = PRODUCT LSB
						;		[RAM:R1:R0+1] = PRODUCT MSB
						DPTRBAMMDNNM:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM RAM
							CLR		A
							MOVX	A, @DPTR
							MOV		B, A
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE MULTIPLY & STORE IN RAM
							MUL		AB
							MOVX	@DPTR, A
							INC		DPTR
							MOV		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;SUBTRACTS THE BYTE IN RAM POINTED TO BY DPTR
						;FROM THE BYTE IN RAM POINTED TO BY R1:R0
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = DIFFERENCE
						DPTRBAMMDNNS:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM RAM
							CLR		A
							MOVX	A, @DPTR
							MOV		B, A
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE SUBTRACTION & STORE IN RAM
							CLR		C
							SUBB	A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

					;*WITH OFFSET*
						;ADDS THE BYTE IN RAM POINTED TO BY DPTR + B
						;TO THE BYTE IN RAM POINTED TO BY R1:R0 + A
						;ON ENTRY:
						;		A = RAM OFFSET
						;		B = RAM OFFSET
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		B = VALUE ON ENTRY
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = SUM
						DPTRBAMMDONA:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM RAM
							XCH		A, B
							MOVX	A, @DPTR
							XCH		A, B
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							CALL	DPTRADDB
							MOVX	A, @DPTR
							;DO THE ADDITION & STORE IN RAM
							ADD		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;DIVIDES THE BYTE IN RAM POINTED TO BY R1:R0
						;BY THE BYTE IN RAM POINTED TO BY DPTR
						;ON ENTRY:
						;		A = RAM OFFSET
						;		B = RAM OFFSET
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		B = VALUE ON ENTRY
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = QUOTIENT
						;		[RAM:R1:R0+1] = REMAINDER
						DPTRBAMMDOND:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM RAM
							XCH		A, B
							MOVX	A, @DPTR
							XCH		A, B
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							CALL	DPTRADDB
							MOVX	A, @DPTR
							;DO THE ADDITION & STORE IN RAM
							DIV		AB
							MOVX	@DPTR, A
							INC		DPTR
							MOV		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;MULTIPLIES THE BYTE IN RAM POINTED TO BY R1:R0
						;BY THE BYTE IN RAM POINTED TO BY DPTR
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR] = PRODUCT LSB
						;		[RAM:DPTR+1] = PRODUCT MSB
						DPTRBAMMDONM:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM RAM
							XCH		A, B
							MOVX	A, @DPTR
							XCH		A, B
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							CALL	DPTRADDB
							MOVX	A, @DPTR
							;DO THE MULTIPLY & STORE IN RAM
							MUL		AB
							MOVX	@DPTR, A
							INC		DPTR
							MOV		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;SUBTRACTS THE BYTE IN RAM POINTED TO BY DPTR
						;FROM THE BYTE IN RAM POINTED TO BY R1:R0
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR] = DIFFERENCE
						DPTRBAMMDONS:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM RAM
							XCH		A, B
							MOVX	A, @DPTR
							XCH		A, B
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							CALL	DPTRADDB
							MOVX	A, @DPTR
							;DO THE SUBTRACTION & STORE IN RAM
							CLR		C
							SUBB	A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET


				;**DESTINATION = RAM, SOURCE = REGISTER**
					;*NO OFFSET*
						;ADDS THE BYTE IN A TO THE BYTE IN RAM POINTED TO BY DPTR
						;ON ENTRY:
						;		A = DATA
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR] = SUM
						DPTRBAMRDNNA:
							PUSH	B
							;ADD THE SUPPLIED VALUE TO THE BYTE IN RAM
							MOV		B, A
							MOVX	A, @DPTR
							ADD		A, B
							MOVX	@DPTR, A
							MOV		A, B
							;RETURN
							POP		B
							RET

						;DIVIDES THE BYTE IN RAM POINTED TO BY DPTR BY THE BYTE IN A
						;ON ENTRY:
						;		A = DATA
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR] = QUOTIENT
						;		[RAM:DPTR+1] = REMAINDER
						DPTRBAMRDNND:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;DIVIDE THE BYTE IN RAM BY THE SUPPLIED VALUE
							MOV		B, A
							MOVX	A, @DPTR
							DIV		AB
							MOVX	@DPTR, A
							INC		DPTR
							MOV		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;MULTIPLIES THE BYTE IN RAM POINTED TO BY DPTR BY THE BYTE IN A
						;ON ENTRY:
						;		A = DATA
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR] = PRODUCT LSB
						;		[RAM:DPTR+1] = PRODUCT MSB
						DPTRBAMRDNNM:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;MULTIPLY THE BYTE IN RAM BY THE SUPPLIED VALUE
							MOV		B, A
							MOVX	A, @DPTR
							MUL		AB
							MOVX	@DPTR, A
							INC		DPTR
							MOV		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;SUBTRACTS THE BYTE IN A FROM THE BYTE IN RAM POINTED TO BY DPTR
						;ON ENTRY:
						;		A = DATA
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR] = DIFFERENCE
						DPTRBAMRDNNS:
							PUSH	B
							;SUBTRACT THE SUPPLIED VALUE FROM THE BYTE IN RAM
							MOV		B, A
							MOVX	A, @DPTR
							CLR		C
							SUBB	A, B
							MOVX	@DPTR, A
							MOV		A, B
							;RETURN
							POP		B
							RET

					;*WITH OFFSET*
						;ADDS THE BYTE IN A TO THE BYTE IN RAM POINTED TO BY DPTR + B
						;ON ENTRY:
						;		A = DATA
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR+B] = SUM
						DPTRBAMRDONA:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;ADD B TO DPTR
							CALL	DPTRADDB
							;ADD THE SUPPLIED VALUE TO THE BYTE IN RAM
							MOV		B, A
							MOVX	A, @DPTR
							ADD		A, B
							MOVX	@DPTR, A
							MOV		A, B
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;DIVIDES THE BYTE IN RAM POINTED TO BY DPTR + B BY THE BYTE IN A
						;ON ENTRY:
						;		A = DATA
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR+B] = QUOTIENT
						;		[RAM:DPTR+B+1] = REMAINDER
						DPTRBAMRDOND:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;ADD B TO DPTR
							CALL	DPTRADDB
							;DIVIDE THE BYTE IN RAM BY THE SUPPLIED VALUE
							MOV		B, A
							MOVX	A, @DPTR
							DIV		AB
							MOVX	@DPTR, A
							INC		DPTR
							MOV		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;MULTIPLIES THE BYTE IN RAM POINTED TO BY DPTR + B BY THE BYTE IN A
						;ON ENTRY:
						;		A = DATA
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR+B] = PRODUCT LSB
						;		[RAM:DPTR+B+1] = PRODUCT MSB
						DPTRBAMRDONM:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;ADD B TO DPTR
							CALL	DPTRADDB
							;MULTIPLY THE BYTE IN RAM BY THE SUPPLIED VALUE
							MOV		B, A
							MOVX	A, @DPTR
							MUL		AB
							MOVX	@DPTR, A
							INC		DPTR
							MOV		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;SUBTRACTS THE BYTE IN A FROM THE BYTE IN RAM POINTED TO BY DPTR + B
						;ON ENTRY:
						;		A = DATA
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR+B] = DIFFERENCE
						DPTRBAMRDONS:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;ADD B TO DPTR
							CALL	DPTRADDB
							;SUBTRACT THE SUPPLIED VALUE FROM THE BYTE IN RAM
							MOV		B, A
							MOVX	A, @DPTR
							CLR		C
							SUBB	A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

				;**DESTINATION = REGISTER, SOURCE = FLASH**
					;*WITH OFFSET*
						;NOT NEEDED BECAUSE OPERATIONS CAN BE PERFORMED USING MOVC A, @A+DPTR FOLLOWED BY ARITHMETIC OPERATION

				;**DESTINATION = REGISTER, SOURCE = RAM**
					;*WITH OFFSET*
						;ADDS THE BYTE IN RAM POINTED TO BY DPTR + B TO THE BYTE IN A
						;ON ENTRY:
						;		A = DATA
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = SUM
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						DPTRBARMDONA:
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							PUSH	ACC
							;ADD B TO DPTR
							CALL	DPTRADDB
							;ADD THE BYTE IN RAM TO THE SUPPLIED VALUE
							POP		B
							MOVX	A, @DPTR
							ADD		A, B
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							RET

						;DIVIDES THE BYTE IN A BY THE BYTE IN RAM POINTED TO BY DPTR + B
						;ON ENTRY:
						;		A = DATA
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = QUOTIENT
						;		B = REMAINDER
						;		DPTR = VALUE ON ENTRY
						DPTRBARMDOND:
							PUSH	DPL
							PUSH	DPH
							PUSH	ACC
							;ADD B TO DPTR
							CALL	DPTRADDB
							;DIVIDE THE SUPPLIED VALUE BY THE BYTE IN RAM
							POP		B
							MOVX	A, @DPTR
							XCH		A, B
							DIV		AB
							;RETURN
							POP		DPH
							POP		DPL
							RET

						;MULTIPLIES THE BYTE IN A BY THE BYTE IN RAM POINTED TO BY DPTR + B
						;ON ENTRY:
						;		A = DATA
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = PRODUCT LSB
						;		B = PRODUCT MSB
						;		DPTR = VALUE ON ENTRY
						DPTRBARMDONM:
							PUSH	DPL
							PUSH	DPH
							PUSH	ACC
							;ADD B TO DPTR
							CALL	DPTRADDB
							;MULTIPLY THE BYTE IN RAM BY THE SUPPLIED VALUE
							POP		B
							MOVX	A, @DPTR
							MUL		AB
							;RETURN
							POP		DPH
							POP		DPL
							RET

						;SUBTRACTS THE BYTE IN RAM POINTED TO BY DPTR + B FROM THE BYTE IN A
						;ON ENTRY:
						;		A = DATA
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = DIFFERENCE
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						DPTRBARMDONS:
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							PUSH	ACC
							;ADD B TO DPTR
							CALL	DPTRADDB
							;SUBTRACT THE BYTE IN RAM FROM THE SUPPLIED VALUE
							POP		B
							MOVX	A, @DPTR
							XCH		A, B
							CLR		C
							SUBB	A, B
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							RET

			;****LOGIC****
				;**DESTINATION = RAM, SOURCE = FLASH**
					;*NO OFFSET*
						;ANDS THE BYTE IN FLASH POINTED TO BY DPTR
						;TO THE BYTE IN RAM POINTED TO BY R1:R0
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = RESULT
						DPTRBGMFDNNA:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							CLR		A
							MOVC	A, @A+DPTR
							MOV		B, A
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE AND & STORE IN RAM
							ANL		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;COMPLIMENTS THE BYTE IN FLASH POINTED TO BY DPTR
						;AND STORES IT IN THE RAM LOCATION POINTED TO BY R1:R0 
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = RESULT
						DPTRBGMFDNNC:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							CLR		A
							MOVC	A, @A+DPTR
							CPL		A
							;STORE THE BYTE IN RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;ORS THE BYTE IN FLASH POINTED TO BY DPTR
						;TO THE BYTE IN RAM POINTED TO BY R1:R0
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = RESULT
						DPTRBGMFDNNO:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							CLR		A
							MOVC	A, @A+DPTR
							MOV		B, A
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE OR & STORE IN RAM
							ORL		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;XORS THE BYTE IN FLASH POINTED TO BY DPTR
						;TO THE BYTE IN RAM POINTED TO BY R1:R0
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = RESULT
						DPTRBGMFDNNX:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							CLR		A
							MOVC	A, @A+DPTR
							MOV		B, A
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE XOR & STORE IN RAM
							XRL		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

					;*WITH OFFSET*
						;ANDS THE BYTE IN FLASH POINTED TO BY DPTR + B
						;TO THE BYTE IN RAM POINTED TO BY R1:R0 + A
						;ON ENTRY:
						;		A = RAM OFFSET
						;		B = FLASH OFFSET
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		B = VALUE ON ENTRY
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = RESULT
						DPTRBGMFDONA:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							XCH		A, B
							MOVC	A, @A+DPTR
							XCH		A, B
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							CALL	DPTRADDB
							MOVX	A, @DPTR
							;DO THE AND & STORE IN RAM
							ANL		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;COMPLIMENTS THE BYTE IN FLASH POINTED TO BY DPTR + B
						;AND STORES IT IN THE RAM LOCATION POINTED TO BY R1:R0 + A
						;ON ENTRY:
						;		A = RAM OFFSET
						;		B = FLASH OFFSET
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		B = VALUE ON ENTRY
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = RESULT
						DPTRBGMFDONC:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							XCH		A, B
							MOVC	A, @A+DPTR
							CPL		A
							;STORE THE BYTE IN RAM
							MOV		DPL, R0
							MOV		DPH, R1
							CALL	DPTRADDB
							MOVX	A, @DPTR
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;ORS THE BYTE IN FLASH POINTED TO BY DPTR + B
						;TO THE BYTE IN RAM POINTED TO BY R1:R0 + A
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR] = RESULT
						DPTRBGMFDONO:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							XCH		A, B
							MOVC	A, @A+DPTR
							XCH		A, B
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							CALL	DPTRADDB
							MOVX	A, @DPTR
							;DO THE OR & STORE IN RAM
							ORL		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;XORS THE BYTE IN FLASH POINTED TO BY DPTR + B
						;TO THE BYTE IN RAM POINTED TO BY R1:R0 + A
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR] = RESULT
						DPTRBGMFDONX:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;LOAD THE BYTE FROM FLASH
							XCH		A, B
							MOVC	A, @A+DPTR
							XCH		A, B
							;LOAD THE BYTE FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							CALL	DPTRADDB
							MOVX	A, @DPTR
							;DO THE XOR & STORE IN RAM
							XRL		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

				;**DESTINATION = RAM, SOURCE = REGISTER**
					;*NO OFFSET*
						;ANDS THE BYTE IN A TO THE BYTE IN RAM POINTED TO BY DPTR
						;ON ENTRY:
						;		A = DATA
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR] = RESULT
						DPTRBGMRDNNA:
							PUSH	B
							;AND THE SUPPLIED VALUE TO THE BYTE IN RAM
							MOV		B, A
							MOVX	A, @DPTR
							ANL		A, B
							MOVX	@DPTR, A
							MOV		A, B
							;RETURN
							POP		B
							RET

						;COMPLIMENTS THE BYTE IN RAM POINTED TO BY DPTR
						;ON ENTRY:
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR] = RESULT
						DPTRBGMRDNNC:
							PUSH	ACC
							;COMPLIMENT THE BYTE IN RAM
							MOVX	A, @DPTR
							CPL		A
							MOVX	@DPTR, A
							;RETURN
							POP		ACC
							RET

						;ORS THE BYTE IN RAM POINTED TO BY DPTR WITH THE BYTE IN A
						;ON ENTRY:
						;		A = DATA
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR] = RESULT
						DPTRBGMRDNNO:
							PUSH	B
							;OR THE BYTE IN RAM WITH THE SUPPLIED VALUE
							MOV		B, A
							MOVX	A, @DPTR
							ORL		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		B
							RET

						;XORS THE BYTE IN RAM POINTED TO BY DPTR WITH THE BYTE IN A
						;ON ENTRY:
						;		A = DATA
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR] = RESULT
						DPTRBGMRDNNX:
							PUSH	B
							;XOR THE BYTE IN RAM WITH THE SUPPLIED VALUE
							MOV		B, A
							MOVX	A, @DPTR
							XRL	A, B
							MOVX	@DPTR, A
							MOV		A, B
							;RETURN
							POP		B
							RET

					;*WITH OFFSET*
						;ANDS THE BYTE IN A WITH THE BYTE IN RAM POINTED TO BY DPTR + B
						;ON ENTRY:
						;		A = DATA
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR+B] = RESULT
						DPTRBGMRDONA:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;ADD B TO DPTR
							CALL	DPTRADDB
							;AND THE SUPPLIED VALUE TO THE BYTE IN RAM
							MOV		B, A
							MOVX	A, @DPTR
							ANL		A, B
							MOVX	@DPTR, A
							MOV		A, B
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;COMPLIMENTS THE BYTE IN RAM POINTED TO BY DPTR + B
						;ON ENTRY:
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR+B] = RESULT
						DPTRBGMRDONC:
							PUSH	ACC
							PUSH	DPL
							PUSH	DPH
							;ADD B TO DPTR
							CALL	DPTRADDB
							;COMPLIMENT THE BYTE IN RAM
							MOVX	A, @DPTR
							CPL		A
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		ACC
							RET

						;ORS THE BYTE IN A WITH THE BYTE IN RAM POINTED TO BY DPTR + B
						;ON ENTRY:
						;		A = DATA
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR+B] = RESULT
						DPTRBGMRDONO:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;ADD B TO DPTR
							CALL	DPTRADDB
							;OR THE SUPPLIED VALUE TO THE BYTE IN RAM
							MOV		B, A
							MOVX	A, @DPTR
							ORL		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;XORS THE BYTE IN A WITH THE BYTE IN RAM POINTED TO BY DPTR + B
						;ON ENTRY:
						;		A = DATA
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:DPTR+B] = RESULT
						DPTRBGMRDONX:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;ADD B TO DPTR
							CALL	DPTRADDB
							;XOR THE SUPPLIED VALUE TO THE BYTE IN RAM
							MOV		B, A
							MOVX	A, @DPTR
							XRL		A, B
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

				;**DESTINATION = REGISTER, SOURCE = FLASH**
					;*WITH OFFSET*
						;NOT NEEDED BECAUSE OPERATIONS CAN BE PERFORMED USING MOVC A, @A+DPTR FOLLOWED BY LOGIC OPERATION

				;**DESTINATION = REGISTER, SOURCE = RAM**
					;*WITH OFFSET*
						;ANDS THE BYTE IN RAM POINTED TO BY DPTR + B TO THE BYTE IN A
						;ON ENTRY:
						;		A = DATA
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = RESULT
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						DPTRBGRMDONA:
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							PUSH	ACC
							;ADD B TO DPTR
							CALL	DPTRADDB
							;AND THE BYTE IN RAM TO THE SUPPLIED VALUE
							POP		B
							MOVX	A, @DPTR
							ANL		A, B
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							RET

						;LOADS THE BYTE IN RAM POINTED TO BY DPTR + B INTO A AND COMPLIMENTS IT
						;ON ENTRY:
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = RESULT
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						DPTRBGRMDONC:
							PUSH	DPL
							PUSH	DPH
							;ADD B TO DPTR
							CALL	DPTRADDB
							;COMPLIMENT THE BYTE IN RAM
							MOVX	A, @DPTR
							CPL		A
							;RETURN
							POP		DPH
							POP		DPL
							RET

						;ORS THE BYTE IN RAM POINTED TO BY DPTR + B TO THE BYTE IN A
						;ON ENTRY:
						;		A = DATA
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = RESULT
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						DPTRBGRMDONO:
							PUSH	DPL
							PUSH	DPH
							PUSH	ACC
							;ADD B TO DPTR
							CALL	DPTRADDB
							;OR THE BYTE IN RAM TO THE SUPPLIED VALUE
							POP		B
							MOVX	A, @DPTR
							ORL		A, B
							;RETURN
							POP		DPH
							POP		DPL
							RET

						;XORS THE BYTE IN RAM POINTED TO BY DPTR + B TO THE BYTE IN A
						;ON ENTRY:
						;		A = DATA
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;ON RETURN:
						;		A = RESULT
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						DPTRBGRMDONX:
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							PUSH	ACC
							;ADD B TO DPTR
							CALL	DPTRADDB
							;OR THE BYTE IN RAM TO THE SUPPLIED VALUE
							POP		B
							MOVX	A, @DPTR
							XRL	A, B
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							RET


			;****MOVEMENT****
				;**DESTINATION = RAM, SOURCE = FLASH**
					;*NO OFFSET*
						;LOADS THE BYTE IN FLASH POINTED TO BY [DPTR] INTO A
						;ON ENTRY:
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		A = DATA
						;		DPTR = VALUE ON ENTRY
						DPTRBLRFINN:
							PUSH B
							PUSH DPL
							PUSH DPH
							;LOAD ADDRESS INTO DPTR
							MOVX A, @DPTR
							MOV B, A
							INC DPTR
							MOVX A, @DPTR
							MOV DPH, A
							MOV DPL, B
							;LOAD A FROM FLASH
							CLR A
							MOVC A, @A+DPTR
							;RETURN
							POP DPH
							POP DPL
							POP B
							RET

				;**DESTINATION = RAM, SOURCE = RAM**
					;*NO OFFSET*
						;COPIES THE BYTE AT XRAM:DPTR TO XRAM:R1:R0
						;ON ENTRY:
						;		DPTR	= SOURCE ADDRESS
						;		R1:R0	= DESTINATION ADDRESS
						;ON RETURN:
						;		DPTR	= VALUE ON ENTRY
						;		R1:R0	= VALUE ON ENTRY
						DPTRBMMMDNN:
							PUSH	ACC
							MOV		A, R0
							PUSH	ACC
							MOV		A, R1
							PUSH	ACC
							PUSH	DPL
							PUSH	DPH
							;COPY BYTE
							MOVX	A, @DPTR
							XCH		A, R0
							XCH		A, DPL
							XCH		A, R0
							XCH		A, R1
							XCH		A, DPH
							XCH		A, R1
							MOVX	@DPTR, A
							;RETURN
							POP		ACC
							MOV		R1, A
							POP		ACC
							MOV		R0, A
							POP		ACC
							POP		DPH
							POP		DPL
							RET

			;COPIES THE VALUE IN THE FLASH LOCATION ADDRESSED BY DPTR + B INTO
			;THE FLASH LOCATION ADDRESSED BY R1:R0 + A
			;ON	ENTRY:
			;	B			= SOURCE OFFSET
			;	DPTR	= SOURCE ADDRESS
			;	A			= DESTINATION OFFSET
			;	R1:R0	= DESTINATION ADDRESS
			;ON RETURN:
			;	C	= 0 IF ERROR
			;		B			= VALUE ON ENTRY
			;		DPTR	= VALUE ON ENTRY
			;		A			= VALUE ON ENTRY
			;		R1:R0	= VALUE ON ENTRY
			;	C	= 1 IF FAIL
			;		B			= VALUE ON ENTRY
			;		DPTR	= VALUE ON ENTRY
			;		A			= VALUE ON ENTRY
			;		R1:R0	= VALUE ON ENTRY
			DPTRBCFFDON:
				;SAVE REGISTERS
				PUSH	B
				PUSH	DPL
				PUSH	DPH
				PUSH	ACC
				;ADD SOURCE OFFSET TO SOURCE ADDRESS
				CALL	DPTRADDB
				;LOAD BYTE FROM FLASH
				CLR		A
				MOVC	A, @A+DPTR
				;ADD DESTINATION OFFSET TO DESITNATION ADDRESS
				POP		B
				MOV		DPL, R0
				MOV		DPH, R1
				CALL	DPTRADDB
				;WRITE BYTE TO FLASH
				CALL	FLASHWRITE
				;RESTORE REGISTERS & RETURN
				MOV		A, B
				POP		DPH
				POP		DPL
				POP		B
				RET

			;COPIES THE BYTE LOCATED AT XRAM:DPTR + B TO THE LOCATION AT FLASH:R1:R0 + A
			;ON ENTRY:
			;	A			= DESTINATION OFFSET
			;	B			= SOURCE OFFSET
			;	DPTR	= SOURCE ADDRESS
			;	R1:R0	= DESTINATION ADDRESS
			;ON RETURN:
			;	A			= VALUE ON ENTRY
			;	B			= VALUE ON ENTRY
			;	DPTR	= VALUE ON ENTRY
			;	R1:R0	= VALUE ON ENTRY
			DPTRBCFMDON:
				;SAVE REGISTERS
				PUSH	B
				PUSH	DPL
				PUSH	DPH
				PUSH	ACC
				;ADD SOURCE OFFSET TO SOURCE ADDRESS
				CALL	DPTRADDB
				;LOAD BYTE FROM XRAM
				MOVX	A, @DPTR
				;ADD DESTINATION OFFSET TO DESITNATION ADDRESS
				POP		B
				MOV		DPL, R0
				MOV		DPH, R1
				CALL	DPTRADDB
				;WRITE BYTE TO FLASH
				CALL	FLASHWRITE
				;RESTORE REGISTERS & RETURN
				MOV		A, B
				POP		DPH
				POP		DPL
				POP		B
				RET

			;COPIES THE VALUE IN THE FLASH LOCATION ADDRESSED BY DPTR + B INTO
			;THE XRAM LOCATION ADDRESSED BY R1:R0 + A
			;ON	ENTRY:
			;	B			= SOURCE OFFSET
			;	DPTR	= SOURCE ADDRESS
			;	A			= DESTINATION OFFSET
			;	R1:R0	= DESTINATION ADDRESS
			;ON RETURN:
			;	B			= VALUE ON ENTRY
			;	DPTR	= VALUE ON ENTRY
			;	A			= VALUE ON ENTRY
			;	R1:R0	= VALUE ON ENTRY
			DPTRBCXFDON:
				;SAVE REGISTERS
				PUSH	B
				PUSH	DPL
				PUSH	DPH
				PUSH	ACC
				;ADD SOURCE OFFSET TO SOURCE ADDRESS
				CALL	DPTRADDB
				;LOAD BYTE FROM FLASH
				CLR		A
				MOVC	A, @A+DPTR
				;ADD DESTINATION OFFSET TO DESITNATION ADDRESS
				POP		B
				MOV		DPL, R0
				MOV		DPH, R1
				CALL	DPTRADDB
				MOVX	@DPTR, A
				;RESTORE REGISTERS & RETURN
				MOV		A, B
				POP		DPH
				POP		DPL
				POP		B
				RET

			;LOADS THE VALUE IN THE FLASH LOCATION ADDRESSED BY DPTR + B INTO A
			;ON ENTRY:
			;		B = OFFEST
			;ON RETURN:
			;		A = VALUE
			;		B = VALUE ON ENTRY
			;		DPTR = VALUE ON ENTRY
			DPTRBLRFDON:
				PUSH B
				PUSH DPL
				PUSH DPH
				;ADD B TO DPTR
				CALL	DPTRADDB
				;LOAD A FROM FLASH
				CLR A
				MOVC A, @A+DPTR
				;RETURN
				POP DPH
				POP DPL
				POP B
				RET

			;LOADS THE VALUE IN THE FLASH LOCATION ADDRESSED BY XRAM:[DPTR + B] INTO A
			;ON ENTRY:
			;		B = OFFEST
			;		DPTR = ADDRESS OF POINTER TO STORE R1:R0 AT
			;ON RETURN:
			;		A = VALUE
			;		B = VALUE ON ENTRY
			;		DPTR = VALUE ON ENTRY
			DPTRBLRFION:
				PUSH B
				PUSH DPL
				PUSH DPH
				;ADD B TO DPTR
				CALL	DPTRADDB
				;LOAD ADDRESS INTO DPTR
				MOVX A, @DPTR
				MOV B, A
				INC DPTR
				MOVX A, @DPTR
				MOV DPH, A
				MOV DPL, B
				;LOAD A FROM FLASH
				CLR A
				MOVC A, @A+DPTR
				;RETURN
				POP DPH
				POP DPL
				POP B
				RET

			;LOADS THE VALUE IN THE LOCATION ADDRESSED BY XRAM:DPTR + B INTO A
			;ON ENTRY:
			;		B = OFFSET
			;		DPTR = BASE ADDRESS
			;ON RETURN:
			;		A = VALUE FROM XRAM
			;		DPTR = VALUE ON ENTRY
			DPTRBLRXDON	PROC
				PUSH DPL
				PUSH DPH
				;ADD B TO DPTR
				CALL	DPTRADDB
				;LOAD A FROM XRAM
				MOVX A, @DPTR
				;RETURN
				POP DPH
				POP DPL
				RET
			ENDP

			;STORES THE VALUE IN A AT THE LOCATION ADDRESSED BY XRAM:DPTR + B
			;ON ENTRY:
			;		A = DATA TO STORE
			;		B = OFFSET
			;		DPTR = BASE ADDRESS
			;ON RETURN:
			;		A = VALUE ON ENTRY
			;		B = VALUE ON ENTRY
			;		DPTR = VALUE ON ENTRY
			DPTRBSRXDON	PROC
				PUSH DPL
				PUSH DPH
				PUSH ACC
				;ADD B TO DPTR
				CALL	DPTRADDB
				;STORE A IN XRAM
				POP ACC
				MOVX @DPTR, A
				;RETURN
				POP DPH
				POP DPL
				RET
			ENDP

			;LOADS THE VALUE IN THE LOCATION ADDRESSED BY XRAM:[DPTR + B] INTO A
			;ON ENTRY:
			;		B = OFFSET
			;		DPTR = ADDRESS OF POINTER TO STORE R1:R0 AT
			;ON RETURN:
			;		A = VALUE FROM XRAM
			;		DPTR = VALUE ON ENTRY
			DPTRBLRXION:
				PUSH B
				PUSH DPL
				PUSH DPH
				;ADD B TO DPTR
				CALL	DPTRADDB
				;LOAD ADDRESS INTO DPTR
				MOVX A, @DPTR
				MOV B, A
				INC DPTR
				MOVX A, @DPTR
				MOV DPH, A
				MOV DPL, B
				;LOAD DATA
				MOVX A, @DPTR
				;RETURN
				POP DPH
				POP DPL
				POP B
				RET

			;STORES THE VALUE IN A IN THE LOCATION ADDRESSED BY XRAM:[DPTR + B]
			;ON ENTRY:
			;		A = VALUE TO STORE
			;		B = OFFEST
			;		DPTR = ADDRESS OF POINTER TO STORE A AT
			;ON RETURN:
			;		B = VALUE ON ENTRY
			;		DPTR = VALUE ON ENTRY
			DPTRBSXRION:
				PUSH B
				PUSH DPL
				PUSH DPH
				PUSH ACC
				;ADD B TO DPTR
				CALL	DPTRADDB
				;LOAD ADDRESS INTO DPTR
				MOVX A, @DPTR
				MOV B, A
				INC DPTR
				MOVX A, @DPTR
				MOV DPH, A
				MOV DPL, B
				;STORE A IN XRAM AT DPTR
				POP ACC
				MOVX @DPTR, A
				;RETURN
				POP DPH
				POP DPL
				POP B
				RET

			;STORES THE VALUE IN A IN THE LOCATION ADDRESSED BY XRAM:[DPTR]
			;ON ENTRY:
			;		A = VALUE TO STORE
			;		DPTR = ADDRESS OF POINTER TO STORE A AT
			;ON RETURN:
			;		DPTR = VALUE ON ENTRY
			DPTRBSXRINN:
				PUSH B
				PUSH DPL
				PUSH DPH
				PUSH ACC
				;LOAD ADDRESS INTO DPTR
				MOVX A, @DPTR
				MOV B, A
				INC DPTR
				MOVX A, @DPTR
				MOV DPH, A
				MOV DPL, B
				;STORE A IN XRAM AT DPTR
				POP ACC
				MOVX @DPTR, A
				;RETURN
				POP DPH
				POP DPL
				POP B
				RET

			;STORES THE VALUE IN A IN THE LOCATION ADDRESSED BY XRAM:[DPTR] THEN INCREMENTS THAT ADDRESS
			;ON ENTRY:
			;		A = VALUE TO STORE
			;		DPTR = ADDRESS OF POINTER TO STORE A AT
			;ON RETURN:
			;		DPTR = VALUE ON ENTRY
			DPTRBSXRING:
				PUSH B
				PUSH DPL
				PUSH DPH
				PUSH ACC
				;INCREMENT THE WORD IN XRAM AT DPTR
				MOVX A, @DPTR
				MOV B, A
				ADD A, #0x01
				MOVX @DPTR, A
				INC DPTR
				MOVX A, @DPTR
				PUSH ACC
				ADD A, #0x00
				MOVX @DPTR, A
				;STORE A IN XRAM AT DPTR
				POP DPH
				MOV DPL, B
				POP ACC
				MOVX @DPTR, A
				;RETURN
				POP DPH
				POP DPL
				POP B
				RET

		;********************************************************************
		;WORD OPERATIONS
		;********************************************************************
			;ARITHMETIC
				;ADDS THE VALUE IN R1:R0 TO THE VALUE IN XRAM LOCATION ADDRESSED BY DPTR + B
				;ON ENTRY:
				;		B = OFFEST
				;		DPTR = BASE ADDRESS OF VALUE
				;		R0 = LSB OF WORD TO BE STORED
				;		R1 = MSB OF WORD TO BE STORED
				;ON RETURN:
				;		B = VALUE ON ENTRY
				;		DPTR = VALUE ON ENTRY
				;		R0 = VALUE ON ENTRY
				;		R1 = VALUE ON ENTRY
				;		C = 0 IF NO CARRY
				;		C = 1 IF CARRY
				DPTRWAXADON:
					;SAVE REGISTERS
					PUSH	ACC
					PUSH	B
					PUSH	DPL
					PUSH	DPH
					;ADD B TO DPTR
					CALL	DPTRADDB
					;ADD R1:R0 TO [DPTR + B]
					MOVX	A, @DPTR
					ADD		A, R0
					MOVX	@DPTR, A
					INC		DPTR
					MOVX	A, @DPTR
					ADDC	A, R1
					MOVX	@DPTR, A
					;RESTORE REGISTERS & RETURN
					POP		DPH
					POP		DPL
					POP		B
					POP		ACC
					RET

				;DECREMENTS THE WORD IN XRAM POINTED TO BY DPTR + B
				;ON ENTRY:
				;		B = OFFEST
				;		DPTR = BASE ADDRESS OF VALUE
				;ON RETURN:
				;		B = VALUE ON ENTRY
				;		DPTR = VALUE ON ENTRY
				;		C = 0 IF NO BORROW
				;		C = 1 IF BORROW
				DPTRWAXDDON:
					;SAVE REGISTERS
					PUSH	ACC
					PUSH	B
					PUSH	DPL
					PUSH	DPH
					;ADD B TO DPTR
					CALL	DPTRADDB
					;DECREMENT THE WORD IN XRAM AT DPTR
					MOVX	A, @DPTR
					CLR		C
					SUBB	A, #0x01
					MOVX	@DPTR, A
					INC		DPTR
					MOVX	A, @DPTR
					SUBB	A, #0x00
					MOVX	@DPTR, A
					;RESTORE REGISTERS & RETURN
					POP		DPH
					POP		DPL
					POP		B
					POP		ACC
					RET

				;INCREMENTS THE WORD IN XRAM POINTED TO BY DPTR
				;ON ENTRY:
				;		B = OFFEST
				;		DPTR = BASE ADDRESS OF VALUE
				;ON RETURN:
				;		B = VALUE ON ENTRY
				;		DPTR = VALUE ON ENTRY
				;		C = 0 IF NO CARRY
				;		C = 1 IF CARRY
				DPTRWAXIDNN:
					PUSH	ACC
					PUSH	DPL
					PUSH	DPH
					;INCREMENT THE WORD IN XRAM AT DPTR
					MOVX	A, @DPTR
					ADD		A, #0x01
					MOVX	@DPTR, A
					INC		DPTR
					MOVX	A, @DPTR
					ADDC	A, #0x00
					MOVX	@DPTR, A
					;RETURN
					POP		DPH
					POP		DPL
					POP		ACC
					RET

			;COPIES
				;COPIES THE WORD LOCATED AT FLASH:[DPTR] TO THE LOCATION AT XRAM:[R1:R0]
				;ON ENTRY:
				;		DPTR = ADDRESS OF WORD IN FLASH
				;		R0 = XRAM ADDRESS LSB
				;		R1 = XRAM ADDRESS MSB
				;ON RETURN:
				;		DPTR = NEW DPTR VALUE
				;		R0 = NEW R0 VALUE
				;		R1 = NEW R1 VALUE
				DPTRWCXFDNN:
					PUSH	ACC
					MOV		A, R0
					PUSH	ACC
					MOV		A, R1
					PUSH	ACC
					PUSH	DPL
					PUSH	DPH
					;COPY LSB
					CALL	DPTRCBMF
					;COPY MSB
					CALL	DPTRCBMF
					;
					POP		DPH
					POP		DPL
					POP		ACC
					MOV		R1, ACC
					POP		ACC
					MOV		R0, ACC
					POP		ACC
					RET

				;COPIES THE WORD LOCATED AT FLASH:DPTR + B TO THE LOCATION AT FLASH:R1:R0 + A
				;ON ENTRY:
				;	A			= DESTINATION OFFSET
				;	B			= SOURCE OFFSET
				;	DPTR	= SOURCE ADDRESS
				;	R1:R0	= DESTINATION ADDRESS
				;ON RETURN:
				;	A			= VALUE ON ENTRY
				;	B			= VALUE ON ENTRY
				;	DPTR	= VALUE ON ENTRY
				;	R1:R0	= VALUE ON ENTRY
				DPTRWCFFDON:
					PUSH ACC
					XCH		A, R0																	;SAVE REGISTERS
					PUSH	ACC
					MOV		A, R1
					PUSH	ACC
					PUSH	DPL
					PUSH	DPH
					MOV A, R0
					;CALCULATE SOURCE ADDRESS
					CALL	DPTRADDB
					;CALCULATE DESTINATION ADDRESS
					ADD A, R0
					MOV R0, A
					CLR A
					ADDC A, R1
					MOV R1, A
					;COPY LSB
					CALL	DPTRCBFF
					JC		DPTRWCFFDONA
					;COPY MSB
					CALL	DPTRCBFF
				DPTRWCFFDONA:
					;RESTORE REGISTERS & RETURN
					POP		DPH
					POP		DPL
					POP		ACC
					MOV		R1, A
					POP		ACC
					MOV		R0, A
					POP		ACC
					RET

				;COPIES THE WORD LOCATED AT XRAM:DPTR + B TO THE LOCATION AT FLASH:R1:R0 + A
				;ON ENTRY:
				;	A			= DESTINATION OFFSET
				;	B			= SOURCE OFFSET
				;	DPTR	= SOURCE ADDRESS
				;	R1:R0	= DESTINATION ADDRESS
				;ON RETURN:
				;	A			= VALUE ON ENTRY
				;	B			= VALUE ON ENTRY
				;	DPTR	= VALUE ON ENTRY
				;	R1:R0	= VALUE ON ENTRY
				DPTRWCFMDON:
					PUSH ACC
					XCH		A, R0																	;SAVE REGISTERS
					PUSH	ACC
					MOV		A, R1
					PUSH	ACC
					PUSH	DPL
					PUSH	DPH
					MOV A, R0
					;CALCULATE SOURCE ADDRESS
					CALL	DPTRADDB
					;CALCULATE DESTINATION ADDRESS
					ADD A, R0
					MOV R0, A
					CLR A
					ADDC A, R1
					MOV R1, A
					;COPY BYTE 0
					CALL	DPTRCBFM
					JC		DPTRWCFMDONA
					;COPY BYTE 1
					CALL	DPTRCBFM
				DPTRWCFMDONA:
					;RESTORE REGISTERS & RETURN
					POP		DPH
					POP		DPL
					POP		ACC
					MOV		R1, A
					POP		ACC
					MOV		R0, A
					POP		ACC
					RET

				;COPIES THE WORD LOCATED AT FLASH:[DPTR + B] TO THE LOCATION AT XRAM:[R1:R0 + A]
				;ON ENTRY:
				;	A = DESTINATION OFFSET
				;	B = SOURCE OFFSET
				;	DPTR = ADDRESS OF WORD IN FLASH
				;	R0 = XRAM ADDRESS LSB
				;	R1 = XRAM ADDRESS MSB
				;ON RETURN:
				;	A = VALUE ON ENTRY
				;	B = VALUE ON ENTRY
				;	DPTR = VALUE ON ENTRY
				;	R0 = VALUE ON ENTRY
				;	R1 = VALUE ON ENTRY
				DPTRWCXFDON:
					PUSH ACC
					XCH		A, R0																	;SAVE REGISTERS
					PUSH	ACC
					MOV		A, R1
					PUSH	ACC
					PUSH	DPL
					PUSH	DPH
					MOV A, R0
					;CALCULATE SOURCE ADDRESS
					CALL	DPTRADDB
					;CALCULATE DESTINATION ADDRESS
					ADD A, R0
					MOV R0, A
					CLR A
					ADDC A, R1
					MOV R1, A
					;COPY LSB
					CALL	DPTRCBMF
					;COPY MSB
					CALL	DPTRCBMF
					;
					POP		DPH
					POP		DPL
					POP		ACC
					MOV		R1, A
					POP		ACC
					MOV		R0, A
					POP ACC
					RET

			;LOADS
				;LOADS THE DPTR WITH THE VALUE LOCATED AT FLASH:DPTR
				;ON ENTRY:
				;		DPTR = ADDRESS TO LOAD DPTR FROM
				;ON RETURN:
				;		DPTR = NEW DPTR VALUE
				DPTRWLDFDNN:
					PUSH	ACC
					PUSH	B
					;LOAD ADDRESS INTO DPTR
					CLR		A
					MOVC	A, @A+DPTR
					MOV		B, A
					INC		DPTR
					CLR		A
					MOVC	A, @A+DPTR
					MOV		DPH, B
					MOV		DPL, A
					;RETURN
					POP		B
					POP		ACC
					RET				

				;LOADS THE DPTR WITH THE VALUE LOCATED AT XRAM:DPTR
				;ON ENTRY:
				;		DPTR = ADDRESS TO LOAD DPTR FROM
				;ON RETURN:
				;		DPTR = NEW DPTR VALUE
				DPTRWLDXDNN:
					PUSH	ACC
					PUSH	B
					;LOAD ADDRESS INTO DPTR
					MOVX	A, @DPTR
					MOV		B, A
					INC		DPTR
					MOVX	A, @DPTR
					MOV		DPH, A
					MOV		DPL, B
					;RETURN
					POP		B
					POP		ACC
					RET

				;LOADS THE DPTR WITH THE VALUE LOCATED AT XRAM:DPTR + B
				;ON ENTRY:
				;		B = OFFSET
				;		DPTR = ADDRESS TO LOAD DPTR FROM
				;ON RETURN:
				;		B = DESTROYED
				;		DPTR = NEW DPTR VALUE
				DPTRWLDXDON:
					PUSH	ACC
					PUSH	B
					;ADD B TO DPTR
					CALL	DPTRADDB
					;LOAD ADDRESS INTO DPTR
					MOVX	A, @DPTR
					MOV		B, A
					INC		DPTR
					MOVX	A, @DPTR
					MOV		DPH, A
					MOV		DPL, B
					;RETURN
					POP		B
					POP		ACC
					RET

				;LOADS R1:R0 WITH THE VALUE IN THE LOCATION ADDRESSED BY XRAM:DPTR
				;ON ENTRY:
				;		DPTR = BASE ADDRESS TO STORE R1:R0 AT
				;ON RETURN:
				;		B = VALUE ON ENTRY
				;		DPTR = VALUE ON ENTRY
				;		R0 = VALUE ON ENTRY
				;		R1 = VALUE ON ENTRY
				DPTRWLRXDNN:
					PUSH ACC
					PUSH DPL
					PUSH DPH
					;LOAD R1:R0
					CALL	DPTRLWX
					;RETURN
					POP DPH
					POP DPL
					POP ACC
					RET

				;LOADS R1:R0 WITH THE VALUE IN THE LOCATION ADDRESSED BY XRAM:DPTR + B
				;ON ENTRY:
				;		B = OFFEST
				;		DPTR = BASE ADDRESS TO STORE R1:R0 AT
				;ON RETURN:
				;		B = VALUE ON ENTRY
				;		DPTR = VALUE ON ENTRY
				;		R0 = VALUE ON ENTRY
				;		R1 = VALUE ON ENTRY
				DPTRWLRXDON:
					PUSH ACC
					PUSH B
					PUSH DPL
					PUSH DPH
					;ADD B TO DPTR
					CALL	DPTRADDB
					;LOAD R1:R0
					CALL	DPTRLWX
					;RETURN
					POP DPH
					POP DPL
					POP B
					POP ACC
					RET

			;STORES
				;STORES THE VALUE IN R1:R0 IN THE LOCATION ADDRESSED BY XRAM:DPTR
				;ON ENTRY:
				;		DPTR	= ADDRESS TO STORE R1:R0 AT
				;		R0		= LSB OF WORD TO BE STORED
				;		R1		= MSB OF WORD TO BE STORED
				;ON RETURN:
				;		DPTR	= VALUE ON ENTRY
				;		R0		= VALUE ON ENTRY
				;		R1		= VALUE ON ENTRY
				DPTRWSXRDNN:
					PUSH ACC
					PUSH B
					PUSH DPL
					PUSH DPH
					;STORE R1:R0
					CALL	DPTRSWX
					;RETURN
					POP DPH
					POP DPL
					POP B
					POP ACC
					RET

				;STORES THE VALUE IN R1:R0 IN THE LOCATION ADDRESSED BY XRAM:[DPTR]
				;ON ENTRY:
				;		DPTR = ADDRESS OF POINTER TO STORE R1:R0 AT
				;		R0 = LSB OF WORD TO BE STORED
				;		R1 = MSB OF WORD TO BE STORED
				;ON RETURN:
				;		DPTR = VALUE ON ENTRY
				;		R0 = VALUE ON ENTRY
				;		R1 = VALUE ON ENTRY
				DPTRWSXRINN:
					PUSH ACC
					PUSH B
					PUSH DPL
					PUSH DPH
					;LOAD ADDRESS INTO DPTR
					MOVX A, @DPTR
					MOV B, A
					INC DPTR
					MOVX A, @DPTR
					MOV DPH, A
					MOV DPL, B
					;STORE R1:R0
					CALL	DPTRSWX
					;RETURN
					POP DPH
					POP DPL
					POP B
					POP ACC
					RET

			;STORES THE VALUE IN R1:R0 IN THE LOCATION ADDRESSED BY XRAM:DPTR + B
			;ON ENTRY:
			;		B = OFFEST
			;		DPTR = BASE ADDRESS TO STORE R1:R0 AT
			;		R0 = LSB OF WORD TO BE STORED
			;		R1 = MSB OF WORD TO BE STORED
			;ON RETURN:
			;		B = VALUE ON ENTRY
			;		DPTR = VALUE ON ENTRY
			;		R0 = VALUE ON ENTRY
			;		R1 = VALUE ON ENTRY
			DPTRWSXRDON:
				PUSH ACC
				PUSH B
				PUSH DPL
				PUSH DPH
				;ADD B TO DPTR
				CALL	DPTRADDB
				;STORE R1:R0
				CALL	DPTRSWX
				;RETURN
				POP DPH
				POP DPL
				POP B
				POP ACC
				RET

			;STORES THE VALUE IN R1:R0 IN THE LOCATION ADDRESSED BY XRAM:[DPTR + B]
			;ON ENTRY:
			;		B = OFFEST
			;		DPTR = ADDRESS OF POINTER TO STORE R1:R0 AT
			;		R0 = LSB OF WORD TO BE STORED
			;		R1 = MSB OF WORD TO BE STORED
			;ON RETURN:
			;		B = VALUE ON ENTRY
			;		DPTR = VALUE ON ENTRY
			;		R0 = VALUE ON ENTRY
			;		R1 = VALUE ON ENTRY
			DPTRWSXRION:
				PUSH ACC
				PUSH B
				PUSH DPL
				PUSH DPH
				;ADD B TO DPTR
				CALL	DPTRADDB
				;LOAD ADDRESS INTO DPTR
				MOVX A, @DPTR
				MOV B, A
				INC DPTR
				MOVX A, @DPTR
				MOV DPH, A
				MOV DPL, B
				;STORE R1:R0
				CALL	DPTRSWX
				;RETURN
				POP DPH
				POP DPL
				POP B
				POP ACC
				RET

		;********************************************************************
		;DWORD OPERATIONS
		;********************************************************************
			;****ARITHMETIC****
				;**DESTINATION = RAM, SOURCE = FLASH**
					;*NO OFFSET*
						;ADDS THE DWORD IN FLASH POINTED TO BY DPTR
						;TO THE DWORD IN RAM POINTED TO BY R1:R0
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = SUM
						DPTRDAMFDNNA:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							MOV		A, R0
							PUSH	ACC
							MOV		A, R1
							PUSH	ACC
							;LOAD BYTE 0 FROM FLASH
							CLR		A
							MOVC	A, @A+DPTR
							MOV		B, A
							INC		DPTR
							PUSH	DPL
							PUSH	DPH
							;LOAD BYTE 0 FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE ADDITION & STORE IN RAM
							ADD		A, B
							MOVX	@DPTR, A
							INC		DPTR
							MOV		R0, DPL
							MOV		R1, DPH
							;LOAD BYTE 1 FROM FLASH
							POP		DPH
							POP		DPL
							CLR		A
							MOVC	A, @A+DPTR
							MOV		B, A
							;LOAD BYTE 1 FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE ADDITION & STORE IN RAM
							ADDC	A, B
							MOVX	@DPTR, A
							;RETURN
							POP		ACC
							MOV		R1, A
							POP		ACC
							MOV		R0, A
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET

						;SUBTRACTS THE DWORD IN FLASH POINTED TO BY DPTR
						;FROM THE DWORD IN RAM POINTED TO BY R1:R0
						;ON ENTRY:
						;		R0 = RAM ADDRESS LSB
						;		R1 = RAM ADDRESS MSB
						;		DPTR = ADDRESS OF FLASH DATA
						;ON RETURN:
						;		R0 = VALUE ON ENTRY
						;		R1 = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		[RAM:R1:R0] = SUM
						DPTRDAMFDNNS:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							MOV		A, R0
							PUSH	ACC
							MOV		A, R1
							PUSH	ACC
							;LOAD BYTE 0 FROM FLASH
							CLR		A
							MOVC	A, @A+DPTR
							MOV		B, A
							INC		DPTR
							PUSH	DPL
							PUSH	DPH
							;LOAD BYTE 0 FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE ADDITION & STORE IN RAM
							CLR		C
							SUBB	A, B
							MOVX	@DPTR, A
							INC		DPTR
							MOV		R0, DPL
							MOV		R1, DPH
							;LOAD BYTE 1 FROM FLASH
							POP		DPH
							POP		DPL
							CLR		A
							MOVC	A, @A+DPTR
							MOV		B, A
							;LOAD BYTE 1 FROM RAM
							MOV		DPL, R0
							MOV		DPH, R1
							MOVX	A, @DPTR
							;DO THE ADDITION & STORE IN RAM
							SUBB	A, B
							MOVX	@DPTR, A
							;RETURN
							POP		ACC
							MOV		R1, A
							POP		ACC
							MOV		R0, A
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET


						;ADDS THE DWORD IN R3-R0 TO THE DWORD IN RAM POINTED TO BY DPTR + B
						;ON ENTRY:
						;		B = OFFEST
						;		DPTR = ADDRESS OF RAM DATA
						;		R3	= VALUE BYTE 0
						;		R3	= VALUE BYTE 1
						;		R3	= VALUE BYTE 2
						;		R3	= VALUE BYTE 3
						;ON RETURN:
						;		A = VALUE ON ENTRY
						;		B = VALUE ON ENTRY
						;		DPTR = VALUE ON ENTRY
						;		R0	= VALUE ON ENTRY
						;		R1	= VALUE ON ENTRY
						;		R2	= VALUE ON ENTRY
						;		R3	= VALUE ON ENTRY
						;		[RAM:DPTR + B] = SUM
						DPTRDAMRDONA:
							PUSH	ACC
							PUSH	B
							PUSH	DPL
							PUSH	DPH
							;ADD B TO DPTR
							CALL	DPTRADDB
							;ADD THE VALUE IN R3-R0 TO THE VALUE IN RAM
							MOVX	A, @DPTR
							ADD		A, R0
							MOVX	@DPTR, A
							INC		DPTR
							MOVX	A, @DPTR
							ADDC	A, R1
							MOVX	@DPTR, A
							INC		DPTR
							MOVX	A, @DPTR
							ADDC	A, R2
							MOVX	@DPTR, A
							INC		DPTR
							MOVX	A, @DPTR
							ADDC	A, R3
							MOVX	@DPTR, A
							;RETURN
							POP		DPH
							POP		DPL
							POP		B
							POP		ACC
							RET


				;COPIES THE DWORD LOCATED AT FLASH:DPTR + B TO THE LOCATION AT FLASH:R1:R0 + A
				;ON ENTRY:
				;	A			= DESTINATION OFFSET
				;	B			= SOURCE OFFSET
				;	DPTR	= SOURCE ADDRESS
				;	R1:R0	= DESTINATION ADDRESS
				;ON RETURN:
				;	A			= VALUE ON ENTRY
				;	B			= VALUE ON ENTRY
				;	DPTR	= VALUE ON ENTRY
				;	R1:R0	= VALUE ON ENTRY
				DPTRDCFFDON:
					PUSH ACC
					XCH		A, R0																	;SAVE REGISTERS
					PUSH	ACC
					XCH		A, R0
					XCH		A, R1
					PUSH	ACC
					PUSH	DPL
					PUSH	DPH
					XCH		 A, R1
					;CALCULATE SOURCE ADDRESS
					CALL	DPTRADDB
					;CALCULATE DESTINATION ADDRESS
					ADD A, R0
					MOV R0, A
					CLR A
					ADDC A, R1
					MOV R1, A
					;COPY BYTE 0
					CALL	DPTRCBFF
					JC		DPTRDCFFDONA
					;COPY BYTE 1
					CALL	DPTRCBFF
					JC		DPTRDCFFDONA
					;COPY BYTE 2
					CALL	DPTRCBFF
					JC		DPTRDCFFDONA
					;COPY BYTE 3
					CALL	DPTRCBFF
				DPTRDCFFDONA:
					;RESTORE REGISTERS & RETURN
					POP		DPH
					POP		DPL
					POP		ACC
					MOV		R1, A
					POP		ACC
					MOV		R0, A
					POP		ACC
					RET

				;COPIES THE DWORD LOCATED AT XRAM:DPTR + B TO THE LOCATION AT FLASH:R1:R0 + A
				;ON ENTRY:
				;	A			= DESTINATION OFFSET
				;	B			= SOURCE OFFSET
				;	DPTR	= SOURCE ADDRESS
				;	R1:R0	= DESTINATION ADDRESS
				;ON RETURN:
				;	A			= VALUE ON ENTRY
				;	B			= VALUE ON ENTRY
				;	DPTR	= VALUE ON ENTRY
				;	R1:R0	= VALUE ON ENTRY
				DPTRDCFMDON:
					PUSH ACC
					XCH		A, R0																	;SAVE REGISTERS
					PUSH	ACC
					MOV		A, R1
					PUSH	ACC
					PUSH	DPL
					PUSH	DPH
					MOV A, R0
					;CALCULATE SOURCE ADDRESS
					CALL	DPTRADDB
					;CALCULATE DESTINATION ADDRESS
					ADD A, R0
					MOV R0, A
					CLR A
					ADDC A, R1
					MOV R1, A
					;COPY BYTE 0
					CALL	DPTRCBFM
					JC		DPTRDCFMDONA
					;COPY BYTE 1
					CALL	DPTRCBFM
					JC		DPTRDCFMDONA
					;COPY BYTE 2
					CALL	DPTRCBFM
					JC		DPTRDCFMDONA
					;COPY BYTE 3
					CALL	DPTRCBFM
				DPTRDCFMDONA:
					;RESTORE REGISTERS & RETURN
					POP		DPH
					POP		DPL
					POP		ACC
					MOV		R1, A
					POP		ACC
					MOV		R0, A
					POP		ACC
					RET
				
				;COPIES THE DWORD LOCATED AT FLASH:DPTR + B TO THE LOCATION AT XRAM:R1:R0 + A
				;ON ENTRY:
				;	A			= DESTINATION OFFSET
				;	B			= SOURCE OFFSET
				;	DPTR	= SOURCE ADDRESS
				;	R1:R0	= DESTINATION ADDRESS
				;ON RETURN:
				;	A			= VALUE ON ENTRY
				;	B			= VALUE ON ENTRY
				;	DPTR	= VALUE ON ENTRY
				;	R1:R0	= VALUE ON ENTRY
				DPTRDCXFDON:
					PUSH ACC
					XCH		A, R0																	;SAVE REGISTERS
					PUSH	ACC
					MOV		A, R1
					PUSH	ACC
					PUSH	DPL
					PUSH	DPH
					MOV A, R0
					;CALCULATE SOURCE ADDRESS
					CALL	DPTRADDB
					;CALCULATE DESTINATION ADDRESS
					ADD A, R0
					MOV R0, A
					CLR A
					ADDC A, R1
					MOV R1, A
					;COPY BYTE 0
					CALL	DPTRCBMF
					JC		DPTRDCXFDONA
					;COPY BYTE 1
					CALL	DPTRCBMF
					JC		DPTRDCXFDONA
					;COPY BYTE 2
					CALL	DPTRCBMF
					JC		DPTRDCXFDONA
					;COPY BYTE 3
					CALL	DPTRCBMF
				DPTRDCXFDONA:
					;RESTORE REGISTERS & RETURN
					POP		DPH
					POP		DPL
					POP		ACC
					MOV		R1, A
					POP		ACC
					MOV		R0, A
					POP		ACC
					RET


			;LOADS
			;LOADS THE DWORD VALUE AT THE LOCATION ADDRESSED BY XRAM:DPTR + B INTO R3-R0 
			;ON ENTRY:
			;		B = OFFEST
			;		DPTR = ADDRESS OF POINTER TO LOAD R3-R0 FROM
			;ON RETURN:
			;		B = VALUE ON ENTRY
			;		DPTR = VALUE ON ENTRY
			;		R0 = BYTE 0 (LSB) OF LOADED DWORD
			;		R1 = BYTE 1 OF LOADED DWORD
			;		R2 = BYTE 2 OF LOADED DWORD
			;		R3 = BYTE 3 (MSB) OF LOADED DWORD
			DPTRDLRXDON:
				PUSH	ACC
				PUSH	B
				PUSH	DPL
				PUSH	DPH
				;ADD B TO DPTR
				CALL	DPTRADDB
				;LOAD R3-R0
				MOVX	A, @DPTR
				MOV		R0, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R1, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R2, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R3, A
				;RETURN
				POP		DPH
				POP		DPL
				POP		B
				POP		ACC
				RET


			;LOADS THE DWORD VALUE AT THE LOCATION ADDRESSED BY XRAM:[DPTR + B] INTO R3-R0 
			;ON ENTRY:
			;		B = OFFEST
			;		DPTR = ADDRESS OF POINTER TO LOAD R3-R0 FROM
			;ON RETURN:
			;		B = VALUE ON ENTRY
			;		DPTR = VALUE ON ENTRY
			;		R0 = BYTE 0 (LSB) OF LOADED DWORD
			;		R1 = BYTE 1 OF LOADED DWORD
			;		R2 = BYTE 2 OF LOADED DWORD
			;		R3 = BYTE 3 (MSB) OF LOADED DWORD
			DPTRDLXRION:
				PUSH	ACC
				PUSH	B
				PUSH	DPL
				PUSH	DPH
				;ADD B TO DPTR
				CALL	DPTRADDB
				;LOAD ADDRESS INTO DPTR
				MOVX	A, @DPTR
				MOV		B, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		DPH, A
				MOV		DPL, B
				;LOAD R3-R0
				MOVX	A, @DPTR
				MOV		R0, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R1, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R2, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R3, A
				;RETURN
				POP		DPH
				POP		DPL
				POP		B
				POP		ACC
				RET

			;STORES
			;STORES THE DWORD VALUE IN R3-R0 AT THE LOCATION ADDRESSED BY XRAM:DPTR + B
			;ON ENTRY:
			;		B = OFFEST
			;		DPTR = ADDRESS OF POINTER TO STORE R3-R0 AT
			;		R0 = BYTE 0 (LSB) OF DWORD TO BE STORED
			;		R1 = BYTE 1 OF DWORD TO BE STORED
			;		R2 = BYTE 2 OF DWORD TO BE STORED
			;		R3 = BYTE 3 (MSB) OF DWORD TO BE STORED
			;ON RETURN:
			;		B = VALUE ON ENTRY
			;		DPTR = VALUE ON ENTRY
			;		R0 = VALUE ON ENTRY
			;		R1 = VALUE ON ENTRY
			;		R2 = VALUE ON ENTRY
			;		R3 = VALUE ON ENTRY
			DPTRDSXRDON:
				PUSH	ACC
				PUSH	B
				PUSH	DPL
				PUSH	DPH
				;ADD B TO DPTR
				CALL	DPTRADDB
				;STORE R3-R0
				MOV		A, R0
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R1
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R2
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R3
				MOVX	@DPTR, A
				;RETURN
				POP		DPH
				POP		DPL
				POP		B
				POP		ACC
				RET

			;STORES THE DWORD VALUE IN R3-R0 AT THE LOCATION ADDRESSED BY XRAM:[DPTR + B]
			;ON ENTRY:
			;		B = OFFEST
			;		DPTR = ADDRESS OF POINTER TO STORE R3-R0 AT
			;		R0 = BYTE 0 (LSB) OF DWORD TO BE STORED
			;		R1 = BYTE 1 OF DWORD TO BE STORED
			;		R2 = BYTE 2 OF DWORD TO BE STORED
			;		R3 = BYTE 3 (MSB) OF DWORD TO BE STORED
			;ON RETURN:
			;		B = VALUE ON ENTRY
			;		DPTR = VALUE ON ENTRY
			;		R0 = VALUE ON ENTRY
			;		R1 = VALUE ON ENTRY
			;		R2 = VALUE ON ENTRY
			;		R3 = VALUE ON ENTRY
			DPTRDSXRION:
				PUSH	ACC
				PUSH	B
				PUSH	DPL
				PUSH	DPH
				;ADD B TO DPTR
				CALL	DPTRADDB
				;LOAD ADDRESS INTO DPTR
				MOVX	A, @DPTR
				MOV		B, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		DPH, A
				MOV		DPL, B
				;STORE R3-R0
				MOV		A, R0
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R1
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R2
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R3
				MOVX	@DPTR, A
				;RETURN
				POP		DPH
				POP		DPL
				POP		B
				POP		ACC
				RET

		;********************************************************************
		;QWORD OPERATIONS
		;********************************************************************
				;COPIES THE QWORD LOCATED AT FLASH:DPTR + B TO THE LOCATION AT FLASH:R1:R0 + A
				;ON ENTRY:
				;	A			= DESTINATION OFFSET
				;	B			= SOURCE OFFSET
				;	DPTR	= SOURCE ADDRESS
				;	R1:R0	= DESTINATION ADDRESS
				;ON RETURN:
				;	A			= VALUE ON ENTRY
				;	B			= VALUE ON ENTRY
				;	DPTR	= VALUE ON ENTRY
				;	R1:R0	= VALUE ON ENTRY
				DPTRQCFFDON:
					PUSH ACC
					XCH		A, R0																	;SAVE REGISTERS
					PUSH	ACC
					MOV		A, R1
					PUSH	ACC
					PUSH	DPL
					PUSH	DPH
					MOV A, R0
					;CALCULATE SOURCE ADDRESS
					CALL	DPTRADDB
					;CALCULATE DESTINATION ADDRESS
					ADD A, R0
					MOV R0, A
					CLR A
					ADDC A, R1
					MOV R1, A
					;COPY BYTE 0
					CALL	DPTRCBFF
					JC		DPTRQCFFDONA
					;COPY BYTE 1
					CALL	DPTRCBFF
					JC		DPTRQCFFDONA
					;COPY BYTE 2
					CALL	DPTRCBFF
					JC		DPTRQCFFDONA
					;COPY BYTE 3
					CALL	DPTRCBFF
					JC		DPTRQCFFDONA
					;COPY BYTE 4
					CALL	DPTRCBFF
					JC		DPTRQCFFDONA
					;COPY BYTE 5
					CALL	DPTRCBFF
					JC		DPTRQCFFDONA
					;COPY BYTE 6
					CALL	DPTRCBFF
					JC		DPTRQCFFDONA
					;COPY BYTE 7
					CALL	DPTRCBFF
				DPTRQCFFDONA:
					;RESTORE REGISTERS & RETURN
					POP		DPH
					POP		DPL
					POP		ACC
					MOV		R1, A
					POP		ACC
					MOV		R0, A
					POP		ACC
					RET


				;COPIES THE QWORD LOCATED AT XRAM:DPTR + B TO THE LOCATION AT FLASH:R1:R0 + A
				;ON ENTRY:
				;	A			= DESTINATION OFFSET
				;	B			= SOURCE OFFSET
				;	DPTR	= SOURCE ADDRESS
				;	R1:R0	= DESTINATION ADDRESS
				;ON RETURN:
				;	A			= VALUE ON ENTRY
				;	B			= VALUE ON ENTRY
				;	DPTR	= VALUE ON ENTRY
				;	R1:R0	= VALUE ON ENTRY
				DPTRQCFMDON:
					PUSH ACC
					XCH		A, R0																	;SAVE REGISTERS
					PUSH	ACC
					MOV		A, R1
					PUSH	ACC
					PUSH	DPL
					PUSH	DPH
					MOV A, R0
					;CALCULATE SOURCE ADDRESS
					CALL	DPTRADDB
					;CALCULATE DESTINATION ADDRESS
					ADD A, R0
					MOV R0, A
					CLR A
					ADDC A, R1
					MOV R1, A
					;COPY BYTE 0
					CALL	DPTRCBFM
					JC		DPTRQCFMDONA
					;COPY BYTE 1
					CALL	DPTRCBFM
					JC		DPTRQCFMDONA
					;COPY BYTE 2
					CALL	DPTRCBFM
					JC		DPTRQCFMDONA
					;COPY BYTE 3
					CALL	DPTRCBFM
					JC		DPTRQCFMDONA
					;COPY BYTE 4
					CALL	DPTRCBFM
					JC		DPTRQCFMDONA
					;COPY BYTE 5
					CALL	DPTRCBFM
					JC		DPTRQCFMDONA
					;COPY BYTE 6
					CALL	DPTRCBFM
					JC		DPTRQCFMDONA
					;COPY BYTE 7
					CALL	DPTRCBFM
				DPTRQCFMDONA:
					;RESTORE REGISTERS & RETURN
					POP		DPH
					POP		DPL
					POP		ACC
					MOV		R1, A
					POP		ACC
					MOV		R0, A
					POP		ACC
					RET


				;COPIES THE QWORD LOCATED AT FLASH:DPTR + B TO THE LOCATION AT XRAM:R1:R0 + A
				;ON ENTRY:
				;	A			= DESTINATION OFFSET
				;	B			= SOURCE OFFSET
				;	DPTR	= SOURCE ADDRESS
				;	R1:R0	= DESTINATION ADDRESS
				;ON RETURN:
				;	A			= VALUE ON ENTRY
				;	B			= VALUE ON ENTRY
				;	DPTR	= VALUE ON ENTRY
				;	R1:R0	= VALUE ON ENTRY
				DPTRQCMFDON:
					PUSH ACC
					XCH		A, R0																	;SAVE REGISTERS
					PUSH	ACC
					MOV		A, R1
					PUSH	ACC
					PUSH	DPL
					PUSH	DPH
					MOV A, R0
					;CALCULATE SOURCE ADDRESS
					CALL	DPTRADDB
					;CALCULATE DESTINATION ADDRESS
					ADD A, R0
					MOV R0, A
					CLR A
					ADDC A, R1
					MOV R1, A
					;COPY BYTE 0
					CALL	DPTRCBMF
					;COPY BYTE 1
					CALL	DPTRCBMF
					;COPY BYTE 2
					CALL	DPTRCBMF
					;COPY BYTE 3
					CALL	DPTRCBMF
					;COPY BYTE 4
					CALL	DPTRCBMF
					;COPY BYTE 5
					CALL	DPTRCBMF
					;COPY BYTE 6
					CALL	DPTRCBMF
					;COPY BYTE 7
					CALL	DPTRCBMF
					;RESTORE REGISTERS & RETURN
					POP		DPH
					POP		DPL
					POP		ACC
					MOV		R1, A
					POP		ACC
					MOV		R0, A
					POP		ACC
					RET


			;LOADS
			;LOADS THE QWORD VALUE AT THE LOCATION ADDRESSED BY XRAM:DPTR + B INTO R7-R0 
			;ON ENTRY:
			;		B = OFFEST
			;		DPTR = ADDRESS OF POINTER TO LOAD R3-R0 FROM
			;ON RETURN:
			;		B = VALUE ON ENTRY
			;		DPTR = VALUE ON ENTRY
			;		R0 = BYTE 0 (LSB) OF LOADED QWORD
			;		R1 = BYTE 1 OF LOADED QWORD
			;		R2 = BYTE 2 OF LOADED QWORD
			;		R3 = BYTE 3 OF LOADED QWORD
			;		R4 = BYTE 4 OF LOADED QWORD
			;		R5 = BYTE 5 OF LOADED QWORD
			;		R6 = BYTE 6 OF LOADED QWORD
			;		R7 = BYTE 7 (MSB) OF LOADED QWORD
			DPTRQLXRDON	PROC
				PUSH	ACC
				PUSH	B
				PUSH	DPL
				PUSH	DPH
				;ADD B TO DPTR
				CALL	DPTRADDB
				;LOAD R7-R0
				MOVX	A, @DPTR
				MOV		R0, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R1, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R2, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R3, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R4, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R5, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R6, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R7, A
				;RETURN
				POP		DPH
				POP		DPL
				POP		B
				POP		ACC
				RET
			ENDP

			;LOADS THE QWORD VALUE AT THE LOCATION ADDRESSED BY XRAM:[DPTR + B] INTO R3-R0 
			;ON ENTRY:
			;		B = OFFEST
			;		DPTR = ADDRESS OF POINTER TO LOAD R3-R0 FROM
			;ON RETURN:
			;		B = VALUE ON ENTRY
			;		DPTR = VALUE ON ENTRY
			;		R0 = BYTE 0 (LSB) OF LOADED QWORD
			;		R1 = BYTE 1 OF LOADED QWORD
			;		R2 = BYTE 2 OF LOADED QWORD
			;		R3 = BYTE 3 OF LOADED QWORD
			;		R4 = BYTE 4 OF LOADED QWORD
			;		R5 = BYTE 5 OF LOADED QWORD
			;		R6 = BYTE 6 OF LOADED QWORD
			;		R7 = BYTE 7 (MSB) OF LOADED QWORD
			DPTRQLXRION:
				PUSH	ACC
				PUSH	B
				PUSH	DPL
				PUSH	DPH
				;ADD B TO DPTR
				CALL	DPTRADDB
				;LOAD ADDRESS INTO DPTR
				MOVX	A, @DPTR
				MOV		B, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		DPH, A
				MOV		DPL, B
				;LOAD R7-R0
				MOVX	A, @DPTR
				MOV		R0, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R1, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R2, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R3, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R4, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R5, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R6, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		R7, A
				;RETURN
				POP		DPH
				POP		DPL
				POP		B
				POP		ACC
				RET

			;STORES
			;STORES THE QWORD VALUE IN R7-R0 AT THE LOCATION ADDRESSED BY XRAM:DPTR + B
			;ON ENTRY:
			;		B = OFFEST
			;		DPTR = ADDRESS OF POINTER TO STORE R7-R0 AT
			;		R0 = BYTE 0 (LSB) OF QWORD TO BE STORED
			;		R1 = BYTE 1 OF QWORD TO BE STORED
			;		R2 = BYTE 2 OF QWORD TO BE STORED
			;		R3 = BYTE 3 OF QWORD TO BE STORED
			;		R4 = BYTE 4 OF QWORD TO BE STORED
			;		R5 = BYTE 5 OF QWORD TO BE STORED
			;		R6 = BYTE 6 OF QWORD TO BE STORED
			;		R7 = BYTE 7 (MSB) OF QWORD TO BE STORED
			;ON RETURN:
			;		B = VALUE ON ENTRY
			;		DPTR = VALUE ON ENTRY
			;		R0 = VALUE ON ENTRY
			;		R1 = VALUE ON ENTRY
			;		R2 = VALUE ON ENTRY
			;		R3 = VALUE ON ENTRY
			;		R4 = VALUE ON ENTRY
			;		R5 = VALUE ON ENTRY
			;		R6 = VALUE ON ENTRY
			;		R7 = VALUE ON ENTRY
			DPTRQSXRDON	PROC
				PUSH	ACC
				PUSH	B
				PUSH	DPL
				PUSH	DPH
				;ADD B TO DPTR
				CALL	DPTRADDB
				;STORE R7-R0
				MOV		A, R0
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R1
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R2
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R3
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R4
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R5
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R6
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R7
				MOVX	@DPTR, A
				;RETURN
				POP		DPH
				POP		DPL
				POP		B
				POP		ACC
				RET
			ENDP

			;STORES THE QWORD VALUE IN R7-R0 AT THE LOCATION ADDRESSED BY XRAM:[DPTR + B]
			;ON ENTRY:
			;		B = OFFEST
			;		DPTR = ADDRESS OF POINTER TO STORE R7-R0 AT
			;		R0 = BYTE 0 (LSB) OF QWORD TO BE STORED
			;		R1 = BYTE 1 OF QWORD TO BE STORED
			;		R2 = BYTE 2 OF QWORD TO BE STORED
			;		R3 = BYTE 3 OF QWORD TO BE STORED
			;		R4 = BYTE 4 OF QWORD TO BE STORED
			;		R5 = BYTE 5 OF QWORD TO BE STORED
			;		R6 = BYTE 6 OF QWORD TO BE STORED
			;		R7 = BYTE 7 (MSB) OF QWORD TO BE STORED
			;ON RETURN:
			;		B = VALUE ON ENTRY
			;		DPTR = VALUE ON ENTRY
			;		R0 = VALUE ON ENTRY
			;		R1 = VALUE ON ENTRY
			;		R2 = VALUE ON ENTRY
			;		R3 = VALUE ON ENTRY
			;		R4 = VALUE ON ENTRY
			;		R5 = VALUE ON ENTRY
			;		R6 = VALUE ON ENTRY
			;		R7 = VALUE ON ENTRY
			DPTRQSXRION:
				PUSH	ACC
				PUSH	B
				PUSH	DPL
				PUSH	DPH
				;ADD B TO DPTR
				CALL	DPTRADDB
				;LOAD ADDRESS INTO DPTR
				MOVX	A, @DPTR
				MOV		B, A
				INC		DPTR
				MOVX	A, @DPTR
				MOV		DPH, A
				MOV		DPL, B
				;STORE R7-R0
				MOV		A, R0
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R1
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R2
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R3
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R4
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R5
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R6
				MOVX	@DPTR, A
				INC		DPTR
				MOV		A, R7
				MOVX	@DPTR, A
				;RETURN
				POP		DPH
				POP		DPL
				POP		B
				POP		ACC
				RET

		END

	#endif
#endif
