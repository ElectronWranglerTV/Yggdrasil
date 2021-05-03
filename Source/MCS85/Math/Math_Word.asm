
;* Yggdrasil (TM) Core Operating System (MCS-80/85): Math (Word) Library
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.


;COMPUTES X + Y
;ON ENTRY:
;	CF	= CARRY IN
;	BC	= AUGEND
;	DE	= ADDEND
;ON RETURN:
;	CF	= CARRY OUT
;	BC	= SUM
;	DE	= VALUE ON ENTRY
LMATHWIUADD:		MOV		A, C
					ADC		E
					MOV		C, A
					MOV		A, B
					ADC		D
					MOV		D, A
					RET


;COMPUTE X / Y
;ON ENTRY:
;	BC	= DIVIDEND
;	DE	= DIVISOR
;ON RETURN:
;	BC	= QUOTIENT
;	DE	= REMAINDER
					;CHECK FOR ZERO DIVIDEND
LMATHWIUDIV:		MOV		A, B
					ORA		C
					JNZ		LMATHWIUDIVA
					;DIVIDEND IS ZERO, RETURN WITH ZERO RESULT
					MOV		D, A
					MOV		E, A
					;RESTORE REGISTERS & RETURN
					RET
					;CHECK FOR ZERO DIVISOR
LMATHWIUDIVA:		MOV		A, D
					ORA		E
					JNZ		LMATHWIUDIVB
					;DIVISOR IS ZERO, RETURN WITH ERROR
					MVI		A, MATH_ERR_DIV_ZERO
					STC
					RET
					;SAVE REGISTERS
LMATHWIUDIVB:		PUSH	H
					;ZEROIZE QUOTIENT
					LXI		H, 00000H
					PUSH	H
					;CLEAR CF
					STC
					CMC
					;
					XCHG
					XRA		A
					;INCREMENT SHIFT COUNT
LMATHWIUDIVC:		INR		A
					;SHIFT DIVISOR LEFT UNTIL A LOGIC 1 IS IN MSB
					DAD		H
					;WAS THERE A LOGIC 1 IN THE MSB?
					JNC		LMATHWIUDIVC
					;MOVE DIVIDEND TO HL & SHIFT COUNT TO C
					XCHG
					MOV		H, B
					MOV		L, C
					MOV		C, A
					;SHIFT DIVISOR RIGHT ONE BIT
LMATHWIUDIVE:		MOV		A, D
					RAR
					MOV		D, A
					MOV		A, E
					RAR
					MOV		E, A
					;SUBTRACT DIVISOR FROM DIVIDEND
					MOV		A, L
					SUB		E
					MOV		L, A
					MOV		A, H
					SBB		D
					MOV		H, A
					;RESULT NEGATIVE?
					JNC		LMATHWIUDIVF
					;RESULT NEGATIVE, RESTORE DIVIDEND
					MOV		A, L
					ADD		E
					MOV		L, A
					MOV		A, H
					ADC		D
					MOV		H, A
					;COPY CF TO QUOTIENT
LMATHWIUDIVF:		XTHL
					CMC
					MOV		A, L
					RAL
					MOV		L, A
					MOV		A, H
					RAL
					MOV		H, A
					XTHL
					;SHIFT COUNT == 0?
					DCR		C
					JNZ		LMATHWIUDIVE
					;SHIFT COUNT == 0, MOVE REMAINDER TO DE
					MOV		E, L
					MOV		D, H
					;MOVE QUOTIENT TO BC
					POP		H
					MOV		B, H
					MOV		C, L
					;RESTORE REGISTERS & RETURN
					POP		H
					RET


;REVERSES THE ORDER OF BITS IN X
;ON ENTRY:
;	BC	= X
;ON RETURN:
;	BC	= RESULT
					;SAVE REGISTERS
LMATHWIUREV:    	PUSH	PSW
					PUSH	H
					;REVERSE BITS
					MOV		A, C
					CALL	LMATHBIUREV
					MOV		C, B
					MOV		B, A
					CALL	LMATHBIUREV
					;RESTORE REGISTERS & RETURN
					POP		PSW
					RET


;COMPUTES X - Y
;ON ENTRY:
;	CF	= BORROW IN
;	BC	= MINUEND
;	DE	= SUBTRAHEND
;ON RETURN:
;	CF	= BORROW OUT
;	BC	= DIFFERENCE
;	DE	= VALUE ON ENTRY
LMATHWIUSUB:		MOV		A, C
					SBB		E
					MOV		C, A
					MOV		A, B
					SBB		D
					MOV		D, A
					RET
