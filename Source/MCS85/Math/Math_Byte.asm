;* Yggdrasil (TM) Core Operating System (MCS-80/85): Math (Byte) Library
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.


;COMPUTE 2 ^ X
;ON ENTRY:
;	A	= X
;ON RETURN:
;	A	= RESULT
					;SAVE REGISTERS
LMATHBIU2X:			PUSH	B
					;X = 0?
					ORA		A
					JZ		LMATHBIU2XA
					;X != 0
					ANI		07H
LMATHBIU2XA:		INR		A
					MOV		C, A
					;CLEAR RESULT
					XRA		A
					STC
					;MULTIPLY RESULT BY TWO
LMATHBIU2XB:		RAL
					;DECREMENT LOOP COUNTER
					DCR		C
					JNZ		LMATHBIU2XB
					;RESTORE REGISTERS & RETURN
					POP		B
					RET


;COMPUTE 2 ^ X - 1
;ON ENTRY:
;	A	= X
;ON RETURN:
;	A	= RESULT
					;SAVE REGISTERS
LMATHBIU2XM1:		PUSH	B
					;X = 0?
					ORA		A
					JZ		LMATHBIU2XM1A
					;X != 0
					DCR		A
					ANI		07H
					INR		A
LMATHBIU2XM1A:		INR		A
					MOV		C, A
					;CLEAR RESULT
					XRA		A
					STC
					;MULTIPLY RESULT BY TWO
LMATHBIU2XM1B:		RAL
					;DECREMENT LOOP COUNTER
					DCR		C
					JNZ		LMATHBIU2XM1B
					;RESTORE REGISTERS & RETURN
					DCR		A
					POP		B
					RET


;COMPUTES X + Y
;IF THE SUM IS GREATER THAN OR EQUAL TO Z
;Z IS SUBTRACTED FROM THE SUM AND CF IS SET
;ON ENTRY:
;	CF	= CARRY IN
;	A	= ADDEND (Y)
;	B	= AUGEND (X)
;	C	= LIMIT (Z)
;ON RETURN:
;	A	= RESULT
;	B	= VALUE ON ENTRY
;	C	= VALUE ON ENTRY
;	CF	= 0 IF SUM < LIMIT
;	CF	= 1 IF SUM >= LIMIT
					;COMPUTE SUM
LMATHBIUADDLIM:		ADC		B
					;SUM >= LIMIT?
					CMP		C
					JNC		LMATHBIUADDLIMA
					JZ		LMATHBIUADDLIMA
					;SUM < LIMIT
					RET
					;SUM >= LIMIT
LMATHBIUADDLIMA:	SUB		C
					STC
					RET


;COUNT LEADING ZEROS IN X
;ON ENTRY:
;	B	= X
;ON RETURN:
;	B	= RESULT
					;SAVE REGISTERS
LMATHBIUCLZ:		PUSH	PSW
					;X == 0?
					MOV		A, B
					ORA		A
					MVI		B, 008H
					JNZ		LMATHBIUCLZA
					;X == 0, RESTORE REGISTERS & RETURN
					POP		PSW
					RET
					;SHIFT X LEFT
LMATHBIUCLZA:		RAL
					JC		LMATHBIUCLZB
					;LOOP COUNTER == 0?
					DCR		B
					JNZ		LMATHBIUCLZA
					;RESTORE REGISTERS & RETURN
LMATHBIUCLZB:		MVI		A, 008H
					SUB		B
					MOV		B, A
					POP		PSW
					RET


;COUNT TRAILING ZEROS IN X
;ON ENTRY:
;	B	= X
;ON RETURN:
;	B	= RESULT
					;SAVE REGISTERS
LMATHBIUCTZ:		PUSH	PSW
					;X == 0?
					MOV		A, B
					ORA		A
					MVI		B, 008H
					JNZ		LMATHBIUCTZA
					;X == 0, RESTORE REGISTERS & RETURN
					POP		PSW
					RET
					;SHIFT X RIGHT
LMATHBIUCTZA:		RAR
					JC		LMATHBIUCTZB
					;LOOP COUNTER == 0?
					DCR		B
					JNZ		LMATHBIUCTZA
					;RESTORE REGISTERS & RETURN
LMATHBIUCTZB:		MVI		A, 008H
					SUB		B
					MOV		B, A
					POP		PSW
					RET


;COMPUTE X / Y
;ON ENTRY:
;	B	= DIVIDEND
;	C	= DIVISOR
;ON RETURN:
;	B	= QUOTIENT
;	C	= REMAINDER
					;CHECK FOR ZERO DIVIDEND
LMATHBIUDIV:		MOV		A, B
					ORA		A
					JNZ		LMATHBIUDIVA
					;DIVIDEND IS ZERO, RETURN WITH ZERO RESULT
					MOV		C, A
					;RESTORE REGISTERS & RETURN
					RET
					;CHECK FOR ZERO DIVISOR
LMATHBIUDIVA:		MOV		A, C
					ORA		A
					JNZ		LMATHBIUDIVB
					;DIVISOR IS ZERO, RETURN WITH ERROR
					MVI		A, MATH_ERR_DIV_ZERO
					STC
					RET
					;SAVE REGISTERS
LMATHBIUDIVB:		PUSH	D
					;ZEROIZE ACCUMULATOR
					MVI		D, 000H
					;ZEROIZE SHIFT COUNTER
					MVI		C, 000H
					;CLEAR CF
					STC
					CMC
					;INCREMENT SHIFT COUNT
LMATHBIUDIVC:		INR		C
					;SHIFT DIVISOR LEFT UNTIL A LOGIC 1 IS IN MSB
					RAL
					;WAS THERE A LOGIC 1 IN THE MSB?
					JNC		LMATHBIUDIVC
					MOV		E, A
					;SHIFT DIVISOR RIGHT ONE BIT
LMATHBIUDIVE:		MOV		A, E
					RAR
					MOV		E, A
					;SUBTRACT DIVISOR FROM DIVIDEND
					MOV		A, B
					SUB		E
					;RESULT NEGATIVE?
					JC		LMATHBIUDIVF
					;RESULT POSITIVE, OVERWRITE DIVIDEND
					MOV		B, A
					;COPY CF TO QUOTIENT
LMATHBIUDIVF:		CMC
					MOV		A, D
					RAL
					MOV		D, A
					;SHIFT COUNT == 0?
					DCR		C
					JNZ		LMATHBIUDIVE
					;SHIFT COUNT == 0
					MOV		C, B
					MOV		B, A
					;RESTORE REGISTERS & RETURN
					POP		D
					RET


;COMPUTE X * Y
;INEFFICIENT LOOP METHOD FOR CONVENIENCE
;REPLACE WITH EFFICIENT SHIFT + ADD ALGORITHM
;ON ENTRY:
;	B	= MULTIPLICAND
;	C	= MULTIPLIER
;ON RETURN:
;	B	= PRODUCT MSB
;	C	= PRODUCT LSB
					;SAVE REGISTERS
LMATHBIUMUL:		PUSH	PSW
					;MULTIPLICAND ZERO?
					MOV		A, B
					ORA		B
					JNZ		LMATHBIUMULB
					;ONE OPERAND ZERO, RETURN WITH ZERO PRODUCT
LMATHBIUMULA:		LXI		B, 0000H
					POP		PSW
					RET
					;MULTIPLIER ZERO?
LMATHBIUMULB:		MOV		A, C
					ORA		C
					JZ		LMATHBIUMULA
					;SET UP REGISTERS FOR ADD LOOP
LMATHBIUMULC:		PUSH	H
					LXI		H, 0000H
					MOV		A, B
					MVI		B, 00H
					;ADD MULTIPLIER TO RESULT
LMATHBIUMULD:		DAD		B
					;MULTIPLICAND ZERO?
					DCR		A
					JNZ		LMATHBIUMULD
					;MOVE RESULT TO BC
					MOV		B, H
					MOV		C, L
					;RESTORE REGISTERS & RETURN
					POP		H
					POP		PSW
					RET


;REVERSES THE ORDER OF BITS IN X
;ON ENTRY:
;	B	= X
;ON RETURN:
;	B	= RESULT
					;SAVE REGISTERS
LMATHBIUREV:    	PUSH	PSW
					PUSH	H
					;CALCULATE LOOKUP TABLE INDEX
					LXI		H, MATHBYTEREVTABLE
					MOV		A, B
					ADD		L
					MOV		L, A
					MVI		A, 000H
					ADC		H
					MOV		H, A
					;LOAD BYTE FROM LOOKUP TABLE
					MOV		A, M
					MOV		B, A
					;RESTORE REGISTERS
					POP		H
					POP		PSW
					RET


;SHIFTS X LEFT BY Y BITS
;ON ENTRY:
;	B	= X
;	C	= Y
;ON RETURN:
;	B	= RESULT
;	C	= VALUE ON ENTRY
					;SAVE REGISTERS
LMATHBIUSHL:		PUSH	PSW
					PUSH	B
					;X == 0?
					MOV		A, B
					ORA		A
					JNZ		LMATHBIUSHLB
					;X == 0, RETURN
LMATHBIUSHLA:		POP		B
					POP		PSW
					RET
					;Y == 0?
LMATHBIUSHLB:		MOV		A, C
					ORA		A
					JZ		LMATHBIUSHLA
					;Y != 0, CHECK FOR  SHIFT BY 1
					RAR
					JNC		LMATHBIUSHLC
					;SHIFT BY 1
					MOV		C, A
					MOV		A, B
					ADD		A
					MOV		B, A
					MOV		A, C
					;CHECK FOR SHIFT BY 2
LMATHBIUSHLC:		RAR
					JNC		LMATHBIUSHLD
					;SHIFT BY 2
					MOV		C, A
					MOV		A, B
					ADD		A
					ADD		A
					MOV		B, A
					MOV		A, C
					;CHECK FOR SHIFT BY 4
LMATHBIUSHLD:		RAR
					JNC		LMATHBIUSHLE
					;SHIFT BY 4
					MOV		C, A
					MOV		A, B
					ADD		A
					ADD		A
					ADD		A
					ADD		A
					MOV		B, A
					;RESTORE REGISTERS & RETURN
LMATHBIUSHLE:		MOV		A, B
					POP		B
					MOV		B, A
					POP		PSW
					RET


;SHIFTS X RIGHT BY Y BITS
;ON ENTRY:
;	B	= X
;	C	= Y
;ON RETURN:
;	B	= RESULT
;	C	= VALUE ON ENTRY
LMATHBIUSHR:		CALL	LMATHBIUREV
					CALL	LMATHBIUSHL
					CALL	LMATHBIUREV
					RET
                

;COMPUTES X - Y
;IF THE DIFFERENCE IS LESS THAN 0
;Y IS SUBTRACTED FROM Z AND CF IS SET
;ON ENTRY:
;	CF	= BORROW IN
;	A	= MINUEND (X)
;	B	= SUBTRAHEND (Y)
;	C	= LIMIT (Z)
;ON RETURN:
;	A	= RESULT
;	B	= VALUE ON ENTRY
;	C	= VALUE ON ENTRY
;	CF	= 0 IF SUM < LIMIT
;	CF	= 1 IF SUM >= LIMIT
					;SAVE REGISTERS
LMATHBIUSUBLIM:		PUSH	D
					;COMPUTE DIFFERENCE
					MOV		D, A
					SUB		B
					;SUM < 0?
					JC		LMATHBIUSUBLIMA
					;SUM < LIMIT
					POP		D
					RET
					;SUM >= LIMIT
LMATHBIUSUBLIMA:	MOV		A, C
					SUB		B
					POP		D
					STC
					RET
        
        
MATHBYTEREVTABLE:	DB	000h, 080h, 040h, 0C0h, 020h, 0A0h, 060h, 0E0h, 010h, 090h, 050h, 0D0h, 030h, 0B0h, 070h, 0F0h
					DB	008h, 088h, 048h, 0C8h, 028h, 0A8h, 068h, 0E8h, 018h, 098h, 058h, 0D8h, 038h, 0B8h, 078h, 0F8h
					DB	004h, 084h, 044h, 0C4h, 024h, 0A4h, 064h, 0E4h, 014h, 094h, 054h, 0D4h, 034h, 0B4h, 074h, 0F4h
					DB	00Ch, 08Ch, 04Ch, 0CCh, 02Ch, 0ACh, 06Ch, 0ECh, 01Ch, 09Ch, 05Ch, 0DCh, 03Ch, 0BCh, 07Ch, 0FCh
					DB	002h, 082h, 042h, 0C2h, 022h, 0A2h, 062h, 0E2h, 012h, 092h, 052h, 0D2h, 032h, 0B2h, 072h, 0F2h
					DB	00Ah, 08Ah, 04Ah, 0CAh, 02Ah, 0AAh, 06Ah, 0EAh, 01Ah, 09Ah, 05Ah, 0DAh, 03Ah, 0BAh, 07Ah, 0FAh
					DB	006h, 086h, 046h, 0C6h, 026h, 0A6h, 066h, 0E6h, 016h, 096h, 056h, 0D6h, 036h, 0B6h, 076h, 0F6h
					DB	00Eh, 08Eh, 04Eh, 0CEh, 02Eh, 0AEh, 06Eh, 0EEh, 01Eh, 09Eh, 05Eh, 0DEh, 03Eh, 0BEh, 07Eh, 0FEh
					DB	001h, 081h, 041h, 0C1h, 021h, 0A1h, 061h, 0E1h, 011h, 091h, 051h, 0D1h, 031h, 0B1h, 071h, 0F1h
					DB	009h, 089h, 049h, 0C9h, 029h, 0A9h, 069h, 0E9h, 019h, 099h, 059h, 0D9h, 039h, 0B9h, 079h, 0F9h
					DB	005h, 085h, 045h, 0C5h, 025h, 0A5h, 065h, 0E5h, 015h, 095h, 055h, 0D5h, 035h, 0B5h, 075h, 0F5h
					DB	00Dh, 08Dh, 04Dh, 0CDh, 02Dh, 0ADh, 06Dh, 0EDh, 01Dh, 09Dh, 05Dh, 0DDh, 03Dh, 0BDh, 07Dh, 0FDh
					DB	003h, 083h, 043h, 0C3h, 023h, 0A3h, 063h, 0E3h, 013h, 093h, 053h, 0D3h, 033h, 0B3h, 073h, 0F3h
					DB	00Bh, 08Bh, 04Bh, 0CBh, 02Bh, 0ABh, 06Bh, 0EBh, 01Bh, 09Bh, 05Bh, 0DBh, 03Bh, 0BBh, 07Bh, 0FBh
					DB	007h, 087h, 047h, 0C7h, 027h, 0A7h, 067h, 0E7h, 017h, 097h, 057h, 0D7h, 037h, 0B7h, 077h, 0F7h
					DB	00Fh, 08Fh, 04Fh, 0CFh, 02Fh, 0AFh, 06Fh, 0EFh, 01Fh, 09Fh, 05Fh, 0DFh, 03Fh, 0BFh, 07Fh, 0FFh
