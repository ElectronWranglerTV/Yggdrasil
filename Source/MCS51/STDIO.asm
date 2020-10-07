;* Yggdrasil (TM) Core Operating System (MCS-51): Standard IO (STDIO) Library
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

$INCLUDE (System.INC)
#if LIB_STDIO == 1
	#ifndef STDIO_INCLUDED
		#define STDIO_INCLUDED 1

		$INCLUDE (STDIO.inc)

		EXTRN CODE	(DPTRBADD, DPTRWAXIDNN, DPTRBLRFINN, DPTRBLRXDON, DPTRBSRXDON)
		EXTRN	CODE	(DPTRBSXRING, DPTRWSXRDNN, DPTRBSXRION, DPTRDEC, DPTRWLDXDNN)
		EXTRN	CODE	(DPTRWLDXDON, DPTRWSXRINN, DPTRWSXRION)
		EXTRN	CODE	(FIFOCREATE, FIFOREADBYTE, FIFOWRITEBYTE)
		EXTRN	CODE	(HWTYPVALID)
		EXTRN	CODE	(MEMALCXRAM)
		EXTRN	CODE	(SPIRECV, SPISEND)
		#if DRV_ST7066 == 1
			EXTRN	CODE	(ST7066SEND, ST7066RECV)
		#endif
		EXTRN	CODE	(STRBYT2STR)
		EXTRN	CODE	(TASKCURR)
		EXTRN	CODE	(UARTRECV, UART0RXB, UART0RXDIS, UART0RXEN, UARTSENDDATA, UART0TXB)

		PUBLIC	LSTDCHARIN,		LSTDCHAROUT,	LSTDDEVSEL,		LSTDINCHARRDY,		LSTDINFULL
		PUBLIC	LSTDIOINIT,		LSTDLINEIN,		LSTDLINEOUT,	LSTDLINEOUTFLASH,	LSTDOPEN
		PUBLIC	LSTDRECEIVE,	LSTDTRANSMIT, LSTDUCHAROUT,	LSTDXRAMRECV,			LSTDXRAMSEND
		PUBLIC	LSTDECHOOFF,	LSTDECHOON

		STDIO_ROUTINES	SEGMENT		CODE

		RSEG	STDIO_ROUTINES

		STDIOINOFFS						EQU	0x00
		STDIOOUTOFFS					EQU	0x03

		STDIO_CB_ENTRY_SIZE		EQU	0x04

		;------------------------------------------------------------------------------
		; STDIO CONTROL BLOCK OFFSETS
		;------------------------------------------------------------------------------
		STDIO_CB_MAX_ENTRIES	EQU 0x00
		;------------------------------------------------------------------------------
		; STDIO CONTROL BLOCK OFFSETS
		;------------------------------------------------------------------------------
		STDIO_CB_FLAGS				EQU 0x00
		STDIO_CB_TASK_ID			EQU 0x01
		STDIO_CB_DEV_TYPE			EQU 0x02
		STDIO_CB_DEV_ID				EQU 0x03

		;------------------------------------------------------------------------------
		; BIT DEFINITIONS
		;------------------------------------------------------------------------------
		STDIO_FLAG_PRESENT_A		EQU	ACC.0
		STDIO_FLAG_ACTIVE_A			EQU	ACC.1
		STDIO_FLAG_NON_BLOCK		EQU	ACC.2
		STDIO_FLAG_ECHO_A				EQU	ACC.3
		STDIO_FLAG_ESC_SEQ_A		EQU	ACC.4

		STDIO_FLAG_PRESENT_B		EQU	B.0
		STDIO_FLAG_ACTIVE_B			EQU	B.1
		STDIO_FLAG_NON_BLOCK_B	EQU	B.2
		STDIO_FLAG_ECHO_B				EQU	B.3
		STDIO_FLAG_ESC_SEQ_B		EQU	B.4

		;STDIO CONTROL/STATUS BLOCK FORMAT
		;+-----------------------------+
		;|      BUFFER POINTER LSB     |
		;+-----------------------------+
		;|      BUFFER POINTER MSB     |
		;+-----------------------------+
		;|     MAX SCB ENTRY COUNT     |
		;+-----------------------------+
		;|            FLAGS 0          |
		;+-----------------------------+
		;|           TASK ID 0         |
		;+-----------------------------+
		;|         DEVICE TYPE 0       |
		;+-----------------------------+
		;|        DEVICE NUMBER 0      |
		;+-----------------------------+
		;|      MAX INPUT LENGTH 0     |
		;+-----------------------------+
		;|          FLAGS N-1          |
		;+-----------------------------+
		;|         TASK ID N-1         |
		;+-----------------------------+
		;|       DEVICE TYPE N-1       |
		;+-----------------------------+
		;|      DEVICE NUMBER N-1      |
		;+-----------------------------+
		;|    MAX INPUT LENGTH N-1     |
		;+-----------------------------+
		;|            FLAGS N          |
		;+-----------------------------+
		;|           TASK ID N         |
		;+-----------------------------+
		;|         DEVICE TYPE N       |
		;+-----------------------------+
		;|        DEVICE NUMBER N      |
		;+-----------------------------+
		;|      MAX INPUT LENGTH N     |
		;+-----------------------------+

		;STDIO FLAGS FORMAT
		;+---------------------------------+
		;|  7   6   5   4   3   2   1   0  |
		;+---------------------------------+
		;|                 ECO NBL ATV PRS |
		;+---------------------------------+
		;PRS: PRESENT. SET TO 1 IF THIS ENTRY IS PRESENT
		;ATV:	ACTIVE. SET TO 1 IF THIS ENTRY IS ACTIVE FOR THE TASK
		;NBL: NON-BLOCKING. SET IF THE DESIRED OPERATION IS NON-BLOCKING
		;ECO:	ECHO. SET IF RECEIVED CHARACTERS ARE ECHOED BACK TO SENDER

		;*RECEIVES A CHARACTER FROM THE CURRENT TASK'S STDIO DEVICE
		;ON ENTRY:
		;	A = CHARACTER
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;	C = 1 IF FAIL
		;		A = ERROR CODE
		LSTDCHARIN:
			;SAVE REGISTERS
			XCH		A, R0
			PUSH	ACC
			MOV		A, R1
			PUSH	ACC
			MOV		A, R0
			PUSH	ACC
			;VERIFY TASK'S DEVICE IS OPEN
			CALL	LSTDDEVGET
			JNC		LSTDCHARINB
			;RETURN
			MOV		R0, A
			POP		ACC
		LSTDCHARINA:
			POP		ACC
			MOV		R1, A
			POP		ACC
			XCH		A, R0
			RET
		LSTDCHARINB:
			;RECEIVE CHARACTER
			POP		ACC
			CALL	LSTDRECEIVE
			MOV		R0, A
			JMP		LSTDCHARINA

		;*SENDS AN ASCII CHARACTER OUT OF THE CURRENT TASK'S STDIO DEVICE
		;ON ENTRY:
		;	A = CHARACTER
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;	C = 1 IF FAIL
		;		A = ERROR CODE
		LSTDCHAROUT:
			;SAVE REGISTERS
			PUSH	B
			XCH		A, R0
			PUSH	ACC
			MOV		A, R1
			PUSH	ACC
			MOV		A, R0
			PUSH	ACC
			;VERIFY TASK'S DEVICE IS OPEN
			CALL	LSTDDEVGET
			JNC		LSTDCHAROUTB
			;RETURN
			MOV		R0, A
			POP		ACC
		LSTDCHAROUTA:
			POP		ACC
			MOV		R1, A
			POP		ACC
			XCH		A, R0
			POP		B
			RET
		LSTDCHAROUTB:
			;TRANSMIT CHARACTER
			POP		ACC
			CALL	LSTDTRANSMIT
			MOV		R0, A
			JMP		LSTDCHAROUTA

		;*SENDS AN UNSIGNED CHARACTER OUT OF THE CURRENT TASK'S STDIO DEVICE
		;ON ENTRY:
		;	A = CHARACTER
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;	C = 1 IF FAIL
		;		A = ERROR CODE
		LSTDUCHAROUT:
			;SAVE REGISTERS
			XCH		A, R0
			PUSH	ACC
			MOV		A, R1
			PUSH	ACC
			MOV		A, R2
			PUSH	ACC
			;CONVERT VALUE TO STRING
			CALL	STRBYT2STR
			MOV		A, #0x30
			XRL		A, R2
			JNZ		LSTDUCHAROUTA
			MOV		A, #0x30
			XRL		A, R1
			JNZ		LSTDUCHAROUTB
			SJMP	LSTDUCHAROUTC
		LSTDUCHAROUTA:
			MOV		A, R2
			CALL	LSTDCHAROUT
			JC		LSTDUCHAROUTD
		LSTDUCHAROUTB:
			MOV		A, R1
			CALL	LSTDCHAROUT
			JC		LSTDUCHAROUTD
		LSTDUCHAROUTC:
			MOV		A, R0
			CALL	LSTDCHAROUT
		LSTDUCHAROUTD:
			;RESTORE REGISTERS & RETURN
			POP		ACC
			MOV		R2, A
			POP		ACC
			MOV		R1, A
			POP		ACC
			MOV		R0, A
			RET

		;RECEIVE BYTE FROM ERAM QUEUE
		;ON ENTRY:
		;	R0 = QUEUE ID
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;		A = RECEIVE BYTE
		;	C = 1 IF ERROR
		;		A = ERROR CODE
		LSTDERAMRECV:
			
			RET

		;TRANSMIT BYTE TO ERAM QUEUE
		;ON ENTRY:
		;	A		= TRANSMIT BYTE
		;	R0	= QUEUE ID
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;		A = RECEIVE BYTE
		;	C = 1 IF ERROR
		;		A = ERROR CODE
		LSTDERAMSEND:
			RET

		;*RETURNS A CHARACTER FROM STDIO IF ONE IS READY
		;ON ENTRY:
		;	NONE
		;ON RETURN:
		;	C = 0 IF A CHARACTER IS NOT READY
		;		A = 0x00
		;	C = 1 IF A CHARACTER IS READY
		;		A = CHARACTER
		LSTDINCHARRDY:
			;CALL UART0RXEN
			;CALL UART0FULL
			JC LSTDINCHARRDYA
			CLR A
			RET
		LSTDINCHARRDYA:
			;CALL UART0BUFFRD
			RET

		;RECEIVE BYTE FROM IRAM QUEUE
		;ON ENTRY:
		;	R0 = QUEUE ID
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;		A = RECEIVE BYTE
		;	C = 1 IF ERROR
		;		A = ERROR CODE
		LSTDIRAMRECV:
			
			RET

		;TRANSMIT BYTE TO IRAM QUEUE
		;ON ENTRY:
		;	A		= TRANSMIT BYTE
		;	R0	= QUEUE ID
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;		A = RECEIVE BYTE
		;	C = 1 IF ERROR
		;		A = ERROR CODE
		LSTDIRAMSEND:
			RET

		;*CLOSES THE SPECIFIED STDIO DEVICE
		;ON ENTRY:
		;	A		 = DEVICE NUMBER
		;	DPTR = POINTER TO DEVICE TYPE STRING
		;	C = 0 IF SUCCESS
		;		A		 = 0x00
		;		DPTR = VALUE ON ENTRY
		;	C = 1 IF FAIL
		;		A		 = ERROR CODE
		;		DPTR = VALUE ON ENTRY
		LSTDCLOSE:
			;SAVE REGISTERS
			PUSH	DPL
			PUSH	DPH
			XCH		A, R1
			PUSH	ACC
			MOV		A, R1
			;CHECK FOR VALID DEVICE TYPE
			CALL	HWTYPVALID
			JNC		LSTDCLOSEB
			MOV		R1, #STDIO_RET_ERR_INVDEVTYP
		LSTDCLOSEA:
			;RETURN
			POP		ACC
			XCH		A, R1
			POP		DPH
			POP		DPL
			RET
		LSTDCLOSEB:
			;DEVICE TYPE VALID
			;SAVE REGISTERS
			XCH		A, R0
			PUSH	ACC
			;ENSURE DEVICE IS OPEN
			CALL	LSTDDEVISOPEN
			JC		LSTDCLOSED
			MOV		R1, #STDIO_RET_ERR_DEVNOTOPEN
			POP		ACC
			MOV		R0, A
			JMP		LSTDCLOSEA
		LSTDCLOSED:
			;FIND FREE STDIOCB ENTRY
			MOV		DPTR, #STDIO_ADR
			CALL	DPTRWLDXDNN
			MOV		A, R2
			PUSH	ACC
			MOVX	A, @DPTR
			MOV		R2, A
			INC		DPTR
		LSTDCLOSEE:
			MOVX	A, @DPTR
			JNB		STDIO_FLAG_PRESENT_A, LSTDCLOSEF
			MOV		A, #STDIO_CB_ENTRY_SIZE
			CALL	DPTRBADD
			DJNZ	R2, LSTDCLOSEE
			MOV		R1, #STDIO_RET_ERR_NOFREE
			POP		ACC
			MOV		R2, A
			JMP		LSTDCLOSEA
		LSTDCLOSEF:
			;SET PRESENT FLAG
			SETB	STDIO_FLAG_PRESENT_A
			MOVX	@DPTR, A
			;STORE TASK ID
			INC		DPTR
			CALL	TASKCURR
			MOVX	@DPTR, A
			;STORE DEVICE TYPE
			INC		DPTR
			MOV		A, R0
			MOVX	@DPTR, A
			;STORE DEVICE NUMBER
			INC		DPTR
			MOV		A, R1
			MOVX	@DPTR, A
			;RETURN
			POP		ACC
			MOV		R2, A
			POP		ACC
			MOV		R0, A
			POP		ACC
			MOV		R1, A
			POP		DPH
			POP		DPL
			CLR		C
			RET

		;RETURNS THE CONTROL BLOCK ADDRESS OF THE CURRENT TASK'S ACTIVE DEVICE
		;ON ENTRY:
		;	NONE
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;		A = 0x00
		;		DPTR = ADDRESS
		;	C = 1 IF FAIL
		;		A = ERROR CODE
		;		DPTR = 0x0000
		LSTDDEVADR:
			;SAVE REGISTERS
			PUSH	B
			MOV		A, R0
			PUSH	ACC
			;GET CURRENT TASK ID
			CALL	TASKCURR
			MOV		B, A
			;LOAD NUMBER OF STDIOCB ENTRIES
			MOV		DPTR, #STDIO_ADR
			CALL	DPTRWLDXDNN
			MOVX	A, @DPTR
			MOV		R0, A
			INC		DPTR
		LSTDDEVADRA:
			;FIND TASK ID IN STCIOCB
			PUSH	DPL
			PUSH	DPH
			MOVX	A, @DPTR
			JNB		STDIO_FLAG_PRESENT_A, LSTDDEVADRB
			JNB		STDIO_FLAG_ACTIVE_A, LSTDDEVADRB
			INC		DPTR
			MOVX	A, @DPTR
			XRL		A, B
			JZ		LSTDDEVADRC
		LSTDDEVADRB:
			POP		DPH
			POP		DPL
			MOV		A, #STDIO_CB_ENTRY_SIZE
			CALL	DPTRBADD
			DJNZ	R0, LSTDDEVADRA
			;TASK ID NOT FOUND
			;RESTORE REGISTERS AND RETURN
			POP		ACC
			MOV		R0, A
			POP		B
			MOV		A, #0x01
			SETB	C
			RET
		LSTDDEVADRC:
			;TASK ID FOUND
			POP		DPH
			POP		DPL
			;RESTORE REGISTERS AND RETURN
			POP		ACC
			MOV		R0, A
			POP		B
			CLR		A
			CLR		C
			RET

		;SETS THE CURRENT TASK'S ACTIVE DEVICE TO INACTIVE
		;ON ENTRY:
		;	NONE
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;		A = 0x00
		;	C = 1 IF FAIL
		;		A = ERROR CODE
		LSTDDEVCLR:
			;SAVE REGISTERS
			PUSH	B
			MOV		A, R0
			PUSH	ACC
			PUSH	DPL
			PUSH	DPH
			;LOAD DEVICE'S FLAGS
			CALL	LSTDDEVADR
			JNC		LSTDDEVCLRA
			;TASK ID NOT FOUND
			;RESTORE REGISTERS AND RETURN
			POP		DPH
			POP		DPL
			POP		ACC
			MOV		R0, A
			POP		B
			MOV		A, #0x01
			SETB	C
			RET
		LSTDDEVCLRA:
			;TASK ID FOUND
			;CLEAR DEVICE'S ACTIVE FLAG
			MOVX	A, @DPTR
			CLR		STDIO_FLAG_ACTIVE_A
			MOVX	@DPTR, A
			;RESTORE REGISTERS AND RETURN
			POP		DPH
			POP		DPL
			POP		ACC
			MOV		R0, A
			POP		B
			CLR		A
			CLR		C
			RET

		;*RETURNS THE ACTIVE STDIO DEVICE TYPE AND ID FOR THE CURRENT TASK
		;ON ENTRY:
		;	NONE
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;		A		= 0x00
		;		B		= FLAGS
		;		R0	= DEVICE TYPE
		;		R1	= DEVICE ID
		;	C = 1 IF FAIL
		;		A		= ERROR CODE
		;		B		=	0x00
		;		R0	= 0x00
		;		R1	= 0x00
		LSTDDEVGET:
			;SAVE REGISTERS
			PUSH	DPL
			PUSH	DPH
			;GET CURRENT TASK ID
			CALL	LSTDDEVADR
			JNC		LSTDDEVGETA
			;TASK ID NOT FOUND
			;RESTORE REGISTERS AND RETURN
			POP		DPH
			POP		DPL
			CLR		A
			MOV		R0, A
			MOV		R1, A
			MOV		A, #0x01
			SETB	C
			RET
		LSTDDEVGETA:
			;DEVICE ENTRY FOUND
			;LOAD FLAGS
			MOVX	A, @DPTR
			MOV		B, A
			;LOAD DEVICE TYPE
			INC		DPTR
			INC		DPTR
			MOVX	A, @DPTR
			MOV		R0, A
			;LOAD DEVICE NUMBER
			INC		DPTR
			MOVX	A, @DPTR
			MOV		R1, A
			;RESTORE REGISTERS AND RETURN
			POP		DPH
			POP		DPL
			CLR		A
			CLR		C
			RET

		;RETURNS TRUE IF THE INDICATED DEVICE IS OPEN
		;DOES NOT CHECK DEVICE NUMBER VALIDITY
		;ON ENTRY:
		;	R0 = DEVICE CODE
		;	R1 = DEVICE NUMBER
		;ON RETURN:
		;	C = 0 IF DEVICE NOT OPEN
		;		A = 0x00
		;	C = 1 IF DEVICE OPEN
		;		A = ID OF TASK USING DEVICE
		LSTDDEVISOPEN:
			;SAVE REGISTERS
			XCH		A, R2
			PUSH	ACC
			PUSH	B
			MOV		DPTR, #STDIO_ADR
			CALL	DPTRWLDXDNN
			;LOAD NUMBER OF STDIOCB ENTRIES
			MOVX	A, @DPTR
			MOV		R2, A
			INC		DPTR
		LSTDDEVISOPENA:
			;ENSURE ENTRY IS PRESENT
			MOVX	A, @DPTR
			JB		STDIO_FLAG_PRESENT_A, LSTDDEVISOPENC
		LSTDDEVISOPENB:
			;POINT TO NEXT STDIOCB ENTRY
			MOV		A, #STDIO_CB_ENTRY_SIZE
			CALL	DPTRBADD
			;PROCESS NEXT STDIOCB ENTRY IF NOT LAST
			DJNZ	R2, LSTDDEVISOPENA
			POP		B
			POP		ACC
			MOV		R2, A
			CLR		C
			RET
		LSTDDEVISOPENC:
			;CHECK DEVICE CODE
			MOV		B, #STDIO_CB_DEV_TYPE
			CALL	DPTRBLRXDON
			XRL		A, R0
			JNZ		LSTDDEVISOPENB
			;CHECK DEVICE NUMBER
			MOV		B, #STDIO_CB_DEV_ID
			CALL	DPTRBLRXDON
			XRL		A, R1
			JNZ		LSTDDEVISOPENB
			;LOAD ID OF TASK USING DEVICE
			MOV		B, #STDIO_CB_TASK_ID
			CALL	DPTRBLRXDON
			MOV		R2, A
			;RETURN
			POP		B
			POP		ACC
			XCH		A, R2
			SETB	C
			RET

		;DISABLES ECHO FOR THE CURRENT TASK'S STDIO DEVICE
		;ON ENTRY:
		;	NONE
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;		A = 0x00
		;	C = 1 IF FAIL
		;		A = ERROR CODE
		LSTDECHOOFF:
			;SAVE REGISTERS
			PUSH	B
			MOV		A, R0
			PUSH	ACC
			PUSH	DPL
			PUSH	DPH
			;LOAD DEVICE'S FLAGS
			CALL	LSTDDEVADR
			JNC		LSTDECHOOFFA
			;TASK ID NOT FOUND
			;RESTORE REGISTERS AND RETURN
			POP		DPH
			POP		DPL
			POP		ACC
			MOV		R0, A
			POP		B
			MOV		A, #0x01
			SETB	C
			RET
		LSTDECHOOFFA:
			;TASK ID FOUND
			;CLEAR DEVICE'S ECHO FLAG
			MOVX	A, @DPTR
			CLR		STDIO_FLAG_ECHO_A
			MOVX	@DPTR, A
			;RESTORE REGISTERS AND RETURN
			POP		DPH
			POP		DPL
			POP		ACC
			MOV		R0, A
			POP		B
			CLR		A
			CLR		C
			RET

		;ENABLES ECHO FOR THE CURRENT TASK'S STDIO DEVICE
		;ON ENTRY:
		;	NONE
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;		A = 0x00
		;	C = 1 IF FAIL
		;		A = ERROR CODE
		LSTDECHOON:
			;SAVE REGISTERS
			PUSH	B
			MOV		A, R0
			PUSH	ACC
			PUSH	DPL
			PUSH	DPH
			;LOAD DEVICE'S FLAGS
			CALL	LSTDDEVADR
			JNC		LSTDECHOONA
			;TASK ID NOT FOUND
			;RESTORE REGISTERS AND RETURN
			POP		DPH
			POP		DPL
			POP		ACC
			MOV		R0, A
			POP		B
			MOV		A, #0x01
			SETB	C
			RET
		LSTDECHOONA:
			;TASK ID FOUND
			;SET DEVICE'S ECHO FLAG
			MOVX	A, @DPTR
			SETB	STDIO_FLAG_ECHO_A
			MOVX	@DPTR, A
			;RESTORE REGISTERS AND RETURN
			POP		DPH
			POP		DPL
			POP		ACC
			MOV		R0, A
			POP		B
			CLR		A
			CLR		C
			RET

		LSTDIOINIT:
			;CALCULATE NUMBER OF BYTES FOR STDCB
			MOV		A, #STDIO_CB_COUNT
			MOV		B, #STDIO_CB_ENTRY_SIZE
			MUL		AB
			INC		A
			MOV		R0, A
			MOV		R1, #0x00
			;ALLOCATE MEMORY
			CALL	MEMALCXRAM
			JC		LSTDIOINITB
			MOV		DPTR, #STDIO_ADR
			MOV		A, R0
			MOVX	@DPTR, A
			INC		DPTR
			MOV		A, R1
			MOVX	@DPTR, A
			;CONFIGURE STDIO CONTROL BLOCK
			MOV		DPL, R0
			MOV		DPH, R1
			MOV		A, #STDIO_CB_COUNT
			MOVX	@DPTR, A
			MOV		B, #STDIO_CB_ENTRY_SIZE
			MUL		AB
			MOV		R0, A
			CLR		A
		LSTDIOINITA:
			INC		DPTR
			MOVX	@DPTR, A
			DJNZ	R0, LSTDIOINITA
			;RETURN
			CLR		C
		LSTDIOINITB:
			RET

		;RETURNS TRUE IF STANDARD INPUT IS FULL
		;ON ENTRY:
		;	NONE
		;ON RETURN:
		;	C = 0 IF STDIN IS NOT FULL
		;	C = 1 IF STDIN IS FULL
		LSTDINFULL:
			;CALL UART0FULL
			RET


		;*READ A CRLF-TERMINATED LINE FROM STANDARD INPUT
		;ON ENTRY:
		;	A		 = MAXIMUM NUMBER OF CHARACTERS TO READ
		;	DPTR = BUFFER ADDRESS
		;ON RETURN:
		;	DPTR = VALUE ON ENTRY
		;	C = 0 IF SUCCESS
		;		A		 = LENGTH OF LINE, EXCLUDING TERMINATOR
		;	C = 1 IF FAIL
		;		A		 = ERROR CODE
		LSTDLINEIN:
			;CHECK FOR ZERO LENGTH CHARACTER COUNT
			JNZ		LSTDLINEINNOTZERO
			SETB	C
			RET
		LSTDLINEINNOTZERO:
			;SAVE REGISTERS
			PUSH	PSW
			XCH		A, R2
			PUSH	ACC
			PUSH	B
			MOV		A, R0
			PUSH	ACC
			MOV		A, R1
			PUSH	ACC
			MOV		A, R3
			PUSH	ACC
			PUSH	DPL
			PUSH	DPH
			;SETUP
			DEC		R2
			MOV		R3, #0x00
			;VERIFY TASK'S DEVICE IS OPEN
			CALL	LSTDDEVGET
			JNC		LSTDLINEINAB
		LSTDINLINEINERR:
			;RETURN
			POP		DPH
			POP		DPL
			POP		ACC
			MOV		R3, A
			POP		ACC
			MOV		R1, A
			POP		ACC
			MOV		R0, A
			POP		B
			POP		ACC
			MOV		R2, A
			POP		PSW
			MOV		A, #0x00
			RET
		LSTDLINEINAB:
			;CLEAR ESCAPE SEQUENCE FLAG
			CLR		STDIO_FLAG_ESC_SEQ_B
		LSTDLINEINA:
			;GET CHARACTER FROM DEVICE
			CALL	LSTDRECEIVE
			JC		LSTDINLINEINERR
			;ESCAPE SEQUENCE ACTIVE?
			;JB		STDIO_FLAG_ESC_SEQ_B,
			;ESCAPE SEQUENCE NOT ACTIVE, CHECK FOR ESCAPE
			CJNE	A, #0x1B, noescape
			;SET ESCAPE SEQUENCE FLAG
			SETB	STDIO_FLAG_ESC_SEQ_B
			SJMP	LSTDLINEINA
		noescape:
			;CHECK FOR CRLF
			CJNE	A, #10d, LSTDLINEINB
			JNB		STDIO_FLAG_ECHO_B, LSTDLINEINC
			CALL	LSTDTRANSMIT
			MOV		A, #13d
			CALL	LSTDTRANSMIT
			JMP		LSTDLINEINC
		LSTDLINEINB:
			;ECHO CRLF
			CJNE	A, #13d, LSTDLINEIND
			JNB		STDIO_FLAG_ECHO_B, LSTDLINEINC
			CALL	LSTDTRANSMIT
			MOV		A, #10d
			CALL	LSTDTRANSMIT
		LSTDLINEINC:
			;STORE ZERO TERMINATOR IN BUFFER AND RETURN
			MOV		A, R3
			MOV		R2, A
			POP		DPH
			POP		DPL
			POP		ACC
			MOV		R3, A
			POP		ACC
			MOV		R1, A
			POP		ACC
			MOV		R0, A
			POP		B
			POP		ACC
			XCH		A, R2
			POP		PSW
			CLR		C
			RET
		LSTDLINEIND:
			;CHECK FOR BACKSPACE
			CJNE	A, #0x08, LSTDLINEINF
		LSTDLINEINE:
			;CHECK FOR EMPTY STRING
			CJNE	R3, #0x00, dobackspace
			JMP		LSTDLINEINA
		dobackspace:
			;PROCESS BACKSPACE
			DEC		R3
			JNB		STDIO_FLAG_ECHO_B, DOBACKSPACEA
			CALL	LSTDTRANSMIT
		DOBACKSPACEA:
			CALL	DPTRDEC
			CLR		A
			MOVX	@DPTR, A
			JMP		LSTDLINEINA
		LSTDLINEINF:
			;CHECK FOR DELETE
			CJNE	A, #0x7F, LSTDLINEINH
			;CHECK FOR EMPTY STRING
			CJNE	R3, #0x00, dodelete
			JMP		LSTDLINEINA
		dodelete:
			;PROCESS DELETE
			DEC		R3
			JNB		STDIO_FLAG_ECHO_B, DODELETEA
			CALL	LSTDTRANSMIT
		DODELETEA:
			CALL	DPTRDEC
			CLR		A
			MOVX	@DPTR, A
			JMP		LSTDLINEINA
		LSTDLINEINH:
			;CHECK FOR BUFFER OVERFLOW
			PUSH	ACC
			MOV		R2, A
			CLR		C
			SUBB	A, R3
			POP		ACC
			JNC		LSTDLINEINBUFFOK
			JMP		LSTDLINEINA
		LSTDLINEINBUFFOK:
			;STORE CHARACTER; ECHO CHARACTER; GET NEXT CHARACTER
			INC		R3
			MOVX	@DPTR, A
			INC		DPTR
			JNB		STDIO_FLAG_ECHO_B, LSTDLINEINI
			CALL	LSTDTRANSMIT
		LSTDLINEINI:
			;STORE TEMPORARY TERMINATOR
			CLR		A
			MOVX	@DPTR, A
			JMP		LSTDLINEINA

		;*SEND A ZERO-TERMINATED LINE TO STANDARD OUTPUT
		;ON ENTRY:
		;	DPTR = STRING ADDRESS
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;		A = 0x00
		;	C = 1 IF FAIL
		;		A = ERROR CODE
		;	DPTR = VALUE ON ENTRY
		LSTDLINEOUT:
			;SAVE REGISTERS
			MOV		A, R0
			PUSH	ACC
			MOV		A, R1
			PUSH	ACC
			PUSH	DPL
			PUSH	DPH
			;VERIFY TASK'S DEVICE IS OPEN
			CALL	LSTDDEVGET
			JNC		LSTDLINEOUTC
		LSTDLINEOUTA:
			;RETURN
			SETB	C
		LSTDLINEOUTB:
			MOV		R0, A
			POP		DPH
			POP		DPL
			POP		ACC
			MOV		R1, A
			POP		ACC
			XCH		A, R0
			RET
		LSTDLINEOUTC:
			;CHECK FOR TERMINATOR
			MOVX	A, @DPTR
			JZ		LSTDLINEOUTD
			;TRANSMIT CHARACTER
			CALL	LSTDTRANSMIT
			JC		LSTDLINEOUTA
			;PROCESS NEXT CHARACTER
			INC		DPTR
			JMP		LSTDLINEOUTC
		LSTDLINEOUTD:
			;RETURN
			CLR		A
			CLR		C
			JMP		LSTDLINEOUTB

		;*SEND A ZERO-TERMINATED STRING FROM FLASH MEMORY TO STANDARD OUTPUT
		;ON ENTRY:
		;	DPTR = STRING ADDRESS
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;		A = 0x00
		;	C = 1 IF FAIL
		;		A = ERROR CODE
		;	DPTR = VALUE ON ENTRY
		LSTDLINEOUTFLASH:
			;SAVE REGISTERS
			PUSH	B
			MOV		A, R0
			PUSH	ACC
			MOV		A, R1
			PUSH	ACC
			PUSH	DPL
			PUSH	DPH
			;VERIFY TASK'S DEVICE IS OPEN
			CALL	LSTDDEVGET
			JNC		LSTDLINEOUTFLASHC
		LSTDLINEOUTFLASHA:
			;RETURN
			SETB	C
		LSTDLINEOUTFLASHB:
			MOV		R0, A
			POP		DPH
			POP		DPL
			POP		ACC
			MOV		R1, A
			POP		ACC
			XCH		A, R0
			POP		B
			RET
		LSTDLINEOUTFLASHC:
			;CHECK FOR TERMINATOR
			CLR		A
			MOVC	A, @A+DPTR
			JZ		LSTDLINEOUTFLASHD
			;TRANSMIT CHARACTER
			CALL	LSTDTRANSMIT
			JC		LSTDLINEOUTFLASHA
			;PROCESS NEXT CHARACTER
			INC		DPTR
			JMP		LSTDLINEOUTFLASHC
		LSTDLINEOUTFLASHD:
			;RETURN
			CLR		A
			CLR		C
			JMP		LSTDLINEOUTFLASHB

		;*OPENS A NEW STDIO DEVICE FOR THE CURRENT TASK
		;ON ENTRY:
		;	A		 = DEVICE NUMBER
		;	DPTR = POINTER TO DEVICE TYPE STRING
		;	C = 0 IF SUCCESS
		;		A		 = 0x00
		;		DPTR = VALUE ON ENTRY
		;	C = 1 IF FAIL
		;		A		 = ERROR CODE
		;		DPTR = VALUE ON ENTRY
		LSTDOPEN:
			;SAVE REGISTERS
			PUSH	DPL
			PUSH	DPH
			XCH		A, R1
			PUSH	ACC
			MOV		A, R1
			;CHECK FOR VALID DEVICE TYPE
			CALL	HWTYPVALID
			JNC		LSTDOPENB
			MOV		R1, #STDIO_RET_ERR_INVDEVTYP
		LSTDOPENA:
			;RETURN
			POP		ACC
			XCH		A, R1
			POP		DPH
			POP		DPL
			RET
		LSTDOPENB:
			;DEVICE TYPE VALID
			;SAVE REGISTERS
			XCH		A, R0
			PUSH	ACC
			;ENSURE DEVICE IS NOT ALREADY OPEN
			CALL	LSTDDEVISOPEN
			JNC		LSTDOPEND
			MOV		R1, #STDIO_RET_ERR_DEVOPEN
			POP		ACC
			MOV		R0, A
			JMP		LSTDOPENA
		LSTDOPEND:
			;FIND FREE STDIOCB ENTRY
			MOV		DPTR, #STDIO_ADR
			CALL	DPTRWLDXDNN
			MOV		A, R2
			PUSH	ACC
			MOVX	A, @DPTR
			MOV		R2, A
			INC		DPTR
		LSTDOPENE:
			MOVX	A, @DPTR
			JNB		STDIO_FLAG_PRESENT_A, LSTDOPENF
			MOV		A, #STDIO_CB_ENTRY_SIZE
			CALL	DPTRBADD
			DJNZ	R2, LSTDOPENE
			MOV		R1, #STDIO_RET_ERR_NOFREE
			POP		ACC
			MOV		R2, A
			JMP		LSTDOPENA
		LSTDOPENF:
			;SET PRESENT FLAG
			SETB	STDIO_FLAG_PRESENT_A
			MOVX	@DPTR, A
			;STORE TASK ID
			INC		DPTR
			CALL	TASKCURR
			MOVX	@DPTR, A
			;STORE DEVICE TYPE
			INC		DPTR
			MOV		A, R0
			MOVX	@DPTR, A
			;STORE DEVICE NUMBER
			INC		DPTR
			MOV		A, R1
			MOVX	@DPTR, A
			;RETURN
			POP		ACC
			MOV		R2, A
			POP		ACC
			MOV		R0, A
			POP		ACC
			MOV		R1, A
			POP		DPH
			POP		DPL
			CLR		C
			CLR		A
			RET

		;RECEIVES A BYTE USING THE SELECTED DEVICE
		;ON ENTRY:
		;	R0 = DEVICE ID
		;	R1 = DEVICE NUMBER
		;ON RETURN:
		;	VARIES WITH DEVICE ROUTINE CALLED
		LSTDRECEIVE:
			;'RESERVE' STACK SPACE FOR FUNCTION ADDRESS
			PUSH	ACC
			PUSH	ACC
			;SAVE REGISTERS
			PUSH	ACC
			PUSH	DPL
			PUSH	DPH
			CLR		EA
			DEC		SP
			DEC		SP
			DEC		SP
			DEC		SP
			;CALCULATE ADDRESS OF DEVICE ENTRY POINT
			MOV		DPTR, #STDIO_DEV_RX_TABLE
			MOV		A, R0
			ADD		A, R0
			ADD		A, DPL
			MOV		DPL, A
			CLR		A
			ADDC	A, DPH
			MOV		DPH, A
			;LOAD ADDRESS OF DEVICE ENTRY POINT
			CLR		A
			MOVC	A, @A+DPTR
			PUSH	ACC
			DEC		SP
			DEC		SP
			INC		DPTR
			CLR		A
			MOVC	A, @A+DPTR
			PUSH	ACC
			;RESTORE REGISTERS
			INC		SP
			INC		SP
			INC		SP
			INC		SP
			SETB	EA
			POP		DPH
			POP		DPL
			POP		ACC
			;'CALL' FUNCTION
			RET

		;*SELECTS THE CURRENT TASK'S ACTIVE STDIO DEVICE
		;DEVICE MUST ALREADY BE OPEN
		;ON ENTRY:
		;	A		 = DEVICE NUMBER
		;	DPTR = POINTER TO DEVICE TYPE STRING
		;ON RETURN:
		;	DPTR = VALUE ON ENTRY
		;	C = 0 IF SUCCESS
		;		A = 0x00
		;	C = 1 IF FAIL
		LSTDDEVSEL:
			;SAVE REGISTERS
			PUSH	DPL
			PUSH	DPH
			XCH		A, R1
			PUSH	ACC
			MOV		A, R1
			;CHECK FOR VALID DEVICE TYPE
			CALL	HWTYPVALID
			JNC		LSTDDEVSELB
			MOV		R1, #STDIO_RET_ERR_INVDEVTYP
		LSTDDEVSELA:
			;RETURN
			POP		ACC
			XCH		A, R1
			POP		DPH
			POP		DPL
			RET
		LSTDDEVSELB:
			;DEVICE TYPE VALID
			;SAVE REGISTERS
			PUSH	B
			XCH		A, R0
			PUSH	ACC
			;ENSURE DEVICE IS OPEN
			CALL	LSTDDEVISOPEN
			JC		LSTDDEVSELD
			MOV		R1, #STDIO_RET_ERR_DEVNOTOPEN
		LSTDDEVSELR:
			POP		ACC
			MOV		R0, A
			POP		B
			JMP		LSTDDEVSELA
		LSTDDEVSELD:
			;ENSURE CURRENT TASK OWNS DEVICE
			MOV		B, A
			CALL	TASKCURR
			XRL		A, B
			JZ		LSTDDEVSELF
			SETB	C
			JMP		LSTDDEVSELR
		LSTDDEVSELE:
		LSTDDEVSELF:
			;CLEAR DEVICE ACTIVE FLAG FOR TASK'S ACTIVE DEVICE
			CALL	LSTDDEVCLR
			;
			CALL	LSTDDEVSET
			;RETURN
			POP		B
			POP		ACC
			MOV		R0, A
			POP		ACC
			MOV		R1, A
			POP		DPH
			POP		DPL
			CLR		C
			RET

		;SET THE ACTIVE FLAG FOR THE INDICATED DEVICE BELONGING TO THE CURRENT TASK
		;DOES NOT CHECK DEVICE NUMBER VALIDITY
		;ON ENTRY:
		;	R0 = DEVICE CODE
		;	R1 = DEVICE NUMBER
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;		A = 0x00
		;	C = 1 IF ERROR
		;		A = ERROR CODE
		LSTDDEVSET:
			;SAVE REGISTERS
			MOV		A, R2
			PUSH	ACC
			PUSH	B
			MOV		DPTR, #STDIO_ADR
			CALL	DPTRWLDXDNN
			;LOAD NUMBER OF STDIOCB ENTRIES
			MOVX	A, @DPTR
			MOV		R2, A
			INC		DPTR
		LSTDDEVSETA:
			;ENSURE ENTRY IS PRESENT
			MOVX	A, @DPTR
			JB		STDIO_FLAG_PRESENT_A, LSTDDEVSETC
		LSTDDEVSETB:
			;POINT TO NEXT STDIOCB ENTRY
			MOV		A, #STDIO_CB_ENTRY_SIZE
			CALL	DPTRBADD
			;PROCESS NEXT STDIOCB ENTRY IF NOT LAST
			DJNZ	R2, LSTDDEVSETA
			POP		B
			POP		ACC
			MOV		R2, A
			CLR		C
			RET
		LSTDDEVSETC:
			;CHECK DEVICE CODE
			MOV		B, #STDIO_CB_DEV_TYPE
			CALL	DPTRBLRXDON
			XRL		A, R0
			JNZ		LSTDDEVSETB
			;CHECK DEVICE NUMBER
			MOV		B, #STDIO_CB_DEV_ID
			CALL	DPTRBLRXDON
			XRL		A, R1
			JNZ		LSTDDEVSETB
			;LOAD ID OF TASK USING DEVICE
			MOV		B, #STDIO_CB_FLAGS
			CALL	DPTRBLRXDON
			SETB	STDIO_FLAG_ACTIVE_A
			CALL	DPTRBSRXDON
			;RETURN
			POP		B
			POP		ACC
			MOV		R2, A
			SETB	C
			RET

		;TRANSMITS A BYTE USING THE SELECTED DEVICE
		;ON ENTRY:
		;	A	 = BYTE
		;	R0 = DEVICE ID
		;	R1 = DEVICE NUMBER
		;ON RETURN:
		;	VARIES WITH DEVICE ROUTINE CALLED
		LSTDTRANSMIT:
			;'RESERVE' STACK SPACE FOR FUNCTION ADDRESS
			PUSH	ACC
			PUSH	ACC
			;SAVE REGISTERS
			PUSH	ACC
			PUSH	DPL
			PUSH	DPH
			CLR		EA
			DEC		SP
			DEC		SP
			DEC		SP
			DEC		SP
			;CALCULATE ADDRESS OF DEVICE ENTRY POINT
			MOV		DPTR, #STDIO_DEV_TX_TABLE
			MOV		A, R0
			ADD		A, R0
			ADD		A, DPL
			MOV		DPL, A
			CLR		A
			ADDC	A, DPH
			MOV		DPH, A
			;LOAD ADDRESS OF DEVICE ENTRY POINT
			CLR		A
			MOVC	A, @A+DPTR
			PUSH	ACC
			DEC		SP
			DEC		SP
			INC		DPTR
			CLR		A
			MOVC	A, @A+DPTR
			PUSH ACC
			;RESTORE REGISTERS
			INC		SP
			INC		SP
			INC		SP
			INC		SP
			SETB	EA
			POP		DPH
			POP		DPL
			POP		ACC
			;'CALL' FUNCTION
			RET

		;RECEIVE BYTE FROM XRAM QUEUE
		;ON ENTRY:
		;	R0 = QUEUE ID
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;		A = RECEIVE BYTE
		;	C = 1 IF ERROR
		;		A = ERROR CODE
		LSTDXRAMRECV:
			MOV		A, R0
			CALL	FIFOREADBYTE
			RET

		;TRANSMIT BYTE TO XRAM QUEUE
		;ON ENTRY:
		;	A		= TRANSMIT BYTE
		;	R0	= QUEUE ID
		;ON RETURN:
		;	C = 0 IF SUCCESS
		;		A = RECEIVE BYTE
		;	C = 1 IF ERROR
		;		A = ERROR CODE
		LSTDXRAMSEND:
			PUSH	B
			MOV		B, A
			MOV		A, R0
			CALL	FIFOWRITEBYTE
			POP		B
			RET

		;
		STDIO_DEV_RX_TABLE:
			;8051 HARDWARE DEVICES
			DW		0x0000				;ADC
			DW		0x0000				;CAN
			DW		0x0000				;CTC
			DW		0x0000				;DAC
			DW		LSTDERAMRECV	;ERAM
			DW		LSTDIRAMRECV	;IRAM
			DW		0x0000				;LIN
			DW		0x0000				;PCA
			DW		0x0000				;PORT
			DW		0x0000				;SMB
			DW		SPIRECV				;SPI
			DW		UARTRECV			;UART
			DW		0x0000				;USB
			DW		LSTDXRAMRECV	;XRAM
			DW		0x00					;FLASH
			;EXTERNAL HARDWARE DEVICES
			#if DRV_ST7066 == 1
				DW		ST7066RECV		;ST7066 LCD CONTROLLER
			#endif
		STDIO_DEV_TX_TABLE:
			;8051 HARDWARE DEVICES
			DW		0x0000				;ADC
			DW		0x0000				;CAN
			DW		0x0000				;CTC
			DW		0x0000				;DAC
			DW		LSTDERAMSEND	;ERAM
			DW		LSTDIRAMSEND	;IRAM
			DW		0x0000				;LIN
			DW		0x0000				;PCA
			DW		0x0000				;PORT
			DW		0x0000				;SMB
			DW		SPISEND				;SPI
			DW		UARTSENDDATA	;UART
			DW		0x0000				;USB
			DW		LSTDXRAMSEND	;XRAM
			DW		0x0000				;FLASH
			;EXTERNAL HARDWARE DEVICES
			#if DRV_ST7066 == 1
				DW		ST7066SEND		;ST7066 LCD CONTROLLER
			#endif

END

	#endif
#endif
