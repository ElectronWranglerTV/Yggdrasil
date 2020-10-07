;* Yggdrasil (TM) Core Operating System (MCS-51): Clock Library
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

$INCLUDE (System.inc)

#if LIB_CLOCK == 1
  #ifndef CLOCK_INCLUDED
    #define CLOCK_INCLUDED 1

    #include "Clock.inc"
    #include "Return_Codes_Clock.inc"
    #include "Task.inc"

    EXTRN CODE	(DPTRBADD, DPTRBLRXDON, DPTRBSRXDON, DPTRDEC, DPTRWLDXDNN, DPTRWSXRDNN, DPTRXCHG01)
    EXTRN	CODE	(FLASHWRITE)
    EXTRN	CODE	(INTASC2BYTE)
    EXTRN	CODE	(MATHBIUADDLIM, MATHBIUSUBLIM, MATHWIUSUBLIM)
    EXTRN CODE	(MEMALCXRAM)
    EXTRN CODE	(STDCHAROUT, STDLINEIN)
    EXTRN CODE	(STRBYT2STR, STRWRD2STR)
    EXTRN CODE	(TASKCURR, TASKPAUSE)

    PUBLIC	CLOCKINIT,					LCLOCKUI,						LCLOCKREADFULL,	LCLOCKSTR2TIME
    PUBLIC	LCLOCKTIMEADD,			LCLOCKTIMECMP,			LCLOCKTIMECOPY,	LCLOCKTIMECOPYFF
    PUBLIC	LCLOCKTIMECOPYFM,		LCLOCKTIMECOPYMF,		LCLOCKTIME2STR,	LCLOCKTIMELOAD
    PUBLIC	LCLOCKTIMEGET,			LCLOCKTIMESTORE,		LCLOCKSUB,			LCLOCKADD
    PUBLIC	LCLOCKTIMELOADOFFS,	LCLOCKTIMESTOREOFFS

    CLOCK_ROUTINES		SEGMENT		CODE

    RSEG	CLOCK_ROUTINES

    ;CLOCK_TASK_ID			EQU	0x06

    ;INITIALIZE CLOCK SYSTEM
    CLOCKINIT:
    ;ALLOCATE MEMORY
    MOV		DPTR, #CLOCK_SIZE_TIME_LONG
    MOV		R0, DPL
    MOV		R1, DPH
    CALL	MEMALCXRAM
    JNC		CLOCKINITA
    ;RETURN WITH ERROR
    MOV		A, #SYS_ERR_NO_MEM
    SETB	C
    RET
    CLOCKINITA:
    ;STORE MEMORY BLOCK BASE ADDRESS
    MOV		DPTR, #CLOCK_ADR
    CALL	DPTRWSXRDNN
    MOV		DPL, R0
    MOV		DPH, R1
    ;STORE CLOCK TASK ID
    ;CALL	TASKCURR
    ;MOV 	B, #CLOCK_TASK_ID
    ;CALL	DPTRBSRXDON
    ;STORE SECONDS
    CLR		A
    MOV		B, #0x00
    CALL	DPTRBSRXDON
    ;STORE MINUTES
    INC		B
    CALL	DPTRBSRXDON
    ;STORE HOURS
    INC		B
    CALL	DPTRBSRXDON
    ;STORE DAYS
    INC		B
    CALL	DPTRBSRXDON
    INC		B
    CALL	DPTRBSRXDON
    ;STORE YEARS
    INC		B
    CALL	DPTRBSRXDON
    CLOCKA:
    ;PAUSE 1 SECOND
    MOV		TASK_PAUSETIME_LSB, #0xE8
    MOV		TASK_PAUSETIME_MSB, #0x03
    CALL	TASKPAUSE
    ;POINT TO TIME
    MOV		DPTR, #CLOCK_ADR
    CALL	DPTRWLDXDNN
    ;DECREMENT SECONDS
    MOVX	A, @DPTR
    INC		A
    CJNE	A, #0x3C, CLOCKE
    JC		CLOCKE
    CLR		A
    MOVX	@DPTR, A
    ;DECREMENT MINUTES
    INC		DPTR
    MOVX	A, @DPTR
    INC		A
    CJNE	A, #0x3C, CLOCKE
    JC		CLOCKE
    CLR		A
    MOVX	@DPTR, A
    ;DECREMENT HOURS
    INC		DPTR
    MOVX	A, @DPTR
    INC		A
    CJNE	A, #0x18, CLOCKE
    JC		CLOCKE
    CLR		A
    MOVX	@DPTR, A
    ;DECREMENT DAYS
    INC		DPTR
    MOVX	A, @DPTR
    ADD		A, #0x01
    MOV		B, A
    INC		DPTR
    MOVX	A, @DPTR
    ADDC	A, #0x00
    CALL	DPTRDEC
    CJNE	A, #0x01, CLOCKC
    JC		CLOCKC
    XCH		A, B
    CJNE	A, #0x6E, CLOCKD
    JC		CLOCKD
    CLR		A
    MOVX	@DPTR, A
    INC		DPTR
    MOVX	@DPTR, A
    ;DECREMENT YEARS
    INC		DPTR
    MOVX	A, @DPTR
    INC		A
    CJNE	A, #0x64, CLOCKE
    JC		CLOCKE
    CLR		A
    SJMP	CLOCKE
    CLOCKC:	
    XCH		A, B
    CLOCKD:
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, B
    CLOCKE:
    MOVX	@DPTR, A
    SJMP	CLOCKA

    ;CONVERTS THE TIME STRING POINTED TO BY DPTR
    ;TO A NUMBERIC VALUE
    ;ON ENTRY:
    ;	[DPTR] = ZERO-TERMINATED TIME STRING, FORMAT = YY:DDD:HH:MM:SS
    ;ON RETURN:
    ;	C = 0 IF SUCCESS
    ;		A		= 0x00
    ;		R2	= YEAR
    ;		R3	= DAY LSB
    ;		R4	= DAY MSB
    ;		R5	= HOUR
    ;		R6	= MINUTE
    ;		R7	= SECOND
    ;		DPTR = VALUE ON ENTRY
    ;	C = 1 IF FAIL
    ;		A		= ERROR CODE
    ;		R2	= 0x00
    ;		R3	= 0x00
    ;		R4	= 0x00
    ;		R5	= 0x00
    ;		R6	= 0x00
    ;		R7	= 0x00
    ;		DPTR = ADDRESS OF ERROR
    LCLOCKSTR2TIME:
    ;SAVE REGISTERS
    MOV		A, R0
    PUSH	ACC
    MOV		A, R1
    PUSH	ACC
    PUSH	DPL
    PUSH	DPH
    ;CONVERT YEAR
    MOVX	A, @DPTR
    MOV		R1, A
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R0, A
    MOV		R2, #0x30
    CALL	INTASC2BYTE
    JNC		LCLOCKSTR2TIMEB
    LCLOCKSTR2TIMEA:
    ;CHARACTER IS NOT NUMBER, RETURN WITH ERROR
    ;POP		ACC
    ;POP		ACC
    POP		ACC
    POP		ACC
    MOV		R1, A
    POP		ACC
    MOV		R0, A
    MOV		A, #0x01
    SETB	C
    RET
    LCLOCKSTR2TIMEB:
    ;CHECK FOR YEAR OUT OF BOUNDS
    ;MOV		A, R0
    ;CLR		C
    ;SUBB	A, #0x64
    ;JNC		LCLOCKSTR2TIMEYEARVALID
    ;LCLOCKSTR2TIMEINVLAIDTIME:
    ;ERROR - INVALID YEAR/DAY/HOUR/MINUTE/SECOND
    ;POP		ACC
    ;MOV		R1, A
    ;POP		ACC
    ;MOV		R0, A
    ;MOV		A, #0x02
    ;SETB	C
    ;RET
    MOV		A, R0
    PUSH	ACC
    ;CHECK FOR ":"
    INC		DPTR
    MOVX	A, @DPTR
    CJNE	A, #':', LCLOCKSTR2TIMEA
    ;CONVERT DAY
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R2, A
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R1, A
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R0, A
    MOV		R3, #0x30
    MOV		R4, #0x30
    CALL	INTASC2BYTE
    JNC		LCLOCKSTR2TIMED
    LCLOCKSTR2TIMEC:
    POP		ACC								;REMOVE YEAR FROM STACK
    SJMP	LCLOCKSTR2TIMEA
    LCLOCKSTR2TIMED:
    ;CHECK DAY FOR OUT OF BOUNDS
    ;MOV		A, R0
    ;CLR		C
    ;SUBB	A, #0x6C
    ;MOV		A, #R1
    ;SUBB	A, #0x01
    ;JNC		LCLOCKSTR2TIMEDAYVALID
    ;LCLOCKSTR2TIMEINVALIDTIMEA:
    ;ERROR - INVALID YEAR
    ;POP		ACC							;REMOVE YEAR FROM STACK
    ;SJMP		LCLOCKSTR2TIMEINVALIDTIME	
    ;LCLOCKSTR2TIMEDAYVALID:
    MOV		A, R0
    PUSH	ACC
    MOV		A, R1
    PUSH	ACC
    ;CHECK FOR ":"
    INC		DPTR
    MOVX	A, @DPTR
    CJNE	A, #':', LCLOCKSTR2TIMEC
    ;CONVERT HOUR
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R1, A
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R0, A
    MOV		R2, #0x30
    CALL	INTASC2BYTE
    JNC		LCLOCKSTR2TIMEF
    LCLOCKSTR2TIMEE:
    POP		ACC
    POP		ACC
    SJMP	LCLOCKSTR2TIMEC
    LCLOCKSTR2TIMEF:
    ;CHECK HOUR FOR OUT OF BOUNDS
    ;MOV		A, R0
    ;CLR		C
    ;SUBB	A, #0x3C
    ;JNC		LCLOCKSTR2TIMEHOURVALID
    ;LCLOCKSTR2TIMEINVALIDTIMEB:
    ;ERROR - HOUR OUT OF BOUNDS
    ;POP		ACC								;REMOVE DAY FROM STACK
    ;POP		ACC
    ;SJMP	LCLOCKSTR2TIMEINVALIDTIMEA
    ;LCLOCKSTR2TIMEHOURVALID:
    MOV		A, R0
    PUSH	ACC
    ;CHECK FOR ":"
    INC		DPTR
    MOVX	A, @DPTR
    CJNE	A, #':', LCLOCKSTR2TIMEE
    ;CONVERT MINUTE
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R1, A
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R0, A
    MOV		R2, #0x30
    CALL	INTASC2BYTE
    JNC		LCLOCKSTR2TIMEH
    LCLOCKSTR2TIMEG:
    POP		ACC
    SJMP	LCLOCKSTR2TIMEE
    LCLOCKSTR2TIMEH:
    ;CHECK MINUTE FOR OUT OF BOUNDS
    ;MOV		A, R0
    ;CLR		C
    ;SUBB	A, #0x3C
    ;JNC		LCLOCKSTR2TIMEMINUTEVALID
    ;LCLOCKSTR2TIMEINVALIDTIMEC:
    ;ERROR - HOUR OUT OF BOUNDS
    ;POP		ACC								;REMOVE HOUR FROM STACK
    ;SJMP	LCLOCKSTR2TIMEINVALIDTIMEB
    ;LCLOCKSTR2TIMEMINUTEVALID:
    MOV		A, R0
    PUSH	ACC
    ;CHECK FOR ":"
    INC		DPTR
    MOVX	A, @DPTR
    CJNE	A, #':', LCLOCKSTR2TIMEG
    ;CONVERT MINUTE
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R1, A
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R0, A
    MOV		R2, #0x30
    CALL	INTASC2BYTE
    JNC		LCLOCKSTR2TIMEJ
    LCLOCKSTR2TIMEI:
    POP		ACC
    SJMP	LCLOCKSTR2TIMEG
    LCLOCKSTR2TIMEJ:
    ;CHECK SECOND FOR OUT OF BOUNDS
    ;MOV		A, R0
    ;CLR		C
    ;SUBB	A, #0x18
    ;JNC		LCLOCKSTR2TIMESECONDVALID
    ;LCLOCKSTR2TIMEINVALIDTIMED:
    ;ERROR - HOUR OUT OF BOUNDS
    ;POP		ACC								;REMOVE MINUTE FROM STACK
    ;SJMP	LCLOCKSTR2TIMEINVALIDTIMEC
    ;LCLOCKSTR2TIMESECONDVALID:
    ;CHECK FOR TERMINATOR
    INC		DPTR
    MOVX	A, @DPTR
    JNZ		LCLOCKSTR2TIMEI
    ;MOVE VALUES INTO REGISTERS
    MOV		A, R0
    MOV		R7, A
    POP		ACC
    MOV		R6, A
    POP		ACC
    MOV		R5, A
    POP		ACC
    MOV		R4, A
    POP		ACC
    MOV		R3, A
    POP		ACC
    MOV		R2, A
    ;RESTORE REGISTERS & RETURN
    POP		DPH
    POP		DPL
    POP		ACC
    MOV		R1, A
    POP		ACC
    MOV		R0, A
    RET

    ;CONVERTS THE TIME VALUE IN R2 - R7 INTO A
    ;ZERO-TERMINATED STRING STORED AT DPTR
    ;ON ENTRY:
    ;	R2		= YEAR
    ;	R3		= DAY LSB
    ;	R4		= DAY MSB
    ;	R5		= HOUR
    ;	R6		= MINUTE
    ;	R7		= SECOND
    ;	DPTR	= POINTER TO STRING STORAGE
    ;ON RETURN:
    ;	C = 0 IF SUCCESS
    ;		R2			= VALUE ON ENTRY
    ;		R3			= VALUE ON ENTRY
    ;		R4			= VALUE ON ENTRY
    ;		R5			= VALUE ON ENTRY
    ;		R6			= VALUE ON ENTRY
    ;		R7			= VALUE ON ENTRY
    ;		[DPTR]	= ZERO-TERMINATED TIME STRING, FORMAT = YY:DDD:HH:MM:SS
    ;	C = 1 IF FAIL
    LCLOCKTIME2STR:
    MOV		A, R0
    PUSH	ACC
    MOV		A, R1
    PUSH	ACC
    MOV		A, R2
    PUSH	ACC
    MOV		A, R3
    PUSH	ACC
    PUSH	DPL
    PUSH	DPH
    ;VERIFY YEARS WITHIN BOUNDS
    MOV		A, R2
    CLR		C
    SUBB	A, #0x64
    JC		LCLOCKTIME2STRB
    LCLOCKTIME2STRA:
    ;ERROR - INVALID YEAR/DAY/HOUR/MINUTE/SECOND
    POP		DPH
    POP		DPL
    POP		ACC
    MOV		R3, A
    POP		ACC
    MOV		R2, A
    POP		ACC
    MOV		R1, A
    POP		ACC
    MOV		R0, A
    RET
    LCLOCKTIME2STRB:
    ;CONVERT YEARS
    MOV		A, R2
    MOV		R0, A
    CALL	STRBYT2STR
    MOV		A, R1
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, R0
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, #':'
    MOVX	@DPTR, A
    LCLOCKTIME2STRE:
    ;CONVERT DAYS
    INC		DPTR
    MOV		A, R3
    MOV		R0, A
    MOV		A, R4
    MOV		R1, A
    CALL	STRWRD2STR
    MOV		A, R2
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, R1
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, R0
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, #':'
    MOVX	@DPTR, A
    ;CONVERT HOURS
    INC		DPTR
    MOV		A, R5
    MOV		R0, A
    CALL	STRBYT2STR
    MOV		A, R1
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, R0
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, #':'
    MOVX	@DPTR, A
    ;CONVERT MINUTES
    INC		DPTR
    MOV		A, R6
    MOV		R0, A
    CALL	STRBYT2STR
    MOV		A, R1
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, R0
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, #':'
    MOVX	@DPTR, A
    ;CONVERT SECONDS
    INC		DPTR
    MOV		A, R7
    MOV		R0, A
    CALL	STRBYT2STR
    MOV		A, R1
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, R0
    MOVX	@DPTR, A
    ;STORE TERMINATOR
    INC		DPTR
    MOV		A, #0x00
    MOVX	@DPTR, A
    ;RESTORE GEGISTERS & RETURN
    POP		DPH
    POP		DPL
    POP		ACC
    MOV		R3, A
    POP		ACC
    MOV		R2, A
    POP		ACC
    MOV		R1, A
    POP		ACC
    MOV		R0, A
    RET

    ;RETURNS THE CLOCK VALUE POINTER TO BY DPTR
    ;PLUS THE TIME VALUE IN R2-R7
    ;ON ENTRY:
    ;	R2		= YEAR
    ;	R3		= DAY LSB
    ;	R4		= DAY MSB
    ;	R5		= HOUR
    ;	R6		= MINUTE
    ;	R7		= SECOND
    ;	DPTR	= POINTER TO TIME VALUE
    ;ON RETURN:
    ;	R2		= RESULT YEAR
    ;	R3		= RESULT DAY LSB
    ;	R4		= RESULT DAY MSB
    ;	R5		= RESULT HOUR
    ;	R6		= RESULT MINUTE
    ;	R7		= RESULT SECOND
    ;	DPTR	= VALUE ON ENTRY
    LCLOCKADD:
    PUSH	ACC
    PUSH	B
    MOV		A, R0
    PUSH	ACC
    PUSH	DPL
    PUSH	DPH
    ;SECONDS
    MOVX	A, @DPTR
    MOV		B, R7
    MOV		R0, #60
    CLR		C
    CALL	MATHBIUADDLIM
    MOV		R7, A
    JNC		LCLOCKADDA
    INC		R6									;COMPENSATE FOR SECONDS OVERFLOW
    LCLOCKADDA:
    ;MINUTES
    INC		DPTR
    MOVX	A, @DPTR
    MOV		B, R6
    CLR		C
    CALL	MATHBIUADDLIM
    MOV		R6, A
    JNC		LCLOCKADDB
    INC		R5									;COMPENSATE FOR MINUTES OVERFLOW
    LCLOCKADDB:
    ;HOURS
    INC		DPTR
    MOVX	A, @DPTR
    MOV		B, R5
    MOV		R0, #24
    CLR		C
    CALL	MATHBIUADDLIM
    MOV		R5, A
    JNC		LCLOCKADDC
    MOV		A, #0x01						;COMPENSATE FOR HOURS OVERFLOW
    ADD		A, R3
    MOV		R3, A
    MOV		A, R4
    ADDC	A, #0x00
    MOV		R4, A
    LCLOCKADDC:
    ;DAYS
    INC		DPTR
    MOVX	A, @DPTR
    ADD		A, R3
    MOV		R3, A
    INC		DPTR
    MOVX	A, @DPTR
    ADDC	A, R4
    MOV		R4, A
    CJNE	R4, #0x01, LCLOCKADDD
    CJNE	R3, #0x6D, LCLOCKADDD
    MOV		A, R3
    CLR		C
    SUBB	A, #0x6D
    MOV		R3, A
    MOV		A, R4
    CLR		C
    SUBB	A, #0x01
    MOV		R4, A
    INC		R2
    LCLOCKADDD:
    ;YEARS
    INC		DPTR
    MOVX	A, @DPTR
    MOV		B, R2
    MOV		R0, #100
    CLR		C
    CALL	MATHBIUADDLIM
    MOV		R2, A
    ;RETURN
    POP		DPH
    POP		DPL
    POP		ACC
    MOV		R0, A
    POP		B
    POP		ACC
    RET

    ;SUBTRACTS THE TIME IN R7:R2 FROM
    ;THE TIME POINTED TO BY DPTR
    ;ON ENTRY:
    ;	R2		= YEAR
    ;	R3		= DAY LSB
    ;	R4		= DAY MSB
    ;	R5		= HOUR
    ;	R6		= MINUTE
    ;	R7		= SECOND
    ;	DPTR	= POINTER TO TIME
    ;ON RETURN:
    ;	R2		= DIFFERENCE YEAR
    ;	R3		= DIFFERENCE DAY LSB
    ;	R4		= DIFFERENCE DAY MSB
    ;	R5		= DIFFERENCE HOUR
    ;	R6		= DIFFERENCE MINUTE
    ;	R7		= DIFFERENCE SECOND
    ;	DPTR	= POINTER TO TIME
    ; C = 0 IF [DPTR] => R7:R0
    ;		ACC = 0 IF [DPTR] = R7:R0
    ;	C = 1 IF [DPTR] < R7:R0
    LCLOCKSUB:
    ;SAVE REGISTERS
    MOV		A, R0
    PUSH	ACC
    MOV		A, R1
    PUSH	ACC
    PUSH	DPL
    PUSH	DPH
    ;SECONDS
    MOVX	A, @DPTR
    MOV		B, R7
    MOV		R0, #0x3C
    CLR		C
    CALL	MATHBIUSUBLIM
    MOV		R7, A
    ;MINUTES
    INC		DPTR
    MOVX	A, @DPTR
    MOV		B, R6
    MOV		R0, #0x3C
    CLR		C
    CALL	MATHBIUSUBLIM
    MOV		R6, A
    ;HOURS
    INC		DPTR
    MOVX	A, @DPTR
    MOV		B, R5
    MOV		R0, #0x18
    CLR		C
    CALL	MATHBIUSUBLIM
    ;MOV		R5, A
    PUSH	ACC
    MOV		A, R2
    PUSH	ACC
    ;DAYS
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R0, A
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R1, A
    MOV		A, R3
    MOV		R2, A
    MOV		A, R4
    MOV		R3, A
    MOV		R4, #0x6D
    MOV		R5, #0x01
    CALL	MATHWIUSUBLIM
    MOV		A, R0
    MOV		R3, A
    MOV		A, R1
    MOV		R4, A
    ;
    POP		ACC
    MOV		R2, A
    POP		ACC
    MOV		R5, A
    ;YEARS
    INC		DPTR
    MOVX	A, @DPTR
    MOV		B, R2
    MOV		R0, #0x3C
    CLR		C
    CALL	MATHBIUSUBLIM
    MOV		R2, A
    ;RESTORE REGISTERS & RETURN
    POP		DPH
    POP		DPL
    POP		ACC
    MOV		R1, A
    POP		ACC
    MOV		R0, A
    RET

    ;RETURNS THE CURRENT CLOCK VALUE PLUS THE TIME
    ;VALUE IN R2-R7
    ;ON ENTRY:
    ;	R2 = YEAR
    ;	R3 = DAY LSB
    ;	R4 = DAY MSB
    ;	R5 = HOUR
    ;	R6 = MINUTE
    ;	R7 = SECOND
    ;ON RETURN:
    ;	R2 = YEAR
    ;	R3 = DAY LSB
    ;	R4 = DAY MSB
    ;	R5 = HOUR
    ;	R6 = MINUTE
    ;	R7 = SECOND
    LCLOCKTIMEADD:
    PUSH	ACC
    PUSH	B
    MOV		A, R0
    PUSH	ACC
    PUSH	DPL
    PUSH	DPH
    PUSH	IE
    CLR		EA
    MOV		DPTR, #CLOCK_ADR
    CALL	DPTRWLDXDNN
    CALL	LCLOCKADD
    ;RETURN
    POP		IE
    POP		DPH
    POP		DPL
    POP		ACC
    MOV		R0, A
    POP		B
    POP		ACC
    RET

    ;COMPARE CURRENT TIME TO THE SUPPLIED TIME
    ;ON ENTRY:
    ;	R2 = YEAR
    ;	R3 = DAY LSB
    ;	R4 = DAY MSB
    ;	R5 = HOUR
    ;	R6 = MINUTE
    ;	R7 = SECOND
    ;ON RETURN:
    ; C = 0 IF CURRENT TIME => SUPPLIED TIME
    ;		ACC = 0 IF CURRENT TIME = SUPPLIED TIME
    ;	C = 1 IF CURRENT TIME < SUPPLIED TIME
    LCLOCKTIMECMP:
    ;SAVE REGISTERS
    PUSH	DPL
    PUSH	DPH
    ;LOAD POINTER TO CLOCK CONTROL/STATUS
    MOV		DPTR, #CLOCK_ADR
    CALL	DPTRWLDXDNN
    ;SECONDS
    MOVX	A, @DPTR
    CLR		C
    SUBB	A, R7
    ;MINUTES
    INC		DPTR
    MOVX	A, @DPTR
    SUBB	A, R6
    ;HOURS
    INC		DPTR
    MOVX	A, @DPTR
    SUBB	A, R5
    ;DAYS
    INC		DPTR
    MOVX	A, @DPTR
    SUBB	A, R3
    INC		DPTR
    MOVX	A, @DPTR
    SUBB	A, R4
    ;YEARS
    INC		DPTR
    MOVX	A, @DPTR
    SUBB	A, R2
    ;RETURN
    POP		DPH
    POP		DPL
    RET

    ;COPIES A TIME IN RAM LOCATED AT DPTR TO
    ;THE LOCATION IN RAM AT R1:R0
    ;ON ENTRY:
    ;	DPTR = 	POINTER TO SOURCE LOCATION
    ;	R1:R0	= POINTER TO DESTINATION LOCATION
    ;ON RETURN:
    ;	NONE
    LCLOCKTIMECOPY:
    ;SAVE REGISTERS
    PUSH	ACC
    MOV		A, R0
    PUSH	ACC
    MOV		A, R1
    PUSH	ACC
    PUSH	B
    PUSH	DPL
    PUSH	DPH
    ;LOAD NUMBER OF BYTES TO COPY
    MOV		B, #0x06
    LCLOCKTIMECOPYA:
    ;COPY TIME
    MOVX	A, @DPTR
    INC		DPTR
    CALL	DPTRXCHG01
    MOVX	@DPTR, A
    INC		DPTR
    CALL	DPTRXCHG01
    DJNZ	B, LCLOCKTIMECOPYA
    ;SAVE REGISTERS & RETURN
    POP		DPH
    POP		DPL
    POP		B
    POP		ACC
    MOV		R1, A
    POP		ACC
    MOV		R0, A
    POP		ACC
    RET

    ;COPIES A TIME IN FLASH LOCATED AT DPTR TO
    ;THE LOCATION IN FLASH AT R1:R0
    ;ON ENTRY:
    ;	DPTR = 	POINTER TO SOURCE LOCATION
    ;	R1:R0	= POINTER TO DESTINATION LOCATION
    ;ON RETURN:
    ;	NONE
    LCLOCKTIMECOPYFF:
    ;SAVE REGISTERS
    PUSH	ACC
    MOV		A, R0
    PUSH	ACC
    MOV		A, R1
    PUSH	ACC
    PUSH	B
    PUSH	DPL
    PUSH	DPH
    ;LOAD NUMBER OF BYTES TO COPY
    MOV		B, #0x06
    LCLOCKTIMECOPYFFA:
    ;COPY TIME
    CLR		A
    MOVC	A, @A+DPTR
    INC		DPTR
    CALL	DPTRXCHG01
    CALL	FLASHWRITE
    JC		LCLOCKTIMECOPYFFB
    INC		DPTR
    CALL	DPTRXCHG01
    DJNZ	B, LCLOCKTIMECOPYFFA
    LCLOCKTIMECOPYFFB:
    ;SAVE REGISTERS & RETURN
    POP		DPH
    POP		DPL
    POP		B
    POP		ACC
    MOV		R1, A
    POP		ACC
    MOV		R0, A
    POP		ACC
    RET

    ;COPIES A TIME IN FLASH LOCATED AT DPTR TO
    ;THE LOCATION IN RAM AT R1:R0
    ;ON ENTRY:
    ;	DPTR = 	POINTER TO SOURCE LOCATION
    ;	R1:R0	= POINTER TO DESTINATION LOCATION
    ;ON RETURN:
    ;	NONE
    LCLOCKTIMECOPYMF:
    ;SAVE REGISTERS
    PUSH	ACC
    MOV		A, R0
    PUSH	ACC
    MOV		A, R1
    PUSH	ACC
    PUSH	B
    PUSH	DPL
    PUSH	DPH
    ;LOAD NUMBER OF BYTES TO COPY
    MOV		B, #0x06
    LCLOCKTIMECOPYMFA:
    ;COPY TIME
    CLR		A
    MOVC	A, @A+DPTR
    INC		DPTR
    CALL	DPTRXCHG01
    MOVX	@DPTR, A
    INC		DPTR
    CALL	DPTRXCHG01
    DJNZ	B, LCLOCKTIMECOPYMFA
    ;SAVE REGISTERS & RETURN
    POP		DPH
    POP		DPL
    POP		B
    POP		ACC
    MOV		R1, A
    POP		ACC
    MOV		R0, A
    POP		ACC
    RET

    ;COPIES A TIME IN RAM LOCATED AT DPTR TO
    ;THE LOCATION IN FLASH AT R1:R0
    ;ON ENTRY:
    ;	DPTR = 	POINTER TO SOURCE LOCATION
    ;	R1:R0	= POINTER TO DESTINATION LOCATION
    ;ON RETURN:
    ;	NONE
    LCLOCKTIMECOPYFM:
    ;SAVE REGISTERS
    PUSH	ACC
    MOV		A, R0
    PUSH	ACC
    MOV		A, R1
    PUSH	ACC
    PUSH	B
    PUSH	DPL
    PUSH	DPH
    ;LOAD NUMBER OF BYTES TO COPY
    MOV		B, #0x06
    LCLOCKTIMECOPYFMA:
    ;COPY TIME
    MOVX	A, @DPTR
    INC		DPTR
    CALL	DPTRXCHG01
    CALL	FLASHWRITE
    JC		LCLOCKTIMECOPYFMB
    INC		DPTR
    CALL	DPTRXCHG01
    DJNZ	B, LCLOCKTIMECOPYFMA
    LCLOCKTIMECOPYFMB:
    ;SAVE REGISTERS & RETURN
    POP		DPH
    POP		DPL
    POP		B
    POP		ACC
    MOV		R1, A
    POP		ACC
    MOV		R0, A
    POP		ACC
    RET

    ;LOADS A TIME FROM RAM INTO R2 - R7
    ;ON ENTRY:
    ;	DPTR = POINTER TO STORAGE LOCATION
    ;ON RETURN:
    ;	NONE
    LCLOCKTIMELOAD:
    ;SAVE REGISTERS
    PUSH	ACC
    PUSH	DPL
    PUSH	DPH
    ;LOAD TIME
    MOVX	A, @DPTR									;SECOND
    MOV		R7, A
    INC		DPTR
    MOVX	A, @DPTR									;MINUTE
    MOV		R6, A
    INC		DPTR
    MOVX	A, @DPTR									;HOUR
    MOV		R5, A
    INC		DPTR
    MOVX	A, @DPTR									;DAY LSB
    MOV		R4, A
    INC		DPTR
    MOVX	A, @DPTR									;DAY MSB
    MOV		R3, A
    INC		DPTR
    MOVX	A, @DPTR									;YEAR
    MOV		R2, A
    ;SAVE REGISTERS & RETURN
    POP		DPH
    POP		DPL
    POP		ACC
    RET

    ;LOADS A TIME FROM RAM INTO R2 - R7
    ;ON ENTRY:
    ;	B			= STORAGE LOCATION OFFSET
    ;	DPTR	= POINTER TO STORAGE LOCATION
    ;ON RETURN:
    ;	NONE
    LCLOCKTIMELOADOFFS:
    ;SAVE REGISTERS
    PUSH	ACC
    PUSH	B
    PUSH	DPL
    PUSH	DPH
    ;CALCULATE ADDRESS
    MOV		A, B
    CALL	DPTRBADD
    ;STORE TIME
    CALL	LCLOCKTIMELOAD
    ;SAVE REGISTERS & RETURN
    POP		DPH
    POP		DPL
    POP		B
    POP		ACC
    RET

    ;STORES A TIME FROM R2 - R7 INTO RAM
    ;ON ENTRY:
    ;	DPTR = POINTER TO STORAGE LOCATION
    ;ON RETURN:
    ;	NONE
    LCLOCKTIMESTORE:
    ;SAVE REGISTERS
    PUSH	ACC
    PUSH	DPL
    PUSH	DPH
    ;STORE TIME
    MOV		A, R7												;SECOND
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, R6												;MINUTE
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, R5												;HOUR
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, R4												;DAY LSB
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, R3												;DAY MSB
    MOVX	@DPTR, A
    INC		DPTR
    MOV		A, R2												;YEAR
    MOVX	@DPTR, A
    ;SAVE REGISTERS & RETURN
    POP		DPH
    POP		DPL
    POP		ACC
    RET

    ;STORES A TIME FROM R2 - R7 INTO RAM
    ;ON ENTRY:
    ;	B			= STORAGE LOCATION OFFSET
    ;	DPTR	= POINTER TO STORAGE LOCATION
    ;ON RETURN:
    ;	NONE
    LCLOCKTIMESTOREOFFS:
    ;SAVE REGISTERS
    PUSH	ACC
    PUSH	B
    PUSH	DPL
    PUSH	DPH
    ;CALCULATE ADDRESS
    MOV		A, B
    CALL	DPTRBADD
    ;STORE TIME
    CALL	LCLOCKTIMESTORE
    ;SAVE REGISTERS & RETURN
    POP		DPH
    POP		DPL
    POP		B
    POP		ACC
    RET

    ;READ YY:DDD:HH:MM:SS FROM CLOCK
    ;ON ENTRY:
    ;	NONE
    ;ON RETURN:
    ;	R2 = YEAR
    ;	R3 = DAY LSB
    ;	R4 = DAY MSB
    ;	R5 = HOUR
    ;	R6 = MINUTE
    ;	R7 = SECOND
    LCLOCKREADFULL:
    ;SAVE REGISTERS
    PUSH	ACC
    PUSH	DPL
    PUSH	DPH
    PUSH	IE
    CLR		EA
    ;LOAD POINTER TO CLOCK CONTROL/STATUS
    MOV		DPTR, #CLOCK_ADR
    CALL	DPTRWLDXDNN
    ;SECONDS
    MOVX	A, @DPTR
    MOV		R7, A
    ;MINUTES
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R6, A
    ;HOURS
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R5, A
    ;DAYS
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R3, A
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R4, A
    ;YEARS
    INC		DPTR
    MOVX	A, @DPTR
    MOV		R2, A
    ;RETURN
    POP		IE
    POP		DPH
    POP		DPL
    POP		ACC
    RET

    ;CLOCK USER INTERFACE
    LCLOCKUI:
    ;CALL	TASKRUNNING
    ;MOV		DPTR, #CLOCK_ADR
    ;CALL	DPTRWLDXDNN
    ;PRINT YEAR
    ;MOV		A, #0x06
    ;CALL	DPTRBADD
    ;MOVX	A, @DPTR
    ;CALL	STRBYT2STR
    ;MOV		A, R2
    ;CALL	STDCHAROUT
    ;MOV		A, R1
    ;CALL	STDCHAROUT
    ;MOV		A, R0
    ;CALL	STDCHAROUT
    ;MOV		A, #':'
    ;CALL	STDCHAROUT
    ;;PRINT DAY
    ;CALL	DPTRDEC
    ;MOVX	A, @DPTR
    ;MOV		R0, A
    ;CALL	DPTRDEC
    ;MOVX	A, @DPTR
    ;MOV		R1, A
    ;CALL	STRWRD2STR
    ;MOV		A, R2
    ;CALL	STDCHAROUT
    ;MOV		A, R1
    ;CALL	STDCHAROUT
    ;MOV		A, R0
    ;CALL	STDCHAROUT
    ;MOV		A, #':'
    ;CALL	STDCHAROUT
    ;PRINT HOUR
    ;CALL	DPTRDEC
    ;MOVX	A, @DPTR
    ;CALL	STRBYT2STR
    ;MOV		A, R1
    ;CALL	STDCHAROUT
    ;MOV		A, R0
    ;CALL	STDCHAROUT
    ;MOV		A, #':'
    ;CALL	STDCHAROUT
    ;PRINT MINUTE
    ;CALL	DPTRDEC
    ;MOVX	A, @DPTR
    ;CALL	STRBYT2STR
    ;MOV		A, R1
    ;CALL	STDCHAROUT
    ;MOV		A, R0
    ;CALL	STDCHAROUT
    ;MOV		A, #':'
    ;CALL	STDCHAROUT
    ;PRINT SECOND
    ;CALL	DPTRDEC
    ;MOVX	A, @DPTR
    ;CALL 	STRBYT2STR
    ;MOV		A, R1
    ;CALL	STDCHAROUT
    ;MOV		A, R0
    ;CALL	STDCHAROUT
    RET


    ;GET NEW TIME FROM USER
    ;ON ENTRY:
    ;	DPTR	= POINTER TO TIME VALUE
    ;ON RETURN:
    ;	C = 0 IF TIME WAS CHANGED
    ;		A			= 0x00
    ;		DPTR	= VALUE ON ENTRY
    ;	C = 1 IF ERROR OR TIME NOT CHANGED
    ;		A			= 0x00 IF TIME NOT CHANGED
    ;		A			!= 0x00 IF ERROR
    ;		DPTR	= VALUE ON ENTRY
    LCLOCKTIMEGET	PROC
    ;SAVE REGISTERS
    PUSH	DPL
    PUSH	DPH
    MOV		A, R2
    PUSH	ACC
    MOV		A, R3
    PUSH	ACC
    MOV		A, R4
    PUSH	ACC
    MOV		A, R5
    PUSH	ACC
    MOV		A, R6
    PUSH	ACC
    MOV		A, R7
    PUSH	ACC
    ;WAIT FOR USER INPUT
    MOV		A, #0x0F
    CALL	STDLINEIN
    JNC		TIMEGETA
    MOV		R2, #CLOCK_ERR_STDIO_RD
    ;ERROR
    TIMEGETY:
    POP		DPH
    POP		DPL
    TIMEGETZ:
    POP		ACC
    MOV		R7, A
    POP		ACC
    MOV		R6, A
    POP		ACC
    MOV		R5, A
    POP		ACC
    MOV		R4, A
    POP		ACC
    MOV		R3, A
    POP		ACC
    XCH		A, R2
    RET
    TIMEGETA:
    ;NEW TIME NOT ENTERED
    JNZ		TIMEGETB
    SETB	C
    SJMP	TIMEGETY
    TIMEGETB:
    ;VERIFY NEW TIME IS CORRECT LENGTH
    XRL		A, #0x0F
    JZ		TIMEGETC
    ;ERROR - TIME IS NOT CORRECT LENGTH
    MOV		R2, #CLOCK_ERR_LENGTH
    SETB	C
    SJMP	TIMEGETY
    TIMEGETC:
    ;VERIFY NEW TIME IS IN CORRECT FORMAT
    CALL	LCLOCKSTR2TIME
    JNC		TIMEGETD
    MOV		R2, #CLOCK_ERR_FORMAT
    SJMP	TIMEGETY
    TIMEGETD:
    ;COPY NEW TIME TO MEMORY
    POP		DPH
    POP		DPL
    CALL	LCLOCKTIMESTORE
    CLR		A
    CLR		C
    SJMP	TIMEGETZ
    ENDP

    END
  #endif
#endif
