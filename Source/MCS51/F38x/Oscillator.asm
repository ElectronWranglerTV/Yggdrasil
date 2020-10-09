;* Yggdrasil (TM) Core Operating System (MCS-51): Silicon Labs C8051F38x Oscillator Library
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

$INCLUDE (System.inc)
$INCLUDE (System_Const.inc)
;
PUBLIC  LOSCFREQGET,  LOSCFREQSET

OSCILLATOR_ABSTRACTION  SEGMENT CODE
RSEG  OSCILLATOR_ABSTRACTION


;READ CURRENT CLOCK SPEED, IN KHZ
;ON ENTRY:
; NONE
;ON RETURN:
; C = 0 IF SUCCESS
;   A   = VALUE ON ENTRY
;   R0  = BYTE 0 OF CLOCK FREQUENCY
;   R1  = BYTE 1 OF CLOCK FREQUENCY
;   R2  = BYTE 2 OF CLOCK FREQUENCY
; C = 1 IF FAIL
;   A   = ERROR CODE
LOSCFREQGET:
PUSH	ACC
MOV		A, #CLKSL
ANL		A, CLKSEL
JNZ		LOSCFREQGETD
;HFO / 4 SCALED BY IFCN BITS
MOV		A, #IFCN
ANL		A, OSCICN
JNZ		LOSCFREQGETA
;1.5MHZ
MOV		R0, #0xDC
MOV		R1, #0x05
MOV		R2, #0x00
POP		ACC
CLR		C
RET
LOSCFREQGETA:
DEC		A
JNZ		LOSCFREQGETB
;3MHZ
MOV		R0, #0xB8
MOV		R1, #0x0B
MOV		R2, #0x00
POP		ACC
CLR		C
RET
LOSCFREQGETB:
DEC		A
JNZ		LOSCFREQGETC
;6MHZ
MOV		R0, #0x70
MOV		R1, #0x17
MOV		R2, #0x00
POP		ACC
CLR		C
RET
LOSCFREQGETC:
;12MHZ
MOV		R0, #0xE0
MOV		R1, #0x2E
MOV		R2, #0x00
POP		ACC
CLR		C
RET
LOSCFREQGETD:
DEC		A
JNZ		LOSCFREQGETE
;EXTERNAL OSCILATOR
MOV		R0, #OSCILLATOR_EXT_FREQ
MOV		R1, #OSCILLATOR_EXT_FREQ >> 8
MOV		R2,	#0x00
POP		ACC
CLR		C
RET
LOSCFREQGETE:
DEC		A
JNZ		LOSCFREQGETF
;HFO / 2 - 24MHZ
MOV		R0, #0xC0
MOV		R1, #0x5D
MOV		R2, #0x00
POP		ACC
CLR		C
RET
LOSCFREQGETF:
DEC		A
JNZ		LOSCFREQGETG
;HFO - 48MHZ
MOV		R0, #0x80
MOV		R1, #0xBB
MOV		R2, #0x00
POP		ACC
CLR		C
RET
LOSCFREQGETG:
DEC		A
JNZ		HA0SCFREQGETZ
MOV		A, #OSCLD
ANL		A, OSCLCN
JNZ		LOSCFREQGETH
;LFO / 8 = 10KHZ
MOV		R0, #0x0A
MOV		R1, #0x00
MOV		R2, #0x00
POP		ACC
CLR		C
RET
LOSCFREQGETH:
DEC		A
JNZ		LOSCFREQGETI
;LFO / 4 = 20KHZ
MOV		R0, #0x14
MOV		R1, #0x00
MOV		R2, #0x00
POP		ACC
CLR		C
RET
LOSCFREQGETI:
DEC		A
JNZ		LOSCFREQGETJ
;LFO / 2 = 40KHZ
MOV		R0, #0x28
MOV		R1, #0x00
MOV		R2, #0x00
POP		ACC
CLR		C
RET
LOSCFREQGETJ:
;LFO / 1 = 80KHZ
MOV		R0, #0x50
MOV		R1, #0x00
MOV		R2, #0x00
POP		ACC
CLR		C
RET
HA0SCFREQGETZ:
;INVALID CLKSL VALUE
POP		ACC
MOV		A, #SYS_ERR_HWSW_MISMCH
SETB	C
RET

;SET CLOCK SPEED
;ON ENTRY:
;	R0 = BYTE 0 OF CLOCK FREQUENCY
;	R1 = BYTE 1 OF CLOCK FREQUENCY
;	R2 = BYTE 3 OF CLOCK FREQUENCY
LOSCFREQSET:
CJNE	R2, #0x00, LOSCFREQSETZ
PUSH	ACC
CJNE	R1, #0xBB, LOSCFREQSETB
CJNE	R0,	#0x80, LOSCFREQSETZ
;48MHZ
MOV		OSCICN, #0x83
LOSCFREQSETA:
MOV		A, OSCICN
JNB		ACC.6, LOSCFREQSETA
MOV		A, #CLKSL XOR 0xFF
ANL		A, CLKSEL
ORL		A, #0x03
MOV		CLKSEL, A
CLR		C
POP		ACC
RET
LOSCFREQSETB:
CJNE	R1, #0x5D, LOSCFREQSETD
CJNE	R0,	#0xC0, LOSCFREQSETZ
;24MHZ
MOV		OSCICN, #0x83
LOSCFREQSETC:
MOV		A, OSCICN
JNB		ACC.6, LOSCFREQSETC
MOV		A, #CLKSL XOR 0xFF
ANL		A, CLKSEL
ORL		A, #0x02
MOV		CLKSEL, A
CLR		C
POP		ACC
RET
LOSCFREQSETD:
CJNE	R1, #0x2E, LOSCFREQSETF
CJNE	R0,	#0xE0, LOSCFREQSETZ
;12MHZ
MOV		OSCICN, #0x83
LOSCFREQSETE:
MOV		A, OSCICN
JNB		ACC.6, LOSCFREQSETE
ANL		CLKSEL, #0xF8
CLR		C
POP		ACC
RET
LOSCFREQSETF:
CJNE	R1, #0x17, LOSCFREQSETH
CJNE	R0,	#0x70, LOSCFREQSETZ
;6MHZ
MOV		OSCICN, #0x82
LOSCFREQSETG:
MOV		A, OSCICN
JNB		ACC.6, LOSCFREQSETG
ANL		CLKSEL, #0xF8
CLR		C
POP		ACC
RET
LOSCFREQSETZ:
;INVALID FREQUENCY
MOV		A, #SYS_ERR_PARM_BOUNDS
SETB	C
RET
LOSCFREQSETH:
CJNE	R1, #0x0B, LOSCFREQSETJ
CJNE	R0,	#0xB8, LOSCFREQSETZ
;3MHZ
MOV		OSCICN, #0x81
LOSCFREQSETI:
MOV		A, OSCICN
JNB		ACC.6, LOSCFREQSETI
ANL		CLKSEL, #0xF8
CLR		C
POP		ACC
RET
LOSCFREQSETJ:
CJNE	R1, #0x05, LOSCFREQSETL
CJNE	R0,	#0xDC, LOSCFREQSETZ
;1.5MHZ
MOV		OSCICN, #0x80
LOSCFREQSETK:
MOV		A, OSCICN
JNB		ACC.6, LOSCFREQSETK
ANL		CLKSEL, #0xF8
CLR		C
POP		ACC
RET
LOSCFREQSETL:
CJNE	R1, #0x00, LOSCFREQSETZ
CJNE	R0,	#0x50, LOSCFREQSETN
;80KHZ
MOV		OSCLCN, #0x83
LOSCFREQSETM:
MOV		A, OSCLCN
JNB		ACC.6, LOSCFREQSETM
MOV		A, #CLKSL XOR 0xFF
ANL		A, CLKSEL
ORL		A, #0x04
MOV		CLKSEL, A
CLR		C
POP		ACC
RET
LOSCFREQSETN:
CJNE	R0,	#0x28, LOSCFREQSETP
;40KHZ
MOV		OSCLCN, #0x82
LOSCFREQSETO:
MOV		A, OSCLCN
JNB		ACC.6, LOSCFREQSETO
MOV		A, #CLKSL XOR 0xFF
ANL		A, CLKSEL
ORL		A, #0x04
MOV		CLKSEL, A
CLR		C
POP		ACC
RET
LOSCFREQSETP:
CJNE	R0,	#0x14, LOSCFREQSETR
;20KHZ
MOV		OSCLCN, #0x81
LOSCFREQSETQ:
MOV		A, OSCLCN
JNB		ACC.6, LOSCFREQSETQ
MOV		A, #CLKSL XOR 0xFF
ANL		A, CLKSEL
ORL		A, #0x04
MOV		CLKSEL, A
CLR		C
POP		ACC
RET
LOSCFREQSETR:
CJNE	R0,	#0x0A, LOSCFREQSETZ
;10KHZ
MOV		OSCLCN, #0x80
LOSCFREQSETS:
MOV		A, OSCLCN
JNB		ACC.6, LOSCFREQSETS
MOV		A, #CLKSL XOR 0xFF
ANL		A, CLKSEL
ORL		A, #0x04
MOV		CLKSEL, A
CLR		C
POP		ACC
RET

LOSCFREQTABL:
DB	0x0A
DB	0x00, 0xBB, 0x80
DB	0x00, 0x5D, 0xC0
DB	0x00, 0x2E, 0xE0
DB	0x00, 0x17, 0x70
DB	0x00, 0x0B, 0xB8
DB	0x00, 0x05, 0xDC
DB	0x00, 0x00, 0x50
DB	0x00, 0x00, 0x28
DB	0x00, 0x00, 0x14
DB	0x00, 0x00, 0x0A