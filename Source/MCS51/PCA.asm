;* Yggdrasil (TM) Core Operating System (MCS-51): Programmable Counter Array (PCA) Library
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

$INCLUDE (System.inc)

#if LIB_PCA0 == 1
  #ifndef PCA_INCLUDED
    #define PCA_INCLUDED 1

    #include "Return_Codes_PCA.inc"
    #include "Const_Math.inc"

    EXTRN CODE  (HAPCA0CLKSET)
    EXTRN CODE  (MATHDIUMUL,	MATHWIUMUL)
    EXTRN CODE  (MEMALCXRAM)
    EXTRN CODE  (PCACPHWR,	PCACPWR,	PCAPWMSIZE)

    PUBLIC  LPCADUTYSET, LPCAGETDTY, LPCAGETFRQ, LPCAINIT, LPCA0ISR, LPCAGETPER, LPCAGETPHA, LPCAGETWID

    PCA_ROUTINES  SEGMENT CODE
    RSEG  PCA_ROUTINES

    ;RETURN CODES
      PCA_RET_CHILD		EQU	000H	;SUCCESS - CHILD
      PCA_RET_PARENT		EQU	001H	;SUCCESS - PARENT
      PCA_ERR_INV_FUNC	EQU	001H	;ERROR - INVALID FUNCTION, FUNCTION NOT SUPPORTED
      PCA_ERR_NO_FREE		EQU	002H	;ERROR - NO FREE PCA SLOTS
      PCA_ERR_NO_PRONG	EQU	003H	;ERROR - PCA DOES NOT EXIST

    ;SET DUTY CYCLE
    ;ON ENTRY:
    ; A7-A4 = ARRAY NUMBER
    ; A3-A0 = MODULE NUMBER
    ; B     = DUTY CYCLE IN %
    ;ON RETURN:
    ; B = VALUE ON ENTRY
    ; C = 0 IF SUCCESS
    ;   A = 0x00
    ; C = 0 IF FAIL
    ;   A = ERROR CODE
    LPCADUTYSET PROC
        ;SAVE REGISTERS
        XCH   A, R0
        PUSH  ACC
        MOV   A, R1
        PUSH  ACC
        MOV   A, R2
        PUSH  ACC
        MOV   A, R3
        PUSH  ACC
        MOV   A, R0
        PUSH  ACC
        ;VERIFY DUTY CYCLE VALID
        MOV   A, B
        CLR   C
        SUBB  A, #0x65
        JC    LPCADUTYSETA
        ;ERROR - DUTY CYCLE GREATER THAN 100%
        MOV   R0, #PCA_ERR_INV_DUTY
        SETB  C
      ERRRET:
        POP   ACC
        POP   ACC
        MOV   R3, A
        POP   ACC
        MOV   R2, A
        POP   ACC
        MOV   R1, A
        POP   ACC
        XCH   A, R0
        RET
      LPCADUTYSETA:
        ;DUTY CYCLE VALID, GET PCA MODULE PWM RESOLUTION
        POP   ACC
        PUSH  ACC
        CALL  PCAPWMSIZE
        JC    ERRRET
        ;PCA ARRAY/MODULE VALID, CHECK FOR 16-BIT PWM MODE
        MOV   R0, A
        CJNE  R0, #0x08, LPCADUTYSETD
        ;8-BIT PCA MODE, CALCULATE PWM CAPTURE VALUE
        MOV   R0, #0x00
        MOV   A, #0x64
        CLR   C
        SUBB  A, B
        MOV   R1, A
        MOV   R2, #MATH_CONST_0_08_08_2_56_BYTE_0
        MOV   R3, #MATH_CONST_0_08_08_2_56_BYTE_1
        CALL  MATHWIUMUL
        ;LOAD VALUE INTO PCA
        MOV   A, R2
        MOV   R0, A
        POP   ACC
        CALL  PCACPHWR
        MOV   R0, A
      LPCADUTYSETC:
        ;RESTORE REGISTERS & RETURN
        POP   ACC
        MOV   R3, A
        POP   ACC
        MOV   R2, A
        POP   ACC
        MOV   R1, A
        POP   ACC
        XCH   A, R0
        RET
      LPCADUTYSETD:
        ;16-BIT PCA MODE, SAVE ADDITIONAL REGISTERS
        MOV   A, R4
        PUSH  ACC
        MOV   A, R5
        PUSH  ACC
        ;CALCULATE PWM CAPTURE VALUE
        MOV   R0, #0x00
        MOV   R1, B
        MOV   R2, #0x00
        MOV   R3, #MATH_CONST_0_16_08_655_36_BYTE_0
        MOV   R4, #MATH_CONST_0_16_08_655_36_BYTE_1
        MOV   R5, #MATH_CONST_0_16_08_655_36_BYTE_2
        CALL  MATHDIUMUL
        ;LOAD VALUE INTO PCA
        MOV   A, R4
        MOV   R0, A
        MOV   A, R5
        MOV   R1, A
        POP   ACC
        CALL  PCACPWR
        MOV   R0, A
        SJMP  LPCADUTYSETC
    ENDP


    ;MEAUSRE DUTY CYCLE
    LPCAGETDTY:
      RET					;RETURN


    ;MEASURE FREQUENCY
    LPCAGETFRQ:
      RET					;RETURN


    ;INITIALIZE PCA SYSTEM
    LPCAINIT  PROC
      PUSH  SFRPAGE
      MOV   SFRPAGE, #0x00
      ;INITIALIZE THE PCA
      ;PCA 0
      MOV   PCA0CN, #0x00
      MOV   PCA0MD, #0x08		;DISABLE WATCHDOG TIMER
      MOV   A, #0x02
      MOV   PCA0CPM0, A
      MOV   PCA0CPM1, A
      MOV   PCA0CPM2, A
      MOV   PCA0CPM3, A
      MOV   PCA0CPM4, A
      ;MOV    PCA0CPM5, #0x00
      MOV   PCA0L, #0x00
      MOV   PCA0H, #0x00
      ;PCA 1
      MOV   SFRPAGE, #0x10
      MOV   PCA0CN, #0x00
      MOV   PCA0MD, #0x08		;DISABLE WATCHDOG TIMER
      MOV   PCA0CPM0, A
      MOV   PCA0CPM1, A
      MOV   PCA0CPM2, A
      MOV   PCA0CPM3, A
      MOV   PCA0CPM4, A
      ;MOV    PCA0CPM5, A
      MOV   PCA0L, #0x00
      MOV   PCA0H, #0x00
      ;ALLOCATE RAM
      MOV   A, #010H
      CALL  MEMALCXRAM
      MOV   DPTR, #PCA_0_ADR
      MOVX  @DPTR, A
      POP   SFRPAGE
      RET					;RETURN
    ENDP
    
    
    ;SET TERMINAL COUNT FREQUENCY FOR PCA MODULE n
    ;ON ENTRY:
    ;	A = PCA MODULE NUMBER
    ;	R0 = BYTE 0 OF FREQUENCY, IN HZ
    ;	R1 = BYTE 1 OF FREQUENCY, IN HZ
    ;	R2 = BYTE 2 OF FREQUENCY, IN HZ
    ;	R3 = BYTE 3 OF FREQUENCY, IN HZ
    ;ON RETURN:
    ;	R0 = DESTROYED
    ;	R1 = DESTROYED
    ;	R2 = DESTROYED
    ;	R3 = DESTROYED
    LPCAFREQSET:
    PUSH ACC
    POP ACC
    ;SAVE R4 - R7
    MOV A, R4
    PUSH ACC
    MOV A, R5
    PUSH ACC
    MOV A, R6
    PUSH ACC
    MOV A, R7
    PUSH ACC
    ;MOVE FREQUENCY TO R4 - R7 FOR DIVISION
    MOV A, R0
    MOV R4, A
    MOV A, R1
    MOV R5, A
    MOV A, R2
    MOV R6, A
    MOV A, R3
    MOV R7, A
    ;LOAD SYSTEM CLOCK FREQUENCY
    MOV R0, #SYS_OSC_FREQ_MAX
    MOV R1, #(SYS_OSC_FREQ_MAX >> 8)
    MOV R2, #(SYS_OSC_FREQ_MAX >> 16)
    MOV R3, #(SYS_OSC_FREQ_MAX >> 24)
    ;CALCULATE PCA COUNT FOR DESIRED TERMINAL COUNT FREQUENCY
    ;CALL MATHDIUDIV
    ;CHECK FOR GREATER THAN 0xFFFF
    CLR A
    ORL A, R0
    ORL A, R1
    JZ LPCAFREQSETM
    ;SET PRESCALER TO 12
    CALL HAPCA0CLKSET
    ;DETERMINE PCA COUNT WITH PRESCALER VALUE OF 12
    MOV R4, #12
    MOV R5, #0x00
    MOV R6, #0x00
    MOV R7, #0x00
    ;CALL MATHDIUDIV
    ;CHECK FOR GREATER THAN 0xFFFF
    CLR A
    ORL A, R0
    ORL A, R1
    JZ LPCAFREQSETM
    ;RETURN WITH 'FREQUENCY TOO LOW' ERROR
    SETB C
    LPCAFREQSETL:
    ;RESTORE REGISTERS AND RETURN
    POP ACC
    MOV R7, A
    POP ACC
    MOV R6, A
    POP ACC
    MOV R5, A
    POP ACC
    MOV R4, A
    RET
    LPCAFREQSETM:
    ;LOAD PCA COUNTER WITH RESULT
    MOV PCA0L, R0
    MOV PCA0H, R1
    CLR C
    JMP LPCAFREQSETL

    ;MEASURE FREQUENCY PERIOD
    LPCAGETPER:
      RET					;RETURN

    ;MEASURE PHASE DIFFERENCE
    LPCAGETPHA:
      RET					;RETURN

    ;MEASURE PULSE WIDTH
    LPCAGETWID:
      RET					;RETURN

    ;PCA 0 ISR
    LPCA0ISR:
      RETI

    ;ENABLE PCA PWM OUTPUT ON PORT x PIN y
    LPCAPWMOUT:
      RET

    #if TARGET > 299 && TARGET < 310
      $INCLUDE (CF\30x\PCA_F30x.asm)
    #elif TARGET > 379 && TARGET < 390
      $INCLUDE (CF\38x\PCA_F38x.asm)
    #elif TARGET > 579 && TARGET < 590
      $INCLUDE (CF\58x\PCA_F58x.asm)
    #endif

    END

  #endif
#endif
