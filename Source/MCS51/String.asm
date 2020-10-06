;* Yggdrasil (TM) Core Operating System (MCS-51): String Library
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

$INCLUDE (System.inc)

#if LIB_STRING == 1
  #ifndef STRING_INCLUDED
    #define STRING_INCLUDED 1

    EXTRN CODE  (FLASHWRITE)

    STRING_ROUTINES SEGMENT CODE
    RSEG  STRING_ROUTINES

    #if STRING_BYT2HEX == 1 
      PUBLIC	LSTRBYT2HEX
      ;CONVERT VALUE TO TWO CHARACTER ASCII HEX STRING
      ;ON ENTRY:
      ; R0  = VALUE
      ;ON RETURN:
      ; R0  = ASCII CHARACTER FOR LOW NIBBLE
      ; R1  = ASCII CHARACTER FOR HIGH NIBBLE
      LSTRBYT2HEX PROC
          ;SAVE REGISTERS
          PUSH  ACC
          MOV   A, R0
          JNZ   LSTRBYT2HEXA
          MOV   R0, #0x00
          MOV   R1, #0x00
          POP   ACC
          RET
        LSTRBYT2HEXA:
          ;CONVERT LOW NIBBLE
          ;MOV		R0, A
          ANL   A, #0x0F
          ADD   A, #0x30
          PUSH  ACC
          CLR   C
          SUBB  A, #0x3A
          POP   ACC
          JC    LSTRBYT2HEXB
          ADD   A, #0x07
        LSTRBYT2HEXB:
          ;CONVERT HIGH NIBBLE
          XCH   A, R0
          SWAP  A
          ANL   A, #0x0F
          ADD   A, #0x30
          PUSH  ACC
          CLR   C
          SUBB  A, #0x3A
          POP   ACC
          JC    LSTRBYT2HEXC
          ADD   A, #0x07
        LSTRBYT2HEXC:
          MOV   R1, A
          ;RESTORE REGISTERS & RETURN
          POP   ACC
          RET
      ENDP
    #endif

    #if STRING_BYT2STR == 1 
      PUBLIC  LSTRBYT2STR
      ;CONVERT UNSIGNED INTEGER VALUE (0 - 255) TO ASCII STRING
      ;ON ENTRY:
      ; R0  = VALUE
      ;ON RETURN:
      ; R0  = ONES CHARACTER
      ; R1  = TENS CHARCTER
      ; R2  = HUNDREDS CHARACTER
      LSTRBYT2STR	PROC
          PUSH  ACC
          MOV   A, R0
          ;CHECK FOR ZERO
          JNZ   LSTRBYT2STRA
          ;LOAD ZERO RESULT
          MOV   R0, #0x30
          MOV   R1, #0x30
          MOV   R2, #0x30
          ;RESTORE REGISTERS & RETURN
          POP   ACC
          RET
        LSTRBYT2STRA:
          PUSH  B
          ;CALCULATE ONES
          MOV   B, #10
          DIV   AB
          XCH   A, B
          ADD   A, #0x30
          MOV   R0, A
          XCH   A, B
          ;CALCULATE TENS
          MOV   B, #10
          DIV   AB
          XCH   A, B
          ADD   A, #0x30
          MOV   R1, A
          XCH   A, B
          ;CALCULATE HUNDREDS
          MOV   B, #10
          DIV   AB
          XCH   A, B
          ADD   A, #0x30
          MOV   R2, A
          ;RESTORE REGISTERS & RETURN
          POP   B
          POP   ACC
          RET
      ENDP
    #endif

    #if STRING_CMP == 1
      PUBLIC  LSTRCMP
      ;COMPARE TWO STRINGS IN RAM TERMINATED WITH SPECIFIED CHARACTER
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; DPTR  = POINTER TO STRING A
      ; R0    = LSB OF POINTER TO STRING B
      ; R1    = MSB OF POINTER TO STRING B
      ; EMI0CF MUST BE CONFIGURED TO POINT TO THE CORRECT MEMORY SPACE
      ;ON RETURN:
      ; CF  = 0 IF STRINGS EQUAL
      ;   DPTR  = POINTER TO END OF STRING A
      ;   R0    = LSB OF END OF STRING B
      ;   R1    = MSB OF END OF STRING B
      ; CF  = 1 IF STRINGS NOT EQUAL
      ;   DPTR  = POINTER TO UNQUAL CHARACTER IN STRING A
      ;   R0    = LSB OF POINTER TO UNEQUAL CHARACTER IN STRING B
      ;   R1    = MSB OF POINTER TO UNEQUAL CHARACTER IN STRING B
      LSTRCMP PROC
          ;SAVE REGISTERS
          XCH   A, R2
          PUSH  ACC
        LSTRCMPA:
          ;COMPARE CHARACTER FROM STRING A WITH CHARACTER FROM STRING B
          MOVX  A, @DPTR				;LOAD CHARACTER FROM STRING A
          MOV   B, A
          MOVX  A, @R0					;LOAD CHARACTER FROM STRING B
          XRL   A, B						;COMPARE CHARACTERS
          JZ    LSTRCMPB				;JUMP IF EQUAL
          ;RESTORE REGISTERS, RETURN WITH NOT EQUAL
          POP   ACC
          XCH   A, R2
          SETB  C								;SET C TO INDICATE NOT EQUAL
          RET
        LSTRCMPB:
          ;CHECK FOR TERMINATOR CHARACTER
          MOVX  A, @R0
          XRL   A, R2
          JZ    LSTRCMPC				;JUMP IF TERMINATOR CHARACTER FOUND
          ;SET UP FOR NEXT COMPARE
          INC   DPTR						;POINT TO NEXT CHARACTER OF STRING A
          MOV   A, R0						;POINT TO NEXT CHARACTER OF STRING B
          ADD   A, #0x01
          MOV   R0, A
          CLR   A
          ADDC  A, EMI0CN
          MOV   EMI0CN, A
          JMP   LSTRCMPA				;PROCESS NEXT CHARACTER
        LSTRCMPC:
          ;RESTORE REGISTERS, RETURN WITH EQUAL
          MOV   R0, EMI0CN
          MOV   R1, A
          POP   ACC
          MOV   R2, A
          CLR   C
          RET
      ENDP
    #endif

    #if STRING_CMPFF == 1 || STRING_CMPFFZT == 1
      PUBLIC  LSTRCMPFF
      ;COMPARE TWO STRINGS IN FLASH TERMINATED WITH SPECIFIED CHARACTER
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; DPTR  = POINTER TO STRING A
      ; R0    = LSB OF POINTER TO STRING B
      ; R1    = MSB OF POINTER TO STRING B
      ;ON RETURN:
      ; A   = VALUE ON ENTRY
      ; CF  = 0 IF STRINGS EQUAL
      ;   DPTR  = POINTER TO TERMINATOR CHARACTER IN STRING A
      ;   R0    = LSB OF POINTER TO TERMINATOR CHARACTER IN STRING B
      ;   R1    = MSB OF POINTER TO TERMINATOR CHARACTER IN STRING B
      ; CF  = 1 IF STRINGS NOT EQUAL
      ;   DPTR  = POINTER TO UNQUAL CHARACTER IN STRING A
      ;   R0    = LSB OF POINTER TO UNEQUAL CHARACTER IN STRING B
      ;   R1    = MSB OF POINTER TO UNEQUAL CHARACTER IN STRING B
      LSTRCMPFF	PROC
          ;SAVE REGISTERS
          PUSH  B
          XCH   A, R2
          PUSH  ACC
        LSTRCMPFFA:
          ;LOAD CHARACTER FROM STRING A
          CLR   A
          MOVC  A, @A+DPTR
          MOV   B, A
          ;SWAP STRING POINTERS
          CALL  DPTRXCHG01
          ;LOAD CHARACTER FROM STRING B
          CLR   A
          MOVC  A, @A+DPTR
          ;COMPARE CHARACTERS
          XRL   A, B
          JZ    LSTRCMPFFC
          ;CHARACTERS UNEQUAL
          SETB  C
        LSTRCMPFFB:
          ;SWAP STRING POINTERS
          CALL  DPTRXCHG01
          ;RESTORE REGISTERS & RETURN
          POP   ACC
          XCH   A, R2
          POP   B
          RET
        LSTRCMPFFC:
          ;CHECK FOR TERMINATOR
          CLR   A
          MOVC  A, @A+DPTR
          XRL   A, R2
          JNZ   LSTRCMPFFD
          ;TERMINATOR FOUND
          CLR   C
          SJMP  LSTRCMPFFB
        LSTRCMPFFD:
          ;POINT TO NEXT CHARACTER IN STRING B
          INC   DPTR
          ;SWAP STRING POINTERS
          CALL  DPTRXCHG01
          ;POINT TO NEXT CHARACTER IN STRING A
          INC   DPTR
          ;PROCESS NEXT CHARACTER
          JMP   LSTRCMPFFA
      ENDP
    #endif

    #if STRING_CMPFFZT == 1
      PUBLIC  LSTRCMPFFZT
      ;COMPARE A ZERO-TERMINATED STRING IN FLASH WITH A ZERO-TERMINATED STRING IN FLASH
      ;ON ENTRY:
      ; DPTR  = POINTER TO STRING IN FLASH
      ; R0    = LSB OF POINTER TO STRING IN XRAM
      ; R1    = MSB OF POINTER TO STRING IN XRAM
      ;ON RETURN:
      ; CF  = 0 IF STRINGS EQUAL
      ;   DPTR  = POINTER TO TERMINATOR CHARACTER IN FLASH
      ;   R0    = LSB OF POINTER TO TERMINATOR CHARACTER IN XRAM
      ;   R1    = MSB OF POINTER TO TERMINATOR CHARACTER IN XRAM
      ; CF  = 1 IF STRINGS NOT EQUAL
      ;   DPTR  = POINTER TO UNQUAL CHARACTER IN FLASH STRING
      ;   R0    = LSB OF POINTER TO UNEQUAL CHARACTER IN XRAM STRING
      ;   R1    = MSB OF POINTER TO UNEQUAL CHARACTER IN XRAM STRING
      LSTRCMPFFZT PROC
        PUSH  ACC
        CLR   A
        CALL  LSTRCMPFF
        POP   ACC
        RET
      ENDP
    #endif


    #if STRING_CMPFX == 1
      PUBLIC  LSTRCMPFX
      ;COMPARE A STRING IN XRAM WITH A STRING IN FLASH TERMINATED WITH SPECIFIED
      ;CHARACTER
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; DPTR  = POINTER TO STRING IN FLASH
      ; R0    = LSB OF POINTER TO STRING IN XRAM
      ; R1    = MSB OF POINTER TO STRING IN XRAM
      ;ON RETURN:
      ; CF  = 0 IF STRINGS EQUAL
      ;   DPTR  = POINTER TO TERMINATOR CHARACTER IN FLASH
      ;   R0    = LSB OF POINTER TO TERMINATOR CHARACTER IN XRAM
      ;   R1    = MSB OF POINTER TO TERMINATOR CHARACTER IN XRAM
      ; CF  = 1 IF STRINGS NOT EQUAL
      ;   DPTR  = POINTER TO UNQUAL CHARACTER IN FLASH STRING
      ;   R0    = LSB OF POINTER TO UNEQUAL CHARACTER IN XRAM STRING
      ;   R1    = MSB OF POINTER TO UNEQUAL CHARACTER IN XRAM STRING
      LSTRCMPFX	PROC
          ;SAVE REGISTERS
          PUSH  B
          XCH   A, R2
          PUSH  ACC
        LSTRCMPFXA:
          ;LOAD CHARACTER FROM FLASH
          CLR   A
          MOVC  A, @A+DPTR
          MOV   B, A
          ;SWAP STRING POINTERS
          CALL  DPTRXCHG01
          ;LOAD CHARACTER FROM XRAM
          MOVX  A, @DPTR
          ;COMPARE CHARACTERS FOR EQUAL
          XRL   A, B
          JZ    LSTRCMPFXC
          ;SET C TO INDICATE NOT EQUAL
          SETB  C
        LSTRCMPFXB:
          ;RESTORE REGISTERS & RETURN
          CALL  DPTRXCHG01
          POP   ACC
          XCH   A, R2
          POP   B
          RET
        LSTRCMPFXC:
          ;CHECK FOR TERMINATOR
          MOVX  A, @DPTR
          XRL   A, R2
          JZ    LSTRCMPFXD
          ;TERMINATOR NOT FOUND - PROCESS NEXT CHARACTERS
          INC   DPTR
          CALL  DPTRXCHG01
          INC   DPTR
          JMP   LSTRCMPFXA
        LSTRCMPFXD:
          ;CLEAR C TO INDICATE EQUAL
          CLR   C
          SJMP  LSTRCMPFXB
      ENDP
    #endif


    #if STRING_CMPFXZT == 1 
      PUBLIC  LSTRCMPFXZT
      ;COMPARE A ZERO-TERMINATED STRING IN XRAM WITH A ZERO-TERMINATED STRING IN FLASH
      ;ON ENTRY:
      ; DPTR  = POINTER TO STRING IN FLASH
      ; R0    = LSB OF POINTER TO STRING IN XRAM
      ; R1    = MSB OF POINTER TO STRING IN XRAM
      ;ON RETURN:
      ; CF  = 0 IF STRINGS EQUAL
      ;   DPTR  = POINTER TO TERMINATOR CHARACTER IN FLASH
      ;   R0    = LSB OF POINTER TO TERMINATOR CHARACTER IN XRAM
      ;   R1    = MSB OF POINTER TO TERMINATOR CHARACTER IN XRAM
      ; CF  = 1 IF STRINGS NOT EQUAL
      ;   DPTR  = POINTER TO UNQUAL CHARACTER IN FLASH STRING
      ;   R0    = LSB OF POINTER TO UNEQUAL CHARACTER IN XRAM STRING
      ;   R1    = MSB OF POINTER TO UNEQUAL CHARACTER IN XRAM STRING
      LSTRCMPFXZT PROC
        PUSH  ACC
        CLR   A
        CALL  LSTRCMPFX
        POP   ACC
        RET
      ENDP
    #endif


    #if STRING_CMPZT == 1 
      PUBLIC  LSTRCMPZT
      ;COMPARE TWO ZERO-TERMINATED STRINGS IN RAM
      ;ON ENTRY:
      ; DPTR  = POINTER TO STRING A
      ; R0    = LSB OF POINTER TO STRING B
      ; R1    = MSB OF POINTER TO STRING B
      ; EMI0CF MUST BE CONFIGURED TO POINT TO THE CORRECT MEMORY SPACE
      ;ON RETURN:
      ; CF  = 0 IF STRINGS EQUAL
      ;   DPTR  = POINTER TO END OF STRING A
      ;   R0    = LSB OF END OF STRING B
      ;   R1    = MSB OF END OF STRING B
      ; CF  = 1 IF STRINGS NOT EQUAL
      ;   DPTR  = POINTER TO UNQUAL CHARACTER IN STRING A
      ;   R0    = LSB OF POINTER TO UNEQUAL CHARACTER IN STRING B
      ;   R1    = MSB OF POINTER TO UNEQUAL CHARACTER IN STRING B
      LSTRCMPZT PROC
        PUSH  ACC
        CLR   A
        CALL  LSTRCMP
        POP   ACC
        RET
      ENDP
    #endif


    #if STRING_CPY == 1 || STRING_CPYZT == 1
      PUBLIC  LSTRCPY
      ;COPY A CHARACTER TEMINATED STRING FROM [DPTR] TO [R1:R0]
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; DPTR  = POINTER TO STRING A
      ; R0    = LSB OF POINTER TO STRING B
      ; R1    = MSB OF POINTER TO STRING B
      ; EMI0CF MUST BE CONFIGURED TO POINT TO THE CORRECT MEMORY SPACE
      ;ON RETURN:
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTRCPY PROC
          ;SAVE REGISTERS
          PUSH  B
          MOV   B, A
          MOV   A, R0
          PUSH  ACC
          MOV   A, R1
          PUSH  ACC
          PUSH  DPL
          PUSH  DPH
        LSTRCPYA:
          ;CHECK FOR TERMINATOR
          MOVX  A, @DPTR
          CJNE  A, B, LSTRCPYB
          ;RESTORE REGISTERS & RETURN
          POP   DPH
          POP   DPL
          POP   ACC
          MOV   R1, A
          POP   ACC
          MOV   R0, A
          POP   B
          RET
        LSTRCPYB:
          ;COPY BYTE
          CALL  DPTRXCHG01
          MOVX  @DPTR, A
          INC   DPTR
          CALL  DPTRXCHG01
          INC   DPTR
          SJMP  LSTRCPYA
      ENDP
    #endif

    #if STRING_CPYZT == 1 
      PUBLIC  LSTRCPYZT
      ;COPY A ZERO-TEMINATED STRING FROM [DPTR] TO [R1:R0]
      ;ON ENTRY:
      ; DPTR  = POINTER TO STRING A
      ; R0    = LSB OF POINTER TO STRING B
      ; R1    = MSB OF POINTER TO STRING B
      ; EMI0CF MUST BE CONFIGURED TO POINT TO THE CORRECT MEMORY SPACE
      ;ON RETURN:
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTRCPYZT PROC
        ;SAVE REGISTERS
        PUSH  ACC
        CLR   A
        CALL  LSTRCPY
        POP   ACC
        RET
      ENDP
    #endif

    #if STRING_CPYFF == 1 || STRING_CPYFFZT == 1
      PUBLIC  LSTRCPYFF
      ;COPY A CHARACTER TEMINATED STRING FROM FLASH:[DPTR] TO FLASH:[R1:R0]
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; DPTR  = POINTER TO STRING A
      ; R0    = LSB OF POINTER TO STRING B
      ; R1    = MSB OF POINTER TO STRING B
      ; EMI0CF MUST BE CONFIGURED TO POINT TO THE CORRECT MEMORY SPACE
      ;ON RETURN:
      ; C     = 0 IF SUCCESS
      ; C     = 1 IF ERROR
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTRCPYFF PROC
          ;SAVE REGISTERS
          PUSH  B
          MOV   B, A
          MOV   A, R0
          PUSH  ACC
          MOV   A, R1
          PUSH  ACC
          MOV   A, R2
          PUSH  ACC
          PUSH  DPL
          PUSH  DPH
          ;SETUP
          MOV   R2, #0x00
        LSTRCPYFFA:
          ;CHECK FOR TERMINATOR
          MOV   A, R2
          MOVC  A, @A+DPTR
          CJNE  A, B, LSTRCPYFFC
          ;STORE TERMINATOR
          MOV   A, B
          CALL  DPTRXCHG01
          CALL  FLASHWRITE
        LSTRCPYFFB:
          ;RESTORE REGISTERS & RETURN
          POP   DPH
          POP   DPL
          POP   ACC
          MOV   R2, A
          POP   ACC
          MOV   R1, A
          POP   ACC
          MOV   R0, A
          POP   B
          RET
        LSTRCPYFFC:
          ;POINT TO NEXT SOURCE ADDRESS
          INC   DPTR
          ;WRITE BYTE TO FLASH
          CALL  DPTRXCHG01
          CALL  FLASHWRITE
          JC    LSTRCPYFFB
          ;POINT TO NEXT DESTINATION ADDRESS
          INC   DPTR
          CALL  DPTRXCHG01
          SJMP  LSTRCPYFFA
      ENDP
    #endif


    #if STRING_CPYFFZT == 1 
      PUBLIC  LSTRCPYFFZT
      ;COPY A ZERO-TEMINATED STRING FROM [DPTR] TO [R1:R0]
      ;ON ENTRY:
      ; DPTR  = POINTER TO STRING A
      ; R0    = LSB OF POINTER TO STRING B
      ; R1    = MSB OF POINTER TO STRING B
      ; EMI0CF MUST BE CONFIGURED TO POINT TO THE CORRECT MEMORY SPACE
      ;ON RETURN:
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTRCPYFFZT PROC
        ;SAVE REGISTERS
        PUSH  ACC
        CLR   A
        CALL  LSTRCPYFF
        POP   ACC
        RET
      ENDP
    #endif

    #if STRING_CPYFM == 1 || STRING_CPYFMZT == 1
      PUBLIC  LSTRCPYFM
      ;COPY A CHARACTER TEMINATED STRING FROM FLASH:[DPTR] TO FLASH:[R1:R0]
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; DPTR  = POINTER TO STRING A
      ; R0    = LSB OF POINTER TO STRING B
      ; R1    = MSB OF POINTER TO STRING B
      ; EMI0CF MUST BE CONFIGURED TO POINT TO THE CORRECT MEMORY SPACE
      ;ON RETURN:
      ; C     = 0 IF SUCCESS
      ; C     = 1 IF ERROR
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTRCPYFM PROC
      ;SAVE REGISTERS
        PUSH  B
        MOV   B, A
        MOV   A, R0
        PUSH  ACC
        MOV   A, R1
        PUSH  ACC
        MOV   A, R2
        PUSH  ACC
        PUSH  DPL
        PUSH  DPH
      LSTRCPYFMA:
        ;CHECK FOR TERMINATOR
        ;MOV  A, R2
        MOVX  A, @DPTR
        CJNE  A, B, LSTRCPYFMC
        ;STORE TERMINATOR
        MOV   A, B
        CALL  DPTRXCHG01
        CALL  FLASHWRITE
      LSTRCPYFMB:
        ;RESTORE REGISTERS & RETURN
        POP   DPH
        POP   DPL
        POP   ACC
        MOV   R2, A
        POP   ACC
        MOV   R1, A
        POP   ACC
        MOV   R0, A
        POP   B
        RET
      LSTRCPYFMC:
        ;POINT TO NEXT SOURCE ADDRESS
        INC   DPTR
        ;WRITE BYTE TO FLASH
        CALL  DPTRXCHG01
        CALL  FLASHWRITE
        JC    LSTRCPYFMB
        ;POINT TO NEXT DESTINATION ADDRESS
        INC   DPTR
        CALL  DPTRXCHG01
        SJMP  LSTRCPYFMA
      ENDP
    #endif

    #if STRING_CPYFMZT == 1 
      PUBLIC  LSTRCPYFMZT
      ;COPY A ZERO-TEMINATED STRING FROM [DPTR] TO [R1:R0]
      ;ON ENTRY:
      ; DPTR  = POINTER TO STRING A
      ; R0    = LSB OF POINTER TO STRING B
      ; R1    = MSB OF POINTER TO STRING B
      ; EMI0CF MUST BE CONFIGURED TO POINT TO THE CORRECT MEMORY SPACE
      ;ON RETURN:
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTRCPYFMZT PROC
        ;SAVE REGISTERS
        PUSH  ACC
        CLR   A
        CALL  LSTRCPYFM
        POP   ACC
        RET
      ENDP
    #endif

    #if STRING_CPYMF == 1 || STRING_CPYMFZT == 1
      PUBLIC  LSTRCPYMF
      ;COPY A CHARACTER TEMINATED STRING FROM FLASH:[DPTR] TO FLASH:[R1:R0]
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; DPTR  = POINTER TO STRING A
      ; R0    = LSB OF POINTER TO STRING B
      ; R1    = MSB OF POINTER TO STRING B
      ; EMI0CF MUST BE CONFIGURED TO POINT TO THE CORRECT MEMORY SPACE
      ;ON RETURN:
      ; CF    = 0 IF SUCCESS
      ; CF    = 1 IF ERROR
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTRCPYMF PROC
          ;SAVE REGISTERS
          PUSH  B
          MOV   B, A
          MOV   A, R0
          PUSH  ACC
          MOV   A, R1
          PUSH  ACC
          MOV   A, R2
          PUSH  ACC
          PUSH  DPL
          PUSH  DPH
          ;SETUP
          MOV   R2, #0x00
        LSTRCPYMFA:
          ;CHECK FOR TERMINATOR
          MOV   A, R2
          MOVC  A, @A+DPTR
          CJNE  A, B, LSTRCPYMFC
          ;STORE TERMINATOR
          MOV   A, B
          CALL  DPTRXCHG01
          MOVX  @DPTR, A
        LSTRCPYMFB:
          ;RESTORE REGISTERS & RETURN
          POP   DPH
          POP   DPL
          POP   ACC
          MOV   R2, A
          POP   ACC
          MOV   R1, A
          POP   ACC
          MOV   R0, A
          POP   B
          RET
        LSTRCPYMFC:
          ;POINT TO NEXT SOURCE ADDRESS
          INC   DPTR
          ;WRITE BYTE TO FLASH
          CALL  DPTRXCHG01
          MOVX  @DPTR, A
          JC    LSTRCPYMFB
          ;POINT TO NEXT DESTINATION ADDRESS
          INC   DPTR
          CALL  DPTRXCHG01
          SJMP  LSTRCPYMFA
      ENDP
    #endif

    #if STRING_CPYMFZT == 1 
      PUBLIC  LSTRCPYMFZT
      ;COPY A ZERO-TEMINATED STRING FROM [DPTR] TO [R1:R0]
      ;ON ENTRY:
      ; DPTR  = POINTER TO STRING A
      ; R0    = LSB OF POINTER TO STRING B
      ; R1    = MSB OF POINTER TO STRING B
      ; EMI0CF MUST BE CONFIGURED TO POINT TO THE CORRECT MEMORY SPACE
      ;ON RETURN:
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTRCPYMFZT PROC
        ;SAVE REGISTERS
        PUSH  ACC
        CLR   A
        CALL  LSTRCPYMF
        POP   ACC
        RET
      ENDP
    #endif

    #if STRING_EXTRACT == 1 || STRING_EXTRACTZT == 1 
      PUBLIC  LSTRXTRACT
      ;EXTRACT SUBSTRINGS FROM STRING
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; B     = DELIMITER
      ; DPTR  = POINTER TO STRING
      ; R0    = LSB OF POINTER TO DESTINATION A
      ; R1    = MSB OF POINTER TO DESTINATION A
      ; R2    = LSB OF POINTER TO DESTINATION B
      ; R3    = MSB OF POINTER TO DESTINATION B
      ;ON RETURN:
      ; B     = VALUE ON ENTRY
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      ; R2    = VALUE ON ENTRY
      ; R3    = VALUE ON ENTRY
      ; CF  = 0 IF SUBSTRING FOUND
      ;   A = SUBSTRING COUNT
      ; CF  = 1 IF SUBSTRING NOT FOUND
      ;   A = 0x00
      LSTRXTRACT  PROC
          ;SAVE REGISTERS
          XCH   A, R6
          PUSH  ACC
          MOV   A, R5
          PUSH  ACC
          MOV   A, R0
          PUSH  ACC
          MOV   A, R1
          PUSH  ACC
          MOV   A, R2
          PUSH  ACC
          MOV   A, R3
          PUSH  ACC
          MOV   A, R4
          PUSH  ACC
          PUSH  DPL
          PUSH  DPH
          PUSH  PSW
          ;CLEAR DELIMITER FOUND & SUBSTRING FOUND FLAGS
          ;ZEROIZE SUBSTRING COUNTER
          CLR   F0
          CLR   F1
          MOV   R5, #0x00
        LSTRXTRACTA:
          ;CHECK FOR TERMINATOR
          MOVX  A, @DPTR
          MOV   R4, A
          XRL   A, R6
          JZ    LSTRXTRACTF
          ;CHECK FOR DELIMITER
          MOV   A, R4
          CJNE  A, B, LSTRXTRACTD
          ;IS DELIMITER
          JNB   F0, LSTRXTRACTC
          ;DELIMITER FOUND
          JNB   F1, LSTRXTRACTC
          ;STORE TERMINATOR
          CLR   F0
          CLR   F1
          INC   R5
          CLR   A
        LSTRXTRACTB:
          XCH   A, R2
          XCH   A, DPL
          XCH   A, R2
          XCH   A, R3
          XCH   A, DPH
          XCH   A, R3
          MOVX  @DPTR, A
          ;POINT TO NEXT DESTINATION B ADDRESS
          INC   DPTR
          XCH   A, R2
          XCH   A, DPL
          XCH   A, R2
          XCH   A, R3
          XCH   A, DPH
          XCH   A, R3
          ;POINT TO NEXT SOURCE ADDRESS
          INC   DPTR
          JMP   LSTRXTRACTA
        LSTRXTRACTC:
          ;COMPLIMENT DELIMITER FOUND FLAG
          CPL   F0
          ;POINT TO NEXT SOURCE ADDRESS
          INC   DPTR
          JMP   LSTRXTRACTA
        LSTRXTRACTD:
          ;NOT DELIMITER
          JB    F0, LSTRXTRACTE
          ;NO DELIMITER FOUND
          CALL  DPTRXCHG01
          MOVX  @DPTR, A
          ;POINT TO NEXT DESTINATION A ADDRESS
          INC   DPTR
          CALL  DPTRXCHG01
          ;POINT TO NEXT SOURCE ADDRESS
          INC   DPTR
          JMP   LSTRXTRACTA
        LSTRXTRACTE:
          SETB  F1
          JMP   LSTRXTRACTB
        LSTRXTRACTF:
          ;STORE TERMINATOR IN DESTINATION STRING A
          MOV   DPL, R0
          MOV   DPH, R1
          MOV   A, R6
          MOVX  @DPTR, A
          ;
          SETB  C
          MOV   A, R5
          JZ    LSTRXTRACTG
          ;STORE TERMINATOR IN DESTINATION STRING B
          MOV   DPL, R2
          MOV   DPH, R3
          MOV   A, R6
          MOVX  @DPTR, A
        LSTRXTRACTG:
          ;RETURN
          POP   PSW
          POP   DPH
          POP   DPL
          POP   ACC
          MOV   R4, A
          POP   ACC
          MOV   R3, A
          POP   ACC
          MOV   R2, A
          POP   ACC
          MOV   R1, A
          POP   ACC
          MOV   R0, A
          POP   ACC
          XCH   A, R5
          MOV   R6, A
          POP   ACC
          XCH   A, R6
          RET
      ENDP
    #endif

    #if STRING_EXTRACTZT == 1 
      PUBLIC  LSTRXTRACTZT
      ;EXTRACT SUBSTRINGS FROM A ZERO-TERMINATED STRING
      ;ON ENTRY:
      ; A     = DELIMITER
      ; DPTR  = POINTER TO STRING
      ; R0    = LSB OF POINTER TO DESTINATION A
      ; R1    = MSB OF POINTER TO DESTINATION A
      ; R2    = LSB OF POINTER TO DESTINATION B
      ; R3    = MSB OF POINTER TO DESTINATION B
      ;ON RETURN:
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      ; R2    = VALUE ON ENTRY
      ; R3    = VALUE ON ENTRY
      ; CF  = 0 IF SUBSTRING FOUND
      ;   A = SUBSTRING COUNT
      ; CF  = 1 IF SUBSTRING NOT FOUND
      ;   A = 0x00
      LSTRXTRACTZT  PROC
        ;SAVE REGISTERS
        PUSH  B
        MOV   B, A
        CLR   A
        CALL  LSTRXTRACT
        POP   B
        RET
      ENDP
    #endif


    #if STRING_FINDCHR == 1
      EXTRN CODE  (DPTRDEC)
      PUBLIC      LSTRFINDCHR
      ;SCAN A STRING IN RAM FOR THE INDICATED CHARACTER AT THE SPECIFIED COUNT
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; B     = CHARACTER
      ; R2    = INDEX
      ; DPTR  = POINTER TO STRING
      ; EMI0CF MUST BE CONFIGURED TO POINT TO THE CORRECT MEMORY SPACE
      ;ON RETURN:
      ; CF  = 0 IF CHARACTER FOUND
      ;   A     = VALUE ON ENTRY
      ;   R2    = VALUE ON ENTRY
      ;   DPTR  = POINTER TO ADDRESS CHARACTER FOUND AT
      ; CF  = 1 IF CHARACTER NOT FOUND
      ;   A     = VALUE ON ENTRY
      ;   R2    = INDEX OF LAST CHARACTER FOUND
      ;   DPTR  = VALUE ON ENTRY
      LSTRFINDCHR PROC
          ;CHECK FOR ZERO INDEX
          XCH   A, R2
          JZ    LSTRFINDCHRD
          XCH   A, R2
          ;SAVE REGISTERS
          PUSH  B
          PUSH  DPL
          PUSH  DPH
          XCH   A, R0
          PUSH  ACC
          MOV   A, R2
          PUSH  ACC
          ;SET UP DPTR
          CALL  DPTRDEC
        LSTRFINDCHRA:
          ;POINT TO NEXT ADDRESS
          INC   DPTR
          ;CHECK FOR TERMINATOR
          MOVX  A, @DPTR
          XRL   A, R0
          JZ    LSTRFINDCHRC
          ;CHECK FOR CHARACTER
          MOVX  A, @DPTR
          CJNE  A, B, LSTRFINDCHRA
          ;DECREMENT INDEX
          DJNZ  R2, LSTRFINDCHRA
          ;RESTORE REGISTERS AND RETURN
          POP   ACC
          MOV   R2, A
        LSTRFINDCHRB:
          POP   ACC
          MOV   R0, A
          POP   DPH
          POP   DPL
          POP   B
          RET
        LSTRFINDCHRC:
          ;CALCULATE INDEX RETURN VALUE
          POP   ACC
          XCH   A, R2
          CLR   C
          SUBB  A, R2
          SETB  C
          JMP   LSTRFINDCHRB
        LSTRFINDCHRD:
          ;RETURN
          XCH   A, R2
          SETB  C
          RET
      ENDP
    #endif


    #if STRING_FINDCHRZT == 1 
      PUBLIC  LSTRFINDCHRZT
      ;SCAN A STRING IN RAM FOR THE INDICATED CHARACTER AT THE SPECIFIED COUNT
      ;ON ENTRY:
      ; A     = CHARACTER
      ; R2    = INDEX
      ; DPTR  = POINTER TO STRING
      ; EMI0CF MUST BE CONFIGURED TO POINT TO THE CORRECT MEMORY SPACE
      ;ON RETURN:
      ; CF  = 0 IF STRINGS EQUAL
      ;   DPTR  = POINTER TO END OF STRING A
      ;   R0    = LSB OF END OF STRING B
      ;   R1    = MSB OF END OF STRING B
      ; CF  = 1 IF STRINGS NOT EQUAL
      ;   DPTR  = POINTER TO UNQUAL CHARACTER IN STRING A
      ;   R0    = LSB OF POINTER TO UNEQUAL CHARACTER IN STRING B
      ;   R1    = MSB OF POINTER TO UNEQUAL CHARACTER IN STRING B
      LSTRFINDCHRZT PROC
        ;SAVE REGISTERS
        PUSH  ACC
        PUSH  B
        MOV   B, A
        CLR   A
        CALL  LSTRCMP
        POP   B
        POP   ACC
        RET
      ENDP
    #endif

    #if STRING_LENGTH == 1 
      PUBLIC  LSTRLEN
      ;RETURNS THE LENGTH OF A STRING
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; DPTR  = POINTER TO STRING
      ;ON RETURN:
      ; DPTR  = VALUE ON ENTRY
      ; C     = 0 IF STRING < 256 CHARACTERS
      ;   A = STRING LENGTH
      ; C     = 1 IF STRING >= 256 CHARACTERS
      ;   A = 0x00
      LSTRLEN PROC
          ;SAVE REGISTERS
          XCH   A, B
          PUSH  ACC
          MOV   A, R0
          PUSH  ACC
          PUSH  DPL
          PUSH  DPH
          ;SETUP
          MOV   R0, #0x00
        LSTRLENA:
          ;CHECK FOR TERMINATOR
          MOVX  A, @DPTR
          XRL   A, B
          JNZ   LSTRLENC
          CLR   C
        LSTRLENB:
          ;RESTORE REGISTERS & RETURN
          POP   DPH
          POP   DPL
          POP   ACC
          XCH   A, R0
          POP   B
          RET
        LSTRLENC:
          ;INCREMENT SOURCE ADDRESS
          INC   DPTR
          ;INCREMENT LENGTH & CHECK FOR OVERFLOW
          MOV   A, R0
          INC   A
          MOV   R0, A
          JNZ   LSTRLENA
          SETB  C
          JMP   LSTRLENB
      ENDP
    #endif

    #if STRING_FRACBYT2STR == 1 
      PUBLIC  LSTRFRACBYT2STR
      ;CONVERT UNSIGNED FRACTIONAL VALUE (0 - 0.00390625) TO ASCII STRING
      ;ON ENTRY:
      ; R0  = VALUE
      ;ON RETURN:
      ; R0  = HUNDRED MILLTIONTHS CHARACTER
      ; R1  = TEN MILLIONTHS CHARCTER
      ; R2  = MILLIONTHS CHARACTER
      ; R3  = HUNDRED THOUSAHDTHS CHARACTER
      ; R4  = TEN THOUSANDTHS CHARACTER
      ; R5  = THOUSANDTHS CHARCTER
      ; R6  = HUNDREDTHS CHARACTER
      ; R7  = TENTHS CHARACTER
      LSTRFRACBYT2STR PROC
          PUSH  ACC
          MOV   A, R0
          PUSH  B
          ;LOAD ZERO RESULT
          MOV   R0, #0x30
          MOV   R1, #0x30
          MOV   R2, #0x30
          MOV   R3, #0x30
          MOV   R4, #0x30
          MOV   R5, #0x30
          MOV   R6, #0x30
          MOV   R7, #0x30
          ;CHECK FOR ZERO
          JNZ   LSTRFRACBYT2STRB
        LSTRFRACBYT2STRA:
          ;RESTORE REGISTERS & RETURN
          POP   B
          POP   ACC
          RET
          LSTRFRACBYT2STRB:
          ;CALCULATE TENTHS
          MOV   B, #10
          MUL   AB
          XCH   A, B
          ADD   A, #0x30
          MOV   R7, A
          XCH   A, B
          ;CHECK FOR ZERO
          JZ    LSTRFRACBYT2STRA
          ;CALCULATE HUNDRETHS
          MOV   B, #10
          MUL   AB
          XCH   A, B
          ADD   A, #0x30
          MOV   R6, A
          XCH   A, B
          ;CHECK FOR ZERO
          JZ    LSTRFRACBYT2STRA
          ;CALCULATE THOUSANDTHS
          MOV   B, #10
          MUL   AB
          XCH   A, B
          ADD   A, #0x30
          MOV   R5, A
          XCH   A, B
          ;CHECK FOR ZERO
          JZ    LSTRFRACBYT2STRA
          ;CALCULATE TEN THOUSANDTHS
          MOV   B, #10
          MUL   AB
          XCH   A, B
          ADD   A, #0x30
          MOV   R4, A
          XCH   A, B
          ;CHECK FOR ZERO
          JZ    LSTRFRACBYT2STRA
          ;CALCULATE HUNDRED THOUSANDTHS
          MOV   B, #10
          MUL   AB
          XCH   A, B
          ADD   A, #0x30
          MOV   R3, A
          XCH   A, B
          ;CHECK FOR ZERO
          JZ    LSTRFRACBYT2STRA
          ;CALCULATE MILLIONTHS
          MOV   B, #10
          MUL   AB
          XCH   A, B
          ADD   A, #0x30
          MOV   R2, A
          XCH   A, B
          ;CHECK FOR ZERO
          JZ    LSTRFRACBYT2STRA
          ;CALCULATE TEN MILLIONTHS
          MOV   B, #10
          MUL   AB
          XCH   A, B
          ADD   A, #0x30
          MOV   R1, A
          XCH   A, B
          ;CHECK FOR ZERO
          JZ    LSTRFRACBYT2STRA
          ;CALCULATE HUNDRED MILLIONTHS
          MOV   B, #10
          MUL   AB
          XCH   A, B
          ADD   A, #0x30
          MOV   R0, A
          XCH   A, B
          ;CHECK FOR ZERO
          JZ    LSTRFRACBYT2STRA
          ;RESTORE REGISTERS & RETURN
          POP   B
          POP   ACC
          RET
      ENDP
    #endif

    #if STRING_FRACWRD2STR == 1
      EXTRN CODE  (MATHWIUMUL)
      PUBLIC      LSTRFRACWRD2STR
      ;CONVERT UNSIGNED FRACTIONAL VALUE (0 - 0.0000152587890625) TO ASCII STRING
      ;ON ENTRY:
      ; R0    = VALUE LSB
      ; R1    = VALUE MSB
      ; DPTR  = POINTER TO DESTINATION
      ;ON RETURN:
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      ; DPTR  = VALUE ON ENTRY
      LSTRFRACWRD2STR	PROC
          ;SAVE REGISTERS
          PUSH  ACC
          MOV   A, R2
          PUSH  ACC
          PUSH  B
          PUSH  DPL
          PUSH  DPH
          ;LOAD ZERO RESULT
          MOV   R2, #0x10
          MOV   A, #0x30
        LSTRFRACWRD2STRA:
          MOVX  @DPTR, A
          INC   DPTR
          DJNZ  R2, LSTRFRACWRD2STRA
          ;CHECK FOR ZERO
          MOV   A, R0
          ORL   A, R1
          JNZ   LSTRFRACWRD2STRC
        LSTRFRACWRD2STRB:
          ;RESTORE REGISTERS & RETURN
          POP   DPH
          POP   DPL
          POP   B
          POP   ACC
          MOV   R2, A
          POP   ACC
          RET
        LSTRFRACWRD2STRC:
          POP   DPH
          POP   DPL
          PUSH  DPL
          PUSH  DPH
        LSTRFRACWRD2STRD:
          ;CALCULATE DIGIT
          MOV   R2, #0x0A
          MOV   R3, #0x00
          CALL  MATHWIUMUL
          MOV   A, R2
          ADD   A, #0x30
          MOVX  @DPTR, A
          INC   DPTR
          MOV   A, R0
          ORL   A, R1
          ;CHECK FOR ZERO
          JNZ   LSTRFRACWRD2STRD
          SJMP  LSTRFRACWRD2STRB
      ENDP
    #endif

    #if STRING_SLICE == 1 
      PUBLIC  LSTRSLICE
      ;SLICES THE STRING IN RAM USING THE DELIMETER IN A
      ;THE RESULT IS AN ARRAY OF STRINGS, EACH TERMINATED
      ;BY THE GIVEN TERMINATOR CHARACTER
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; B     = DELIMETER
      ; DPTR  = POINTER TO SOURCE STRING
      ; R0    = LSB OF POINTER TO DESTINATION STRING
      ; R1    = MSB OF POINTER TO DESTINATION STRING
      ;ON RETURN:
      ; A     = NUMBER OF NEW STRINGS
      ; B     = VALUE ON ENTRY
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTRSLICE PROC
          ;SAVE REGISTERS
          PUSH  DPL
          PUSH  DPH
          XCH   A, R2
          PUSH  ACC
          MOV   A, R1
          PUSH  ACC
          MOV   A, R0
          PUSH  ACC
          MOV   A, R3
          PUSH  ACC
          PUSH  PSW
          ;SETUP
          CLR   F0
          SETB  F1
          MOV   R3, #0x00
        LSTRSLICEA:
          ;CHECK FOR TERMINATOR
          MOVX  A, @DPTR
          XRL   A, R2
          JZ    LSTRSLICEE
          ;CHECK FOR DELIMITER
          MOVX  A, @DPTR
          CJNE  A, B, LSTRSLICEB
          ;PROCESS NEXT BYTE IF PREVIOUS WAS DELIMITER
          JB    F1, LSTRSLICED
          ;SET DELIMITER FOUND FLAG
          SETB  F1
          ;PREPARE TO STORE TERMINATOR
          MOV   A, R2
          ;JNB		F0, LSTRSLICEC
          ;INCREMENT SUBSTRING COUNT
          INC   R3
          JMP   LSTRSLICEC
        LSTRSLICEB:
          ;SET DATA FOUND FLAG
          SETB  F0
          ;CLEAR DELIMITER FOUND FLAG
          CLR   F1
        LSTRSLICEC:
          ;STORE BYTE IN DESTINATION
          CALL  DPTRXCHG01
          MOVX  @DPTR, A
          ;POINT TO NEXT DESTINATION ADDRESS
          INC   DPTR
          ;
          CALL  DPTRXCHG01
        LSTRSLICED:
          ;POINT TO NEXT SOURCE ADDRESS
          INC   DPTR
          JMP   LSTRSLICEA
        LSTRSLICEE:
          ;
          JB    F1, LSTRSLICEF
          JNB   F0, LSTRSLICEF
          ;ADJUST COUNT FOR DATA FOUND/NOT FOUND
          INC   R3
        LSTRSLICEF:
          ;STORE TERMINATOR IN DESTINATION
          MOV   A, R2
          XCH   A, DPL
          XCH   A, R0
          XCH   A, DPL
          MOVX  @DPTR, A
          XCH   A, DPH
          XCH   A, R1
          XCH   A, DPH
          ;RESTORE REGISTERS
          MOV   A, R3
          MOV   R2, A
          POP   PSW
          POP   ACC
          MOV   R3, A
          POP   ACC
          MOV   R0, A
          POP   ACC
          MOV   R1, A
          POP   ACC
          XCH   A, R2
          POP   DPH
          POP   DPL
          RET
      ENDP
    #endif


    ;#if STRING_SLICEFIFO == 1
      EXTRN	CODE  (FIFOWRITESTR)
      PUBLIC      LSTRSLICE2FIFO
      ;SLICES THE STRING IN RAM USING THE DELIMETER IN A
      ;EACH SUBSTRING IS TERMINATED WITH THE SPECIFIED
      ;TERMINATOR CHARACTER AND PLACED IN THE SPECIFIED FIFO
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; B     = DELIMETER
      ; DPTR  = POINTER TO SOURCE STRING
      ; R0    = FIFO ID
      ;ON RETURN:
      ; C = 0 IF SUCCESS
      ;   A     = 0x00
      ;   B     = VALUE ON ENTRY
      ;   DPTR  = VALUE ON ENTRY
      ;   R0    = VALUE ON ENTRY
      ; C = 1 IF FAIL
      ;   A     = ERROR CODE
      ;   B     = VALUE ON ENTRY
      ;   DPTR  = VALUE ON ENTRY
      ;   R0    = VALUE ON ENTRY
      LSTRSLICE2FIFO	PROC
          ;SAVE REGISTERS
          PUSH  B
          XCH   A, R3
          PUSH  ACC
          MOV   A, R2
          PUSH  ACC
          MOV   A, R1
          PUSH  ACC
          MOV   A, R0
          PUSH  ACC
          MOV   A, R3
          ;SPLIT STRING ALONG SPACE CHARACTERS
          MOV   R0, DPL
          MOV   R1, DPH
          CALL  LSTRSLICE
          ;COPY SUBSTRINGS TO FIFO
          MOV   B, A
          POP   ACC
          MOV   R0, A
          MOV   R1, DPL
          MOV   R2, DPH
        LSTRSLICE2FIFOA:
          CALL  FIFOWRITESTR
          MOV   A, R1
          ADD   A, #0x01
          MOV   R1, A
          CLR   A
          ADDC  A, R2
          MOV   R2, A
          DJNZ  B, LSTRSLICE2FIFOA
          ;RESTORE REGISTERS & RETURN
          POP   ACC
          MOV   R1, A
          POP   ACC
          MOV   R2, A
          POP   ACC
          XCH   A, R3
          POP   B
          RET
      ENDP
    ;#endif


    #if STRING_INSERT == 1
    PUBLIC			LSTRINS
    ;COMPARE TWO STRINGS IN RAM TERMINATED WITH SPECIFIED CHARACTER
    ;ON ENTRY:
    ;		A		 = TERMINATOR
    ;		B		 = CHARACTER
    ;		R2	 = INDEX
    ;		DPTR = POINTER TO SOURCE STRING
    ;		R0	 = LSB OF POINTER TO DESTINATION STRING
    ;		R1	 = MSB OF POINTER TO DESTINATION STRING
    ;ON RETURN:
    ;		DPTR = VALUE ON ENTRY
    ;		R0	 = VALUE ON ENTRY
    ;		R1	 = VALUE ON ENTRY
    ;		CF = 0 IF NO ERROR
    ;			A = 0x00
    ;		CF = 1 IF ERROR
    ;			A = 0x00
    LSTRINS PROC
    ;SAVE REGISTERS
    XCH		A, R2
    PUSH	ACC
    LSTRINSA:
    ;COMPARE CHARACTER FROM STRING A WITH CHARACTER FROM STRING B
    MOVX	A, @DPTR				;LOAD CHARACTER FROM STRING A
    MOV		B, A
    MOVX	A, @R0					;LOAD CHARACTER FROM STRING B
    XRL		A, B						;COMPARE CHARACTERS
    JZ		LSTRINSB				;JUMP IF EQUAL
    ;RESTORE REGISTERS, RETURN WITH NOT EQUAL
    POP		ACC
    XCH		A, R2
    SETB	C								;SET C TO INDICATE NOT EQUAL
    RET
    LSTRINSB:
    ;CHECK FOR TERMINATOR CHARACTER
    MOVX	A, @R0
    XRL		A, R2
    JZ		LSTRINSC				;JUMP IF TERMINATOR CHARACTER FOUND
    ;SET UP FOR NEXT COMPARE
    INC		DPTR						;POINT TO NEXT CHARACTER OF STRING A
    MOV		A, R0						;POINT TO NEXT CHARACTER OF STRING B
    ADD		A, #0x01
    MOV		R0, A
    CLR		A
    ADDC	A, EMI0CN
    MOV		EMI0CN, A
    JMP		LSTRINSA				;PROCESS NEXT CHARACTER
    LSTRINSC:
    ;RESTORE REGISTERS, RETURN WITH EQUAL
    MOV		R0, EMI0CN
    MOV		R1, A
    POP		ACC
    MOV		R2, A
    CLR		C								;CLEAR C TO INDICATE EQUAL
    RET
    ENDP
    #endif

    ;#if STRING_OBJ_NEW == 1
    ;	PUBLIC			LSTROBJNEW
    ;CREATES A NEW STRING OBJECT
    ;ON ENTRY:
    ;	NONE
    ;ON RETURN:
    ;	C = 0 IF SUCCESS
    ;		A = 0x00
    ;		R0 = LSB OF POINTER TO STRING OBJECT
    ;		R1 = MSB OF POINTER TO STRING OBJECT
    ;	C = 1 IF FAIL
    ;		A = ERROR CODE
    ;		R0 = VALUE ON ENTRY
    ;		R1 = VALUE ON ENTRY
    ;	LSTROBJNEW:

    ;#endif

    #if STRING_SPLITL == 1 
      PUBLIC  LSTRSPLITL
      ;SPLITS THE STRING IN RAM USING THE DELIMETER IN A
      ;RETURNS A NEW STRING CONTAINING CHARACTERS FROM
      ;THE START OF THE STRING TO THE DELIMITER
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; B     = DELIMETER
      ; DPTR  = POINTER TO SOURCE STRING
      ; R0    = LSB OF POINTER TO DESTINATION STRING
      ; R1    = MSB OF POINTER TO DESTINATION STRING
      ;ON RETURN:
      ; B     = VALUE ON ENTRY
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      ; C     = 0 IF DELIMITER FOUND
      ;   A = LENGTH OF NEW STRING
      ; C     = 1 IF DELIMITER NOT FOUND
      ;   A = 0x00
      LSTRSPLITL  PROC
          ;SAVE REGISTERS
          PUSH  DPL
          PUSH  DPH
          XCH   A, R4
          PUSH  ACC
          MOV   A, R3
          PUSH  ACC
          MOV   A, R2
          PUSH  ACC
          MOV   A, R1
          PUSH  ACC
          MOV   A, R0
          PUSH  ACC
          ;SETUP
          SETB  C
          MOV   R3, #0x00
        LSTRSPLITLA:
          ;CHECK FOR TERMINATOR
          MOVX  A, @DPTR
          MOV   R2, A
          XRL   A, R4
          JZ    LSTRSPLITLC
          ;CHECK FOR DELIMITER
          MOV   A, R2
          XRL   A, B
          JZ    LSTRSPLITLB
          ;INCREMENT LENGTH COUNTER
          INC   R3
          ;STORE BYTE IN DESTINATION
          CALL  DPTRXCHG01
          MOV   A, R2
          MOVX  @DPTR, A
          ;POINT TO NEXT DESTINATION ADDRESS
          INC   DPTR
          CALL  DPTRXCHG01
          ;POINT TO NEXT SOURCE ADDRESS
          INC   DPTR
          JMP   LSTRSPLITLA
        LSTRSPLITLB:
          CPL   C
        LSTRSPLITLC:
          ;STORE TERMINATOR IN DESTINATION
          MOV   A, R4
          XCH   A, DPL
          XCH   A, R0
          XCH   A, DPL
          MOV   A, R4
          MOVX  @DPTR, A
          XCH   A, DPH
          XCH   A, R1
          XCH   A, DPH
          ;RESTORE REGISTERS
          POP   ACC
          MOV   R0, A
          POP   ACC
          MOV   R1, A
          POP   ACC
          MOV   R2, A
          POP   ACC
          XCH   A, R3
          XCH   A, R4
          POP   ACC
          XCH   A, R4
          POP   DPH
          POP   DPL
          RET
      ENDP
    #endif

    #if STRING_SPLITR == 1 
      PUBLIC  LSTRSPLITR
      ;SPLITS THE STRING IN RAM USING THE DELIMETER IN A
      ;RETURNS A NEW STRING CONTAINING CHARACTERS FROM
      ;THE LAST DELIMITER LOCATION TO THE END OF THE STRING
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; B     = DELIMETER
      ; DPTR  = POINTER TO SOURCE STRING
      ; R0    = LSB OF POINTER TO DESTINATION STRING A
      ; R1    = MSB OF POINTER TO DESTINATION STRING A
      ; R2    = LSB OF POINTER TO DESTINATION STRING B
      ; R3    = MSB OF POINTER TO DESTINATION STRING B
      ;ON RETURN:
      ; B     = VALUE ON ENTRY
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      ; R2    = VALUE ON ENTRY
      ; R3    = VALUE ON ENTRY
      ; C     = 0 IF DELIMITER FOUND
      ;   A = NUMBER OF SUBSTRINGS
      ; C     = 1 IF DELIMITER NOT FOUND
      ;   A = 0x00
      LSTRSPLITR  PROC
          ;SAVE REGISTERS
          PUSH  DPL
          PUSH  DPH
          XCH   A, R6
          PUSH  ACC
          MOV   A, R5
          PUSH  ACC
          MOV   A, R4
          PUSH  ACC
          MOV   A, R3
          PUSH  ACC
          MOV   A, R2
          PUSH  ACC
          MOV   A, R1
          PUSH  ACC
          MOV   A, R0
          PUSH  ACC
          ;SETUP
          SETB  C
          MOV   R5, #0x00
          MOV   A, R0
          MOV   R2, A
          MOV   A, R1
          MOV   R3, A
        LSTRSPLITRA:
          ;CHECK FOR TERMINATOR
          MOVX  A, @DPTR
          MOV   R4, A
          XRL   A, R6
          JZ    LSTRSPLITRC
          ;CHECK FOR DELIMITER
          MOV   A, R4
          XRL   A, B
          JZ    LSTRSPLITRB
          ;INCREMENT LENGTH COUNTER
          INC   R5
          ;STORE BYTE IN DESTINATION
          CALL  DPTRXCHG01
          MOVX  @DPTR, A
          ;POINT TO NEXT DESTINATION ADDRESS
          INC   DPTR
          CALL  DPTRXCHG01
          ;POINT TO NEXT SOURCE ADDRESS
          INC   DPTR
          JMP   LSTRSPLITRA
        LSTRSPLITRB:
          CPL   C
        LSTRSPLITRC:
          ;STORE TERMINATOR IN DESTINATION
          MOV   A, R6
          XCH   A, DPL
          XCH   A, R0
          XCH   A, DPL
          MOVX  @DPTR, A
          XCH   A, DPH
          XCH   A, R1
          XCH   A, DPH
          ;RESTORE REGISTERS & RETURN
          POP   ACC
          MOV   R0, A
          POP   ACC
          MOV   R1, A
          POP   ACC
          MOV   R2, A
          POP   ACC
          MOV   R3, A
          POP   ACC
          MOV   R4, A
          POP   ACC
          XCH   A, R5
          XCH   A, R6
          POP   ACC
          XCH   A, R6
          POP   DPH
          POP   DPL
          RET
      ENDP
    #endif

    #if STRING_STRIPCHAR == 1 
      PUBLIC  LSTRSTRIPCHAR
      ;STRIP THE SPECIFIED CHARACTER FROM THE STRING IN RAM
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; B     = CHARACTER
      ; DPTR  = POINTER TO SOURCE STRING
      ; R0    = LSB OF POINTER TO DESTINATION STRING
      ; EMI0CN= MSB OF POINTER TO DESTINATION STRING
      ;ON RETURN:
      ; A     = NUMBER OF SUBSTRINGS
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; EMI0CN= VALUE ON ENTRY
      LSTRSTRIPCHAR PROC
          ;SAVE REGISTERS
          PUSH  DPL
          PUSH  DPH
          XCH   A, R2
          PUSH  ACC
          MOV   A, R0
          PUSH  ACC
          PUSH  EMI0CN
        LSTRSTRIPCHARA:
          ;LOAD CHARACTER FROM XRAM AT SOURCE ADDRESS
          MOVX  A, @DPTR																											;LOAD BYTE FROM XRAM
          XCH   A, B																													;SWAP BYTE<->CHARACTER
          CJNE  A, B, LSTRSTRIPCHARC																					;CHECK FOR CHARACTER
        LSTRSTRIPCHARB:
          ;POINT TO NEXT SOURCE LOCATION IN RAM
          XCH   A, B
          INC   DPTR
          JMP   LSTRSTRIPCHARA
        LSTRSTRIPCHARC:
          XCH   A, R2																													;SWAP CHARACTER<->TERMINATOR
          CJNE  A, B, LSTRSTRIPCHARD																					;CHECK FOR TERMINATOR
          XCH   A, R2
          XCH   A, B
          ;RESTORE REGISTERS & RETURN
          POP   EMI0CN
          POP   ACC
          MOV   R0, A
          POP   ACC
          MOV   R2, A
          POP   DPH
          POP   DPL
          RET
        LSTRSTRIPCHARD:
          ;STORE CHARACTER IN RAM AT DESTINATION ADDRESS
          XCH   A, R2																													;SWAP CHARACTER<->TERMINATOR
          XCH   A, B																													;SWAP BYTE<->CHARACTER
          MOVX  @R0, A																												;STORE CHARACTER IN XRAM
          INC   DPTR																													;POINT TO NEXT FLASH CHARACTER
          MOV   A, R0																													;POINT TO NEXT XRAM CHARACTER
          ADD   A, #0x01
          MOV   R0, A
          CLR   A
          ADDC  A, EMI0CN
          MOV   EMI0CN, A
          JMP   LSTRSTRIPCHARA
      ENDP
    #endif

    #if STRING_STRIPCHARL == 1 
      PUBLIC  LSTRSTRIPCHARL
      ;STRIP THE SPECIFIED CHARACTER FROM THE STRING IN RAM
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; B     = CHARACTER
      ; DPTR  = POINTER TO SOURCE STRING
      ; R0    = LSB OF POINTER TO DESTINATION STRING
      ; R1    = MSB OF POINTER TO DESTINATION STRING
      ;ON RETURN:
      ; A     = VALUE ON ENTRY
      ; B     = VALUE ON ENTRY
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTRSTRIPCHARL  PROC
          ;SAVE REGISTERS
          PUSH  DPL
          PUSH  DPH
          XCH   A, R2
          PUSH  ACC
          MOV   A, R0
          PUSH  ACC
          MOV   A, R1
          PUSH  ACC
        LSTRSTRIPCHARLA:
          ;CHECK FOR TERMINATOR
          MOVX  A, @DPTR
          XRL   A, R2
          JZ    LSTRSTRIPCHARLC
          ;CHECK FOR CHARACTER
          MOVX  A, @DPTR
          XRL   A, B
          JNZ   LSTRSTRIPCHARLB
          ;POINT TO NEXT SOURCE ADDRESS
          INC   DPTR
          JMP   LSTRSTRIPCHARLA
        LSTRSTRIPCHARLB:
          ;STORE CHARACTER IN DESTINATION
          CALL  DPTRXCHG01
          MOVX  @R0, A
          ;POINT TO NEXT DESTINATION ADDRESS
          INC   DPTR
          ;POINT TO NEXT SOURCE ADDRESS
          CALL  DPTRXCHG01
          INC   DPTR
          ;CHECK FOR TERMINATOR
          MOVX  A, @DPTR
          XRL   A, R2
          JNZ   LSTRSTRIPCHARLB
        LSTRSTRIPCHARLC:
          ;STORE TERMINATOR IN DESTINATION
          MOV   DPL, R0
          MOV   DPH, R1
          MOVX  @R0, A
          ;RESTORE REGISTERS & RETURN
          POP   ACC
          MOV   R1, A
          POP   ACC
          MOV   R0, A
          POP   ACC
          MOV   R2, A
          POP   DPH
          POP   DPL
          RET
      ENDP
    #endif

    #if STRING_STRIPCHARR == 1 
      PUBLIC  LSTRSTRIPCHARR
      ;STRIP THE SPECIFIED CHARACTER FROM THE STRING IN RAM
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; B     = CHARACTER
      ; DPTR  = POINTER TO SOURCE STRING
      ; R0    = LSB OF POINTER TO DESTINATION STRING
      ; R1    = MSB OF POINTER TO DESTINATION STRING
      ;ON RETURN:
      ; A     = VALUE ON ENTRY
      ; B     = VALUE ON ENTRY
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTRSTRIPCHARR  PROC
          ;SAVE REGISTERS
          PUSH  DPL
          PUSH  DPH
          XCH   A, R2
          PUSH  ACC
          MOV   A, R0
          PUSH  ACC
          MOV   A, R1
          PUSH  ACC
        LSTRSTRIPCHARRA:
          ;CHECK FOR TERMINATOR
          MOVX  A, @DPTR
          XRL   A, R2
          JZ    LSTRSTRIPCHARRC
          ;CHECK FOR CHARACTER
          MOVX  A, @DPTR
          XRL   A, B
          JNZ   LSTRSTRIPCHARRB
          ;POINT TO NEXT SOURCE ADDRESS
          INC   DPTR
          JMP   LSTRSTRIPCHARRA
        LSTRSTRIPCHARRB:
          ;STORE CHARACTER IN DESTINATION
          CALL  DPTRXCHG01
          MOVX  @R0, A
          ;POINT TO NEXT DESTINATION ADDRESS
          INC   DPTR
          ;POINT TO NEXT SOURCE ADDRESS
          CALL  DPTRXCHG01
          INC   DPTR
          ;CHECK FOR TERMINATOR
          MOVX  A, @DPTR
          XRL   A, R2
          JNZ   LSTRSTRIPCHARRB
        LSTRSTRIPCHARRC:
          ;STORE TERMINATOR IN DESTINATION
          MOV   DPL, R0
          MOV   DPH, R1
          MOVX  @R0, A
          POP   ACC
          MOV   R1, A
          POP   ACC
          MOV   R0, A
          POP   ACC
          MOV   R2, A
          POP   DPH
          POP   DPL
          RET
      ENDP
    #endif

    #if STRING_TOLOWER == 1
      EXTRN CODE  (CHAR2LOWER)
      PUBLIC      LSTR2LOWER
      ;CONVERT STRING TO LOWERCASE
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; DPTR  = POINTER TO STRING
      ; R0    = LSB OF POINTER TO DESTINATION
      ; R1    = LSB OF POINTER TO DESTINATION
      ;ON RETURN:
      ; A     = VALUE ON ENTRY
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTR2LOWER  PROC
          ;SAVE REGISTERS
          PUSH  B
          PUSH  DPL
          PUSH  DPH
          MOV   B, A
        LSTR2LOWERA:
          ;CHECK FOR TERMINATOR
          MOVX  A, @DPTR
          CJNE  A, B, LSTR2LOWERB
          ;STORE TERMNATOR
          CALL  DPTRXCHG01
          MOVX  @DPTR, A
          ;RESTORE REGISTERS & RETURN
          POP   DPH
          POP   DPL
          POP   B
          RET
        LSTR2LOWERB:
          ;CHECK FOR UPPERCASE AND CONVERT
          CALL  CHAR2LOWER
          ;STORE CHARACTER
          CALL  DPTRXCHG01
          MOVX  @DPTR, A
          ;POINT TO NEXT DESTINATION ADDRESS
          INC   DPTR
          ;POINT TO NEXT SOURCE ADDRESS
          CALL  DPTRXCHG01
          INC   DPTR
          JMP   LSTR2LOWERA
      ENDP
    #endif

    #if STRING_TOLOWERZT == 1 
      PUBLIC  LSTR2LOWERZT
      ;CONVERT STRING TO LOWERCASE
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; DPTR  = POINTER TO STRING
      ; R0    = LSB OF POINTER TO DESTINATION
      ; R1    = LSB OF POINTER TO DESTINATION
      ;ON RETURN:
      ; A     = VALUE ON ENTRY
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTR2LOWERZT  PROC
        PUSH  ACC
        CLR   A
        CALL  LSTR2LOWER
        POP ACC
        RET
      ENDP
    #endif

    #if STRING_TOUPPER == 1
      EXTRN CODE  (CHAR2UPPER)
      PUBLIC      LSTR2UPPER
      ;CONVERT STRING TO YPPERCASE
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; DPTR  = POINTER TO STRING
      ; R0    = LSB OF POINTER TO DESTINATION
      ; R1    = LSB OF POINTER TO DESTINATION
      ;ON RETURN:
      ; A     = VALUE ON ENTRY
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTR2UPPER  PROC
          ;SAVE REGISTERS
          PUSH  B
          PUSH  DPL
          PUSH  DPH
          MOV   B, A
        LSTR2UPPERA:
          ;CHECK FOR TERMINATOR
          MOVX  A, @DPTR
          CJNE  A, B, LSTR2UPPERB
          ;STORE TERMNATOR
          CALL  DPTRXCHG01
          MOVX  @DPTR, A
          ;RESTORE REGISTERS & RETURN
          POP   DPH
          POP   DPL
          POP   B
          RET
        LSTR2UPPERB:
          ;CHECK FOR UPPERCASE AND CONVERT
          CALL  CHAR2UPPER
          ;STORE CHARACTER
          CALL  DPTRXCHG01
          MOVX  @DPTR, A
          ;POINT TO NEXT DESTINATION ADDRESS
          INC   DPTR
          ;POINT TO NEXT SOURCE ADDRESS
          CALL  DPTRXCHG01
          INC   DPTR
          JMP   LSTR2UPPERA
      ENDP
    #endif

    #if STRING_TOUPPERZT == 1 
      PUBLIC  LSTR2UPPERZT
      ;CONVERT STRING TO YPPERCASE
      ;ON ENTRY:
      ; A     = TERMINATOR
      ; DPTR  = POINTER TO STRING
      ; R0    = LSB OF POINTER TO DESTINATION
      ; R1    = LSB OF POINTER TO DESTINATION
      ;ON RETURN:
      ; A     = VALUE ON ENTRY
      ; DPTR  = VALUE ON ENTRY
      ; R0    = VALUE ON ENTRY
      ; R1    = VALUE ON ENTRY
      LSTR2UPPERZT:
        PUSH  ACC
        CLR   A
        CALL  LSTR2UPPER
        POP   ACC
        RET
    #endif

    #if STRING_WRD2STR == 1 
      EXTRN CODE  (MATHWIUDIV)
      PUBLIC      LSTRWRD2STR
      ;CONVERT INTEGER VALUE TO ASCII STRING (0 - 65535)
      ;ON ENTRY:
      ; R0  = VALUE LSB
      ; R1  = VALUE MSB
      ;ON RETURN:
      ; R0  = ONES CHARACTER
      ; R1  = TENS CHARCTER
      ; R2  = HUNDREDS CHARACTER
      ; R3  = THOUSANDS CHARACTER
      ; R4  = TEN THOUSANDS CHARACTER
      LSTRWRD2STR PROC
          ;SAVE REGISTERS
          PUSH  ACC
          PUSH  B
          ;CHECK FOR ZERO
          MOV   A, R0
          JNZ   LSTRWRD2STRA
          MOV   R1, A
          JNZ   LSTRWRD2STRA
          MOV   R0, #0x30
          MOV   R1, #0x30
          MOV   R2, #0x30
          MOV   R3, #0x30
          MOV   R4, #0x30
          POP   B
          POP   ACC
          RET
        LSTRWRD2STRA:
          ;PROCESS ONES
          MOV   R2, #0x0A
          MOV   R3, #0x00
          CALL  MATHWIUDIV
          MOV   A, #0x30
          ADD   A, R2
          PUSH  ACC
          ;PROCESS TENS
          MOV   R2, #0x0A
          MOV   R3, #0x00
          CALL  MATHWIUDIV
          MOV   A, #0x30
          ADD   A, R2
          PUSH  ACC
          ;PROCESS HUNDREDS
          MOV   R2, #0x0A
          MOV   R3, #0x00
          CALL  MATHWIUDIV
          MOV   A, #0x30
          ADD   A, R2
          PUSH  ACC
          ;PROCESS THOUSANDS
          MOV   R2, #0x0A
          MOV   R3, #0x00
          CALL  MATHWIUDIV
          MOV   A, #0x30
          ADD   A, R2
          PUSH  ACC
          ;PROCESS TEN THOUSANDS
          MOV   R2, #0x0A
          MOV   R3, #0x00
          CALL  MATHWIUDIV
          MOV   A, #0x30
          ADD   A, R2
          ;PLACE RESULTS IN REGISTERS
          MOV   R4, A
          POP   ACC
          MOV   R3, A
          POP   ACC
          MOV   R2, A
          POP   ACC
          MOV   R1, A
          POP   ACC
          MOV   R0, A
          ;RESTORE REGISTERS & RETURN
          POP   B
          POP   ACC
          RET
      ENDP
    #endif

    ;HELPER FUNCTIONS
    DPTRXCHG01:
      XCH   A, DPL
      XCH   A, R0
      XCH   A, DPL
      XCH   A, DPH
      XCH   A, R1
      XCH   A, DPH
      RET

    END
  #endif
#endif
