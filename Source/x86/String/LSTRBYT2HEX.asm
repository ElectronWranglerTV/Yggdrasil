;* Yggdrasil (TM) Core Operating System (x86): String Library - Convert Byte to Hex String
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

;CONVERT BYTE TO HEX STRING
;DOES NOT BLANK LEADING ZEROES
;ON ENTRY:
; AL    = VALUE
; AH    = TERMINATOR
; DS:DI = DESTINATION ADDRESS
;ON RETURN:
; AL    = VALUE ON ENTRY
; AH    = VALUE ON ENTRY
; DS:DI = VALUE ON ENTRY
LSTRBYT2HEX:
    ;SAVE REGISTERS
    PUSHF
    PUSH  DI
    PUSH  AX
    ;SET DF TO AUTO-INCREMENT
    CLD
    ;VALUE == 0?
    OR    AL, AL
    JNZ   LSTRBYT2HEXA
    ;VALUE == 0
    ;STORE ZERO VALUE
    MOV   AL, 0x30
    STOSB
    STOSB
    MOV   AL, AH
    STOSB
    ;RESTORE REGISTERS & RETURN
    POP   AX
    POP   DI
    POPF
    RET
  LSTRBYT2HEXA:
    ;VALUE !=0
    ;SHIFT HIGH NIBBLE TO LOW NIBBLE
    SHR   AL, 4
    ;ADD 0x30 TO VALUE
    ADD   AL, 0x30
    ;RESULT > 0x39?
    CMP   AL, 0x39
    JLE   LSTRBYT2HEXB
    ;RESULT <= 0x39
    ;ADD 0x07 TO RESULT
    ADD   AL, 0x07
  LSTRBYT2HEXB:
    ;STORE RESULT
    STOSB
    ;RESTORE VALUE
    POP   AX
    PUSH  AX
    ;MASK OUT HIGH NIBBLE
    AND   AL, 0x0F
    ;ADD 0x30 TO VALUE
    ADD   AL, 0x30
    ;RESULT > 0x39?
    CMP   AL, 0x39
    JLE   LSTRBYT2HEXC
    ;RESULT <= 0x39
    ;ADD 0x07 TO RESULT
    ADD   AL, 0x07
  LSTRBYT2HEXC:
    ;STORE RESULT
    STOSB
    ;STORE TERMINATOR
    MOV   AL, AH
    STOSB
    ;RESTORE REGISTERS & RETURN
    POP   AX
    POP   DI
    POPF
    RET
