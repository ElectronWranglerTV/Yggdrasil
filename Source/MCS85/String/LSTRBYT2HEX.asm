;* Yggdrasil (TM) Core Operating System (MCS-85): String Library - Convert Byte to Hex String
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

;CONVERT BYTE TO HEX STRING
;DOES NOT BLANK LEADING ZEROES
;ON ENTRY:
; A   = VALUE
; B   = TERMINATOR
; HL  = DESTINATION ADDRESS
;ON RETURN:
; A   = VALUE ON ENTRY
; B   = VALUE ON ENTRY
; HL  = VALUE ON ENTRY
LSTRBYT2HEX:
    ;SAVE REGISTERS
    PUSH  H
    PUSH  PSW
    ;VALUE == 0?
    ORA   A
    JNZ		LSTRBYT2HEXA
    ;VALUE == 0
    ;STORE ZERO VALUE
    MVI   A, 0x30
    MOV   M, A
    INX   H
    MOV   M, A
    ;STORE TERMINATOR
    INX   H
    MOV   M, B
    ;RESTORE REGISTERS & RETURN
    POP   PSW
    POP   H
    RET
  LSTRBYT2HEXA:
    ;VALUE != 0
    ;MASK OUT LOW NIBBLE
    ANI   A, 0xF0
    ;MOVE NIBBLE INTO POSITION
    RRC
    RRC
    RRC
    RRC
    ;ADD 0x30 TO NIBBLE
    ADI   A, 0x30
    ;RESULT > 0x39?
    CPI   A, 0x3A
    JNC   LSTRBYT2HEXB
    ;RESULT > 0x39
    ;ADD 0x07 TO RESULT
    ADI   A, 0x07
  LSTRBYT2HEXB:
    ;STORE RESULT
    MOV   M, A
    ;RESTORE VALUE
    POP   PSW
    PUSH  PSW
    INX   H
    ;MASK OUT HIGH NIBBLE
    ANI   A, 0x0F
    ;ADD 0x30 TO NIBBLE
    ADI   A, 0x30
    ;RESULT > 0x39?
    CPI   A, 0x3A
    JNC   LSTRBYT2HEXC
    ;RESULT > 0x39
    ;ADD 0x07 TO RESULT
    ADI   A, 0x07
  LSTRBYT2HEXC:
    ;STORE RESULT
    MOV   M, A
    ;STORE TERMINATOR
    INX   H
    MOV   M, B
    ;RESTORE REGISTERS & RETURN
    POP   PSW
    POP   H
    RET
