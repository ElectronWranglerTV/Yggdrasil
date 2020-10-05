;* Yggdrasil (TM) Core Operating System (8051): String Library - Convert Byte to Hex String
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

PUBLIC	LSTRBYT2HEX
;CONVERT VALUE TO TWO CHARACTER ASCII HEX STRING
;ON ENTRY:
; R0  = VALUE
;ON RETURN:
; R0  = ASCII CHARACTER FOR LOW NIBBLE
; R1  = ASCII CHARACTER FOR HIGH NIBBLE
LSTRBYT2HEX	PROC
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
