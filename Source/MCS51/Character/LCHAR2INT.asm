;* Yggdrasil (TM) Core Operating System (MCS-51): Character Library - Convert ASCII Character to an Integer
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

PUBLIC  LCHAR2INT
;CONVERTS THE ASCII CHARACTER IN A TO AN INTEGER
;ON ENTRY:
; A = ASCII VALUE
;ON RETURN:
; C = 0 IF VALUE IS A NUMBER
;   A = INTEGER VALUE
; C = 1 IF VALUE IS NOT A NUMBER
;   A = 0x00
LCHAR2INT PROC
    ;CHECK FOR LESS THAN "0"
    CLR   C
    SUBB  A, #'0'
    JC    LCHAR2INTA
    ;CHECK FOR GREATER THAN "9"
    PUSH  ACC
    CLR   C
    SUBB  A, #0x0A
    POP   ACC
    JNC   LCHAR2INTA
    CLR   C
    RET
  LCHAR2INTA:
    CLR   A
    SETB  C
    RET
ENDP
