;* Yggdrasil (TM) Core Operating System (MCS-51): Power Management Library
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.


$INCLUDE (System.inc)
$INCLUDE (System_Const.inc)
;
PUBLIC  LCPUSTOP, LVDDMONDIS, LVDDMONEN

POWER_ABSTRACTION SEGMENT CODE
RSEG  POWER_ABSTRACTION

;PLACES THE CPU IN STOP MODE
;ON ENTRY:
; NONE
;ON RETURN:
; C = 0 IF SUCCESS
;   A = 0x00
; C = 1 IF FAIL
;   A = ERROR CODE
LCPUSTOP  PROC
  MOV   A, #(PD XOR 0xFF)
  ANL   A, PCON
  MOV   PCON, A
  CLR   A
  CLR   C
  RET
ENDP


;VDD MONITOR 0 ABSTRACTION
;DISABLE VDD MONITOR
;ON ENTRY:
; NONE
;ON RETURN:
; C = 0 IF SUCCESS
LVDDMONDIS  PROC
  ANL   VDM0CN, #0x7F
  CLR   C
  RET
ENDP


;ENABLE VDD MONITOR
;ON ENTRY:
; NONE
;ON RETURN:
; C = 0 IF SUCCESS
LVDDMONEN PROC
    PUSH  ACC
    MOV   A, R0
    MOV   R0, #0x00
    ORL   VDM0CN, #0x80
  LVDDMONENA:
    DJNZ  R0, LVDDMONENA
    MOV   R0, A
    POP   ACC
    CLR   C
    RET
ENDP

END
