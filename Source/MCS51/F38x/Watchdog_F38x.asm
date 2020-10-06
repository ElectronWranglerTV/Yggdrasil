;* Yggdrasil (TM) Core Operating System (MCS-51): Silicon Labs C8051F38x Watchdog Timer Library
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

$INCLUDE (System.inc)
$INCLUDE (System_Const.inc)

PUBLIC LWDTDIS, LWDTEN, LWDTLOCK

WATCHDOG_ABSTRACTION		SEGMENT		CODE
RSEG WATCHDOG_ABSTRACTION

;DISABLE WATCHDOG TIMER
;ON ENTRY:
; NONE
;ON RETURN:
; C = 0 IF SUCCESS
; C = 1 IF FAIL
;   A = ERROR CODE
LWDTDIS PROC
    MOV   A, #WDLCK
    ANL   A, PCA0MD
    JZ    LWDTDISA
    MOV   A, #SYS_ERR_LOCKED_RES
    SETB  C
    RET
  LWDTDISA:
    ANL   PCA0MD, #WDTE XOR 0xFF
    CLR   C
    RET
ENDP


;ENABLE WATCHDOG TIMER
;ON ENTRY:
; NONE
;ON RETURN:
; C = 0 IF SUCCESS
; C = 1 IF FAIL
;   A = ERROR CODE
LWDTEN  PROC
  ORL   PCA0MD, #WDTE
  CLR   C
  RET
ENDP


;LOCK WATCHDOG TIMER
;ON ENTRY:
; NONE
;ON RETURN:
; C = 0 IF SUCCESS
; C = 1 IF FAIL
;   A = ERROR CODE
LWDTLOCK  PROC
  ORL   PCA0MD, #WDLCK
  CLR   C
  RET
ENDP

END
