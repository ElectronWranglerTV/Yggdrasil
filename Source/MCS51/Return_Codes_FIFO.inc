;* Yggdrasil (TM) Core Operating System (MCS-51): FIFO Library Return Codes
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

;FIFO ENTRY ERROR CODES
FIFO_ERR_FIFO_INV_ID      EQU 0x01  ;INVALID FIFO ID
FIFO_ERR_FIFO_BUSY        EQU 0x02  ;FIFO BUSY
FIFO_ERR_FIFO_EMPTY       EQU 0x03  ;FIFO EMPTY
FIFO_ERR_FIFO_FULL        EQU 0x04  ;FIFO FULL


;MISC ERROR CODES
FIFO_ERR_FIFO_MEM_ALLOC   EQU 0x20  ;COULD NOT ALLOCATE MEMORY FOR FIFO
FIFO_ERR_FIFO_MEM_RELSE   EQU 0x21  ;COULD NOT FIFO'S MEMORY
