;* Yggdrasil (TM) Core Operating System (MCS-51): FIFO Library Include File
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

;------------------------------------------------------------------------------
; BIT DEFINITIONS
;------------------------------------------------------------------------------
;FIFO MANAGER DEFINITIONS
FIFO_COUNT_MAX        EQU 16d                                             ;MAXIMUM NUMBER OF FIFOS
FIFO_CBL_SIZE         EQU 2d                                              ;SIZE OF THE FIFO CONTROL BLOCK
FIFO_SBL_SIZE         EQU 7d                                              ;SIZE OF EACH FIFO ENTRY
FIFO_MEMSIZE          EQU FIFO_CBL_SIZE + FIFO_COUNT_MAX * FIFO_SBL_SIZE

;FIFO BITS
FIFO_BIT_PRESENT      BIT ACC.0
FIFO_BIT_EMPTY        BIT ACC.1
FIFO_BIT_FULL         BIT ACC.2
FIFO_BIT_BUSY         BIT ACC.3
FIFO_BIT_ACCESS_EN    BIT ACC.4
FIFO_BIT_ACCESS_TYP   BIT ACC.5

;FIFO FLAGS
FIFO_FLAG_PRESENT     EQU 0x01
FIFO_FLAG_EMPTY       EQU 0x02
FIFO_FLAG_FULL        EQU 0x04
FIFO_FLAG_BUSY        EQU 0x08
FIFO_FLAG_ACCESS_EN   EQU 0x10
FIFO_FLAG_ACCESS_TYP  EQU 0x20


;+======================+
;|      FCB FORMAT      |
;+======================+
;|  FIFO CONTROL FLAGS  |
;+----------------------+
;|  MAXIMUM FIFO COUNT  |
;+----------------------+
;| FIFO 0 CONTROL FLAGS |
;+----------------------+
;|   FIFO 0 BASE LOW    |
;+----------------------+
;|   FIFO 0 BASE HIGH   |
;+----------------------+
;|     FIFO 0 SIZE      |
;+----------------------+
;|     FIFO 0 PARENT    |
;+----------------------+
;|   FIFO 0 READ PTR    |
;+----------------------+
;|   FIFO 0 WRITE PTR   |
;+----------------------+
;| FIFO n CONTROL FLAGS |
;+----------------------+
;|     FIFO n SIZE      |
;+----------------------+
;|   FIFO n BASE LOW    |
;+----------------------+
;|   FIFO n BASE HIGH   |
;+----------------------+
;|     FIFO n PARENT    |
;+----------------------+
;|   FIFO n READ PTR    |
;+----------------------+
;|   FIFO n WRITE PTR   |
;+----------------------+

;+=================================+
;|    FIFO CONTROL FLAGS FORMAT    |
;+=================================+
;|  7   6   5   4   3   2   1   0  |
;+=================================+
;| EXT         ACT ACE FUL EMP PRS |
;+---------------------------------+
;PRS - PRESENT. SET IF TASK IS PRESENT
;EMP - EMPTY. SET IF FIFO IS EMPTY
;FUL - FULL. SET IF FIFO IS FULL
;BSY - BUSY. SET IF ANOTHER PROCESS IS ACCESSING THE FIFO
;ACE - ACCESS CONTROL ENABLE. SET IF ACCESS CONTROL IS ENABLED
;ACT - ACCESS CONTROL TYPE. CLEAR IF ACCESS CONTROL IS SINGLE TASK.
;                           SET IF ACCESS CONTROL IS GROUP.
;EXT - EXTENSION. SET IF EXTENDED FUNCTIONS ARE ACTIVE


FIFO_ENTRY_MAX_COUNT          EQU	0x08

;FIFO CONTROL BLOCK (FCB) OFFSETS
FIFO_FCB_OFFS_FLAGS           EQU	0x00
FIFO_FCB_OFFS_ENTRY_SIZE      EQU	0x01
FIFO_FCB_OFFS_MAX_FIFOS       EQU	0x02
FIFO_FCB_OFFS_FIFO_COUNT      EQU	0x03
FIFO_FCB_OFFS_ENTRY_0         EQU	0x04

FIFO_FCB_SIZE                 EQU	0x04


;FIFO ENTRY OFFSETS
FIFO_ENTRY_OFFS_FLAGS         EQU	0x00
FIFO_ENTRY_OFFS_ID            EQU	0x01
FIFO_ENTRY_OFFS_PARENT_TASK   EQU	0x02
FIFO_ENTRY_OFFS_SOURCE_TYPE   EQU	0x03
FIFO_ENTRY_OFFS_SOURCE_ID     EQU	0x04
FIFO_ENTRY_OFFS_BASE_ADDR_LSB EQU	0x05
FIFO_ENTRY_OFFS_BASE_ADDR_MSB EQU	0x06
FIFO_ENTRY_OFFS_BANK          EQU	0x07
FIFO_ENTRY_OFFS_SIZE          EQU	0x08
FIFO_ENTRY_OFFS_READ_INDEX    EQU	0x09
FIFO_ENTRY_OFFS_WRITE_INDEX   EQU	0x0A

FIFO_ENTRY_SIZE               EQU	0x0B
