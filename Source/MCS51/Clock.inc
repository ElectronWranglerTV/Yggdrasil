;* Yggdrasil (TM) Core Operating System (MCS-51): Clock Library Include File
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

CLOCK_ENTRY_LONG_START      EQU 0x00
CLOCK_ENTRY_LONG_OFFS_SECS  EQU CLOCK_ENTRY_LONG_START      + 0
CLOCK_ENTRY_LONG_OFFS_MINS  EQU CLOCK_ENTRY_LONG_OFFS_SECS  + 1
CLOCK_ENTRY_LONG_OFFS_HRS   EQU CLOCK_ENTRY_LONG_OFFS_MINS  + 1
CLOCK_ENTRY_LONG_OFFS_DAY   EQU CLOCK_ENTRY_LONG_OFFS_HRS   + 1
CLOCK_ENTRY_LONG_OFFS_DAYL  EQU CLOCK_ENTRY_LONG_OFFS_DAY
CLOCK_ENTRY_LONG_OFFS_DAYH  EQU CLOCK_ENTRY_LONG_OFFS_DAY   + 1
CLOCK_ENTRY_LONG_OFFS_YEAR  EQU CLOCK_ENTRY_LONG_OFFS_DAY   + 2
CLOCK_ENTRY_LONG_END        EQU CLOCK_ENTRY_LONG_OFFS_YEAR  + 1

CLOCK_SIZE_TIME_LONG        EQU CLOCK_ENTRY_LONG_END - CLOCK_ENTRY_LONG_START
