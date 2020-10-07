;* Yggdrasil (TM) Core Operating System (MCS-51): Clock Library Return Codes
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

;CLOCK ERROR CODES
CLOCK_ERR_STDIO_RD  EQU 0x01  ;ERROR READING FROM STDIO
CLOCK_ERR_STDIO_WR  EQU 0x02  ;ERROR WRITING TO STDIO
CLOCK_ERR_LENGTH    EQU 0x03  ;TIME LENGTH ERROR
CLOCK_ERR_FORMAT    EQU 0x04  ;INCORRECT TIME FORMAT
