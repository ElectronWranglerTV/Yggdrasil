;* Yggdrasil (TM) Core Operating System: CPU ID Utility
;* Copyright (C) DeRemee Systems - All Rights Reserved
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, FemtoRover, Weland
;* Unauthorized use or distribution of this file, via any medium, is strictly prohibited
;* Authorized use is subject to limitations included in documents accompanying this file
;* If you have not received these documents, or do not agree to the limitations within, immediately delete this file
;* Proprietary and confidential
;* Contact: Katrina Alexandra Adelheid DeRemee <kat@deremee.com>

USE16

CPU_ID:
        XOR BX, BX                      ;LOAD TYPE CODE FOR 8086/8088
        ;CHECK FOR 8086/8088
        PUSHF                           ;GET AND SAVE ORIGINAL FLAGS
        POP AX
        MOV CX, AX
        AND AX, 0x0FFF                  ;CLEAR BITS 12-15 IN FLAGS
        PUSH AX                         ;TRANSFER NEW FLAGS TO FLAGS REGISTER THEN BACK TO AX
        POPF
        PUSHF
        POP AX
        AND AX, 0xF000                  ;CHECK FOR 8086/8088
        CMP AX, 0xF000
        JE CPU_FOUND                    ;JUMP IF CPU IS 8086/8088
        ;CHECK FOR 80286
        INC BX                          ;LOAD TYPE CODE FOR 80286
        INC BX
        OR CX, 0xF000                   ;TRY TO SET FLAGS 12-15
        PUSH CX
        POPF
        PUSHF
        POP AX
        AND AX, 0xF000                  ;CHECK FOR 80286
        JZ CPU_FOUND                    ;JUMP IF CPU IS 80286
        ;CHECK FOR 80386
        INC EBX                         ;LOAD TYPE CODE FOR 80386
        PUSHFD
        POP EAX
        MOV ECX, EAX
        XOR EAX, 0x40000
        PUSH EAX
        POPFD
        PUSHFD
        POP EAX
        XOR EAX, ECX
        JZ CPU_FOUND
        PUSH ECX
        POPFD
        ;CHECK FOR 80486
        INC EBX
        MOV EAX, ECX
        XOR EAX, 0x200000
        PUSH EAX
        POPFD
        PUSHFD
        POP EAX
        XOR EAX, ECX
        JE CPU_FOUND
        ;CHECK FOR 586+
        INC BX
CPU_FOUND:
        RET

;STRINGS
        ;MSG_EVAL_CPU         DB      'Determining CPU type...', 0x00
        ;MSG_FOUND            DB      ' found', 0x0D, 0x0A, 0x00
        ;MSG_CPU_8086         DB      '8086/8088', 0x00
        ;MSG_CPU_80286        DB      '80286', 0x00
        ;MSG_CPU_80386        DB      '80386', 0x00
        ;MSG_CPU_80486        DB      '80486', 0x00
        ;MSG_CPU_PENT         DB      'Pentium+', 0x00
