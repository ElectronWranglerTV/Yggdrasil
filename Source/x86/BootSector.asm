;* Yggdrasil (TM) Core Operating System (x86): Boot Sector
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

USE16
ORG     7C00h

READ_ADDRESS    EQU 0x1000
SYSSEG          EQU 0x1000
SYSSIZE         EQU 0x3000
ENDSEG          EQU SYSSEG + SYSSIZE
SectorSize      EQU 0x12

;OFFSET 00
ENTRYPOINT:
        JMP 0x0:CANON_CSIP
        NOP
;OFFSET 03
b08_OEM         DB 'Yggdrasi'
;OFFSET 0B
BPBSecSize      DW 512                  ;SECTOR SIZE, IN BYTES
;OFFSET 0D
BPBSecPerClus   DB 1                    ;NUMBER OF SECTORS PER CLUSTER
;OFFSET 0E
BPBResSecs      DW 1                    ;NUMBER OF RESERVED SECTORS
;OFFSET 10
BPBFATCount     DB 2                    ;NUMBER OF FATS
;OFFSET 11
BPBRootEnt      DW 224                  ;NUMBER OF ROOT ENTRIES
;OFFSET 13
BPBSecCount     DW 2880                 ;TOTAL SECTORS
;OFFSET 15
BPBMediaType    DB 0xF0                 ;MEDIA DESCRIPTOR
;OFFSET 16
BPBSecsPerFAT   DW 9                    ;NUMBER OF SECTORS PER FAT
;OFFSET 18
BPBSecsPerTrk   DW 18                   ;NUMBER OF SECTORS PER TRACK
;OFFSET 1A
BPBHeadsPerCyl  DW 2                    ;NUMBER OF HEADS PER CYLINDER
;OFFSET 1C
BPBHiddenSecs   DD 0                    ;NUMBER OF HIDDEN SECTORS
;OFFSET 20
BPBLogSecs      DD 0                    ;
;OFFSET 24
Boot_Drive      DB 'X'                  ;STORAGE FOR DRIVE NUMBER
;OFFSET 25
Reserved        DB 0
;OFFSET 26
ExtBootSig      DB 0x29
;OFFSET 27
SerNum          DD 0xBA5EC0DE
;OFFSET 2B
VolumeLabel     DB 'RepRobotics'
;OFFSET 36
FileSysID       DB 'FAT12   '
;OFFSET 3E
CANON_CSIP:
    ;CONFIGURE STACK
        CLI                             ;DISABLE INTERRUPTS
        MOV AX, 0x7C00 + 0x0100         ;INITIALIZE SS:SP
        MOV SS, AX
        MOV SP, 0x1000
        STI
    ;
        PUSH CS
        POP AX
        MOV DS, AX
        MOV ES, AX
        MOV [Boot_Drive], DL            ;SAVE BOOT DRIVE NUMBER
    ;PRINT BOOT MESSAGE
        MOV SI, BootText
        CALL PRINT
    ;LOAD KERNEL
        MOV AX, SYSSEG
        MOV ES, AX
        CALL KERNEL_LOAD
        JNC LOAD_GOOD
LOAD_FAIL:
    ;PRINT FAIL MESSAGE
        PUSH CS
        POP DS
        MOV SI, BootLoadFail
        CALL PRINT
        JMP $
        INT 0x19
LOAD_GOOD:
    ;CHECK FOR YGGDRASIL KERNEL SIGNATURE
        MOV SI, READ_ADDRESS
        MOV AX, SYSSEG
        MOV DS, AX
        MOV DI, SIGNATURE
        PUSH CS
        POP ES
        MOV CX, 0x0009
LOAD_CHECK:
        CMPSB
        JNE LOAD_FAIL
        DEC CX
        JCXZ KERNEL_GOOD
        JMP LOAD_CHECK
KERNEL_GOOD:
    ;PRINT LOADED MESSAGE
        PUSH CS
        POP DS
        MOV SI, GOOD
        CALL PRINT
    ;DETERMINE CPU TYPE
        CALL CPU_ID
    ;TRANSFER TO STAGE 2
        JMP SYSSEG:READ_ADDRESS + 0x0009
KERNEL_LOAD:


        MOV AX, ES
        TEST AX, 0x0FFF
DIE:    JNE DIE
        MOV BX, READ_ADDRESS                            ;LOAD DISK TRANSFER ADDRESS
READA:
    ;RETURN IF ALL DATA HAS BEEN READ
        MOV AX, ES
        CMP AX, ENDSEG
        JB READB
        CLC
        RET
READB:
        MOV AX, SectorSize
        SUB AX, [SECTORS]
        MOV CX, AX
        SHL CX, 0x09
        ADD CX, BX
        JNC READC
        JE READC
        XOR AX, AX
        SUB AX, BX
        SHR AX, 0x09
READC:
        CALL READ
        JC READ_FAIL
        MOV CX, AX
        ADD AX, [SECTORS]
        CMP AX, SectorSize
        JNE READE
        MOV AX, 0x01
        SUB AX, [HEAD]
        JNE READD
        PUSH AX
        MOV AX, [TRACK]
        INC AX
        MOV [TRACK], AX
        POP AX
READD:
        MOV [HEAD], AX
        XOR AX, AX
READE:
        MOV [SECTORS], AX
        SHL CX, 0x09
        ADD BX, CX
        JNC READA
        MOV AX, ES
        ADD AX, 0x1000
        MOV ES, AX
        XOR BX, BX
        JMP READA
READ_FAIL:
        RET

        TRACK:   DW 0x0000
        SECTORS: DW 0x0001
        HEAD:    DW 0x0000

READ:
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        MOV DX, [TRACK]
        MOV CX, [SECTORS]
        INC CX
        MOV CH, DL
        MOV DX, [HEAD]
        MOV DH, DL
        AND DX, 0x0100
        MOV AH, 0x02
        INT 0x13
        JC READ_BAD
        POP DX
        POP CX
        POP BX
        POP AX
        CLC
        RET
READ_BAD:
        XOR AX, AX
        MOV DX, AX
        INT 0x13
        POP DX
        POP CX
        POP BX
        POP AX
        STC
        RET

;PRINT A ZERO-TERMINATED STRING
PRINT:
        LODSB                                           ;LOAD BYTE
        OR AL, AL                                       ;TEST FOR ZERO
        JZ PRINTA                                       ;RETURN IF ZERO
        MOV BX, 0x000F                                  ;CHARACTER ATTRIBUTE
        MOV AH, 0x0E                                    ;TTY MODE
        INT 0x10                                        ;PRINT CHARACTER
        JMP PRINT                                       ;REPEAT UNTIL FINISHED
PRINTA:
        RET                                             ;RETURN

;PRINT A CRLF
PRINTCRLF:
        MOV AX, 0x0E0A
        INT 0x10
        MOV AX, 0x0E0D
        INT 0x10
        RET

INCLUDE 'CPU_UTIL.ASM'

BootText             DB 'Loading Yggdrasil...', 0x00
BootLoadFail         DB 'Failed', 0x00
GOOD                 DB 'Good', 0x0A, 0x0D, 0x00
SIGNATURE            DB 'YGRSLGDAI'
                     TIMES 510-($-$$) DB 0x00           ;FILL SPACE
                     DB 0x55, 0xAA                      ;BOOT RECORD SIGNATURE
