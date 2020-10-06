;* Yggdrasil (TM) Core Operating System (MCS-51): Character Library
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

$INCLUDE (System.inc)

#if LIB_CHAR == 1
  #ifndef CHAR_INCLUDED
    #define CHAR_INCLUDED 1

    CHARACTER_ROUTINES  SEGMENT CODE
    RSEG  CHARACTER_ROUTINES

    #if CHAR_CHAR2INT == 1 
      PUBLIC  LCHAR2INT
      ;CONVERTS THE ASCII CHARACTER IN A TO AN INTEGER
      ;ON ENTRY:
      ; A = ASCII VALUE
      ;ON RETURN:
      ; C = 0 IF VALUE IS A NUMBER
      ;   A = INTEGER VALUE
      ; C = 1 IF VALUE IS NOT A NUMBER
      ;   A = 0x00
      LCHAR2INT	PROC
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
    #endif


    #if CHAR_CHAR2LOWER == 1
      PUBLIC  LCHAR2LOWER
      ;CONVERTS AN UPPERCASE LETTER TO AN LOWERCASE LETTER
      ;ON ENTRY:
      ; A = CHARACTER
      ;ON RETURN:
      ; C = 0 IF CHARACTER WAS UPPERCASE
      ;   A = CONVERTED VALUE
      ; C = 1 IF CHARACTER WAS NOT UPPERCASE
      ;   A = VALUE ON ENTRY
      LCHAR2LOWER	PROC
          ;CHECK FOR LESS THAN "A"
          PUSH  ACC
          CLR   C
          SUBB  A, #'A'
          POP   ACC
          JC    LCHAR2LOWERB
          ;CHECK FOR GREATER THAN "Z"
          PUSH  ACC
          CLR   C
          SUBB  A, #'['
          POP   ACC
          JNC   LCHAR2LOWERA
          ;CONVERT TO LOWERCASE
          ADD   A, #0x20
          RET
        LCHAR2LOWERA:
          SETB  C
        LCHAR2LOWERB:
          RET
      ENDP
    #endif


    #if CHAR_CHAR2UPPER == 1 
      PUBLIC  LCHAR2UPPER
      ;CONVERTS A LOWERCASE LETTER TO AN UPPERCASE LETTER
      ;ON ENTRY:
      ; A = CHARACTER
      ;ON RETURN:
      ; C = 0 IF CHARACTER WAS LOWERCASE
      ;   A = CONVERTED VALUE
      ; C = 1 IF CHARACTER WAS NOT LOWERCASE
      ;   A = VALUE ON ENTRY
      LCHAR2UPPER	PROC
          ;CHECK FOR LESS THAN "a"
          PUSH  ACC
          CLR   C
          SUBB  A, #'a'
          POP   ACC
          JC    LCHAR2UPPERB
          ;CHECK FOR GREATER THAN "z"
          PUSH  ACC
          CLR   C
          SUBB  A, #'{'
          POP   ACC
          JNC   LCHAR2UPPERA
          ;CONVERT TO UPPERCASE
          CLR   C
          SUBB  A, #0x20
          RET
        LCHAR2UPPERA:
          SETB  C
        LCHAR2UPPERB:
          RET
      ENDP
    #endif


    #if CHAR_CHARISARITH == 1 
      PUBLIC  LCHARISARITH
      ;RETURNS TRUE IF VALUE IS AN ARITHMETIC OPERATOR
      ;ON ENTRY:
      ; A = VALUE
      ;ON RETURN:
      ; A = VALUE ON ENTRY
      ; C = 0 IF VALUE IS NOT AN ARITHMETIC OPERATOR
      ; C = 1 IF VALUE IS AN ARITHMETIC OPERATOR
      LCHARISARITH	PROC
          ;CHECK FOR FACTORAL "!"
          PUSH  ACC
          XRL   A, '!'
          JZ    LCHARISARITHA
          ;CHECK FOR MODULO "%"
          POP   ACC
          PUSH  ACC
          XRL   A, '%'
          JZ    LCHARISARITHA
          ;CHECK FOR MULTIPLICATION "*"
          POP   ACC
          PUSH  ACC
          XRL   A, '*'
          JZ    LCHARISARITHA
          ;CHECK FOR ADDITION "+"
          POP   ACC
          PUSH  ACC
          XRL   A, '+'
          JZ    LCHARISARITHA
          ;CHECK FOR SUBTRACTION "-"
          POP   ACC
          PUSH  ACC
          XRL   A, '-'
          JZ    LCHARISARITHA
          ;CHECK FOR DIVISION "/"
          POP   ACC
          PUSH  ACC
          XRL   A, '/'
          JZ    LCHARISARITHA
          ;CHECK FOR EQUALS "="
          POP   ACC
          PUSH  ACC
          XRL   A, '='
          JZ    LCHARISARITHA
          ;CHECK FOR EXPONENT "^"
          POP   ACC
          PUSH  ACC
          XRL   A, '^'
          JNZ   LCHARISARITHB
        LCHARISARITHA:
          POP   ACC
          SETB  C
          RET
        LCHARISARITHB:
          POP   ACC
          CLR   C
          RET
      ENDP
    #endif


    #if CHAR_CHARISBRACC == 1 
      PUBLIC  LCHARISBRACC
      ;RETURNS TRUE IF VALUE IS A CLOSING BRACKET
      ;ON ENTRY:
      ; A = VALUE
      ;ON RETURN:
      ; A = VALUE ON ENTRY
      ; C = 0 IF VALUE IS NOT A CLOSING BRACKET
      ; C = 1 IF VALUE IS AN A CLOSING BRACKET
      LCHARISBRACC  PROC
          ;CHECK FOR ")"
          PUSH  ACC
          XRL   A, ')'
          JZ    LCHARISBRACCA
          ;CHECK FOR "]"
          POP   ACC
          PUSH  ACC
          XRL   A, ']'
          JZ    LCHARISBRACCA
          ;CHECK FOR "}"
          POP   ACC
          PUSH  ACC
          XRL   A, '}'
          JNZ   LCHARISBRACCB
        LCHARISBRACCA:
          POP   ACC
          SETB  C
          RET
        LCHARISBRACCB:
          POP   ACC
          CLR   C
          RET
      ENDP
    #endif


    #if CHAR_CHARISBRACO == 1 
      PUBLIC  LCHARISBRACO
      ;RETURNS TRUE IF VALUE IS AN OPENING BRACKET
      ;ON ENTRY:
      ; A = VALUE
      ;ON RETURN:
      ; A = VALUE ON ENTRY
      ; C = 0 IF VALUE IS NOT AN OPENING BRACKET
      ; C = 1 IF VALUE IS AN AN OPENING BRACKET
      LCHARISBRACO	PROC
          ;CHECK FOR "("
          PUSH  ACC
          XRL   A, '('
          JZ    LCHARISBRACOA
          ;CHECK FOR "["
          POP   ACC
          PUSH  ACC
          XRL   A, '['
          JZ    LCHARISBRACOA
          ;CHECK FOR "{"
          POP   ACC
          PUSH  ACC
          XRL   A, '{'
          JNZ   LCHARISBRACOB
        LCHARISBRACOA:
          POP   ACC
          SETB  C
          RET
        LCHARISBRACOB:
          POP   ACC
          CLR   C
          RET
      ENDP
    #endif


    #if CHAR_CHARISHEX == 1 
      PUBLIC  LCHARISHEX
      ;RETURNS TRUE IF VALUE IS A HEX CHARACTER
      ;ON ENTRY:
      ; A = VALUE
      ;ON RETURN:
      ; A = VALUE ON ENTRY
      ; C = 0 IF VALUE IS NOT A HEX CHARACTER
      ; C = 1 IF VALUE IS A HEX CHARACTER
      LCHARISHEX  PROC
          ;CHECK FOR LESS THAN "0"
          PUSH  ACC
          CLR   C
          SUBB  A, #'0'
          POP   ACC
          JC    LCHARISHEXA
          ;CHECK FOR GREATER THAN "9"
          PUSH  ACC
          CLR   C
          SUBB  A, #':'
          POP   ACC
          JC    LCHARISHEXB
          ;CHECK FOR LESS THAN "A"
          PUSH  ACC
          CLR   C
          SUBB  A, #'A'
          POP   ACC
          JC    LCHARISHEXA
          ;CHECK FOR GREATER THAN "F"
          PUSH  ACC
          CLR   C
          SUBB  A, #'G'
          POP   ACC
          JC    LCHARISHEXB
        LCHARISHEXA:
          CLR   C
        LCHARISHEXB:
          RET
      ENDP
    #endif


    #if CHAR_CHARISLETTER == 1 
      PUBLIC  LCHARISLETTER
      ;RETURNS TRUE IF VALUE IS A LETTER
      ;ON ENTRY:
      ; A = VALUE
      ;ON RETURN:
      ; A = VALUE ON ENTRY
      ; C = 0 IF VALUE IS NOT A LETTER
      ; C = 1 IF VALUE IS A LETTER
      LCHARISLETTER PROC
          ;CHECK FOR LESS THAN "A"
          PUSH  ACC
          CLR   C
          SUBB  A, #'A'
          POP   ACC
          JC    LCHARISLETTERA
          ;CHECK FOR GREATER THAN "Z"
          PUSH  ACC
          CLR   C
          SUBB  A, #'['
          POP   ACC
          JNC   LCHARISLETTERB
          ;CHECK FOR LESS THAN "a"
          PUSH  ACC
          CLR   C
          SUBB  A, #'a'
          POP   ACC
          JC    LCHARISLETTERA
          ;CHECK FOR GREATER THAN "z"
          PUSH  ACC
          CLR   C
          SUBB  A, #'{'
          POP   ACC
          JNC   LCHARISLETTERB
          RET
        LCHARISLETTERA:
          CLR   C
        LCHARISLETTERB:
          RET
      ENDP
    #endif


    #if CHAR_CHARISLOGIC == 1 
      PUBLIC  LCHARISLOGIC
      ;RETURNS TRUE IF VALUE IS A LOGICAL OPERATOR
      ;ON ENTRY:
      ; A = VALUE
      ;ON RETURN:
      ; A = VALUE ON ENTRY
      ; C = 0 IF VALUE IS NOT A LOGICAL OPERATOR
      ; C = 1 IF VALUE IS AN A LOGICAL OPERATOR
      LCHARISLOGIC  PROC
          ;CHECK FOR NOT "!"
          PUSH  ACC
          XRL   A, '!'
          JZ    LCHARISLOGICA
          ;CHECK FOR AND "&"
          POP   ACC
          PUSH  ACC
          XRL   A, '&'
          JZ    LCHARISLOGICA
          ;CHECk FOR XOR "^"
          POP   ACC
          PUSH  ACC
          XRL   A, '^'
          JZ    LCHARISLOGICA
          ;CHECK FOR OR "|"
          POP   ACC
          PUSH  ACC
          XRL   A, '|'
          JNZ   LCHARISLOGICB
        LCHARISLOGICA:
          POP   ACC
          SETB  C
          RET
        LCHARISLOGICB:
          POP   ACC
          CLR   C
          RET
      ENDP
    #endif


    #if CHAR_CHARISNUM == 1 
      PUBLIC  LCHARISNUM
      ;RETURNS TRUE IF VALUE IS A NUMBER
      ;ON ENTRY:
      ; A = VALUE
      ;ON RETURN:
      ; A = VALUE ON ENTRY
      ; C = 0 IF VALUE IS NOT A NUMBER
      ; C = 1 IF VALUE IS A NUMBER
      LCHARISNUM  PROC
          ;CHECK FOR LESS THAN "0"
          PUSH  ACC
          CLR   C
          SUBB  A, #'0'
          POP   ACC
          JC    LCHARISNUMA
          ;CHECK FOR GREATER THAN "9"
          PUSH  ACC
          CLR   C
          SUBB  A, #':'
          POP   ACC
          JNC   LCHARISNUMB
          RET
        LCHARISNUMA:
          CLR   C
        LCHARISNUMB:
          RET
      ENDP
    #endif

    #if CHAR_CHARISPUNCT == 1 
      PUBLIC  LCHARISPUNCT
      ;RETURNS TRUE IF VALUE IS PUNCTUATION
      ;ON ENTRY:
      ; A = VALUE
      ;ON RETURN:
      ; A = VALUE ON ENTRY
      ; C = 0 IF VALUE IS NOT PUNCTUATION
      ; C = 1 IF VALUE IS PUNCTUATION
      LCHARISPUNCT  PROC
          ;CHECK FOR "!"
          PUSH  ACC
          XRL   A, '!'
          JZ    LCHARISPUNCTA
          ;CHECK FOR "."
          POP   ACC
          PUSH  ACC
          XRL   A, '.'
          JZ    LCHARISPUNCTA
          ;CHECK FOR "?"
          POP   ACC
          PUSH  ACC
          XRL   A, '?'
          JNZ   LCHARISPUNCTB
        LCHARISPUNCTA:
          POP   ACC
          SETB  C
          RET
        LCHARISPUNCTB:
          POP   ACC
          CLR   C
          RET
      ENDP
    #endif

    #if CHAR_CHARISREL == 1 
      PUBLIC  LCHARISREL
      ;RETURNS TRUE IF VALUE IS A RELATIONAL OPERATOR
      ;ON ENTRY:
      ; A = VALUE
      ;ON RETURN:
      ; A = VALUE ON ENTRY
      ; C = 0 IF VALUE IS NOT A RELATIONAL OPERATOR
      ; C = 1 IF VALUE IS A RELATIONAL OPERATOR
      LCHARISREL  PROC
          ;CHECK FOR LESS THAN "<"
          PUSH  ACC
          CLR   C
          SUBB  A, #'<'
          POP   ACC
          JC    LCHARISRELA
          ;CHECK FOR GREATER THAN ">"
          PUSH  ACC
          CLR   C
          SUBB  A, #'?'
          POP   ACC
          JNC   LCHARISRELB
          RET
        LCHARISRELA:
          CLR   C
        LCHARISRELB:
          RET
      ENDP
    #endif

    END
  #endif
#endif
