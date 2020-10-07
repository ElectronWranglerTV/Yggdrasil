;* Yggdrasil (TM) Core Operating System (MCS-51): Standard MCS-51 Registers
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

#ifndef REGISTERS_INCLUDED
	#define REGISTERS_INCLUDED 1
	;-----------------------------------------------------------------------------
	;8051 STANDARD REGISTERS
	;-----------------------------------------------------------------------------
	;INTERRUPTS
	IE		DATA	0A8H			;Interrupt Enable
	IP		DATA	0B8H			;Interrupt Priority
	;POINTERS
	SP		DATA	081H			;Stack Pointer
	DPL		DATA	082H			;Data Pointer Low
	DPH		DATA	083H			;Data Pointer High
	;PORTS
	P0		DATA	080H			;Port 0 Latch
	P1		DATA	090H			;Port 1 Latch
	P2		DATA	0A0H			;Port 2 Latch
	P3		DATA	0B0H			;Port 3 Latch
	;POWER
	PCON		DATA	087H			;Power Control
	;UART
	SCON0		DATA	098H			;UART0 Control
	SBUF0		DATA	099H			;UART0 Buffer
	;TIMERS
	TCON		DATA	088H			;Timer/Counter Control
	TMOD		DATA	089H			;Timer/Counter Mode
	TL0		DATA	08AH			;Timer/Counter 0 Low
	TL1		DATA	08BH			;Timer/Counter 1 Low
	TH0		DATA	08CH			;Timer/Counter 0 High
	TH1		DATA	08DH			;Timer/Counter 1 High
	;WORKING REGISTERS
	PSW		DATA	0D0H			;Program Status Word
	ACC		DATA	0E0H			;Accumulator
	B		DATA	0F0H			;B

	;-----------------------------------------------------------------------------
	; Bit Definitions
	;-----------------------------------------------------------------------------
	;TCON 0x88
	TF1 		BIT 	TCON.7                	;Timer1 overflow flag
	TR1 		BIT 	TCON.6                	;Timer1 on/off control
	TF0 		BIT 	TCON.5                	;Timer0 overflow flag
	TR0 		BIT 	TCON.4                	;Timer0 on/off control
	IE1 		BIT 	TCON.3			;Ext interrupt 1 edge flag
	IT1 		BIT 	TCON.2			;Ext interrupt 1 type
	IE0 		BIT 	TCON.1			;Ext interrupt 0 edge flag
	IT0 		BIT 	TCON.0			;Ext interrupt 0 type

	;SCON0 0x98
	S0MODE 		BIT 	SCON0.7			;Serial mode control bit 0
							;Bit6 UNUSED
	MCE0		BIT 	SCON0.5			;Multiprocessor communication enable
	REN0		BIT 	SCON0.4			;Receive enable
	TB80 		BIT 	SCON0.3			;Transmit bit 8
	RB80 		BIT 	SCON0.2			;Receive bit 8
	TI0  		BIT 	SCON0.1			;Transmit interrupt flag
	RI0  		BIT 	SCON0.0			;Receive interrupt flag

	;IE 0xA8
	EA    BIT IE.7                ; Global interrupt enable
	ESPI0 BIT IE.6                ; SPI0 interrupt enable
	ET2   BIT IE.5                ; Timer2 interrupt enable
	ES0   BIT IE.4                ; UART0 interrupt enable
	ET1   BIT IE.3                ; Timer1 interrupt enable
	EX1   BIT IE.2                ; External interrupt 1 enable
	ET0   BIT IE.1                ; Timer0 interrupt enable
	EX0   BIT IE.0                ; External interrupt 0 enable

	; IP 0xB8
	                              ; Bit7 UNUSED
	PSPI0 BIT IP.6                ; SPI0 interrupt priority
	PT2   BIT IP.5                ; Timer2 priority
	PS0   BIT IP.4                ; UART0 priority
	PT1   BIT IP.3                ; Timer1 priority
	PX1   BIT IP.2                ; External interrupt 1 priority
	PT0   BIT IP.1                ; Timer0 priority
	PX0   BIT IP.0                ; External interrupt 0 priority

	; PSW 0xD0
	CY  BIT PSW.7                 ; Carry flag
	AC  BIT PSW.6                 ; Auxiliary carry flag
	F0  BIT PSW.5                 ; User flag 0
	RS1 BIT PSW.4                 ; Register bank select 1
	RS0 BIT PSW.3                 ; Register bank select 0
	OV  BIT PSW.2                 ; Overflow flag
	F1  BIT PSW.1                 ; User flag 1
	P   BIT PSW.0                 ; Accumulator parity flag

	;-----------------------------------------------------------------------------
	; Interrupt Priorities
	;-----------------------------------------------------------------------------
	INTERRUPT_INT0              EQU 0   ; External Interrupt 0
	INTERRUPT_TIMER0            EQU 1   ; Timer0 Overflow
	INTERRUPT_INT1              EQU 2   ; External Interrupt 1
	INTERRUPT_TIMER1            EQU 3   ; Timer1 Overflow
	INTERRUPT_UART0             EQU 4   ; Serial Port 0

	;-----------------------------------------------------------------------------
	; NON-BIT ADDRESSABLE BIT DEFINITIONS
	;-----------------------------------------------------------------------------
	;CLOCK CONTROL CONSTANTS
	SCA			EQU	0x03		;TIMER 0&1 PRESCALEER
	SCA0		EQU	0x01
	SCA1		EQU	0x02
	T0M			EQU	0x04		;TIMER 0 CLOCK SELECT
	T1M			EQU	0x08		;TIMER 1 CLOCK SELECT

	;POWER CONTROL (PCON)
	IDL		EQU	001H		;IDLE
	PD		EQU	0x02		;POWER DOWN / STOP

	;TIMER 0/1 MODE CONSTANTS
	T0M0		EQU	001H		;TIMER 0 MODE BIT 0
	T0M1		EQU	002H		;TIMER 0 MODE BIT 1
	CT0		EQU	004H		;COUNTER / TIMER SELECT 0
	GATE0		EQU	008H		;TIMER 0 GATE CONTROL
	T1M0		EQU	010H		;TIMER 1 MODE BIT 0
	T1M1		EQU	020H		;TIMER 1 MODE BIT 1
	CT1		EQU	040H		;COUNTER / TIMER SELECT 1
	GATE1		EQU	080H		;TIMER 1 GATE CONTROL

	;-----------------------------------------------------------------------------
	; REGISTER BANK SELECT CONSTANTS (XOR'D WITH PSW TO SELECT REGISTER BANKS)
	;-----------------------------------------------------------------------------
	RBSEL	EQU	0x18
	RB0		EQU	000H
	RB1		EQU	008H
	RB2		EQU	010H
	RB3		EQU	018H

	;-----------------------------------------------------------------------------
	; PERIPHERAL DESCRIPTIONS
	;-----------------------------------------------------------------------------
	UART_0_PRESENT		SET	1
	TIMER_0_PRESENT		SET	1
	TIMER_1_PRESENT		SET	1

#endif
