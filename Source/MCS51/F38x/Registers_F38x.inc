;* Yggdrasil (TM) Core Operating System (MCS-51): Silicon Labs C8051F38x Registers
;* Copyright (C) DeRemee Systems, IXE Electronics LLC
;* Portions copyright IXE Electronics LLC, Republic Robotics, FemtoLaunch, FemtoSat, FemtoTrack, Weland
;* This work is made available under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
;* To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/.

$INCLUDE (Registers.inc)

;ADC 0
ADC0CF		DATA	0xBC			;ADC0 Configuration
ADC0CN		DATA	0xE8			;ADC0 Control
ADC0GTH		DATA	0xC4			;ADC0 Greater-Than Compare High
ADC0GTL		DATA	0xC3			;ADC0 Greater-Than Compare Low
ADC0H			DATA	0xBE			;ADC0 Data High Byte
ADC0L			DATA	0xBD			;ADC0 Data Low Byte
ADC0LTH		DATA	0xC6			;ADC0 Less-Than Compare High
ADC0LTL		DATA	0xC5			;ADC0 Less-Than Compare Low
AMX0N			DATA	0xBA			;AMUX Negative Select
AMX0P			DATA	0xBB			;AMUX Positive Select

;Clock
CKCON			DATA	0x8E			;Clock Control
CKCON1		DATA	0xE4			;Clock Control 1
CLKMUL		DATA	0xB9			;Clock Multiplier
CLKSEL		DATA	0xA9			;Clock Select

;Comparators
CPT0CN		DATA	0x9B			;Control
CPT0MD		DATA	0x9D			;Mode Selection
CPT0MX		DATA	0x9F			;MUX Selection
CPT1CN		DATA	0x9A			;Control
CPT1MD		DATA	0x9C			;Mode Selection
CPT1MX		DATA	0x9E			;MUX Selection

;Crossbar
XBR0			DATA	0xE1			;Port I/O Crossbar Control 0
XBR1			DATA	0xE2			;Port I/O Crossbar Control 1
XBR2			DATA	0xE3			;Port I/O Crossbar Control 2

;Interrupts
EIE1			DATA	0xE6			;Extended Interrupt Enable 1
EIE2			DATA	0xE7			;Extended Interrupt Enable 2
EIP1			DATA	0xF6			;Extended Interrupt Priority 1
EIP2			DATA	0xF7			;Extended Interrupt Priority 2
IT01CF		DATA	0xE4			;INT0/INT1 Configuration

;EMIF
EMI0CF		DATA	0x85			;Configuration
EMI0CN		DATA	0xAA			;Control
EMI0TC		DATA	0x84			;Timing

;Flash
FLKEY			DATA	0xB7			;Flash Lock and Key
FLSCL			DATA	0xB6			;Flash Scale
PSCTL			DATA	0x8F			;Program Store Read/Write Control

;Oscillator
OSCICL		DATA	0xB3			;Internal Oscillator Calibration
OSCICN		DATA	0xB2			;Internal Oscillator Control
OSCLCN		DATA	0x86			;Low Frequency Oscillator Control
OSCXCN		DATA	0xB1			;External Oscillator Control

;PCA
PCA0H			DATA	0xFA			;PCA Counter High
PCA0L			DATA	0xF9			;PCA Counter Low
PCA0CN		DATA	0xD8			;PCA Control
PCA0CPH0	DATA	0xFC			;PCA Capture 0 High
PCA0CPH1	DATA	0xEA			;PCA Capture 1 High
PCA0CPH2	DATA	0xEC			;PCA Capture 2 High
PCA0CPH3	DATA	0xEE			;PCA Capture 3 High
PCA0CPH4	DATA	0xFE			;PCA Capture 4 High
PCA0CPL0	DATA	0xFB			;PCA Capture 0 Low
PCA0CPL1	DATA	0xE9			;PCA Capture 1 Low
PCA0CPL2	DATA	0xEB			;PCA Capture 2 Low
PCA0CPL3	DATA	0xED			;PCA Capture 3 Low
PCA0CPL4	DATA	0xFD			;PCA Capture 4 Low
PCA0CPM0	DATA	0xDA			;PCA Module 0 Mode
PCA0CPM1	DATA	0xDB			;PCA Module 1 Mode
PCA0CPM2	DATA	0xDC			;PCA Module 2 Mode
PCA0CPM3	DATA	0xDD			;PCA Module 3 Mode
PCA0CPM4	DATA	0xDE			;PCA Module 4 Mode
PCA0MD		DATA	0xD9			;PCA Mode

;Ports
P4				DATA	0xC7
P0MDIN		DATA	0xF1			;Port 0 Input Mode
P0MDOUT		DATA	0xA4			;Port 0 Output Mode
P0SKIP		DATA	0xD4			;Port 0 Skip
P1MDIN		DATA	0xF2			;Port 1 Input Mode
P1MDOUT		DATA	0xA5			;Port 1 Output Mode
P1SKIP		DATA	0xD5			;Port 1 Skip
P2MDIN		DATA	0xF3			;Port 2 Input Mode
P2MDOUT		DATA	0xA6			;Port 2 Output Mode
P2SKIP		DATA	0xD6			;Port 2 Skip
P3MDIN		DATA	0xF4			;Port 3 Input Mode
P3MDOUT		DATA	0xA7			;Port 3 Output Mode
P3SKIP		DATA	0xDF			;Port 3 Skip
P4MDIN		DATA	0xF5			;Port 3 Input Mode
P4MDOUT		DATA	0xAE			;Port 3 Output Mode

;Prefetch Engine
PFE0CN		DATA	0xAF			;Control

;Reset
RSTSRC		DATA	0xEF			;Reset Source Configuration/Status

;SFR Paging
SFRPAGE		DATA	0xBF			;Page Select

;SMB
SMB0ADM		DATA	0xCE			;Address Mask 0
SMB0ADR		DATA	0xCF			;Address 0
SMB0CF		DATA	0xC1			;Configuration 0
SMB0CN		DATA	0xC0			;Control 0
SMB0DAT		DATA	0xC2			;Data 0
SMB1ADM		DATA	0xCE			;Address Mask 1
SMB1ADR		DATA	0xCF			;Address 1
SMB1CF		DATA	0xC1			;Configuration 1
SMB1CN		DATA	0xC0			;Control 1
SMB1DAT		DATA	0xC2			;Data 1
SMBTC			DATA	0xB9			;Timing Control

;SPI
SPI0CFG		DATA	0xA1			;Configuration
SPI0CKR		DATA	0xA2			;Clock Rate
SPI0CN		DATA	0xF8			;Control
SPI0DAT		DATA	0xA3			;Data

;Timers
TMR2CN		DATA	0xC8			;Timer/Counter 2 Control
TMR2H			DATA	0xCD			;Timer/Counter 2 High
TMR2L			DATA	0xCC			;Timer/Counter 2 Low
TMR2RLH		DATA	0xCB			;Timer/Counter 2 Reload High
TMR2RLL		DATA	0xCA			;Timer/Counter 2 Reload Low
TMR3CN		DATA	0x91			;Timer/Counter 3 Control
TMR3H			DATA	0x95			;Timer/Counter 3 High
TMR3L			DATA	0x94			;Timer/Counter 3 Low
TMR3RLH		DATA	0x93			;Timer/Counter 3 Reload High
TMR3RLL		DATA	0x92			;Timer/Counter 3 Reload Low
TMR4CN		DATA	0x91			;Timer/Counter 4 Control
TMR4H			DATA	0x95			;Timer/Counter 4 High
TMR4L			DATA	0x94			;Timer/Counter 4 Low
TMR4RLH		DATA	0x93			;Timer/Counter 4 Reload High
TMR4RLL		DATA	0x92			;Timer/Counter 4 Reload Low
TMR5CN		DATA	0xC8			;Timer/Counter 5 Control
TMR5H			DATA	0xCD			;Timer/Counter 5 High
TMR5L			DATA	0xCC			;Timer/Counter 5 Low
TMR5RLH		DATA	0xCB			;Timer/Counter 5 Reload High
TMR5RLL		DATA	0xCA			;Timer/Counter 5 Reload Low

;UART
SBCON1		DATA	0xAC			;Baud Rate Generator Control 1
SBRLH1		DATA	0xB5			;Baud Rate Generator 1 High Byte
SBRLL1		DATA	0xB4			;Baud Rate Generator 1 High Byte
SBUF1			DATA	0xD3			;Buffer 1
SCON1			DATA	0xD2			;Control 1
SMOD1			DATA	0xE5			;Mode 1

;USB
USB0ADR		DATA	0x96			;Indirect Address
USB0DAT		DATA	0x97			;Data
USB0XCN		DATA	0xD7			;Transceiver Control

;Vdd Monitor
VDM0CN		DATA	0xFF			;Control

;Voltage Reference
REF0CN		DATA	0xD1			;Voltage Reference Control

;Voltage Regulators
REG01CN		DATA	0xC9			;Regulator 0 & 1 Control

;-----------------------------------------------------------------------------
; Bit Definitions
;-----------------------------------------------------------------------------
; SMB0CN 0xC0 (Legacy Definitions)
MASTER  BIT SMB0CN.7          ; Master/slave indicator
TXMODE  BIT SMB0CN.6          ; Transmit mode indicator
STA     BIT SMB0CN.5          ; Start flag
STO     BIT SMB0CN.4          ; Stop flag
ACKRQ   BIT SMB0CN.3          ; Acknowledge request
ARBLOST BIT SMB0CN.2          ; Arbitration lost indicator
ACK     BIT SMB0CN.1          ; Acknowledge flag
SI      BIT SMB0CN.0          ; SMBus interrupt flag

; SMB0CN 0xC0 (Page 0)
MASTER0  BIT SMB0CN.7         ; Master/slave indicator
TXMODE0  BIT SMB0CN.6         ; Transmit mode indicator
STA0     BIT SMB0CN.5         ; Start flag
STO0     BIT SMB0CN.4         ; Stop flag
ACKRQ0   BIT SMB0CN.3         ; Acknowledge request
ARBLOST0 BIT SMB0CN.2         ; Arbitration lost indicator
ACK0     BIT SMB0CN.1         ; Acknowledge flag
SI0      BIT SMB0CN.0         ; SMBus interrupt flag

; SMB1CN 0xC0 (Page F)
MASTER1  BIT SMB1CN.7         ; Master/slave indicator
TXMODE1  BIT SMB1CN.6         ; Transmit mode indicator
STA1     BIT SMB1CN.5         ; Start flag
STO1     BIT SMB1CN.4         ; Stop flag
ACKRQ1   BIT SMB1CN.3         ; Acknowledge request
ARBLOST1 BIT SMB1CN.2         ; Arbitration lost indicator
ACK1     BIT SMB1CN.1         ; Acknowledge flag
SI1      BIT SMB1CN.0         ; SMBus interrupt flag

; TMR2CN 0xC8 (Page 0)
TF2H    BIT TMR2CN.7          ; Timer2 high byte overflow flag
TF2L    BIT TMR2CN.6          ; Timer2 low byte overflow flag
TF2LEN  BIT TMR2CN.5          ; Timer2 low byte interrupt enable
T2CE    BIT TMR2CN.4          ; Timer2 capture enable
T2SPLIT BIT TMR2CN.3          ; Timer2 split mode enable
TR2     BIT TMR2CN.2          ; Timer2 on/off control
T2CSS   BIT TMR2CN.1          ; Timer 2 Capture Source select
T2XCLK  BIT TMR2CN.0          ; Timer2 external clock select

; TMR5CN 0xC8 (Page F)
TF5H    BIT TMR5CN.7          ; Timer5 high byte overflow flag
TF5L    BIT TMR5CN.6          ; Timer5 low byte overflow flag
TF5LEN  BIT TMR5CN.5          ; Timer5 low byte interrupt enable
T5CE    BIT TMR5CN.4          ; Timer5 capture enable
T5SPLIT BIT TMR5CN.3          ; Timer5 split mode enable
TR5     BIT TMR5CN.2          ; Timer5 on/off control
T5CSS   BIT TMR5CN.1          ; Timer 5 Capture Source select
T5XCLK  BIT TMR5CN.0          ; Timer5 external clock select

; PCA0CN 0xD8
CF BIT PCA0CN.7               ; PCA0 counter overflow flag
CR BIT PCA0CN.6               ; PCA0 counter run control
                              ; Bit5 UNUSED
CCF4 BIT PCA0CN.4             ; PCA0 module4 capture/compare flag
CCF3 BIT PCA0CN.3             ; PCA0 module3 capture/compare flag
CCF2 BIT PCA0CN.2             ; PCA0 module2 capture/compare flag
CCF1 BIT PCA0CN.1             ; PCA0 module1 capture/compare flag
CCF0 BIT PCA0CN.0             ; PCA0 module0 capture/compare flag

; ADC0CN 0xE8
AD0EN   BIT ADC0CN.7          ; ADC0 enable
AD0TM   BIT ADC0CN.6          ; ADC0 track mode
AD0INT  BIT ADC0CN.5          ; ADC0 conversion complete interrupt flag
AD0BUSY BIT ADC0CN.4          ; ADC0 busy flag
AD0WINT BIT ADC0CN.3          ; ADC0 window compare interrupt flag
AD0CM2  BIT ADC0CN.2          ; ADC0 conversion mode select 2
AD0CM1  BIT ADC0CN.1          ; ADC0 conversion mode select 1
AD0CM0  BIT ADC0CN.0          ; ADC0 conversion mode select 0

; SPI0CN 0xF8
SPIF   BIT SPI0CN.7           ; SPI0 interrupt flag
WCOL   BIT SPI0CN.6           ; SPI0 write collision flag
MODF   BIT SPI0CN.5           ; SPI0 mode fault flag
RXOVRN BIT SPI0CN.4           ; SPI0 rx overrun flag
NSSMD		EQU	0x0C
NSSMD1 BIT SPI0CN.3           ; SPI0 slave select mode 1
NSSMD0 BIT SPI0CN.2           ; SPI0 slave select mode 0
TXBMT  BIT SPI0CN.1           ; SPI0 transmit buffer empty
SPIEN  BIT SPI0CN.0           ; SPI0 SPI enable


;-----------------------------------------------------------------------------
; Interrupt Priorities
;-----------------------------------------------------------------------------
INTERRUPT_TIMER2            EQU 5   ; Timer2 Overflow
INTERRUPT_SPI0              EQU 6   ; Serial Peripheral Interface 0
INTERRUPT_SMBUS0            EQU 7   ; SMBus0 Interface
INTERRUPT_USB0              EQU 8   ; USB Interface
INTERRUPT_ADC0_WINDOW       EQU 9   ; ADC0 Window Comparison
INTERRUPT_ADC0_EOC          EQU 10  ; ADC0 End Of Conversion
INTERRUPT_PCA0              EQU 11  ; PCA0 Peripheral
INTERRUPT_COMPARATOR0       EQU 12  ; Comparator0
INTERRUPT_COMPARATOR1       EQU 13  ; Comparator1
INTERRUPT_TIMER3            EQU 14  ; Timer3 Overflow
INTERRUPT_VBUS_LEVEL        EQU 15  ; VBUS level-triggered interrupt
INTERRUPT_UART1             EQU 16  ; Serial Port 1
                                    ; #17 Reserved
INTERRUPT_SMBUS1            EQU 18  ; SMBus1 Interface
INTERRUPT_TIMER4            EQU 19  ; Timer4 Overflow
INTERRUPT_TIMER5            EQU 20  ; Timer5 Overflow

;-----------------------------------------------------------------------------
; SFR Page Definitions
;-----------------------------------------------------------------------------

LEGACY_PAGE        EQU 000H         ; LEGACY SFR PAGE
CONFIG_PAGE        EQU 00FH         ; TIMER4 and TIMER5 CONFIGURATION PAGE
SMBUS1_PAGE        EQU 00FH         ; SMBUS1 SFR PAGE

;-----------------------------------------------------------------------------
; NON-BIT ADDRESSABLE BIT DEFINITIONS
;-----------------------------------------------------------------------------
AD0MUXGND		EQU	0x1F
AD0MUXTEMP	EQU 0x1E
AD0MUXVDD		EQU	0x1F
AD0MUXVREF	EQU	0x1E

;EXTENDED INTERRUPT ENABLE 1 CONSTANTS
ESMB0			EQU	0x01		;SMB 0
EUSB0			EQU	0x02		;USB 0
EWADC0		EQU	0x04		;ADC 0 WINDOW COMPARATOR
EADC0			EQU	0x08		;ADC 0 EOC
EPCA0			EQU	0x10		;PCA 0
ECP0			EQU	0x20		;COMPARATOR 0
ECP1			EQU	0x40		;COMPARATOR 1
ET3				EQU	0x80		;TIMER 3

;EXTENDED INTERRUPT ENABLE 2 CONSTANTS
EVBUS			EQU	0x01		;VBUS LEVEL
ES1				EQU	0x02		;UART 1
EIE22			EQU	0x04		;RESERVED
ESMB1			EQU	0x08		;SMB 1
ET4				EQU	0x10		;TIMER 4
ET5				EQU	0x20		;TIMER 5
EIE26			EQU	0x40		;RESERVED
EIE27			EQU	0x80		;RESERVED

;CLOCK CONTROL CONSTANTS
T2ML		EQU	010H		;TIMER 2 LOW BYTE CLOCK SELECT
T2MH		EQU	020H		;TIMER 2 HIGH BYTE CLOCK SELECT
T3ML		EQU	040H		;TIMER 3 LOW BYTE CLOCK SELECT
T3MH		EQU	080H		;TIMER 3 HIGH BYTE CLOCK SELECT

;CLOCK CONTROL 1 CONSTANTS
T4ML		EQU	001H		;TIMER 4 LOW BYTE CLOCK SELECT
T4MH		EQU	002H		;TIMER 4 HIGH BYTE CLOCK SELECT
T5ML		EQU	004H		;TIMER 5 LOW BYTE CLOCK SELECT
T5MH		EQU	008H		;TIMER 5 HIGH BYTE CLOCK SELECT

;CLOCK MULTIPLIER (CLKMUL)
MULSEL		EQU	003H		;MULTIPLIER INPUT SELECT BITS
MULSEL0		EQU	001H		;MULTIPLIER SELECT BIT 0
MULSEL1		EQU	002H		;MULTIPLIER SELECT BIT 1
MULRDY		EQU	020H		;MULTIPLIER READY FLAG
MULINIT		EQU	040H		;MULTIPLIER INITIALIZE
MULEN		EQU	080H		;MULTIPLIER ENABLE

;CLOCK SELECT (CLKSEL)
CLKSL		EQU	0x07		;CLOCK SELECT BITS
CLKSL0		EQU	0x01		;CLOCK SELECT BIT 0
CLKSL1		EQU	0x02		;CLOCK SELECT BIT 1
CLKSL2		EQU	0x04		;CLOCK SELECT BIT 2
OUTCLK		EQU	0x08		;CROSSBAR CLOCK OUT ENABLE
USBCLK		EQU	0x70		;USB CLOCK SELECT BITS
USBCLK0		EQU	0x10		;USB CLOCK SELECT BIT 0
USBCLK1		EQU	0x20		;USB CLOCK SELECT BIT 1
USBCLK2		EQU	0x40		;USB CLOCK SELECT BIT 2
CLKSEL7		EQU	0x80		;CLOCK SELECT BIT 7

;CROSSBAR 0 BITS
BIT_URT0E		BIT	ACC.0		;UART 0 I/O ENABLE
BIT_SPI0E		BIT	ACC.1		;SPI 0 I/O ENABLE
BIT_SMB0E		BIT	ACC.2		;SMB 0 I/O ENABLE
BIT_SYSCKE	BIT	ACC.3		;SYSTEM CLOCK OUTPUT ENABLE
BIT_CP0E		BIT	ACC.4		;COMPARATOR 0 OUTPUT ENABLE
BIT_CP0AE		BIT	ACC.5		;COMPARATOR 0 ASYNCHRONOUS OUTPUT ENABLE
BIT_CP1E		BIT	ACC.6		;COMPARATOR 1 OUTPUT ENABLE
BIT_CP1AE		BIT	ACC.7		;COMPARATOR 1 ASYNCHRONOUS OUTPUT ENABLE

;CROSSBAR 0 CONSTANTS
URT0E				EQU	0x01		;UART 0 I/O ENABLE
SPI0E				EQU	0x02		;SPI 0 I/O ENABLE
SMB0E				EQU	0x04		;SMB 0 I/O ENABLE
SYSCKE			EQU	0x08		;SYSTEM CLOCK OUTPUT ENABLE
CP0E				EQU	0x10		;COMPARATOR 0 OUTPUT ENABLE
CP0AE				EQU	0x20		;COMPARATOR 0 ASYNCHRONOUS OUTPUT ENABLE
CP1E				EQU	0x40		;COMPARATOR 1 OUTPUT ENABLE
CP1AE				EQU	0x80		;COMPARATOR 1 ASYNCHRONOUS OUTPUT ENABLE

;CROSSBAR 1 BITS
BIT_PCA0ME0	BIT	ACC.0		;PCA 0 I/O ENABLE BIT 0
BIT_PCA0ME1	BIT	ACC.1		;PCA 0 I/O ENABLE BIT 1
BIT_PCA0ME2	BIT	ACC.2		;PCA 0 I/O ENABLE BIT 2
BIT_ECIE		BIT	ACC.3		;PCA 0 EXTERNAL COUNTER INPUT ENABLE
BIT_T0E			BIT	ACC.4		;T0 ENABLE
BIT_				BIT	ACC.5		;T1 ENABLE
BIT_XBARE		BIT	ACC.6		;CROSSBAR ENABLE
BIT_WEAKPUD	BIT	ACC.7		;WEAK PULLUP DISABLE

;CROSSBAR 1 CONSTANTS
PCA0ME			EQU	0x07		;PCA 0 I/O ENABLE BITS
PCA0ME0			EQU	0x01		;PCA 0 I/O ENABLE BIT 0
PCA0ME1			EQU	0x02		;PCA 0 I/O ENABLE BIT 1
PCA0ME2			EQU	0x04		;PCA 0 I/O ENABLE BIT 2
ECIE				EQU	0x08		;PCA 0 EXTERNAL COUNTER INPUT ENABLE
T0E					EQU	0x10		;T0 ENABLE
T1E					EQU	0x20		;T1 ENABLE
XBARE				EQU	0x40		;CROSSBAR ENABLE
WEAKPUD			EQU	0x80		;WEAK PULLUP DISABLE

;CROSSBAR 2 BITS
BIT_URT1E		BIT	ACC.0		;UART 1 I/O ENABLE
BIT_SMB1E		BIT	ACC.1		;SMB 1 I/E ENABLE

;CROSSBAR 2 CONSTANTS
URT1E				EQU	0x01		;UART 1 I/O ENABLE
SMB1E				EQU	0x02		;SMB 1 I/E ENABLE

;EMI 0 (EMI0CF) BITS
BIT_EALE0		BIT	ACC.0		;ALE PULSE WIDTH SELECT BIT 0
BIT_EALE1		BIT	ACC.1		;ALE PULSE WIDTH SELECT BIT 1
BIT_EMD0		BIT	ACC.2		;OPERATING MODE BIT 0
BIT_EMD1		BIT	ACC.3		;OPERATING MODE BIT 1
BIT_EMD2		BIT	ACC.4		;MULTIPLEX MODE SELECT
												;BIT 5 RESERVED
BIT_USBFAE	BIT	ACC.6		;USB FIFO ACCESS ENABLE
												;BIT 7 RESERVED

;EMI 0 (EMI0CF) CONSTANTS
EALE				EQU	0x03		;ALE PULSE WIDTH SELECT
EALE0				EQU	0x01		;ALE PULSE WIDTH SELECT BIT 0
EALE1				EQU	0x02		;ALE PULSE WIDTH SELECT BIT 1
EALECLK1		EQU	0x00		;SET ALE PULSE = 1
EALECLK2		EQU	0x01		;SET ALE PULSE = 2
EALECLK3		EQU	0x02		;SET ALE PULSE = 3
EALECLK4		EQU	0x03		;SET ALE PULSE = 4
EMD					EQU	0x0C		;OPERATING MODE
EMD0				EQU	0x04		;OPERATING MODE BIT 0
EMD1				EQU	0x08		;OPERATING MODE BIT 1
EMD2				EQU	0x10		;MULTIPLEX MODE SELECT
EMDX				EQU	0x00		;SET TO XRAM ONLY
EMDXBS			EQU	0x04		;SET TO XRAM WITH BANK SELECT
EMDXNB			EQU	0x08		;SET TO XRAM WITHOUT BANK SELECT
EMDEXT			EQU	0x0C		;SET TO EXTERNAL RAM
EMI0CF5			EQU	0x20		;EMI0CF BIT 5
USBFAE			EQU	0x40		;USB FIFO ACCESS ENABLE
EMI0CF7			EQU	0x80		;EMI0CF BIT 7

;FLASH PROGRAMMING KEYS
FLKEY_0			EQU	0xA5
FLKEY_1			EQU	0xF1

;INTERNAL OSCILLATOR CONTROL (OSCICN) BITS
BIT_IFCN0		BIT	ACC.0		;FREQUENCY CONTROL BIT 0
BIT_IFCN1		BIT	ACC.1		;FREQUENCY CONTROL BIT 1
												;BIT 2 RESERVED
												;BIT 3 RESERVED
												;BIT 4 RESERVED
BIT_SUSPEND	BIT	ACC.5		;SUSPEND
BIT_IFRDY		BIT	ACC.6		;OSCILLATOR READY
BIT_IOSCEN	BIT	ACC.7		;OSCILLATOR ENABLE

;INTERNAL OSCILLATOR CONTROL (OSCICN) CONSTANTS
IFCN				EQU	0x03		;FREQUENCY CONTROL
IFCN0				EQU	0x01		;FREQUENCY CONTROL BIT 0
IFCN1				EQU	0x02		;FREQUENCY CONTROL BIT 1
SUSPEND			EQU	0x20		;SUSPEND
IFRDY				EQU	0x40		;OSCILLATOR READY
IOSCEN			EQU	0x80		;OSCILLATOR ENABLE

;INTERNAL LOW-FREQUENCY OSCILLATOR (OSCLCN) BITS
BIT_OSCLD0	BIT	ACC.0		;LFO DIVIDER BIT 0
BIT_OSCLD1	BIT	ACC.1		;LFO DIVIDER BIT 1
BIT_OSCLF0	BIT	ACC.2		;LFO FREQUENCY FINE TUNE BIT 0
BIT_OSCLF1	BIT	ACC.3		;LFO FREQUENCY FINE TUNE BIT 1
BIT_OSCLF2	BIT	ACC.4		;LFO FREQUENCY FINE TUNE BIT 2
BIT_OSCLF3	BIT	ACC.5		;LFO FREQUENCY FINE TUNE BIT 3
BIT_OSCLRDY	BIT	ACC.6		;LFO READY
BIT_OCSLEN	BIT	ACC.7		;LFO ENABLE

;INTERNAL LOW-FREQUENCY OSCILLATOR (OSCLCN) CONSTANTS
OSCLD				EQU	0x03		;LFO DIVIDER SELECT
OSCLD0			EQU	0x01		;LFO DIVIDER BIT 0
OSCLD1			EQU	0x02		;LFO DIVIDER BIT 1
OSCLF				EQU	0x3C		;LFO FREQUENCY FINE TUNE
OSCLF0			EQU	0x04		;LFO FREQUENCY FINE TUNE BIT 0
OSCLF1			EQU	0x08		;LFO FREQUENCY FINE TUNE BIT 1
OSCLF2			EQU	0x10		;LFO FREQUENCY FINE TUNE BIT 2
OSCLF3			EQU	0x20		;LFO FREQUENCY FINE TUNE BIT 3
OSCLRDY			EQU	0x40		;LFO READY
OCSLEN			EQU	0x80		;LFO ENABLE

;PCA 0 MODE BITS
BIT_ECF			BIT	ACC.0		;PCA 0 COUNTER/TIMER OVERFLOW INTERRUPT ENABLE
BIT_CPS0		BIT	ACC.1		;PCA 0 COUNTER/TIMER PULSE SELECT BIT 0
BIT_CPS1		BIT	ACC.2		;PCA 0 COUNTER/TIMER PULSE SELECT BIT 1
BIT_CPS2		BIT	ACC.3		;PCA 0 COUNTER/TIMER PULSE SELECT BIT 2
												;BIT 4 RESERVED
BIT_WDLCK		BIT	ACC.5		;WATCHDOG LOCK
BIT_WDTE		BIT	ACC.6		;WATCHDOG ENABLE
BIT_CIDL		BIT	ACC.7		;PCA 0 COUNTER/TIMER IDLE CONTROL

;PCA 0 MODE CONSTANTS
ECF					EQU	0x01		;PCA 0 COUNTER/TIMER OVERFLOW INTERRUPT ENABLE
CPS					EQU	0x0E
CPS0				EQU	0x02		;PCA 0 COUNTER/TIMER PULSE SELECT BIT 0
CPS1				EQU	0x04		;PCA 0 COUNTER/TIMER PULSE SELECT BIT 1
CPS2				EQU	0x08		;PCA 0 COUNTER/TIMER PULSE SELECT BIT 2
												;BIT 4 RESERVED
WDLCK				EQU	0x20		;WATCHDOG LOCK
WDTE				EQU	0x40		;WATCHDOG ENABLE
CIDL				EQU	0x80		;PCA 0 COUNTER/TIMER IDLE CONTROL

;PCA MODULE MODE BITS
BIT_ECCF				BIT	ACC.0	;MODULE CAPTURE/COMPARE INTERRUPT ENABLE
BIT_PWM					BIT	ACC.1	;MODULE PWM ENABLE
BIT_TOG					BIT	ACC.2	;MODULE TOGGLE ENABLE
BIT_MAT					BIT ACC.3	;MODULE MATCH ENABLE
BIT_CAPN				BIT	ACC.4	;MODULE CAPTURE NEGATIVE ENABLE
BIT_CAPP				BIT	ACC.5	;MODULE CAPTURE POSITIVE ENABLE
BIT_ECOM				BIT	ACC.6	;MODULE COMPARE FUNCTION ENABLE
BIT_PWM16				BIT	ACC.7	;MODULE 16-BIT PWM ENABLE

;PCA MODULE MODE BIT INDEX CONSTANTS
BIT_INDEX_ECCF	EQU	0x00	;MODULE CAPTURE/COMPARE INTERRUPT ENABLE
BIT_INDEX_PWM		EQU	0x01	;MODULE PWM ENABLE
BIT_INDEX_TOG		EQU	0x02	;MODULE TOGGLE ENABLE
BIT_INDEX_MAT		EQU	0x03	;MODULE MATCH ENABLE
BIT_INDEX_CAPN	EQU	0x04	;MODULE CAPTURE NEGATIVE ENABLE
BIT_INDEX_CAPP	EQU	0x05	;MODULE CAPTURE POSITIVE ENABLE
BIT_INDEX_ECOM	EQU	0x06	;MODULE COMPARE FUNCTION ENABLE
BIT_INDEX_PWM16	EQU	0x07	;MODULE 16-BIT PWM ENABLE

;PCA MODULE MODE CONSTANTS
ECCF						EQU	0x01	;MODULE CAPTURE/COMPARE INTERRUPT ENABLE
PWM							EQU	0x02	;MODULE PWM ENABLE
TOG							EQU	0x04	;MODULE TOGGLE ENABLE
MAT							EQU	0x08	;MODULE MATCH ENABLE
CAPN						EQU	0x10	;MODULE CAPTURE NEGATIVE ENABLE
CAPP						EQU	0x20	;MODULE CAPTURE POSITIVE ENABLE
ECOM						EQU	0x40	;MODULE COMPARE FUNCTION ENABLE
PWM16						EQU	0x80	;MODULE 16-BIT PWM ENABLE

;PREFETCH ENGINE CONTROL
FLBWE				EQU	0x01		;FLASH BLOCK WRITE ENABLE
PFEN				EQU	0x20		;PREFETCH ENABLE

;PROGRAM STORE CONTROL BITS
BIT_PSWE		BIT	ACC.0		;PROGRAM STORE WRITE ENABLE
BIT_PSEE		BIT	ACC.1		;PROGRAM STORE ERASE ENABLE
												;BIT 2 RESERVED
												;BIT 3 RESERVED
												;BIT 4 RESERVED
												;BIT 5 RESERVED
												;BIT 6 RESERVED
												;BIT 7 RESERVED

;PROGRAM STORE CONTROL CONSTANTS
PSWE				EQU	0x01		;PROGRAM STORE WRITE ENABLE
PSEE				EQU	0x02		;PROGRAM STORE ERASE ENABLE
												;BIT 2 RESERVED
												;BIT 3 RESERVED
												;BIT 4 RESERVED
												;BIT 5 RESERVED
												;BIT 6 RESERVED
												;BIT 7 RESERVED

;RESET CONFIGURATION BITS
BIT_PINRSF	BIT	ACC.0		;HARDWARE RESET FLAG
BIT_PORSF		BIT	ACC.1		;POWER ON/VDD MONITOR RESET & ENABLE
BIT_MCDRSF	BIT	ACC.2		;MISSING CLOCK DETECTOR RESET & ENABLE
BIT_WDTRSF	BIT	ACC.3		;WATCHDOG TIMER RESET FLAG
BIT_SWRSF		BIT	ACC.4		;SOFTWARE RESET FLAG
BIT_C0RSEF	BIT	ACC.5		;COMPARATOR 0 RESET & ENABLE
BIT_FERROR	BIT	ACC.6		;FLASH ERROR RESET FLAG
BIT_USBRSF	BIT	ACC.7		;USB RESET & ENABLE

;RESET CONFIGURATION CONSTANTS
PINRSF			EQU	0x01		;HARDWARE RESET FLAG
PORSF				EQU	0x02		;POWER ON/VDD MONITOR RESET & ENABLE
MCDRSF			EQU	0x04		;MISSING CLOCK DETECTOR RESET & ENABLE
WDTRSF			EQU	0x08		;WATCHDOG TIMER RESET FLAG
SWRSF				EQU	0x10		;SOFTWARE RESET FLAG
C0RSEF			EQU	0x20		;COMPARATOR 0 RESET & ENABLE
FERROR			EQU	0x40		;FLASH ERROR RESET FLAG
USBRSF			EQU	0x80		;USB RESET & ENABLE

;SPI 0 CONFIGURATION BITS
BIT_RXBMT		BIT	ACC.0		;RECEIVE BUFFER EMPTY
BIT_SRMT		BIT	ACC.1		;SHIFT REGISTER EMPTY
BIT_NSSIN		BIT	ACC.2		;NSS INSTANTANEOUS INPUT
BIT_SLVSEL	BIT	ACC.3		;SLAVE SELECTED
BIT_CKPOL		BIT	ACC.4		;CLOCK POLARITY
BIT_CKPHA		BIT	ACC.5		;CLOCK PHASE
BIT_MSTEN		BIT	ACC.6		;MASTER MODE ENABLE
BIT_SPIBSY	BIT	ACC.7		;SPI BUSY

;SPI 0 CONFIGURATION CONSTANTS
RXBMT				EQU	0x01		;RECEIVE BUFFER EMPTY
SRMT				EQU	0x02		;SHIFT REGISTER EMPTY
NSSIN				EQU	0x04		;NSS INSTANTANEOUS INPUT
SLVSEL			EQU	0x08		;SLAVE SELECTED
CKPOL				EQU	0x10		;CLOCK POLARITY
CKPHA				EQU	0x20		;CLOCK PHASE
MSTEN				EQU	0x40		;MASTER MODE ENABLE
SPIBSY			EQU	0x80		;SPI BUSY

;TIMER 3 CONTROL BITS
BIT_T3XCLK	BIT	ACC.0		;TIMER 3 EXTERNAL CLOCK SELECT
BIT_T3CSS		BIT	ACC.1		;TIMER 3 CAPTURE SOURCE SELECT
BIT_TR3			BIT	ACC.2		;TIMER 3 RUN CONTROL
BIT_T3SPLIT	BIT	ACC.3		;TIMER 3 SPLIT ENABLE
BIT_TF3CEN	BIT	ACC.4		;TIMER 3 LFO CAPTURE ENABLE
BIT_TF3LEN	BIT	ACC.5		;TIMER 3 LOW BYTE INTERRUPT ENABLE
BIT_TF3L		BIT	ACC.6		;TIMER 3 LOW BYTE OVERFLOW
BIT_TF3H		BIT	ACC.7		;TIMER 3 HIGH BYTE OVERFLOW

;TIMER 3 CONTROL CONSTANTS
T3XCLK			EQU	0x01		;TIMER 3 EXTERNAL CLOCK SELECT
T3CSS				EQU	0x02		;TIMER 3 CAPTURE SOURCE SELECT
TR3					EQU	0x04		;TIMER 3 RUN CONTROL
T3SPLIT			EQU	0x08		;TIMER 3 SPLIT ENABLE
TF3CEN			EQU	0x10		;TIMER 3 LFO CAPTURE ENABLE
TF3LEN			EQU	0x20		;TIMER 3 LOW BYTE INTERRUPT ENABLE
TF3L				EQU	0x40		;TIMER 3 LOW BYTE OVERFLOW
TF3H				EQU	0x80		;TIMER 3 HIGH BYTE OVERFLOW

;TIMER 4 CONTROL BITS
BIT_T4XCLK	BIT	ACC.0		;TIMER 4 EXTERNAL CLOCK SELECT
												;BIT 1 RESERVED
BIT_TR4			BIT	ACC.2		;TIMER 4 RUN CONTROL
BIT_T4SPLIT	BIT	ACC.3		;TIMER 4 SPLIT ENABLE
												;BIT 4 RESERVED
BIT_TF4LEN	BIT	ACC.5		;TIMER 4 LOW BYTE INTERRUPT ENABLE
BIT_TF4L		BIT	ACC.6		;TIMER 4 LOW BYTE OVERFLOW
BIT_TF4H		BIT	ACC.7		;TIMER 4 HIGH BYTE OVERFLOW

;TIMER 4 CONTROL CONSTANTS
T4XCLK			EQU	0x01		;TIMER 4 EXTERNAL CLOCK SELECT
TR4					EQU	0x04		;TIMER 4 RUN CONTROL
T4SPLIT			EQU	0x08		;TIMER 4 SPLIT ENABLE
TF4LEN			EQU	0x20		;TIMER 4 LOW BYTE INTERRUPT ENABLE
TF4L				EQU	0x40		;TIMER 4 LOW BYTE OVERFLOW
TF4H				EQU	0x80		;TIMER 4 HIGH BYTE OVERFLOW

;UART 1 CONTROL BITS
BIT_RI1			BIT	ACC.0		;UART 1 RECEIVE INTERRUPT
BIT_TI1			BIT	ACC.1		;UART 1 TRANSMIT INTERRUPT
BIT_RBX1		BIT	ACC.2		;UART 1 EXTRA RECEIVE BIT
BIT_TBX1		BIT	ACC.3		;UART 1 EXTRA TRANSMIT BIT
BIT_REN1		BIT	ACC.4		;UART 1 RECEIVE ENABLE
BIT_THRE1		BIT	ACC.5		;UART 1 TRANSMIT HOLDING REGISTER EMPTY
BIT_PERR1		BIT	ACC.6		;UART 1 PARITY ERROR
BIT_OVR1		BIT	ACC.7		;UART 1 RECEIVE OVERFLOW

;UART 1 CONTROL CONSTANTS
RI1					EQU	0x01		;UART 1 RECEIVE INTERRUPT
TI1					EQU	0x02		;UART 1 TRANSMIT INTERRUPT
RBX1				EQU	0x04		;UART 1 EXTRA RECEIVE BIT
TBX1				EQU	0x08		;UART 1 EXTRA TRANSMIT BIT
REN1				EQU	0x10		;UART 1 RECEIVE ENABLE
THRE1				EQU	0x20		;UART 1 TRANSMIT HOLDING REGISTER EMPTY
PERR1				EQU	0x40		;UART 1 PARITY ERROR
OVR1				EQU	0x80		;UART 1 RECEIVE OVERFLOW

;UART 1 MODE BITS
BIT_SBL1		BIT	ACC.0		;UART 1 STOP BIT LENGTH
BIT_XBE1		BIT	ACC.1		;UART 1 EXTRA TRANSMIT BIT ENABLE
BIT_S1DL0		BIT	ACC.2		;UART 1 DATA LENGTH BIT 0
BIT_S1DL1		BIT	ACC.3		;UART 1 DATA LENGTH BIT 1
BIT_PE1			BIT	ACC.4		;UART 1 PARITY ENABLE
BIT_S1PT0		BIT	ACC.5		;UART 1 PARITY TYPE BIT 0
BIT_S1PT1		BIT	ACC.6		;UART 1 PARITY TYPE BIT 1
BIT_MCE1		BIT	ACC.7		;UART 1 MULTIPROCESSOR COMMUNICATION ENABLE

;UART 1 MODE CONSTANTS
SBL1				EQU	0x01		;UART 1 STOP BIT LENGTH
XBE1				EQU	0x02		;UART 1 EXTRA TRANSMIT BIT ENABLE
S1DL				EQU	0x0C		;UART 1 DATA LENGTH SELECT BITS
S1DL0				EQU	0x04		;UART 1 DATA LENGTH BIT 0
S1DL1				EQU	0x08		;UART 1 DATA LENGTH BIT 1
PE1					EQU	0x10		;UART 1 PARITY ENABLE
S1PT				EQU	0x60		;UART 1 PARITY TYPE BITS
S1PT0				EQU	0x20		;UART 1 PARITY TYPE BIT 0
S1PT1				EQU	0x40		;UART 1 PARITY TYPE BIT 1
MCE1				EQU	0x80		;UART 1 MULTIPROCESSOR COMMUNICATION ENABLE

;UART 1 BAUD RATE GENERATOR CONTROL BITS
BIT_SB1PS0	BIT	ACC.0		;UART 1 PRESCALER SELECT BIT 0
BIT_SB1PS1	BIT	ACC.1		;UART 1 PRESCALER SELECT BIT 1
												;BIT 2 RESERVED
												;BIT 3 RESERVED
												;BIT 4 RESERVED
												;BIT 5 RESERVED
BIT_SB1RUN	BIT	ACC.6		;UART 1 BAUD RATE GENERATOR ENABLE
												;BIT 7 RESERVED

;UART 1 BAUD RATE GENERATOR CONTROL CONSTANTS
SB1PS				EQU	0x03		;UART 1 PRESCALER SELECT BITS
SB1PS0			EQU	0x01		;UART 1 PRESCALER SELECT BIT 0
SB1PS1			EQU	0x02		;UART 1 PRESCALER SELECT BIT 1
												;BIT 2 RESERVED
												;BIT 3 RESERVED
												;BIT 4 RESERVED
												;BIT 5 RESERVED
SB1RUN			EQU	0x40		;UART 1 BAUD RATE GENERATOR ENABLE
												;BIT 7 RESERVED