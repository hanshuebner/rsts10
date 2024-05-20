;
; TKB CMD file for MACRO-11/RSX 5.5
; For RSTS/E
; using default library LB:SYSLIB
; Created by MACRO-11 baselevel procedure
;
SY:MACRST/-FP/CP/-MU,SY:MACRST/-SP/CR/MA=
SY:MACRSTBLD/MP
;
; Option input
;
UNITS=9
;
; LUN 1 = Static  for .GCML
; LUN 2 = Static  for error messages
; LUN 3 = Dynamic for object file
; LUN 4 = Dynamic for listing file
; LUN 5 = Dynamic for input file
; LUN 6 = Dynamic for macro library
; LUN 7 = Dynamic for cross reference file
; LUN 8 = Static (See below) for .INCLUDE / .LIBRARY
; LUN 9 = Static (See below) for workfile
;
ASG=TI:1:2,OV:9
TASK=...MAC
PAR=GEN:0:177700
;
; Set stack size
;
; Note: The stack size controls nested expression depth
;
STACK=192
;
; Inhibit inclusion of unused fcs read/write long code
;
GBLDEF=..RWLG:0
GBLDEF=$PGLEN:0
;
; Specify global definitions for the virtual memory routines
;
GBLDEF=TM$LUN:10		;.INCLUDE/.LIBRARY lun
GBLDEF=W$KLUN:11		;Work file lun
GBLDEF=N$MPAG:10		;Page threshold for fast search
GBLDEF=W$KEXT:31		;Work file extension (25. Contig blks)
GBLDEF=W$BSIZ:31		;Initial work file size (25. Contig blks)
;
; Specify global definitions to control task expansion
;	(Works only on a mapped RSX-11 system)
;
GBLDEF=T$KINC:41		;Task extension (32WD blocks)
GBLDEF=T$KMAX:177700		;Maximum task size
;
; Specify global definitions to control initial hash table allocation
; for permanent and user symbol tables
;
GBLDEF=N$PSPG:5			;Initial number of permanent symbol
;				; table pages.
GBLDEF=N$SYPG:30		;Initial number of user symbol
;				; table pages.  Decrease for
;				; smaller, slower task.
;
;
; Specify default output page size
;
;	GBLPAT=MACRO:LINPPG:OOOOOO
;
;	Where "oooooo" represents an octal number. Note that
;	macro always uses the first three lines at the top of
;	the page for title, sub-title, and a blank line.
;
; Default from assembly is 60. lines (3 header + 57 source lines)
;
;
; Specify default listing output format
;
;	GBLPAT=MACRO:LCBITS:XXXXXX
;
;	Where the following bit significance applies
;	(Bit asserted implies ".NLIST"):
;
;		BEX = 2
;		BIN = 4
;		CND = 10
;		COM = 20
;		HEX = 40
;		LOC = 100
;		MC  = 200
;		MD  = 400
;		ME  = 1000
;		MEB = 2000
;		SEQ = 4000
;		SRC = 10000
;		SYM = 20000
;		TOC = 40000
;		TTM = 100000
;
; The default from assembly is 103040 = ".NLIST HEX,ME,MEB,TTM"
;
;
; Specify default .ENABL/.DSABL options
;
;	GBLPAT=MACRO:EDBITS:XXXXXX
;
;	Where the following bit significance applies
;	(Bit asserted implies ".DSABL"):
;
;		ABS = 1
;		AMA = 2
;		CDR = 10
;		CRF = 20
;		DBG = 40	(Internal flag only)
;		FPT = 100
;		GBL = 200
;		LC  = 400
;		LCM = 1000
;		LSB = 2000
;		MCL = 4000
;		PNC = 10000
;		REG = 20000
;
; The default from assembly is 007157 = ".DSABL ABS,AMA,CDR,DBG,FPT,
;                                              LCM,LSB,MCL"
;
; Specify listing file extension size
;
;	Positive means contiguous extend and
;  negative means non-contiguous.
;
;  177761 Octal  = -15. Decimal
;
; Default is 15. non-contiguous blocks.
;
GBLDEF=$LSEXT:177761
;
;
; Specify default polarity of RSX-11 "/[-]SP" switch.
;
; Removing the semi-colon in front of the next GBLPAT
; changes the default spooling action from /-SP to
; /SP.
;
;GBLPAT=MACIO:$SPOPT:0
;
/
